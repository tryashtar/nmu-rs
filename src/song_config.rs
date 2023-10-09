use colored::Colorize;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::{
    borrow::{Borrow, Cow},
    collections::{HashMap, HashSet},
    ffi::OsStr,
    io::ErrorKind,
    ops::Deref,
    path::{Path, PathBuf},
    rc::Rc,
};
use strum::{Display, EnumIter, IntoEnumIterator};
use thiserror::Error;

use crate::{
    get_metadata,
    library_config::{load_yaml, LibraryConfig, LibraryError, YamlError},
};

#[derive(Deserialize, Serialize)]
pub struct RawSongConfig<'a> {
    pub songs: Option<ReferencableOperation<'a>>,
    pub set: Option<HashMap<PathBuf, ReferencableOperation<'a>>>,
    #[serde(rename = "set all")]
    pub set_all: Option<Vec<RawAllSetter<'a>>>,
}

#[derive(Deserialize, Serialize)]
pub struct RawAllSetter<'a> {
    pub names: ItemSelector,
    pub set: ReferencableOperation<'a>,
}

pub struct SongConfig<'a> {
    pub set: Vec<AllSetter<'a>>,
}

pub struct AllSetter<'a> {
    pub names: ItemSelector,
    pub set: Borrowable<'a, MetadataOperation<'a>>,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum Borrowable<'a, T>
where
    T: 'a,
{
    #[serde(skip)]
    Borrowed(&'a T),
    Owned(T),
}
impl<T> AsRef<T> for Borrowable<'_, T> {
    fn as_ref(&self) -> &T {
        self
    }
}
impl<'a, T> Borrow<T> for Borrowable<'a, T>
where
    T: ToOwned,
{
    fn borrow(&self) -> &T {
        self
    }
}
impl<B> Deref for Borrowable<'_, B> {
    type Target = B;

    fn deref(&self) -> &B {
        match *self {
            Self::Borrowed(borrowed) => borrowed,
            Self::Owned(ref owned) => owned,
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ReferencableOperation<'a> {
    Reference(String),
    Sequence(Vec<ReferencableOperation<'a>>),
    Direct(MetadataOperation<'a>),
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum MetadataOperation<'a> {
    Blank {
        remove: FieldSelector,
    },
    Keep {
        keep: FieldSelector,
    },
    Context {
        source: ValueGetter,
        modify: HashMap<MetadataField, ValueModifier>,
    },
    Modify {
        modify: HashMap<MetadataField, ValueModifier>,
    },
    Sequence(Vec<Borrowable<'a, MetadataOperation<'a>>>),
    Set(HashMap<MetadataField, ValueGetter>),
}
impl MetadataOperation<'_> {
    pub fn apply(
        &self,
        metadata: &mut PendingMetadata,
        path: &Path,
        config: &LibraryConfig,
    ) {
        match self {
            Self::Blank { remove } => {
                for field in &config.custom_fields {
                    if remove.is_match(field) {
                        metadata
                            .fields
                            .insert(field.clone(), MetadataValue::blank().into());
                    }
                }
                for field in BuiltinMetadataField::iter() {
                    let builtin = field.into();
                    if remove.is_match(&builtin) {
                        metadata
                            .fields
                            .insert(builtin, MetadataValue::blank().into());
                    }
                }
            }
            Self::Keep { keep } => {
                metadata.fields.retain(|k, _| !keep.is_match(k));
            }
            Self::Set(set) => {
                for (field, value) in set {
                    if let Ok(value) = value.get(path, config) {
                        metadata.fields.insert(field.clone(), value);
                    }
                }
            }
            Self::Context { source, modify } => {
                if let Ok(value) = source.get(path, config) {
                    for (field, modifier) in modify {
                        if let Ok(modified) = modifier.modify(value.clone(), path, config) {
                            metadata.fields.insert(field.clone(), modified);
                        }
                    }
                }
            }
            Self::Sequence(many) => {
                for op in many {
                    op.apply(metadata, path, config);
                }
            }
            Self::Modify { modify } => {
                for (field, modifier) in modify {
                    if let Some(existing) = metadata.fields.get(field) {
                        if let Ok(modified) = modifier.modify(existing.clone(), path, config) {
                            metadata.fields.insert(field.clone(), modified);
                        }
                    }
                }
            }
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum CombineMode {
    Replace,
    Append,
    Prepend,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ItemSelector {
    All,
    Path(PathBuf),
    Multi(Vec<ItemSelector>),
    Segmented {
        path: Vec<PathSegment>,
    },
    Subpath {
        subpath: Box<ItemSelector>,
        select: Box<ItemSelector>,
    },
}
impl ItemSelector {
    pub fn matches(&self, check_path: &Path) -> bool {
        match self {
            Self::All => true,
            Self::Multi(checks) => checks.iter().any(|x| x.matches(check_path)),
            Self::Path(path) => check_path.starts_with(path),
            Self::Segmented { path } => {
                let components = check_path.components().collect::<Vec<_>>();
                if path.len() > components.len() {
                    return false;
                };
                path.iter()
                    .zip(components.iter())
                    .all(|(x, y)| x.matches(y.as_os_str()))
            }
            Self::Subpath { .. } => !self.consume(check_path).is_empty(),
        }
    }
    fn consume<'a>(&self, check_path: &'a Path) -> Vec<&'a Path> {
        match self {
            Self::All => vec![check_path],
            Self::Path(path) => check_path.strip_prefix(path).into_iter().collect(),
            Self::Multi(multi) => multi.iter().flat_map(|x| x.consume(check_path)).collect(),
            Self::Segmented { path } => {
                let components = check_path.components().collect::<Vec<_>>();
                if path.len() > components.len() {
                    return vec![];
                }
                for (segment, component) in path.iter().zip(components.iter()) {
                    if !segment.matches(component.as_os_str()) {
                        return vec![];
                    }
                }
                check_path
                    .strip_prefix(components.into_iter().take(path.len()).collect::<PathBuf>())
                    .into_iter()
                    .collect()
            }
            Self::Subpath { subpath, select } => subpath
                .consume(check_path)
                .into_iter()
                .flat_map(|x| select.consume(x))
                .collect(),
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum PathSegment {
    Literal(String),
    Regex {
        #[serde(with = "serde_regex")]
        regex: Regex,
    },
}
impl PathSegment {
    pub fn matches(&self, component: &OsStr) -> bool {
        match component.to_str() {
            None => false,
            Some(str) => match self {
                Self::Literal(literal) => str == literal,
                Self::Regex { regex } => regex.is_match(str),
            },
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
#[serde(untagged)]
pub enum LocalItemSelector {
    #[serde(deserialize_with = "item_selector_this")]
    This,
    DrillUp {
        up: Range,
    },
    DrillDown {
        must_be: Option<MusicItemType>,
        from_root: Range,
    },
    Selector {
        selector: ItemSelector,
    },
}
fn item_selector_this<'de, D>(deserializer: D) -> Result<(), D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    struct Visitor;

    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = ();

        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
            formatter.write_str("string")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            if value == "this" || value == "self" {
                Ok(())
            } else {
                Err(serde::de::Error::custom("invalid"))
            }
        }
    }

    deserializer.deserialize_str(Visitor)
}
impl LocalItemSelector {
    fn get(&self, start: &Path, config: &LibraryConfig) -> Vec<PathBuf> {
        match self {
            Self::This => vec![start.to_owned()],
            Self::DrillUp { up } => {
                let ancestors = start.ancestors().collect::<Vec<_>>();
                up.slice(ancestors.as_slice(), OutOfBoundsDecision::Clamp)
                    .iter()
                    .map(|x| (*x).to_owned())
                    .collect()
            }
            Self::DrillDown { must_be, from_root } => {
                let ancestors = start.ancestors().collect::<Vec<_>>();
                let last = ancestors.len() - 1;
                let ancestors = ancestors
                    .into_iter()
                    .rev()
                    .enumerate()
                    .map(|(i, x)| match must_be {
                        None => Some(x),
                        Some(MusicItemType::Song) => (i == last).then_some(x),
                        Some(MusicItemType::Folder) => (i != last).then_some(x),
                    })
                    .collect::<Vec<_>>();
                from_root
                    .slice(ancestors.as_slice(), OutOfBoundsDecision::Clamp)
                    .iter()
                    .filter_map(|x| x.as_deref())
                    .map(|x| x.to_owned())
                    .collect()
            }
            Self::Selector { selector } => crate::file_stuff::find_matches(selector, start, config),
        }
    }
}

#[derive(Serialize)]
pub struct Range {
    pub start: i32,
    pub stop: i32,
}
impl<'de> Deserialize<'de> for Range {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Start,
            Stop,
        }

        struct RangeVisitor;

        impl<'de> serde::de::Visitor<'de> for RangeVisitor {
            type Value = Range;

            fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
                formatter.write_str("Range")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Range, V::Error>
            where
                V: serde::de::SeqAccess<'de>,
            {
                let start = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;
                let stop = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::invalid_length(1, &self))?;
                Ok(Range::new(start, stop))
            }

            fn visit_map<V>(self, mut map: V) -> Result<Range, V::Error>
            where
                V: serde::de::MapAccess<'de>,
            {
                let mut start = None;
                let mut stop = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Start => {
                            if start.is_some() {
                                return Err(serde::de::Error::duplicate_field("start"));
                            }
                            start = Some(map.next_value()?);
                        }
                        Field::Stop => {
                            if stop.is_some() {
                                return Err(serde::de::Error::duplicate_field("stop"));
                            }
                            stop = Some(map.next_value()?);
                        }
                    }
                }
                Ok(Range::new(start.unwrap_or(0), stop.unwrap_or(-1)))
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok((v as i32).into())
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok((v as i32).into())
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match v {
                    "all" => Ok(Range::from_start(0)),
                    "first" => Ok(0.into()),
                    "last" => Ok((-1).into()),
                    _ => Err(serde::de::Error::unknown_variant(
                        v,
                        &["all", "first", "last"],
                    )),
                }
            }
        }

        deserializer.deserialize_any(RangeVisitor)
    }
}

impl Range {
    pub fn new(start: i32, stop: i32) -> Self {
        Self { start, stop }
    }
    pub fn from_start(start: i32) -> Self {
        Self { start, stop: -1 }
    }
}
impl From<i32> for Range {
    fn from(value: i32) -> Self {
        Self {
            start: value,
            stop: value,
        }
    }
}

#[derive(Deserialize, Serialize, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum OutOfBoundsDecision {
    Exit,
    Clamp,
}
impl Range {
    pub fn in_range(&self, index: usize, length: usize, decision: OutOfBoundsDecision) -> bool {
        match self.to_range(length, decision) {
            None => false,
            Some(range) => range.contains(&index),
        }
    }
    pub fn slice<'a, T>(&self, items: &'a [T], decision: OutOfBoundsDecision) -> &'a [T] {
        match self.to_range(items.len(), decision) {
            Some(range) => &items[range],
            None => &[],
        }
    }
    fn to_range(
        &self,
        length: usize,
        decision: OutOfBoundsDecision,
    ) -> Option<std::ops::RangeInclusive<usize>> {
        self.wrap_both(length, decision)
            .map(|(start, stop)| start..=stop)
    }
    fn wrap_both(&self, length: usize, decision: OutOfBoundsDecision) -> Option<(usize, usize)> {
        Option::zip(
            Self::wrap(self.start, length, decision),
            Self::wrap(self.stop, length, decision),
        )
    }
    fn wrap(index: i32, length: usize, decision: OutOfBoundsDecision) -> Option<usize> {
        let result = if index >= 0 {
            index as usize
        } else {
            ((length as i32) + index) as usize
        };
        match decision {
            OutOfBoundsDecision::Exit => (result < length).then_some(result),
            OutOfBoundsDecision::Clamp => Some(std::cmp::min(result, length - 1)),
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum MusicItemType {
    Song,
    Folder,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum FieldSelector {
    #[serde(deserialize_with = "field_selector_all")]
    All,
    Single(MetadataField),
    Multiple(HashSet<MetadataField>),
}
fn field_selector_all<'de, D>(deserializer: D) -> Result<(), D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    struct Visitor;

    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = ();

        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
            formatter.write_str("string")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            if value == "*" {
                Ok(())
            } else {
                Err(serde::de::Error::custom("invalid"))
            }
        }
    }

    deserializer.deserialize_str(Visitor)
}
impl FieldSelector {
    fn is_match(&self, field: &MetadataField) -> bool {
        match self {
            Self::All => true,
            Self::Single(single) => field == single,
            Self::Multiple(set) => set.contains(field),
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ValueGetter {
    Direct(MetadataValue),
    From {
        from: LocalItemSelector,
        #[serde(default = "default_value")]
        value: FieldValueGetter,
        modify: Option<ValueModifier>,
    },
    Copy {
        from: LocalItemSelector,
        copy: MetadataField,
        modify: Option<Rc<ValueModifier>>,
    },
}
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FieldValueGetter {
    FileName,
    CleanName,
    Path,
}
impl ValueGetter {
    fn get(&self, path: &Path, config: &LibraryConfig) -> Result<PendingValue, ValueError> {
        match self {
            Self::Direct(value) => Ok(value.clone().into()),
            Self::From {
                from,
                value,
                modify,
            } => {
                let items = from.get(path, config);
                if items.is_empty() {
                    return Err(ValueError::ItemNotFound);
                }
                let result = MetadataValue::List(
                    items
                        .into_iter()
                        .map(|x| value.get(x.as_path(), config).to_string())
                        .collect(),
                );
                if let Some(modify) = modify {
                    modify.modify(result.into(), path, config)
                } else {
                    Ok(result.into())
                }
            }
            Self::Copy { from, copy, modify } => Ok(PendingValue::CopyField {
                field: copy.clone(),
                modify: modify.clone(),
                sources: from.get(path, config),
            }),
        }
    }
}

fn default_value() -> FieldValueGetter {
    FieldValueGetter::CleanName
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ValueModifier {
    Prepend {
        prepend: Box<ValueGetter>,
        index: Option<Range>,
    },
    Append {
        append: Box<ValueGetter>,
        index: Option<Range>,
    },
    InsertBefore {
        insert: Box<ValueGetter>,
        before: Range,
        #[serde(default = "default_oob")]
        out_of_bounds: OutOfBoundsDecision,
    },
    InsertAfter {
        insert: Box<ValueGetter>,
        after: Range,
        #[serde(default = "default_oob")]
        out_of_bounds: OutOfBoundsDecision,
    },
    Join {
        join: Box<ValueGetter>,
    },
    Split {
        split: String,
    },
    Regex {
        #[serde(with = "serde_regex")]
        regex: Regex,
    },
    Replace {
        replace: String,
    },
    Take {
        take: TakeModifier,
    },
    Sequence(Vec<ValueModifier>),
}

#[derive(Clone)]
pub enum PendingValue {
    Ready(MetadataValue),
    RegexMatches {
        source: String,
        regex: Regex,
    },
    CopyField {
        field: MetadataField,
        sources: Vec<PathBuf>,
        modify: Option<Rc<ValueModifier>>,
    },
}
impl From<MetadataValue> for PendingValue {
    fn from(value: MetadataValue) -> Self {
        PendingValue::Ready(value)
    }
}
impl ValueModifier {
    fn take(
        list: &[String],
        range: &Range,
        oob: OutOfBoundsDecision,
        min_length: Option<usize>,
    ) -> MetadataValue {
        let result = range.slice(list, oob);
        if let Some(min) = min_length {
            if result.len() < min {
                return MetadataValue::blank();
            }
        }
        MetadataValue::List(result.to_vec())
    }
    fn append(
        value: &MetadataValue,
        extra: &str,
        index: Option<&Range>,
        appending: bool,
    ) -> Result<MetadataValue, ValueError> {
        let formatter = if appending {
            |str: &str, extra: &str| format!("{str}{extra}")
        } else {
            |str: &str, extra: &str| format!("{extra}{str}")
        };
        match value {
            MetadataValue::List(list) => Ok(MetadataValue::List(
                list.iter()
                    .enumerate()
                    .map(|(i, x)| match index {
                        None => formatter(x, extra),
                        Some(index) => {
                            if index.in_range(i, list.len(), OutOfBoundsDecision::Clamp) {
                                formatter(x, extra)
                            } else {
                                x.clone()
                            }
                        }
                    })
                    .collect(),
            )),
            MetadataValue::Number(_) => Err(ValueError::UnexpectedType),
        }
    }
    fn modify(
        &self,
        value: PendingValue,
        path: &Path,
        config: &LibraryConfig,
    ) -> Result<PendingValue, ValueError> {
        match self {
            Self::Replace { replace } => {
                if let PendingValue::RegexMatches { source, regex } = value {
                    return Ok(
                        MetadataValue::string(regex.replace(&source, replace).into_owned()).into(),
                    );
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Regex { regex } => {
                if let PendingValue::Ready(val) = value {
                    if let Some(str) = val.as_string() {
                        return Ok(PendingValue::RegexMatches {
                            source: str.to_owned(),
                            regex: regex.clone(),
                        });
                    } else {
                        return Err(ValueError::NoMatchFound);
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::InsertBefore {
                insert,
                before,
                out_of_bounds,
            } => {
                if let PendingValue::Ready(MetadataValue::List(val)) = insert.get(path, config)? {
                    if let PendingValue::Ready(MetadataValue::List(list)) = value {
                        return match Range::to_range(before, list.len(), *out_of_bounds) {
                            None => Err(ValueError::ConditionsNotMet),
                            Some(range) => {
                                let mut result = list.clone();
                                result.splice(range, val);
                                Ok(MetadataValue::List(result).into())
                            }
                        };
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::InsertAfter {
                insert,
                after,
                out_of_bounds,
            } => {
                if let PendingValue::Ready(MetadataValue::List(val)) = insert.get(path, config)? {
                    if let PendingValue::Ready(MetadataValue::List(list)) = value {
                        return match Range::wrap_both(after, list.len(), *out_of_bounds) {
                            None => Err(ValueError::ConditionsNotMet),
                            Some((start, stop)) => {
                                let mut result = list.clone();
                                result.splice((start + 1)..=(stop + 1), val);
                                Ok(MetadataValue::List(result).into())
                            }
                        };
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Take { take } => {
                if let PendingValue::Ready(MetadataValue::List(list)) = value {
                    return match take {
                        TakeModifier::Defined {
                            index,
                            out_of_bounds,
                            min_length,
                        } => Ok(Self::take(
                            &list,
                            index,
                            *out_of_bounds,
                            min_length.as_ref().copied(),
                        )
                        .into()),
                        TakeModifier::Simple(range) => {
                            Ok(Self::take(&list, range, OutOfBoundsDecision::Exit, None).into())
                        }
                    };
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Append { append, index } => {
                if let PendingValue::Ready(val) = append.get(path, config)? {
                    if let Some(str) = val.as_string() {
                        if let PendingValue::Ready(value) = value {
                            return Ok(Self::append(&value, str, index.as_ref(), true)?.into());
                        }
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Prepend { prepend, index } => {
                if let PendingValue::Ready(val) = prepend.get(path, config)? {
                    if let Some(str) = val.as_string() {
                        if let PendingValue::Ready(value) = value {
                            return Ok(Self::append(&value, str, index.as_ref(), false)?.into());
                        }
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Join { join } => {
                if let PendingValue::Ready(extra) = join.get(path, config)? {
                    if let Some(str) = extra.as_string() {
                        if let PendingValue::Ready(MetadataValue::List(list)) = value {
                            return Ok(MetadataValue::string(list.join(str)).into());
                        }
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Split { split } => {
                if let PendingValue::Ready(MetadataValue::List(list)) = value {
                    let vec = list
                        .iter()
                        .flat_map(|x| x.split(split))
                        .map(|x| x.to_owned())
                        .collect::<Vec<_>>();
                    return Ok(MetadataValue::List(vec).into());
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Sequence(seq) => {
                if seq.is_empty() {
                    Err(ValueError::EmptyList)
                } else {
                    let value = seq[0].modify(value, path, config)?;
                    seq.iter()
                        .skip(1)
                        .try_fold(value, |x, y| y.modify(x, path, config))
                }
            }
        }
    }
}

pub enum ValueError {
    UnexpectedType,
    ItemNotFound,
    NoMatchFound,
    EmptyList,
    ConditionsNotMet,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum TakeModifier {
    Simple(Range),
    Defined {
        index: Range,
        #[serde(default = "default_oob")]
        out_of_bounds: OutOfBoundsDecision,
        min_length: Option<usize>,
    },
}
fn default_oob() -> OutOfBoundsDecision {
    OutOfBoundsDecision::Exit
}

impl FieldValueGetter {
    fn file_name(path: &Path) -> Cow<str> {
        path.file_name()
            .map(|x| x.to_string_lossy())
            .unwrap_or_else(|| path.to_string_lossy())
    }
    fn clean<'a>(name: Cow<'a, str>, config: &LibraryConfig) -> Cow<'a, str> {
        let mut result = name;
        for (find, replace) in &config.find_replace {
            result = result.replace(find, replace).into();
        }
        result
    }
    fn get<'a>(&self, path: &'a Path, config: &LibraryConfig) -> Cow<'a, str> {
        match self {
            Self::CleanName => Self::clean(Self::file_name(path), config),
            Self::FileName => Self::file_name(path),
            Self::Path => path.to_string_lossy(),
        }
    }
}

pub struct PendingMetadata {
    pub fields: HashMap<MetadataField, PendingValue>,
}
pub struct Metadata {
    pub fields: HashMap<MetadataField, MetadataValue>,
}

pub fn load_config<'a>(
    path: &Path,
    library_config: &'a LibraryConfig<'a>,
) -> Result<SongConfig<'a>, ConfigError> {
    match load_yaml::<RawSongConfig>(path) {
        Err(YamlError::Io(error)) if error.kind() == ErrorKind::NotFound => {
            Err(ConfigError::Yaml(YamlError::Io(error)))
        }
        Err(error) => {
            eprintln!("{}", path.display().to_string().red());
            eprintln!("{}", error.to_string().red());
            Err(ConfigError::Yaml(error))
        }
        Ok(config) => library_config
            .resolve_config(config)
            .map_err(ConfigError::Library),
    }
}
#[derive(Error, Debug)]
#[error("{0}")]
pub enum ConfigError {
    Yaml(#[from] YamlError),
    Library(#[from] LibraryError),
}
impl PendingMetadata {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }
    pub fn resolve<'a>(
        mut self,
        path: &Path,
        config: &'a LibraryConfig,
        cache: &mut HashMap<PathBuf, Option<SongConfig<'a>>>,
    ) -> Metadata {
        let mut metadata = Metadata::new();
        let mut metadata_cache: HashMap<PathBuf, Metadata> = HashMap::new();
        loop {
            let mut added_any = false;
            self.fields.retain(|field, value| {
                let result = match value {
                    PendingValue::Ready(ready) => {
                        metadata.fields.insert(field.clone(), ready.clone());
                        false
                    }
                    PendingValue::RegexMatches { .. } => true,
                    PendingValue::CopyField {
                        field: from,
                        sources,
                        modify,
                    } => {
                        let source_path = sources[0].clone();
                        let source_path2 = sources[0].clone();
                        match {
                            if path == source_path {
                                &mut metadata
                            } else {
                                metadata_cache
                                    .entry(source_path)
                                    .or_insert_with_key(|key| get_metadata(key, config, cache))
                            }
                        }
                        .fields
                        .get(from)
                        .cloned()
                        .and_then(|x| match modify {
                            Some(modify) => {
                                modify.modify(x.into(), source_path2.as_path(), config).ok()
                            }
                            None => Some(x.into()),
                        }) {
                            Some(PendingValue::Ready(ready)) => {
                                metadata.fields.insert(field.clone(), ready);
                                false
                            }
                            _ => true,
                        }
                    }
                };
                added_any |= !result;
                result
            });
            if !added_any {
                break;
            }
        }
        metadata
    }
}
impl From<Metadata> for PendingMetadata {
    fn from(value: Metadata) -> Self {
        Self {
            fields: value
                .fields
                .into_iter()
                .map(|(k, v)| (k, PendingValue::Ready(v)))
                .collect(),
        }
    }
}
impl Metadata {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Debug, Clone)]
#[serde(untagged)]
pub enum MetadataValue {
    Number(u32),
    #[serde(deserialize_with = "string_or_seq_string")]
    List(Vec<String>),
}
fn string_or_seq_string<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    struct StringOrVec(std::marker::PhantomData<Vec<String>>);

    impl<'de> serde::de::Visitor<'de> for StringOrVec {
        type Value = Vec<String>;

        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
            formatter.write_str("string or list of strings")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(vec![value.to_owned()])
        }

        fn visit_seq<S>(self, visitor: S) -> Result<Self::Value, S::Error>
        where
            S: serde::de::SeqAccess<'de>,
        {
            Deserialize::deserialize(serde::de::value::SeqAccessDeserializer::new(visitor))
        }
    }

    deserializer.deserialize_any(StringOrVec(std::marker::PhantomData))
}
impl MetadataValue {
    pub fn blank() -> MetadataValue {
        MetadataValue::List(vec![])
    }
    pub fn string(single: String) -> MetadataValue {
        MetadataValue::List(vec![single])
    }
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Self::Number(_) => None,
            Self::List(list) => {
                if list.is_empty() {
                    None
                } else {
                    Some(list[0].as_ref())
                }
            }
        }
    }
}

#[derive(Deserialize, Serialize, Eq, Hash, PartialEq, Debug, Clone)]
#[serde(untagged)]
pub enum MetadataField {
    Builtin(BuiltinMetadataField),
    Custom(String),
}

#[derive(Deserialize, Serialize, Eq, Hash, PartialEq, Debug, Display, EnumIter, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum BuiltinMetadataField {
    Title,
    Album,
    #[serde(alias = "performer")]
    Performers,
    #[serde(rename = "album artists")]
    #[serde(alias = "album artist")]
    AlbumArtists,
    #[serde(alias = "composer")]
    Composers,
    Arranger,
    Comment,
    Track,
    #[serde(rename = "track total")]
    #[serde(alias = "track count")]
    TrackTotal,
    Disc,
    #[serde(rename = "disc total")]
    #[serde(alias = "disc count")]
    DiscTotal,
    Year,
    #[serde(alias = "lang")]
    Language,
    #[serde(alias = "genre")]
    Genres,
    Art,
    #[serde(rename = "simple lyrics")]
    #[serde(alias = "lyrics")]
    SimpleLyrics,
}
impl From<BuiltinMetadataField> for MetadataField {
    fn from(value: BuiltinMetadataField) -> Self {
        Self::Builtin(value)
    }
}
