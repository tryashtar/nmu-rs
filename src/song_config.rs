use jwalk::WalkDir;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::{
    borrow::{Borrow, Cow},
    collections::{HashMap, HashSet},
    ops::Deref,
    path::{Component, Path, PathBuf},
    rc::Rc,
};
use strum::{Display, EnumIter, IntoEnumIterator};

use crate::library_config::LibraryConfig;

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
            Self::Owned(ref owned) => owned.borrow(),
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
    Moded {
        mode: CombineMode,
        values: HashMap<MetadataField, ValueGetter>,
    },
    Sequence(Vec<Borrowable<'a, MetadataOperation<'a>>>),
    Set(HashMap<MetadataField, ValueGetter>),
}
impl MetadataOperation<'_> {
    pub fn apply<'a>(
        &'a self,
        metadata: &mut PendingMetadata<'a>,
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
            Self::Moded { mode, values } => {
                for (field, value) in values {
                    if let Ok(PendingValue::Ready(adding)) = value.get(path, config) {
                        match metadata.fields.get(field) {
                            None => {
                                metadata.fields.insert(field.clone(), adding.into());
                            }
                            Some(PendingValue::Ready(existing)) => {
                                if let Ok(combined) = existing.combine(&adding, mode) {
                                    metadata.fields.insert(field.clone(), combined.into());
                                }
                            }
                            Some(_) => {}
                        }
                    }
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
pub fn file_path(item: jwalk::Result<jwalk::DirEntry<((), ())>>) -> Option<PathBuf> {
    match item {
        Err(_) => None,
        Ok(entry) => {
            let path: PathBuf = entry.path();
            if path.extension().and_then(|x| x.to_str()).is_some() {
                Some(path)
            } else {
                None
            }
        }
    }
}
pub fn match_extension(path: &Path, extensions: &HashSet<String>) -> bool {
    match path.extension().and_then(|x| x.to_str()) {
        Some(ext) => extensions.contains(ext),
        None => false,
    }
}
pub fn match_name(path: &Path, name: &str) -> bool {
    match path.file_name().and_then(|x| x.to_str()) {
        Some(file_name) => file_name == name,
        None => false,
    }
}
impl ItemSelector {
    pub fn matches(&self, start: &Path, check_path: &Path, config: &LibraryConfig) -> bool {
        match self {
            Self::All => true,
            Self::Multi(checks) => checks.iter().any(|x| x.matches(start, check_path, config)),
            Self::Path(path) => check_path.starts_with(path),
            Self::Segmented { path } => {
                let components = check_path.components().collect::<Vec<_>>();
                if path.len() > components.len() {
                    return false;
                };
                path.iter()
                    .zip(components.iter())
                    .all(|(x, y)| x.matches(&y))
            }
            Self::Subpath { subpath, select } => self
                .find_matches(start, config)
                .into_iter()
                .any(|x| x == check_path),
        }
    }
    pub fn find_matches(&self, start: &Path, config: &LibraryConfig) -> Vec<PathBuf> {
        let full_start = config.library_folder.join(start);
        match self {
            Self::All => WalkDir::new(full_start)
                .into_iter()
                .filter_map(file_path)
                .filter(|x| match_extension(x, &config.song_extensions))
                .filter_map(|x| {
                    x.strip_prefix(&config.library_folder)
                        .ok()
                        .map(|x| x.to_owned())
                })
                .collect(),
            Self::Multi(checks) => checks
                .iter()
                .flat_map(|x| x.find_matches(start, config))
                .collect(),
            Self::Path(path) => {
                if let Some(name) = full_start.file_name().and_then(|x| x.to_str()) {
                    WalkDir::new(full_start.parent().unwrap_or(Path::new("")))
                        .into_iter()
                        .filter_map(file_path)
                        .filter(|x| match_name(x.with_extension("").as_path(), name))
                        .filter_map(|x| {
                            x.strip_prefix(&config.library_folder)
                                .ok()
                                .map(|x| x.to_owned())
                        })
                        .collect()
                } else {
                    vec![]
                }
            }
            Self::Segmented { path } => {
                let mut items = vec![full_start];
                for segment in path {
                    items = items
                        .into_iter()
                        .flat_map(|x| WalkDir::new(x).into_iter().filter_map(file_path))
                        .filter_map(|x| {
                            x.strip_prefix(&config.library_folder)
                                .ok()
                                .map(|x| x.to_owned())
                        })
                        .collect();
                }
                items
            }
            Self::Subpath { subpath, select } => subpath
                .find_matches(start, config)
                .into_iter()
                .flat_map(|x| select.find_matches(x.as_path(), config))
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
    fn matches(&self, component: &Component) -> bool {
        let str = component.as_os_str().to_str();
        match str {
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
            Self::Selector { selector } => selector.find_matches(start, config),
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
        let (start, stop) = self.wrap_both(length, decision);
        index >= start && index <= stop
    }
    pub fn slice<'a, T>(&self, items: &'a [T], decision: OutOfBoundsDecision) -> &'a [T] {
        let (start, stop) = self.wrap_both(items.len(), decision);
        if stop > items.len() {
            &[]
        } else {
            &items[start..=stop]
        }
    }
    fn wrap_both(&self, length: usize, decision: OutOfBoundsDecision) -> (usize, usize) {
        (
            Self::wrap(self.start, length, decision),
            Self::wrap(self.stop, length, decision),
        )
    }
    fn wrap(index: i32, length: usize, decision: OutOfBoundsDecision) -> usize {
        let result = if index >= 0 {
            index as usize
        } else {
            ((length as i32) + index) as usize
        };
        match decision {
            OutOfBoundsDecision::Exit => result,
            OutOfBoundsDecision::Clamp => std::cmp::min(result, length - 1),
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
        modify: Option<ValueModifier>,
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
                modify: modify.as_ref(),
                sources: from.get(path, config),
            }),
        }
    }
}

fn default_value() -> FieldValueGetter {
    FieldValueGetter::CleanName
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum NoSeparatorDecision {
    Exit,
    Ignore,
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
    Join {
        join: Box<ValueGetter>,
    },
    Split {
        split: String,
        #[serde(default = "default_sep")]
        when_none: NoSeparatorDecision,
    },
    Regex {
        #[serde(with = "serde_regex")]
        regex: Regex,
    },
    Group {
        group: String,
    },
    Take {
        take: TakeModifier,
    },
    Sequence(Vec<ValueModifier>),
}
fn default_sep() -> NoSeparatorDecision {
    NoSeparatorDecision::Ignore
}

#[derive(Clone)]
pub enum PendingValue<'a> {
    Ready(MetadataValue),
    RegexMatches(HashMap<String, String>),
    CopyField {
        field: MetadataField,
        sources: Vec<PathBuf>,
        modify: Option<&'a ValueModifier>,
    },
}
impl From<MetadataValue> for PendingValue<'_> {
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
            Self::Group { group } => {
                if let PendingValue::RegexMatches(matches) = value {
                    if let Some(capture) = matches.get(group) {
                        return Ok(MetadataValue::string(capture.to_owned()).into());
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Regex { regex } => {
                if let PendingValue::Ready(val) = value {
                    if let Some(str) = val.as_string() {
                        if let Some(captures) = regex.captures(str) {
                            let values: HashMap<String, String> = regex
                                .capture_names()
                                .filter_map(|x| {
                                    x.and_then(|y| {
                                        captures
                                            .name(y)
                                            .map(|z| (y.to_owned(), z.as_str().to_owned()))
                                    })
                                })
                                .collect();
                            return Ok(PendingValue::RegexMatches(values));
                        } else {
                            return Err(ValueError::NoMatchFound);
                        }
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
            Self::Split { split, when_none } => {
                if let PendingValue::Ready(MetadataValue::List(list)) = value {
                    let vec = list
                        .iter()
                        .flat_map(|x| x.split(split))
                        .map(|x| x.to_owned())
                        .collect::<Vec<_>>();
                    if let NoSeparatorDecision::Exit = when_none {
                        if vec.len() == 1 {
                            return Err(ValueError::ConditionsNotMet);
                        }
                    }
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

pub struct PendingMetadata<'a> {
    pub fields: HashMap<MetadataField, PendingValue<'a>>,
}
pub struct Metadata {
    pub fields: HashMap<MetadataField, MetadataValue>,
}
impl PendingMetadata<'_> {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }
    pub fn resolve(self) -> Metadata {
        Metadata {
            fields: self
                .fields
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        match v {
                            PendingValue::Ready(ready) => ready,
                            _ => MetadataValue::blank(),
                        },
                    )
                })
                .collect(),
        }
    }
}
impl From<Metadata> for PendingMetadata<'_> {
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
impl MetadataValue {
    fn combine(&self, other: &Self, mode: &CombineMode) -> Result<Self, ValueError> {
        match mode {
            CombineMode::Replace => Ok(other.clone()),
            CombineMode::Append => {
                let mut list1 = self.to_list()?.clone();
                let mut list2 = other.to_list()?.clone();
                list1.append(&mut list2);
                Ok(Self::List(list1))
            }
            CombineMode::Prepend => {
                let mut list1 = other.to_list()?.clone();
                let mut list2 = self.to_list()?.clone();
                list1.append(&mut list2);
                Ok(Self::List(list1))
            }
        }
    }
    fn to_list(&self) -> Result<&Vec<String>, ValueError> {
        match self {
            Self::List(list) => Ok(list),
            Self::Number(_) => Err(ValueError::UnexpectedType),
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
