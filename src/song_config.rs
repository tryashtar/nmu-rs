use regex::Regex;
use serde::{de, Deserialize, Serialize};
use std::{
    borrow::{Borrow, Cow},
    collections::{HashMap, HashSet},
    ops::Deref,
    path::{Component, Path, PathBuf},
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
    pub fn apply(&self, metadata: &mut Metadata, path: &Path, config: &LibraryConfig) {
        match self {
            Self::Blank { remove } => {
                for field in &config.custom_fields {
                    if remove.is_match(field) {
                        metadata.fields.insert(field.clone(), MetadataValue::Blank);
                    }
                }
                for field in BuiltinMetadataField::iter() {
                    let builtin = field.into();
                    if remove.is_match(&builtin) {
                        metadata.fields.insert(builtin, MetadataValue::Blank);
                    }
                }
            }
            Self::Keep { keep } => {
                metadata.fields.retain(|k, _| !keep.is_match(k));
            }
            Self::Set(set) => {
                for (field, value) in set {
                    if let Ok(PendingValue::Safe(value)) = value.get(path, config) {
                        metadata.fields.insert(field.clone(), value);
                    }
                }
            }
            Self::Context { source, modify } => {
                if let Ok(value) = source.get(path, config) {
                    for (field, modifier) in modify {
                        if let Ok(PendingValue::Safe(modified)) =
                            modifier.modify(&value, path, config)
                        {
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
                    if let Ok(PendingValue::Safe(adding)) = value.get(path, config) {
                        match metadata.fields.get(field) {
                            None => {
                                metadata.fields.insert(field.clone(), adding);
                            }
                            Some(existing) => {
                                if let Ok(combined) = existing.combine(&adding, mode) {
                                    metadata.fields.insert(field.clone(), combined);
                                }
                            }
                        }
                    }
                }
            }
            Self::Modify { modify } => {
                for (field, modifier) in modify {
                    if let Some(existing) = metadata.fields.get(field) {
                        if let Ok(PendingValue::Safe(modified)) =
                            modifier.modify(&existing.clone().into(), path, config)
                        {
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
        !self.consume(check_path).is_empty()
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
                    if !segment.matches(component) {
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
    This(SelfItemSelector),
    DrillUp {
        up: RawRange,
    },
    DrillDown {
        must_be: Option<MusicItemType>,
        from_root: RawRange,
    },
}
impl LocalItemSelector {
    fn get<'a>(&self, start: &'a Path) -> Vec<&'a Path> {
        match self {
            Self::This(_) => vec![start],
            Self::DrillUp { up } => {
                let range: Range = up.into();
                let ancestors = start.ancestors().collect::<Vec<_>>();
                range
                    .slice(ancestors.as_slice(), OutOfBoundsDecision::Clamp)
                    .to_vec()
            }
            Self::DrillDown { must_be, from_root } => {
                let range: Range = from_root.into();
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
                range
                    .slice(ancestors.as_slice(), OutOfBoundsDecision::Clamp)
                    .iter()
                    .filter_map(|x| *x)
                    .collect::<Vec<_>>()
            }
        }
    }
}

// not directly in LocalItemSelector as a workaround for serde
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum SelfItemSelector {
    #[serde(alias = "self")]
    This,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum RawRange {
    Index(i32),
    Named(NamedRange),
    Tuple(i32, i32),
    StartOnly { start: i32 },
    StopOnly { stop: i32 },
    StartStop { start: i32, stop: i32 },
}
pub struct Range {
    start: i32,
    stop: i32,
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
impl From<&RawRange> for Range {
    fn from(value: &RawRange) -> Self {
        match value {
            RawRange::Index(i) => (*i).into(),
            RawRange::Named(NamedRange::All) => Range::from_start(0),
            RawRange::Named(NamedRange::First) => 0.into(),
            RawRange::Named(NamedRange::Last) => (-1).into(),
            RawRange::Tuple(start, end) => Range::new(*start, *end),
            RawRange::StartOnly { start } => Range::from_start(*start),
            RawRange::StopOnly { stop } => Range::new(0, *stop),
            RawRange::StartStop { start, stop } => Range::new(*start, *stop),
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

// not directly in Range as a workaround for serde
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum NamedRange {
    All,
    First,
    Last,
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
    All(AllFieldSelector),
    Single(MetadataField),
    Multiple(HashSet<MetadataField>),
}
impl FieldSelector {
    fn is_match(&self, field: &MetadataField) -> bool {
        match self {
            Self::All(_) => true,
            Self::Single(single) => field == single,
            Self::Multiple(set) => set.contains(field),
        }
    }
}

// not directly in FieldSelector as a workaround for serde
#[derive(Deserialize, Serialize)]
pub enum AllFieldSelector {
    #[serde(rename = "*")]
    All,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ValueGetter {
    Direct(MetadataValue),
    Copy {
        from: LocalItemSelector,
        #[serde(default = "default_value")]
        value: ItemValueGetter,
        modify: Option<ValueModifier>,
    },
}
impl ValueGetter {
    fn get(&self, path: &Path, config: &LibraryConfig) -> Result<PendingValue, ValueError> {
        match self {
            Self::Direct(value) => Ok(value.clone().into()),
            Self::Copy {
                from,
                value,
                modify,
            } => {
                let items = from.get(path);
                if items.is_empty() {
                    return Err(ValueError::ItemNotFound);
                }
                let values = items.into_iter().filter_map(|x| value.get(x, config).ok());
                let result = MetadataValue::from_list(
                    values
                        .map(|x| x.to_list())
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .flatten()
                        .collect(),
                )
                .ok_or(ValueError::UnexpectedType)?;
                if let Some(modify) = modify {
                    modify.modify(&result.into(), path, config)
                } else {
                    Ok(result.into())
                }
            }
        }
    }
}

fn default_value() -> ItemValueGetter {
    ItemValueGetter::Field(FieldValueGetter::CleanName)
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
        index: Option<RawRange>,
    },
    Append {
        append: Box<ValueGetter>,
        index: Option<RawRange>,
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
pub enum PendingValue {
    RegexMatches(HashMap<String, String>),
    Safe(MetadataValue),
}
impl From<MetadataValue> for PendingValue {
    fn from(value: MetadataValue) -> Self {
        PendingValue::Safe(value)
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
                return MetadataValue::Blank;
            }
        }
        if result.len() == 1 {
            MetadataValue::String(result[0].clone())
        } else {
            MetadataValue::List(result.to_vec())
        }
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
            MetadataValue::String(str) if index.is_none() => {
                Ok(MetadataValue::String(formatter(str, extra)))
            }
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
            _ => Err(ValueError::UnexpectedType),
        }
    }
    fn modify(
        &self,
        value: &PendingValue,
        path: &Path,
        config: &LibraryConfig,
    ) -> Result<PendingValue, ValueError> {
        match self {
            Self::Group { group } => {
                if let PendingValue::RegexMatches(matches) = value {
                    if let Some(capture) = matches.get(group) {
                        return Ok(MetadataValue::String(capture.as_str().to_owned()).into());
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Regex { regex } => {
                if let PendingValue::Safe(MetadataValue::String(str)) = value {
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
                Err(ValueError::UnexpectedType)
            }
            Self::Take { take } => {
                if let PendingValue::Safe(MetadataValue::List(list)) = value {
                    return match take {
                        TakeModifier::Defined {
                            index,
                            out_of_bounds,
                            min_length,
                        } => Ok(Self::take(
                            list,
                            &index.into(),
                            *out_of_bounds,
                            min_length.as_ref().copied(),
                        )
                        .into()),
                        TakeModifier::Simple(range) => {
                            Ok(
                                Self::take(list, &range.into(), OutOfBoundsDecision::Exit, None)
                                    .into(),
                            )
                        }
                    };
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Append { append, index } => {
                if let PendingValue::Safe(MetadataValue::String(extra)) =
                    append.get(path, config)?
                {
                    if let PendingValue::Safe(value) = value {
                        let range = index.as_ref().map(|x| x.into());
                        return Ok(Self::append(value, &extra, range.as_ref(), true)?.into());
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Prepend { prepend, index } => {
                if let PendingValue::Safe(MetadataValue::String(extra)) =
                    prepend.get(path, config)?
                {
                    let range = index.as_ref().map(|x| x.into());
                    if let PendingValue::Safe(value) = value {
                        return Ok(Self::append(value, &extra, range.as_ref(), false)?.into());
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Join { join } => {
                if let PendingValue::Safe(MetadataValue::String(extra)) = join.get(path, config)? {
                    if let PendingValue::Safe(MetadataValue::List(list)) = value {
                        return Ok(MetadataValue::String(list.join(&extra)).into());
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Split { split, when_none } => {
                if let PendingValue::Safe(MetadataValue::String(str)) = value {
                    let vec = str.split(split).map(|x| x.to_owned()).collect::<Vec<_>>();
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
                        .try_fold(value, |x, y| y.modify(&x, path, config))
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
    Simple(RawRange),
    Defined {
        index: RawRange,
        #[serde(default = "default_oob")]
        out_of_bounds: OutOfBoundsDecision,
        min_length: Option<usize>,
    },
}
fn default_oob() -> OutOfBoundsDecision {
    OutOfBoundsDecision::Exit
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ItemValueGetter {
    Field(FieldValueGetter),
    Copy { copy: MetadataField },
}
impl ItemValueGetter {
    fn file_name(path: &Path) -> String {
        path.file_name()
            .map(|x| x.to_string_lossy())
            .unwrap_or_else(|| path.to_string_lossy())
            .into_owned()
    }
    fn clean<'a>(name: &'a str, config: &LibraryConfig) -> Cow<'a, str> {
        let mut result = Cow::from(name);
        for (find, replace) in &config.find_replace {
            result = result.replace(find, replace).into();
        }
        result
    }
    fn get(&self, path: &Path, config: &LibraryConfig) -> Result<MetadataValue, ValueError> {
        match self {
            Self::Field(field) => match field {
                FieldValueGetter::CleanName => Ok(MetadataValue::String(
                    Self::clean(Self::file_name(path).as_str(), config).into_owned(),
                )),
                FieldValueGetter::FileName => Ok(MetadataValue::String(Self::file_name(path))),
                FieldValueGetter::Path => {
                    Ok(MetadataValue::String(path.to_string_lossy().into_owned()))
                }
            },
            Self::Copy { copy } => Err(ValueError::ItemNotFound),
        }
    }
}

// not directly in ItemValueGetter as a workaround for serde
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FieldValueGetter {
    FileName,
    CleanName,
    Path,
}

#[derive(Debug, Clone)]
pub struct Metadata {
    pub fields: HashMap<MetadataField, MetadataValue>,
}
impl Metadata {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }
}

#[derive(Deserialize, PartialEq, Eq, Serialize, Debug, Clone)]
#[serde(untagged)]
pub enum MetadataValue {
    Blank,
    String(String),
    Number(u32),
    List(Vec<String>),
}
impl MetadataValue {
    fn combine(&self, other: &Self, mode: &CombineMode) -> Result<Self, ValueError> {
        match mode {
            CombineMode::Replace => Ok(other.clone()),
            CombineMode::Append => {
                let mut list1 = self.to_list()?;
                let mut list2 = other.to_list()?;
                list1.append(&mut list2);
                Ok(Self::List(list1))
            }
            CombineMode::Prepend => {
                let mut list1 = other.to_list()?;
                let mut list2 = self.to_list()?;
                list1.append(&mut list2);
                Ok(Self::List(list1))
            }
        }
    }
    pub fn from_list(list: Vec<String>) -> Option<Self> {
        match list.len() {
            0 => None,
            1 => Some(MetadataValue::String(list[0].clone())),
            _ => Some(MetadataValue::List(list)),
        }
    }
    fn to_list(&self) -> Result<Vec<String>, ValueError> {
        match self {
            Self::Blank | Self::Number(_) => Err(ValueError::UnexpectedType),
            Self::String(str) => Ok(vec![str.clone()]),
            Self::List(list) => Ok(list.clone()),
        }
    }
    pub fn canonicalize(&self) -> Self {
        match self {
            Self::Blank | Self::Number(_) | Self::String(_) => self.clone(),
            Self::List(list) => {
                if list.len() == 1 {
                    Self::String(list[0].clone())
                } else {
                    self.clone()
                }
            }
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum MetadataField {
    Builtin(BuiltinMetadataField),
    Custom(String),
}

// not directly in MetadataField as a workaround for serde
#[derive(Eq, Hash, PartialEq, Debug, Display, EnumIter, Clone, Copy, Deserialize, Serialize)]
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
