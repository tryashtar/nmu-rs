use regex::Regex;
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    path::{Component, Path, PathBuf},
    rc::Rc,
};
use strum::{EnumIter, IntoEnumIterator};

use crate::library_config::LibraryConfig;

#[derive(Deserialize, Serialize)]
pub struct RawSongConfig {
    pub songs: Option<ReferencableOperation>,
    pub set: Option<HashMap<PathBuf, ReferencableOperation>>,
    #[serde(rename = "set all")]
    pub set_all: Option<Vec<RawAllSetter>>,
}

#[derive(Deserialize, Serialize)]
pub struct RawAllSetter {
    pub names: ItemSelector,
    pub set: ReferencableOperation,
}

pub struct SongConfig {
    pub set: Vec<AllSetter>,
}

pub struct AllSetter {
    pub names: ItemSelector,
    pub set: Rc<MetadataOperation>,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ReferencableOperation {
    Reference(String),
    Sequence(Vec<ReferencableOperation>),
    Direct(MetadataOperation),
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum MetadataOperation {
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
    Sequence(Vec<Rc<MetadataOperation>>),
    Set(HashMap<MetadataField, ValueGetter>),
}
impl MetadataOperation {
    pub fn apply(&self, metadata: &mut Metadata, path: &Path, config: &LibraryConfig) {
        match self {
            Self::Blank { remove } => {
                for field in &config.custom_fields {
                    if remove.is_match(field) {
                        metadata.fields.insert(field.clone(), MetadataValue::Blank);
                    }
                }
                for field in BuiltinMetadataField::iter() {
                    let builtin = MetadataField::Builtin(field);
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
                    if let Ok(value) = value.get(path) {
                        metadata.fields.insert(field.clone(), value);
                    }
                }
            }
            Self::Context { source, modify } => {
                if let Ok(value) = source.get(path) {
                    for (field, modifier) in modify {
                        if let Ok(modified) = modifier.modify(&value) {
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
                    if let Ok(adding) = value.get(path) {
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
                        if let Ok(modified) = modifier.modify(existing) {
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
        must_be: Option<MusicItemType>,
        up: Range,
    },
    DrillDown {
        must_be: Option<MusicItemType>,
        from_root: Range,
    },
}
impl LocalItemSelector {
    fn get<'a>(&self, start: &'a Path) -> Option<&'a Path> {
        Some(start)
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
pub enum Range {
    Index(i32),
    Named(NamedRange),
    Tuple(i32, i32),
    Struct {
        start: Option<i32>,
        stop: Option<i32>,
    },
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
    fn get(&self, path: &Path) -> Result<MetadataValue, ValueError> {
        match self {
            Self::Direct(value) => Ok(value.clone()),
            Self::Copy {
                from,
                value,
                modify,
            } => Err(ValueError::UnexpectedType),
        }
    }
}

fn default_value() -> ItemValueGetter {
    ItemValueGetter::Field(FieldValueGetter::CleanName)
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
impl ValueModifier {
    fn modify(&self, value: &MetadataValue) -> Result<MetadataValue, ValueError> {
        Err(ValueError::UnexpectedType)
    }
}

pub enum ValueError {
    UnexpectedType,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum TakeModifier {
    Simple(Range),
    Defined { index: Range },
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ItemValueGetter {
    Field(FieldValueGetter),
    Copy { copy: MetadataField },
}
impl ItemValueGetter {
    fn get(&self) {}
}

// not directly in ItemValueGetter as a workaround for serde
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FieldValueGetter {
    FileName,
    CleanName,
    Path,
}

#[derive(Debug)]
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

#[derive(Deserialize, Serialize, Debug, Clone)]
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
    fn to_list(&self) -> Result<Vec<String>, ValueError> {
        match self {
            Self::Blank | Self::Number(_) => Err(ValueError::UnexpectedType),
            Self::String(str) => Ok(vec![str.clone()]),
            Self::List(list) => Ok(list.clone()),
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
#[derive(Eq, Hash, PartialEq, Debug, EnumIter, Clone, Deserialize, Serialize)]
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
