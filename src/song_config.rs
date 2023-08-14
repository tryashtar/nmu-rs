use regex::Regex;
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
};

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
    pub songs: Option<Rc<MetadataOperation>>,
    pub set: Option<HashMap<PathBuf, Rc<MetadataOperation>>>,
    pub set_all: Option<Vec<AllSetter>>,
}

pub struct AllSetter {
    names: ItemSelector,
    set: Rc<MetadataOperation>,
}
impl AllSetter {
    pub fn new(names: ItemSelector, set: Rc<MetadataOperation>) -> Self {
        Self { names, set }
    }
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
    pub fn apply(&self, metadata: &mut Metadata) {}
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

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum PathSegment {
    Literal(String),
    Regex {
        #[serde(with = "serde_regex")]
        regex: Regex,
    },
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
#[serde(untagged)]
pub enum LocalItemSelector {
    This(SelfItemSelector),
    Select {
        selector: ItemSelector,
    },
    DrillUp {
        must_be: Option<MusicItemType>,
        up: Range,
    },
    DrillDown {
        must_be: Option<MusicItemType>,
        from_root: Range,
    },
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

// not directly in ItemValueGetter as a workaround for serde
#[derive(Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FieldValueGetter {
    FileName,
    CleanName,
    Path,
}

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

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum MetadataValue {
    Blank,
    String(String),
    Number(u32),
    List(Vec<String>),
}

#[derive(Eq, Hash, PartialEq, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum MetadataField {
    Builtin(BuiltinMetadataField),
    Custom(String),
}

// not directly in MetadataField as a workaround for serde
#[derive(Eq, Hash, PartialEq, Debug, Deserialize, Serialize)]
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
