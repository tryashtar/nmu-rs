use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    ops::Range,
    path::PathBuf,
};

#[derive(Deserialize, Serialize)]
pub struct SongConfig {
    pub songs: Option<MetadataOperation>,
    pub set: Option<HashMap<PathBuf, MetadataOperation>>,
    pub set_all: Option<Vec<AllSetter>>,
}

#[derive(Deserialize, Serialize)]
pub struct AllSetter {
    names: ItemSelector,
    set: MetadataOperation,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum MetadataOperation {
    Blank { remove: FieldSelector },
    Keep { keep: FieldSelector },
    Sequence(Vec<MetadataOperation>),
    Set(HashMap<MetadataField, ValueGetter>),
}
impl MetadataOperation {
    pub fn apply(&self, metadata: &mut Metadata) {}
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
    Regex { regex: String },
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
#[serde(untagged)]
pub enum LocalItemSelector {
    #[serde(alias = "self")]
    This,
    Select {
        selector: ItemSelector,
    },
    DrillUp {
        must_be: Option<MusicItemType>,
        up: Range<i32>,
    },
    DrillDown {
        must_be: Option<MusicItemType>,
        from_root: Range<i32>,
    },
}

#[derive(Deserialize, Serialize)]
pub enum MusicItemType {
    Song,
    Folder,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum FieldSelector {
    Single(MetadataField),
    Multiple(HashSet<MetadataField>),
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
    ItemValueGetter::CleanName
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ValueModifier {}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ItemValueGetter {
    FileName,
    CleanName,
    Path,
    Copy { copy: MetadataField },
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
    List(Vec<String>),
}

#[derive(Eq, Hash, PartialEq, Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum MetadataField {
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
    Custom(String),
}
