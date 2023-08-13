use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

#[derive(Deserialize, Serialize)]
pub struct SongConfig {
    pub songs: Option<MetadataOperation>,
    pub set: Option<HashMap<ItemSelector, MetadataOperation>>,
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

#[derive(Eq, Hash, PartialEq, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum ItemSelector {
    Path(PathBuf),
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum LocalItemSelector {
    This,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum FieldSelector {
    Single(MetadataField),
    Multiple(HashSet<MetadataField>),
    #[serde(rename = "*")]
    All,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ValueGetter {
    Direct(MetadataValue),
    Copy { from: LocalItemSelector },
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
#[serde(rename_all = "snake_case")]
pub enum MetadataField {
    Title,
    Album,
    Performers,
    AlbumArtists,
    Composers,
    Arranger,
    Comment,
    Track,
    TrackTotal,
    Disc,
    DiscTotal,
    Year,
    Language,
    Genres,
    Art,
    SimpleLyrics,
    Custom(String),
}
