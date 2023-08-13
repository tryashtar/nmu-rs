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
    All,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ValueGetter {
    Direct(MetadataValue),
    Copy {
        from: LocalItemSelector,
        value: ItemValueGetter,
        modify: Option<ValueModifier>,
    },
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
    Copy(MetadataField),
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
