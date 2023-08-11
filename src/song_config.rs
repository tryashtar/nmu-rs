use serde::Deserialize;
use std::collections::{HashMap, HashSet};

#[derive(Deserialize)]
pub struct SongConfig {
    pub songs: Option<MetadataStrategy>,
}

#[derive(Deserialize)]
#[serde(untagged)]
pub enum MetadataStrategy {
    Map(MapStrategy),
}
impl MetadataStrategy {
    pub fn apply(&self, metadata: &mut Metadata) {}
}

#[derive(Deserialize)]
pub struct MapStrategy {
    //pub fields: HashSet<MetadataField, ValueGetter>,
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

pub enum MetadataValue {
    Blank,
    String(String),
    List(Vec<String>),
}

#[derive(Eq, Hash, PartialEq, Debug)]
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
