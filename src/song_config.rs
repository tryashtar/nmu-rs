use std::collections::HashSet;
use serde::Deserialize;

#[derive(Deserialize)]
pub struct SongConfig {
    pub songs: Option<MetadataStrategy>,
}

#[derive(Deserialize)]
#[serde(untagged)]
pub enum MetadataStrategy {
    Map(MapStrategy)
}

#[derive(Deserialize)]
pub struct MapStrategy {
    //pub fields: HashSet<MetadataField, ValueGetter>,
}

pub struct Metadata {
    pub fields: HashSet<MetadataField, MetadataValue>,
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
    Custom(String)
}
