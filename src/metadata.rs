use core::fmt;
use std::{collections::HashMap, path::PathBuf, rc::Rc};

use image::DynamicImage;
use regex::Regex;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

use crate::{
    lyrics::{RichLyrics, SyncedLyrics},
    modifier::ValueError,
    GetMetadataResults, Results,
};

pub type Metadata = HashMap<MetadataField, MetadataValue>;
pub type PendingMetadata = HashMap<MetadataField, PendingValue>;
type MetadataCache = HashMap<PathBuf, GetMetadataResults>;

#[derive(Clone, Debug)]
pub enum PendingValue {
    Ready(MetadataValue),
    RegexMatches { source: String, regex: Regex },
}
impl From<MetadataValue> for PendingValue {
    fn from(value: MetadataValue) -> Self {
        PendingValue::Ready(value)
    }
}
impl fmt::Display for PendingValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ready(r) => r.fmt(f),
            Self::RegexMatches { source, regex } => write!(f, "regex {} on {}", source, regex),
        }
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[serde(untagged)]
pub enum MetadataValue {
    Number(u32),
    #[serde(deserialize_with = "crate::util::string_or_seq_string")]
    List(Vec<String>),
}
pub static BLANK_VALUE: MetadataValue = MetadataValue::List(vec![]);
impl Serialize for MetadataValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            MetadataValue::Number(num) => serializer.serialize_u32(*num),
            MetadataValue::List(list) if list.len() == 1 => serializer.serialize_str(&list[0]),
            MetadataValue::List(list) => list.serialize(serializer),
        }
    }
}
impl MetadataValue {
    pub fn blank() -> MetadataValue {
        MetadataValue::List(vec![])
    }
    pub fn string(single: String) -> MetadataValue {
        MetadataValue::List(vec![single])
    }
    pub fn option(value: Option<String>) -> MetadataValue {
        match value {
            None => Self::blank(),
            Some(val) => Self::string(val),
        }
    }
    pub fn option_num(value: Option<u32>) -> MetadataValue {
        match value {
            None => Self::blank(),
            Some(val) => Self::Number(val),
        }
    }
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Self::Number(_) => None,
            Self::List(list) => {
                if list.len() == 1 {
                    Some(list[0].as_ref())
                } else {
                    None
                }
            }
        }
    }
}
impl fmt::Display for MetadataValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{n}"),
            Self::List(l) if l.is_empty() => write!(f, "[]"),
            Self::List(l) if l.len() == 1 => write!(f, "{}", l[0]),
            Self::List(l) => write!(f, "[{}]", l.join("; ")),
        }
    }
}

#[derive(Deserialize, Serialize, Hash, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[serde(untagged)]
pub enum MetadataField {
    Builtin(BuiltinMetadataField),
    Custom(String),
}
impl fmt::Display for MetadataField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Custom(s) => write!(f, "{s}"),
            Self::Builtin(b) => write!(f, "{b}"),
        }
    }
}

pub enum SetValue<T> {
    Skip,
    Set(T),
}
pub struct FinalMetadata {
    pub title: SetValue<Option<String>>,
    pub album: SetValue<Option<String>>,
    pub performers: SetValue<Vec<String>>,
    pub album_artist: SetValue<Option<String>>,
    pub composers: SetValue<Vec<String>>,
    pub arranger: SetValue<Option<String>>,
    pub comments: SetValue<Vec<String>>,
    pub track: SetValue<Option<u32>>,
    pub track_total: SetValue<Option<u32>>,
    pub disc: SetValue<Option<u32>>,
    pub disc_total: SetValue<Option<u32>>,
    pub year: SetValue<Option<u32>>,
    pub language: SetValue<Option<String>>,
    pub genres: SetValue<Vec<String>>,
    pub art: SetValue<Option<Rc<DynamicImage>>>,
    pub simple_lyrics: SetValue<Option<String>>,
    pub synced_lyrics: SetValue<Option<SyncedLyrics>>,
    pub rich_lyrics: SetValue<Option<RichLyrics>>,
}
impl FinalMetadata {
    pub fn create(metadata: &Metadata) -> Results<FinalMetadata, ValueError> {
        let mut errors = vec![];
        let mut convert_string = |field: BuiltinMetadataField| {
            let field = field.into();
            match metadata.get(&field) {
                None => SetValue::Skip,
                Some(MetadataValue::List(list)) if list.is_empty() => SetValue::Set(None),
                Some(value) => match value.as_string() {
                    Some(val) => SetValue::Set(Some(val.to_owned())),
                    None => {
                        errors.push(ValueError::WrongFieldType {
                            field,
                            got: value.clone(),
                            expected: "single string",
                        });
                        SetValue::Skip
                    }
                },
            }
        };
        let title = convert_string(BuiltinMetadataField::Title);
        let album = convert_string(BuiltinMetadataField::Album);
        let album_artist = convert_string(BuiltinMetadataField::AlbumArtist);
        let arranger = convert_string(BuiltinMetadataField::Arranger);
        let language = convert_string(BuiltinMetadataField::Language);
        let simple_lyrics = convert_string(BuiltinMetadataField::SimpleLyrics);
        let mut convert_vec = |field: BuiltinMetadataField| {
            let field = field.into();
            match metadata.get(&field) {
                None => SetValue::Skip,
                Some(value) => match value {
                    MetadataValue::List(list) => SetValue::Set(list.clone()),
                    MetadataValue::Number(_) => {
                        errors.push(ValueError::WrongFieldType {
                            field,
                            got: value.clone(),
                            expected: "list",
                        });
                        SetValue::Skip
                    }
                },
            }
        };
        let performers = convert_vec(BuiltinMetadataField::Performers);
        let composers = convert_vec(BuiltinMetadataField::Composers);
        let comments = convert_vec(BuiltinMetadataField::Comment);
        let genres = convert_vec(BuiltinMetadataField::Genres);
        let mut convert_num = |field: BuiltinMetadataField| {
            let field = field.into();
            match metadata.get(&field) {
                None => SetValue::Skip,
                Some(MetadataValue::List(list)) if list.is_empty() => SetValue::Set(None),
                Some(value) => match value {
                    MetadataValue::Number(num) => SetValue::Set(Some(*num)),
                    MetadataValue::List(_) => {
                        errors.push(ValueError::WrongFieldType {
                            field,
                            got: value.clone(),
                            expected: "number",
                        });
                        SetValue::Skip
                    }
                },
            }
        };
        let track = convert_num(BuiltinMetadataField::Track);
        let track_total = convert_num(BuiltinMetadataField::TrackTotal);
        let disc = convert_num(BuiltinMetadataField::Disc);
        let disc_total = convert_num(BuiltinMetadataField::DiscTotal);
        let year = convert_num(BuiltinMetadataField::Year);
        Results {
            result: FinalMetadata {
                title,
                album,
                performers,
                album_artist,
                composers,
                arranger,
                comments,
                track,
                track_total,
                disc,
                disc_total,
                year,
                language,
                genres,
                art: SetValue::Skip,
                simple_lyrics,
                synced_lyrics: SetValue::Skip,
                rich_lyrics: SetValue::Skip,
            },
            errors,
        }
    }
}
impl From<FinalMetadata> for Metadata {
    fn from(value: FinalMetadata) -> Self {
        let mut metadata = Self::new();
        if let SetValue::Set(val) = value.title {
            metadata.insert(
                BuiltinMetadataField::Title.into(),
                MetadataValue::option(val),
            );
        }
        if let SetValue::Set(val) = value.album {
            metadata.insert(
                BuiltinMetadataField::Album.into(),
                MetadataValue::option(val),
            );
        }
        if let SetValue::Set(val) = value.performers {
            metadata.insert(
                BuiltinMetadataField::Performers.into(),
                MetadataValue::List(val),
            );
        }
        if let SetValue::Set(val) = value.album_artist {
            metadata.insert(
                BuiltinMetadataField::AlbumArtist.into(),
                MetadataValue::option(val),
            );
        }
        if let SetValue::Set(val) = value.composers {
            metadata.insert(
                BuiltinMetadataField::Composers.into(),
                MetadataValue::List(val),
            );
        }
        if let SetValue::Set(val) = value.arranger {
            metadata.insert(
                BuiltinMetadataField::Arranger.into(),
                MetadataValue::option(val),
            );
        }
        if let SetValue::Set(val) = value.comments {
            metadata.insert(
                BuiltinMetadataField::Comment.into(),
                MetadataValue::List(val),
            );
        }
        if let SetValue::Set(val) = value.track {
            metadata.insert(
                BuiltinMetadataField::Track.into(),
                MetadataValue::option_num(val),
            );
        }
        if let SetValue::Set(val) = value.track_total {
            metadata.insert(
                BuiltinMetadataField::TrackTotal.into(),
                MetadataValue::option_num(val),
            );
        }
        if let SetValue::Set(val) = value.disc {
            metadata.insert(
                BuiltinMetadataField::Disc.into(),
                MetadataValue::option_num(val),
            );
        }
        if let SetValue::Set(val) = value.disc_total {
            metadata.insert(
                BuiltinMetadataField::DiscTotal.into(),
                MetadataValue::option_num(val),
            );
        }
        if let SetValue::Set(val) = value.year {
            metadata.insert(
                BuiltinMetadataField::Year.into(),
                MetadataValue::option_num(val),
            );
        }
        if let SetValue::Set(val) = value.language {
            metadata.insert(
                BuiltinMetadataField::Language.into(),
                MetadataValue::option(val),
            );
        }
        if let SetValue::Set(val) = value.genres {
            metadata.insert(
                BuiltinMetadataField::Genres.into(),
                MetadataValue::List(val),
            );
        }
        if let SetValue::Set(val) = value.simple_lyrics {
            metadata.insert(
                BuiltinMetadataField::SimpleLyrics.into(),
                MetadataValue::option(val),
            );
        }

        metadata
    }
}

#[derive(
    Deserialize,
    Serialize,
    Debug,
    Display,
    EnumIter,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Clone,
    Copy,
)]
#[serde(rename_all = "lowercase")]
pub enum BuiltinMetadataField {
    Title,
    Album,
    #[serde(alias = "performer")]
    Performers,
    #[serde(rename = "album artist")]
    AlbumArtist,
    #[serde(alias = "composer")]
    Composers,
    Arranger,
    #[serde(alias = "comments")]
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
