use core::fmt;
use std::{collections::HashMap, rc::Rc};

use image::DynamicImage;
use regex::Regex;
use serde::{Deserialize, Serialize};
use strum::EnumIter;

use crate::{
    lyrics::{RichLyrics, SyncedLyrics},
    modifier::ValueError,
    Results,
};

pub type Metadata = HashMap<MetadataField, MetadataValue>;

#[derive(Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[serde(untagged)]
pub enum MetadataValue {
    Number(u32),
    #[serde(deserialize_with = "crate::util::string_or_seq_string")]
    List(Vec<String>),
    #[serde(skip)]
    RegexMatches {
        source: String,
        regex: RegexWrap,
    },
}

// stupid stuff required to derive Eq/Ord for MetadataValue
#[derive(Debug, Clone)]
pub struct RegexWrap(pub Regex);
impl PartialEq for RegexWrap {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}
impl Eq for RegexWrap {}
impl PartialOrd for RegexWrap {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for RegexWrap {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

pub static BLANK_VALUE: MetadataValue = MetadataValue::List(vec![]);
impl Serialize for MetadataValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Number(num) => serializer.serialize_u32(*num),
            Self::List(list) if list.len() == 1 => serializer.serialize_str(&list[0]),
            Self::List(list) => list.serialize(serializer),
            _ => serializer.serialize_unit(),
        }
    }
}
impl MetadataValue {
    pub const fn blank() -> Self {
        Self::List(vec![])
    }
    pub fn string(single: String) -> Self {
        Self::List(vec![single])
    }
    pub fn option(value: Option<String>) -> Self {
        value.map_or_else(Self::blank, Self::string)
    }
    pub fn option_num(value: Option<u32>) -> Self {
        value.map_or_else(Self::blank, Self::Number)
    }
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Self::List(list) => {
                if list.len() == 1 {
                    Some(list[0].as_ref())
                } else {
                    None
                }
            }
            _ => None,
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
            Self::RegexMatches { source, regex } => {
                write!(f, "regex '{}' on '{}'", regex.0, source)
            }
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
        let mut convert_string = |field: MetadataField| match metadata.get(&field) {
            None => SetValue::Skip,
            Some(MetadataValue::List(list)) if list.is_empty() => SetValue::Set(None),
            Some(value) => value.as_string().map_or_else(
                || {
                    errors.push(ValueError::WrongFieldType {
                        field,
                        got: value.clone(),
                        expected: "single string",
                    });
                    SetValue::Skip
                },
                |val| SetValue::Set(Some(val.to_owned())),
            ),
        };
        let title = convert_string(MetadataField::Title);
        let album = convert_string(MetadataField::Album);
        let album_artist = convert_string(MetadataField::AlbumArtist);
        let arranger = convert_string(MetadataField::Arranger);
        let language = convert_string(MetadataField::Language);
        let simple_lyrics = convert_string(MetadataField::SimpleLyrics);
        let mut convert_vec = |field: MetadataField| match metadata.get(&field) {
            None => SetValue::Skip,
            Some(value) => match value {
                MetadataValue::List(list) => SetValue::Set(list.clone()),
                _ => {
                    errors.push(ValueError::WrongFieldType {
                        field,
                        got: value.clone(),
                        expected: "list",
                    });
                    SetValue::Skip
                }
            },
        };
        let performers = convert_vec(MetadataField::Performers);
        let composers = convert_vec(MetadataField::Composers);
        let comments = convert_vec(MetadataField::Comment);
        let genres = convert_vec(MetadataField::Genres);
        let mut convert_num = |field: MetadataField| match metadata.get(&field) {
            None => SetValue::Skip,
            Some(MetadataValue::List(list)) if list.is_empty() => SetValue::Set(None),
            Some(value) => match value {
                MetadataValue::Number(num) => SetValue::Set(Some(*num)),
                _ => {
                    errors.push(ValueError::WrongFieldType {
                        field,
                        got: value.clone(),
                        expected: "number",
                    });
                    SetValue::Skip
                }
            },
        };
        let track = convert_num(MetadataField::Track);
        let track_total = convert_num(MetadataField::TrackTotal);
        let disc = convert_num(MetadataField::Disc);
        let disc_total = convert_num(MetadataField::DiscTotal);
        let year = convert_num(MetadataField::Year);
        Results {
            result: Self {
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
            metadata.insert(MetadataField::Title, MetadataValue::option(val));
        }
        if let SetValue::Set(val) = value.album {
            metadata.insert(MetadataField::Album, MetadataValue::option(val));
        }
        if let SetValue::Set(val) = value.performers {
            metadata.insert(MetadataField::Performers, MetadataValue::List(val));
        }
        if let SetValue::Set(val) = value.album_artist {
            metadata.insert(MetadataField::AlbumArtist, MetadataValue::option(val));
        }
        if let SetValue::Set(val) = value.composers {
            metadata.insert(MetadataField::Composers, MetadataValue::List(val));
        }
        if let SetValue::Set(val) = value.arranger {
            metadata.insert(MetadataField::Arranger, MetadataValue::option(val));
        }
        if let SetValue::Set(val) = value.comments {
            metadata.insert(MetadataField::Comment, MetadataValue::List(val));
        }
        if let SetValue::Set(val) = value.track {
            metadata.insert(MetadataField::Track, MetadataValue::option_num(val));
        }
        if let SetValue::Set(val) = value.track_total {
            metadata.insert(MetadataField::TrackTotal, MetadataValue::option_num(val));
        }
        if let SetValue::Set(val) = value.disc {
            metadata.insert(MetadataField::Disc, MetadataValue::option_num(val));
        }
        if let SetValue::Set(val) = value.disc_total {
            metadata.insert(MetadataField::DiscTotal, MetadataValue::option_num(val));
        }
        if let SetValue::Set(val) = value.year {
            metadata.insert(MetadataField::Year, MetadataValue::option_num(val));
        }
        if let SetValue::Set(val) = value.language {
            metadata.insert(MetadataField::Language, MetadataValue::option(val));
        }
        if let SetValue::Set(val) = value.genres {
            metadata.insert(MetadataField::Genres, MetadataValue::List(val));
        }
        if let SetValue::Set(val) = value.simple_lyrics {
            metadata.insert(MetadataField::SimpleLyrics, MetadataValue::option(val));
        }

        metadata
    }
}

#[derive(Deserialize, Serialize, Debug, EnumIter, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[serde(rename_all = "lowercase")]
pub enum MetadataField {
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
    #[serde(untagged)]
    Custom(String),
}
impl std::fmt::Display for MetadataField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Title => write!(f, "Title"),
            Self::Album => write!(f, "Album"),
            Self::Performers => write!(f, "Performers"),
            Self::AlbumArtist => write!(f, "Album Artist"),
            Self::Composers => write!(f, "Composers"),
            Self::Arranger => write!(f, "Arranger"),
            Self::Comment => write!(f, "Comment"),
            Self::Track => write!(f, "Track"),
            Self::TrackTotal => write!(f, "Track Total"),
            Self::Disc => write!(f, "Disc"),
            Self::DiscTotal => write!(f, "Disc Total"),
            Self::Year => write!(f, "Year"),
            Self::Language => write!(f, "Language"),
            Self::Genres => write!(f, "Genres"),
            Self::Art => write!(f, "Art"),
            Self::SimpleLyrics => write!(f, "Simple Lyrics"),
            Self::Custom(val) => write!(f, "Custom ({val})"),
        }
    }
}
