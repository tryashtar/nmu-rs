use core::fmt;
use std::collections::HashMap;

use regex::Regex;
use serde::{Deserialize, Serialize};
use strum::EnumIter;

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
