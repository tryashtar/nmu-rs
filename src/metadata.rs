use core::fmt;
use std::{collections::HashMap, path::PathBuf};

use regex::Regex;
use serde::{Deserialize, Serialize};
use strum::EnumIter;

use crate::{
    library_config::LibraryConfig, modifier::ValueError, song_config::LoadedConfig, util::ItemPath,
};

pub type PendingMetadata = HashMap<MetadataField, Result<MetadataValue, ValueError>>;
pub type Metadata = HashMap<MetadataField, MetadataValue>;

pub struct GetMetadataResults {
    pub metadata: Metadata,
    pub reports: Vec<SourcedReport>,
}
pub struct SourcedReport {
    pub full_path: PathBuf,
    pub errors: Vec<ValueError>,
}

pub fn get_metadata(
    nice_path: &ItemPath,
    configs: &[LoadedConfig],
    library_config: &LibraryConfig,
) -> GetMetadataResults {
    let mut metadata;
    let mut config_reports;
    let mut copy_source = Metadata::new();
    loop {
        metadata = PendingMetadata::new();
        config_reports = vec![];
        for config in configs {
            let select_path = nice_path
                .strip_prefix(&config.nice_folder)
                .unwrap_or(&config.nice_folder);
            let report = config.config.apply(
                nice_path,
                select_path,
                &mut metadata,
                &copy_source,
                library_config,
            );
            config_reports.push(SourcedReport {
                full_path: config.full_path.clone(),
                errors: report.errors,
            });
        }
        let mut redo = false;
        for error in config_reports.iter().flat_map(|x| &x.errors) {
            if let ValueError::CopyNotFound { field } = error {
                if metadata.get(field).is_some_and(|x| x.is_ok()) {
                    redo = true;
                    break;
                }
            }
        }
        if !redo {
            break;
        }
        copy_source = finalize(metadata);
    }
    GetMetadataResults {
        metadata: finalize(metadata),
        reports: config_reports,
    }
}

fn finalize(metadata: PendingMetadata) -> Metadata {
    metadata
        .into_iter()
        .filter_map(|(k, v)| v.ok().map(|v| (k, v)))
        .collect()
}

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
    pub fn is_blank(&self) -> bool {
        match self {
            MetadataValue::List(list) => list.is_empty(),
            _ => false,
        }
    }
    pub fn string(single: String) -> Self {
        Self::List(vec![single])
    }
    pub fn from_option(value: Option<String>) -> Self {
        match value {
            None => Self::blank(),
            Some(val) => Self::string(val),
        }
    }
    pub fn into_num(self) -> Option<u32> {
        match self {
            Self::Number(num) => Some(num),
            _ => None,
        }
    }
    pub fn into_list(self) -> Option<Vec<String>> {
        match self {
            Self::List(list) => Some(list),
            _ => None,
        }
    }
    pub fn into_string(self) -> Option<String> {
        match self {
            Self::List(mut list) => {
                if list.len() == 1 {
                    Some(list.remove(0))
                } else {
                    None
                }
            }
            _ => None,
        }
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
    Subtitle,
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
            Self::Subtitle => write!(f, "Subtitle"),
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
