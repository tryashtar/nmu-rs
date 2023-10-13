use core::fmt;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

use regex::Regex;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

use crate::{
    file_stuff::ConfigError, get_metadata, library_config::LibraryConfig, modifier::ValueModifier,
    song_config::SongConfig, util::ItemPath, ConfigCache,
};

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
pub struct PendingMetadata {
    pub fields: HashMap<MetadataField, PendingValue>,
}
impl PendingMetadata {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }
    pub fn resolve(
        mut self,
        nice_path: &Path,
        library_config: &LibraryConfig,
        config_cache: &mut ConfigCache,
    ) -> Metadata {
        let mut metadata = Metadata::new();
        let mut metadata_cache: HashMap<PathBuf, Result<Metadata, Rc<ConfigError>>> = HashMap::new();
        loop {
            let mut added_any = false;
            self.fields.retain(|field, value| {
                let processed = match value {
                    PendingValue::Ready(ready) => {
                        metadata.fields.insert(field.clone(), ready.clone());
                        true
                    }
                    PendingValue::RegexMatches { .. } => false,
                    PendingValue::CopyField {
                        field: from,
                        sources,
                        modify,
                    } => {
                        let mut results = sources.iter().filter_map(|source| {
                            let source_metadata = {
                                if nice_path == source.as_ref() {
                                    Some(&metadata)
                                } else {
                                    metadata_cache
                                        .entry(source.clone().into())
                                        .or_insert_with(|| {
                                            get_metadata(source, library_config, config_cache)
                                        })
                                        .as_ref()
                                        .ok()
                                }
                            };
                            source_metadata.and_then(|meta| {
                                meta.fields.get(from).cloned().and_then(|x| match modify {
                                    None => Some(x.into()),
                                    Some(modify) => modify
                                        .modify(x.into(), source.as_ref(), library_config)
                                        .ok(),
                                })
                            })
                        });
                        match results.nth(0) {
                            Some(PendingValue::Ready(ready)) => {
                                metadata.fields.insert(field.clone(), ready);
                                true
                            }
                            _ => false,
                        }
                    }
                };
                added_any |= processed;
                !processed
            });
            if !added_any {
                break;
            }
        }
        // remaining items that couldn't be resolved
        for (key, _) in self.fields {
            metadata.fields.insert(key, MetadataValue::blank());
        }
        metadata
    }
}
impl From<Metadata> for PendingMetadata {
    fn from(value: Metadata) -> Self {
        Self {
            fields: value
                .fields
                .into_iter()
                .map(|(k, v)| (k, PendingValue::Ready(v)))
                .collect(),
        }
    }
}

#[derive(Clone)]
pub enum PendingValue {
    Ready(MetadataValue),
    RegexMatches {
        source: String,
        regex: Regex,
    },
    CopyField {
        field: MetadataField,
        sources: Vec<ItemPath>,
        modify: Option<Rc<ValueModifier>>,
    },
}
impl From<MetadataValue> for PendingValue {
    fn from(value: MetadataValue) -> Self {
        PendingValue::Ready(value)
    }
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Debug, Clone)]
#[serde(untagged)]
pub enum MetadataValue {
    Number(u32),
    #[serde(deserialize_with = "string_or_seq_string")]
    List(Vec<String>),
}
fn string_or_seq_string<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    struct StringOrVec(std::marker::PhantomData<Vec<String>>);

    impl<'de> serde::de::Visitor<'de> for StringOrVec {
        type Value = Vec<String>;

        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
            formatter.write_str("string or list of strings")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(vec![value.to_owned()])
        }

        fn visit_seq<S>(self, visitor: S) -> Result<Self::Value, S::Error>
        where
            S: serde::de::SeqAccess<'de>,
        {
            Deserialize::deserialize(serde::de::value::SeqAccessDeserializer::new(visitor))
        }
    }

    deserializer.deserialize_any(StringOrVec(std::marker::PhantomData))
}
impl MetadataValue {
    pub fn blank() -> MetadataValue {
        MetadataValue::List(vec![])
    }
    pub fn string(single: String) -> MetadataValue {
        MetadataValue::List(vec![single])
    }
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Self::Number(_) => None,
            Self::List(list) => {
                if list.is_empty() {
                    None
                } else {
                    Some(list[0].as_ref())
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

#[derive(Deserialize, Serialize, Eq, Hash, PartialEq, Debug, Clone)]
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

#[derive(Deserialize, Serialize, Eq, Hash, PartialEq, Debug, Display, EnumIter, Clone, Copy)]
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
impl From<BuiltinMetadataField> for MetadataField {
    fn from(value: BuiltinMetadataField) -> Self {
        Self::Builtin(value)
    }
}
