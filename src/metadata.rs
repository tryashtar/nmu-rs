use std::{collections::HashMap, path::{Path, PathBuf}};

use regex::Regex;
use serde::{Deserialize, Serialize};
use strum::{EnumIter, Display};

use crate::{library_config::LibraryConfig, song_config::SongConfig, get_metadata, modifier::ValueModifier, util::Borrowable};

pub struct PendingMetadata<'a> {
    pub fields: HashMap<MetadataField, PendingValue<'a>>,
}
pub struct Metadata {
    pub fields: HashMap<MetadataField, MetadataValue>,
}

impl PendingMetadata<'_> {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }
    pub fn resolve<'a>(
        mut self,
        path: &Path,
        config: &'a LibraryConfig,
        cache: &mut HashMap<PathBuf, Option<SongConfig<'a>>>,
    ) -> Metadata {
        let mut metadata = Metadata::new();
        let mut metadata_cache: HashMap<PathBuf, Metadata> = HashMap::new();
        loop {
            let mut added_any = false;
            self.fields.retain(|field, value| {
                let result = match value {
                    PendingValue::Ready(ready) => {
                        metadata.fields.insert(field.clone(), ready.clone());
                        false
                    }
                    PendingValue::RegexMatches { .. } => true,
                    PendingValue::CopyField {
                        field: from,
                        sources,
                        modify,
                    } => {
                        let source_path = sources[0].clone();
                        let source_path2 = sources[0].clone();
                        match {
                            if path == source_path {
                                &mut metadata
                            } else {
                                metadata_cache
                                    .entry(source_path)
                                    .or_insert_with_key(|key| get_metadata(key, config, cache))
                            }
                        }
                        .fields
                        .get(from)
                        .cloned()
                        .and_then(|x| match modify {
                            Some(modify) => modify.modify(x.into(), &source_path2, config).ok(),
                            None => Some(x.into()),
                        }) {
                            Some(PendingValue::Ready(ready)) => {
                                metadata.fields.insert(field.clone(), ready);
                                false
                            }
                            _ => true,
                        }
                    }
                };
                added_any |= !result;
                result
            });
            if !added_any {
                break;
            }
        }
        metadata
    }
}
impl From<Metadata> for PendingMetadata<'_> {
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
impl Metadata {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }
}

#[derive(Clone)]
pub enum PendingValue<'a> {
    Ready(MetadataValue),
    RegexMatches {
        source: String,
        regex: Regex,
    },
    CopyField {
        field: MetadataField,
        sources: Vec<PathBuf>,
        modify: Option<Borrowable<'a, ValueModifier<'a>>>,
    },
}
impl From<MetadataValue> for PendingValue<'_> {
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

#[derive(Deserialize, Serialize, Eq, Hash, PartialEq, Debug, Clone)]
#[serde(untagged)]
pub enum MetadataField {
    Builtin(BuiltinMetadataField),
    Custom(String),
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
