use core::fmt;
use std::{
    collections::HashMap,
    ops::Deref,
    path::{Path, PathBuf},
    rc::Rc,
};

use regex::Regex;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumIter};

use crate::{
    file_stuff::ConfigError,
    get_metadata,
    library_config::LibraryConfig,
    modifier::{ValueError, ValueModifier},
    util::ItemPath,
    ConfigCache, Results,
};

pub struct PendingMetadata {
    pub fields: HashMap<MetadataField, PendingValue>,
}
pub type Metadata = HashMap<MetadataField, MetadataValue>;
type MetadataCache = HashMap<PathBuf, Result<Metadata, Rc<ConfigError>>>;
impl PendingMetadata {
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }
    fn try_resolve_value(
        value: &mut PendingValue,
        nice_path: &Path,
        metadata: &Metadata,
        metadata_cache: &mut MetadataCache,
        config_cache: &mut ConfigCache,
        library_config: &LibraryConfig,
    ) -> Option<MetadataValue> {
        match value {
            PendingValue::Ready(ready) => Some(ready.clone()),
            PendingValue::RegexMatches { .. } => None,
            PendingValue::CopyField {
                field: from,
                sources,
                modify,
            } => {
                let mut results = sources.iter().filter_map(|source| {
                    let source_metadata = {
                        if nice_path == source.deref() {
                            Some(metadata)
                        } else {
                            metadata_cache
                                .entry(source.clone().into())
                                .or_insert_with(|| {
                                    get_metadata(source, library_config, config_cache)
                                        .map(|x| x.result)
                                })
                                .as_ref()
                                .ok()
                        }
                    };
                    source_metadata.and_then(|meta| {
                        meta.get(from).cloned().and_then(|x| match modify {
                            None => Some(x.into()),
                            Some(modify) => modify
                                .modify(x.into(), source.as_ref(), library_config)
                                .ok(),
                        })
                    })
                });
                match results.nth(0) {
                    Some(PendingValue::Ready(ready)) => Some(ready),
                    _ => None,
                }
            }
        }
    }
    pub fn resolve(
        mut self,
        nice_path: &Path,
        library_config: &LibraryConfig,
        config_cache: &mut ConfigCache,
    ) -> Results<Metadata, ValueError> {
        let mut metadata = Metadata::new();
        let mut metadata_cache: MetadataCache = HashMap::new();
        loop {
            let mut added_any = false;
            self.fields.retain(|field, value| {
                if let Some(resolved) = Self::try_resolve_value(
                    value,
                    nice_path,
                    &metadata,
                    &mut metadata_cache,
                    config_cache,
                    library_config,
                ) {
                    metadata.insert(field.clone(), resolved);
                    added_any = true;
                    return false;
                }
                true
            });
            if !added_any {
                break;
            }
        }
        let mut errors = vec![];
        // remaining items that couldn't be resolved
        for (field, value) in self.fields {
            errors.push(ValueError::ResolutionFailed { field, value })
        }
        Results {
            result: metadata,
            errors,
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
impl fmt::Display for PendingValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ready(r) => r.fmt(f),
            Self::RegexMatches { source, regex } => write!(f, "regex {} on {}", source, regex),
            Self::CopyField { field, sources, .. } => {
                write!(f, "copy {} from {} sources", field, sources.len())
            }
        }
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

pub struct FinalMetadata {
    pub title: Option<String>,
    pub album: Option<String>,
    pub performers: Vec<String>,
    pub album_artist: Option<String>,
    pub composers: Vec<String>,
    pub arranger: Option<String>,
    pub comment: Option<String>,
    pub track: Option<u32>,
    pub track_total: Option<u32>,
    pub disc: Option<u32>,
    pub disc_total: Option<u32>,
    pub year: Option<u32>,
    pub language: Option<String>,
    pub genres: Vec<String>,
    pub art: Vec<PathBuf>,
    pub simple_lyrics: Option<String>,
}
impl FinalMetadata {
    pub fn create(mut metadata: Metadata) -> Results<FinalMetadata, ValueError> {
        let mut errors = vec![];
        let mut convert_string = |field: BuiltinMetadataField| {
            let field = field.into();
            match metadata.remove(&field) {
                None => None,
                Some(MetadataValue::List(list)) if list.is_empty() => None,
                Some(value) => match value.as_string() {
                    Some(val) => Some(val.to_owned()),
                    None => {
                        errors.push(ValueError::WrongFieldType {
                            field,
                            got: value,
                            expected: "single string",
                        });
                        None
                    }
                },
            }
        };
        let title = convert_string(BuiltinMetadataField::Title);
        let album = convert_string(BuiltinMetadataField::Album);
        let album_artist = convert_string(BuiltinMetadataField::AlbumArtist);
        let arranger = convert_string(BuiltinMetadataField::Arranger);
        let comment = convert_string(BuiltinMetadataField::Comment);
        let language = convert_string(BuiltinMetadataField::Language);
        let simple_lyrics = convert_string(BuiltinMetadataField::SimpleLyrics);
        let mut convert_vec = |field: BuiltinMetadataField| {
            let field = field.into();
            match metadata.remove(&field) {
                None => vec![],
                Some(value) => match value {
                    MetadataValue::List(list) => list,
                    MetadataValue::Number(_) => {
                        errors.push(ValueError::WrongFieldType {
                            field,
                            got: value,
                            expected: "list",
                        });
                        vec![]
                    }
                },
            }
        };
        let performers = convert_vec(BuiltinMetadataField::Performers);
        let composers = convert_vec(BuiltinMetadataField::Composers);
        let genres = convert_vec(BuiltinMetadataField::Genres);
        let art = convert_vec(BuiltinMetadataField::Art)
            .into_iter()
            .map(PathBuf::from)
            .collect();
        let mut convert_num = |field: BuiltinMetadataField| {
            let field = field.into();
            match metadata.remove(&field) {
                None => None,
                Some(MetadataValue::List(list)) if list.is_empty() => None,
                Some(value) => match value {
                    MetadataValue::Number(num) => Some(num),
                    MetadataValue::List(_) => {
                        errors.push(ValueError::WrongFieldType {
                            field,
                            got: value,
                            expected: "number",
                        });
                        None
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
                comment,
                track,
                track_total,
                disc,
                disc_total,
                year,
                language,
                genres,
                art,
                simple_lyrics,
            },
            errors,
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
    #[serde(alias = "album artist")]
    AlbumArtist,
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
