use chrono::{DateTime, Utc};
use path_absolutize::Absolutize;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs::{self, File};
use std::io::BufWriter;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::SystemTime;
use thiserror::Error;

use crate::file_stuff::{self, load_yaml, YamlError};
use crate::metadata::{BuiltinMetadataField, Metadata, MetadataField, MetadataValue, BLANK_VALUE};
use crate::song_config::{AllSetter, RawSongConfig, ReferencableOperation, SongConfig};
use crate::strategy::{FieldSelector, ItemSelector, MetadataOperation, MusicItemType, ValueGetter};

#[derive(Deserialize)]
pub struct RawLibraryConfig {
    library: PathBuf,
    reports: Vec<RawLibraryReport>,
    logs: Option<PathBuf>,
    config_folders: Vec<PathBuf>,
    extensions: HashSet<String>,
    custom_fields: Vec<String>,
    cache: Option<PathBuf>,
    art: Option<RawArtRepo>,
    named_strategies: HashMap<String, MetadataOperation>,
    find_replace: HashMap<String, String>,
    artist_separator: String,
}

#[derive(Deserialize)]
#[serde(untagged)]
pub enum RawLibraryReport {
    Fields {
        path: PathBuf,
        key: MetadataField,
        #[serde(default = "default_false")]
        split: bool,
        #[serde(default = "default_false")]
        blanks: bool,
    },
    Items {
        path: PathBuf,
        values: FieldSelector,
        #[serde(default = "default_false")]
        embedded: bool,
        #[serde(default = "default_false")]
        blanks: bool,
    },
}
fn default_false() -> bool {
    false
}

pub enum LibraryReport {
    SplitFields {
        path: PathBuf,
        key: MetadataField,
        include_blanks: bool,
        map: BTreeMap<Option<String>, BTreeSet<PathBuf>>,
    },
    MergedFields {
        path: PathBuf,
        key: MetadataField,
        include_blanks: bool,
        map: BTreeMap<Option<String>, BTreeSet<PathBuf>>,
    },
    ItemData {
        path: PathBuf,
        values: FieldSelector,
        embedded: bool,
        include_blanks: bool,
        map: BTreeMap<PathBuf, BTreeMap<MetadataField, ReportValue>>,
    },
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ReportValue {
    String(String),
    List(Vec<String>),
    Number(u32),
    None,
}
impl LibraryReport {
    pub fn save(&self) -> Result<(), YamlError> {
        match self {
            Self::SplitFields { path, map, .. } => {
                let file = File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, map)?;
                Ok(())
            }
            Self::MergedFields { path, map, .. } => {
                let file = File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, map)?;
                Ok(())
            }
            Self::ItemData { path, map, .. } => {
                let file = File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, map)?;
                Ok(())
            }
        }
    }
    fn load(source: RawLibraryReport, folder: &Path) -> LibraryReport {
        match source {
            RawLibraryReport::Fields {
                path,
                key,
                split,
                blanks,
            } => {
                let file_path = folder.join(path);
                let map = load_yaml(&file_path).unwrap_or_default();
                if split {
                    LibraryReport::SplitFields {
                        path: file_path,
                        key,
                        include_blanks: blanks,
                        map,
                    }
                } else {
                    LibraryReport::MergedFields {
                        path: file_path,
                        key,
                        include_blanks: blanks,
                        map,
                    }
                }
            }
            RawLibraryReport::Items {
                path,
                values,
                embedded,
                blanks,
            } => {
                let file_path = folder.join(path);
                let map = load_yaml(&file_path).unwrap_or_default();
                LibraryReport::ItemData {
                    path: file_path,
                    values,
                    embedded,
                    include_blanks: blanks,
                    map,
                }
            }
        }
    }
    fn val_to_str(value: &MetadataValue, sep: &str) -> Option<String> {
        match value {
            MetadataValue::Number(n) => Some(n.to_string()),
            MetadataValue::List(l) if l.is_empty() => None,
            MetadataValue::List(l) => Some(l.join(sep)),
        }
    }
    fn val_to_strs(value: &MetadataValue) -> Vec<String> {
        match value {
            MetadataValue::Number(n) => vec![n.to_string()],
            MetadataValue::List(l) => l.clone(),
        }
    }
    fn is_blank(value: &MetadataValue) -> bool {
        match value {
            MetadataValue::Number(_) => false,
            MetadataValue::List(l) => l.is_empty(),
        }
    }
    fn to_value(value: &MetadataValue) -> ReportValue {
        match value {
            MetadataValue::Number(n) => ReportValue::Number(*n),
            MetadataValue::List(l) if l.is_empty() => ReportValue::None,
            MetadataValue::List(l) if l.len() == 1 => ReportValue::String(l[0].clone()),
            MetadataValue::List(l) => ReportValue::List(l.clone()),
        }
    }
    pub fn record(
        &mut self,
        item_path: &Path,
        metadata: &Metadata,
        embedded_metadata: Option<&Metadata>,
        sep: &str,
    ) {
        match self {
            Self::MergedFields {
                key,
                include_blanks,
                map,
                ..
            } => {
                let value = metadata.get(key).unwrap_or(&BLANK_VALUE);
                if *include_blanks || !Self::is_blank(value) {
                    let list = map.entry(Self::val_to_str(value, sep)).or_default();
                    list.insert(item_path.to_owned());
                }
            }
            Self::SplitFields {
                key,
                include_blanks,
                map,
                ..
            } => {
                let value = metadata.get(key).unwrap_or(&BLANK_VALUE);
                if *include_blanks && Self::is_blank(value) {
                    let list = map.entry(None).or_default();
                    list.insert(item_path.to_owned());
                } else {
                    for entry in Self::val_to_strs(value) {
                        let list = map.entry(Some(entry)).or_default();
                        list.insert(item_path.to_owned());
                    }
                }
            }
            Self::ItemData {
                values,
                embedded,
                include_blanks,
                map,
                ..
            } => {
                let mut results = BTreeMap::new();
                if let Some(save) = {
                    if *embedded {
                        embedded_metadata
                    } else {
                        Some(metadata)
                    }
                } {
                    for (field, value) in save {
                        if values.is_match(field) && (*include_blanks || !Self::is_blank(value)) {
                            results.insert(field.clone(), Self::to_value(value));
                        }
                    }
                    if !results.is_empty() {
                        map.insert(item_path.into(), results);
                    }
                }
            }
        }
    }
}

pub struct LibraryConfig {
    pub library_folder: PathBuf,
    pub reports: Vec<LibraryReport>,
    pub log_folder: Option<PathBuf>,
    pub config_folders: Vec<PathBuf>,
    pub song_extensions: HashSet<String>,
    pub custom_fields: Vec<MetadataField>,
    pub date_cache: DateCache,
    pub art_repo: Option<ArtRepo>,
    pub named_strategies: HashMap<String, Rc<MetadataOperation>>,
    pub find_replace: HashMap<String, String>,
    pub artist_separator: String,
}
impl LibraryConfig {
    pub fn new(folder: &Path, raw: RawLibraryConfig) -> Self {
        Self {
            library_folder: folder.join(raw.library),
            reports: raw
                .reports
                .into_iter()
                .map(|x| LibraryReport::load(x, folder))
                .collect(),
            log_folder: raw.logs.map(|x| folder.join(x)),
            config_folders: raw
                .config_folders
                .into_iter()
                .map(|x| folder.join(x))
                .collect(),
            song_extensions: raw
                .extensions
                .into_iter()
                .map(|x| match x.strip_prefix('.') {
                    Some(stripped) => stripped.to_owned(),
                    None => x,
                })
                .collect(),
            custom_fields: raw
                .custom_fields
                .into_iter()
                .map(MetadataField::Custom)
                .collect(),
            date_cache: DateCache::new(raw.cache.map(|x| folder.join(x))),
            art_repo: raw.art.map(|x| ArtRepo::new(folder, x)),
            named_strategies: raw
                .named_strategies
                .into_iter()
                .map(|(k, v)| (k, Rc::new(v)))
                .collect(),
            find_replace: raw.find_replace,
            artist_separator: raw.artist_separator,
        }
    }
    pub fn resolve_config(
        &self,
        raw_config: RawSongConfig,
        folder: &Path,
    ) -> Result<SongConfig, LibraryError> {
        let songs = raw_config
            .songs
            .map(|x| {
                self.resolve_operation(x).map(|ops| AllSetter {
                    names: ItemSelector::All,
                    must_be: Some(MusicItemType::Song),
                    set: ops,
                })
            })
            .transpose()?;
        let folders = raw_config
            .folders
            .map(|x| {
                self.resolve_operation(x).map(|ops| AllSetter {
                    names: ItemSelector::All,
                    must_be: Some(MusicItemType::Folder),
                    set: ops,
                })
            })
            .transpose()?;
        let this = raw_config
            .this
            .map(|x| {
                self.resolve_operation(x).map(|ops| AllSetter {
                    names: ItemSelector::This,
                    must_be: None,
                    set: ops,
                })
            })
            .transpose()?;
        let discs = raw_config
            .discs
            .map(|map| {
                let disc_total = *(map.keys().max().unwrap_or(&1));
                map.into_iter()
                    .flat_map(|(disc, sel)| {
                        let matches = file_stuff::find_matches(&sel, folder, self);
                        let track_total = matches.len();
                        matches.into_iter().enumerate().map(move |(track, path)| {
                            AllSetter::new(
                                ItemSelector::Path(path.into()),
                                MetadataOperation::Set(HashMap::from([
                                    (
                                        BuiltinMetadataField::Track.into(),
                                        ValueGetter::Direct(MetadataValue::Number(
                                            (track + 1) as u32,
                                        )),
                                    ),
                                    (
                                        BuiltinMetadataField::TrackTotal.into(),
                                        ValueGetter::Direct(MetadataValue::Number(
                                            track_total as u32,
                                        )),
                                    ),
                                    (
                                        BuiltinMetadataField::Disc.into(),
                                        ValueGetter::Direct(MetadataValue::Number(disc)),
                                    ),
                                    (
                                        BuiltinMetadataField::DiscTotal.into(),
                                        ValueGetter::Direct(MetadataValue::Number(disc_total)),
                                    ),
                                ])),
                            )
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let order = raw_config
            .order
            .map(|x| {
                let matches = file_stuff::find_matches(&x, folder, self);
                let total = matches.len();
                matches
                    .into_iter()
                    .enumerate()
                    .map(|(track, path)| {
                        AllSetter::new(
                            ItemSelector::Path(path.into()),
                            MetadataOperation::Set(HashMap::from([
                                (
                                    BuiltinMetadataField::Track.into(),
                                    ValueGetter::Direct(MetadataValue::Number((track + 1) as u32)),
                                ),
                                (
                                    BuiltinMetadataField::TrackTotal.into(),
                                    ValueGetter::Direct(MetadataValue::Number(total as u32)),
                                ),
                            ])),
                        )
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let set_fields = raw_config
            .set_fields
            .map(|sets| {
                sets.into_iter()
                    .flat_map(|setter| {
                        setter
                            .set
                            .into_iter()
                            .map(|(path, getter)| {
                                AllSetter::new(
                                    ItemSelector::Path(path),
                                    MetadataOperation::Set(HashMap::from([(
                                        setter.field.clone(),
                                        getter,
                                    )])),
                                )
                            })
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let set_all = raw_config
            .set_all
            .map(|x| {
                x.into_iter()
                    .map(|y| {
                        self.resolve_operation(y.set).map(|ops| AllSetter {
                            names: y.names,
                            must_be: None,
                            set: ops,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .unwrap_or_default();
        let set = raw_config
            .set
            .map(|map| {
                map.into_iter()
                    .map(|(path, ops)| {
                        self.resolve_operation(ops).map(|ops| AllSetter {
                            names: ItemSelector::Path(path),
                            must_be: None,
                            set: ops,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .unwrap_or(vec![]);
        let merged = songs
            .into_iter()
            .chain(folders)
            .chain(this)
            .chain(discs)
            .chain(order)
            .chain(set_fields)
            .chain(set_all)
            .chain(set)
            .collect::<Vec<_>>();
        Ok(SongConfig { set: merged })
    }
    fn resolve_operation(
        &self,
        operation: ReferencableOperation,
    ) -> Result<Rc<MetadataOperation>, LibraryError> {
        match operation {
            ReferencableOperation::Direct(direct) => Ok(Rc::new(direct)),
            ReferencableOperation::Reference(reference) => self
                .named_strategies
                .get(&reference)
                .cloned()
                .ok_or(LibraryError::MissingNamedStrategy(reference)),
            ReferencableOperation::Many(many) => many
                .into_iter()
                .map(|x| self.resolve_operation(x))
                .collect::<Result<Vec<_>, _>>()
                .map(|x| Rc::new(MetadataOperation::Many(x))),
        }
    }
}

#[derive(Error, Debug)]
#[error("{0}")]
pub enum LibraryError {
    MissingNamedStrategy(String),
}

#[derive(Deserialize)]
struct RawArtRepo {
    templates: PathBuf,
    cache: Option<PathBuf>,
    icons: Option<PathBuf>,
    file_cache: Option<PathBuf>,
    named_settings: HashMap<String, ArtTemplateSettings>,
    extensions: HashSet<String>,
}

pub struct ArtRepo {
    pub templates_folder: PathBuf,
    pub cache_folder: Option<PathBuf>,
    pub icon_folder: Option<PathBuf>,
    pub used_templates: ArtCache,
    pub named_settings: HashMap<String, ArtTemplateSettings>,
    pub image_extensions: HashSet<String>,
}
impl ArtRepo {
    fn new(folder: &Path, raw: RawArtRepo) -> Self {
        Self {
            templates_folder: folder.join(raw.templates),
            cache_folder: raw.cache.map(|x| folder.join(x)),
            icon_folder: raw.icons.map(|x| folder.join(x)),
            used_templates: ArtCache::new(raw.file_cache.map(|x| folder.join(x))),
            named_settings: raw.named_settings,
            image_extensions: raw.extensions,
        }
    }
}

pub struct ArtCache {
    path: Option<PathBuf>,
    pub cache: HashMap<PathBuf, Vec<PathBuf>>,
}
impl ArtCache {
    fn new(path: Option<PathBuf>) -> Self {
        match path {
            None => Self {
                path: None,
                cache: HashMap::new(),
            },
            Some(path) => Self {
                path: Some(path.clone()),
                cache: match file_stuff::load_yaml(&path) {
                    Err(_) => HashMap::new(),
                    Ok(map) => map,
                },
            },
        }
    }
    pub fn save(&self) -> Result<(), YamlError> {
        match &self.path {
            None => Ok(()),
            Some(path) => {
                let file = File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, &self.cache)?;
                Ok(())
            }
        }
    }
}

#[derive(Deserialize)]
pub struct ArtTemplateSettings {}

pub struct DateCache {
    path: Option<PathBuf>,
    cache: HashMap<PathBuf, DateTime<Utc>>,
}
impl DateCache {
    pub fn new(path: Option<PathBuf>) -> Self {
        match path {
            None => Self {
                path: None,
                cache: HashMap::new(),
            },
            Some(path) => Self {
                path: Some(path.clone()),
                cache: match file_stuff::load_yaml(&path) {
                    Err(_) => HashMap::new(),
                    Ok(map) => map,
                },
            },
        }
    }
    pub fn save(&self) -> Result<(), YamlError> {
        match &self.path {
            None => Ok(()),
            Some(path) => {
                let file = File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, &self.cache)?;
                Ok(())
            }
        }
    }
    pub fn changed_recently(&self, path: &Path) -> bool {
        match path.absolutize() {
            Err(_) => true,
            Ok(path) => match self.cache.get(path.as_ref()) {
                None => true,
                Some(cache_time) => match fs::metadata(&path).and_then(|x| x.modified()) {
                    Err(_) => true,
                    Ok(file_time) => Into::<SystemTime>::into(*cache_time) < file_time,
                },
            },
        }
    }
}
