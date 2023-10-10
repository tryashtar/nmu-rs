use chrono::{DateTime, Utc};
use path_absolutize::Absolutize;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use thiserror::Error;

use crate::file_stuff;
use crate::song_config::{
    AllSetter, Borrowable, BuiltinMetadataField, ItemSelector, MetadataField, MetadataOperation,
    MetadataValue, RawSongConfig, ReferencableOperation, SongConfig, ValueGetter,
};

#[derive(Deserialize)]
pub struct RawLibraryConfig<'a> {
    library: PathBuf,
    logs: Option<PathBuf>,
    config_folders: Vec<PathBuf>,
    extensions: HashSet<String>,
    custom_fields: Vec<String>,
    cache: Option<PathBuf>,
    art: Option<RawArtRepo>,
    named_strategies: HashMap<String, MetadataOperation<'a>>,
    find_replace: HashMap<String, String>,
}

pub struct LibraryConfig<'a> {
    pub library_folder: PathBuf,
    pub log_folder: Option<PathBuf>,
    pub config_folders: Vec<PathBuf>,
    pub song_extensions: HashSet<String>,
    pub custom_fields: Vec<MetadataField>,
    pub date_cache: DateCache,
    pub art_repo: Option<ArtRepo>,
    pub named_strategies: HashMap<String, MetadataOperation<'a>>,
    pub find_replace: HashMap<String, String>,
}
impl<'a> LibraryConfig<'a> {
    pub fn new(folder: &Path, raw: RawLibraryConfig<'a>) -> Self {
        Self {
            library_folder: folder.join(raw.library),
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
            named_strategies: raw.named_strategies,
            find_replace: raw.find_replace,
        }
    }
    pub fn resolve_config(
        &'a self,
        raw_config: RawSongConfig<'a>,
        folder: &Path,
    ) -> Result<SongConfig<'a>, LibraryError> {
        let songs = raw_config
            .songs
            .map(|x| {
                self.resolve_operation(x).map(|q| AllSetter {
                    names: ItemSelector::All,
                    set: q,
                })
            })
            .transpose()?;
        let set = raw_config
            .set
            .map(|x| {
                x.into_iter()
                    .map(|(y, z)| {
                        self.resolve_operation(z).map(|q| AllSetter {
                            names: ItemSelector::Path(y),
                            set: q,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .unwrap_or(vec![]);
        let set_all = raw_config
            .set_all
            .map(|x| {
                x.into_iter()
                    .map(|y| {
                        self.resolve_operation(y.set).map(|q| AllSetter {
                            names: y.names,
                            set: q,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .unwrap_or(vec![]);
        let order = raw_config
            .order
            .map(|x| {
                let matches = file_stuff::find_matches(&x, folder, self);
                let total = matches.len();
                matches
                    .into_iter()
                    .enumerate()
                    .map(|(track, path)| AllSetter {
                        names: ItemSelector::Path(path),
                        set: Borrowable::Owned(MetadataOperation::Set(HashMap::from([
                            (
                                BuiltinMetadataField::Track.into(),
                                ValueGetter::Direct(MetadataValue::Number((track + 1) as u32)),
                            ),
                            (
                                BuiltinMetadataField::TrackTotal.into(),
                                ValueGetter::Direct(MetadataValue::Number(total as u32)),
                            ),
                        ]))),
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let discs = raw_config
            .discs
            .map(|map| {
                let disc_total = *(map.keys().max().unwrap_or(&1));
                map.into_iter()
                    .flat_map(|(disc, sel)| {
                        let matches = file_stuff::find_matches(&sel, folder, self);
                        let track_total = matches.len();
                        matches
                            .into_iter()
                            .enumerate()
                            .map(move |(track, path)| AllSetter {
                                names: ItemSelector::Path(path),
                                set: Borrowable::Owned(MetadataOperation::Set(HashMap::from([
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
                                ]))),
                            })
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let merged = songs
            .into_iter()
            .chain(discs)
            .chain(order)
            .chain(set_all)
            .chain(set)
            .collect::<Vec<_>>();
        Ok(SongConfig { set: merged })
    }
    fn resolve_operation(
        &self,
        operation: ReferencableOperation<'a>,
    ) -> Result<Borrowable<MetadataOperation>, LibraryError> {
        match operation {
            ReferencableOperation::Direct(direct) => Ok(Borrowable::Owned(direct)),
            ReferencableOperation::Reference(reference) => {
                match self.named_strategies.get(&reference) {
                    None => Err(LibraryError::MissingNamedStrategy(reference)),
                    Some(strat) => Ok(Borrowable::Borrowed(strat)),
                }
            }
            ReferencableOperation::Sequence(sequence) => sequence
                .into_iter()
                .map(|x| self.resolve_operation(x))
                .collect::<Result<Vec<_>, _>>()
                .map(|x| Borrowable::Owned(MetadataOperation::Sequence(x))),
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
                cache: match load_yaml(&path) {
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
                cache: match load_yaml(&path) {
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

#[derive(Error, Debug)]
#[error("{0}")]
pub enum YamlError {
    Io(#[from] std::io::Error),
    Yaml(#[from] serde_yaml::Error),
}

pub fn load_yaml<T>(path: &Path) -> Result<T, YamlError>
where
    T: DeserializeOwned,
{
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let yaml: T = serde_yaml::from_reader(reader)?;
    Ok(yaml)
}
