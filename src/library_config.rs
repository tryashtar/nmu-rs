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

#[derive(Deserialize)]
pub struct RawLibraryConfig {
    library: PathBuf,
    logs: Option<PathBuf>,
    config_folders: Vec<PathBuf>,
    extensions: HashSet<String>,
    custom_fields: Vec<String>,
    cache: Option<PathBuf>,
    art: Option<RawArtRepo>,
}

pub struct LibraryConfig {
    pub library_folder: PathBuf,
    pub log_folder: Option<PathBuf>,
    pub config_folders: Vec<PathBuf>,
    pub song_extensions: HashSet<String>,
    pub custom_fields: Vec<String>,
    pub date_cache: DateCache,
    pub art_repo: Option<ArtRepo>,
}
impl LibraryConfig {
    pub fn new(folder: &Path, raw: RawLibraryConfig) -> Self {
        Self {
            library_folder: folder.join(raw.library),
            log_folder: raw.logs.map(|x| folder.join(x)),
            config_folders: raw.config_folders.iter().map(|x| folder.join(x)).collect(),
            song_extensions: raw.extensions,
            custom_fields: raw.custom_fields,
            date_cache: DateCache::new(raw.cache.map(|x| folder.join(x))),
            art_repo: raw.art.map(|x| ArtRepo::new(&folder, x)),
        }
    }
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
                cache: match load_yaml(path.as_path()) {
                    Err(_) => HashMap::new(),
                    Ok(map) => map,
                },
            },
        }
    }
    fn save(&self) -> Result<(), YamlError> {
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
    fn new(path: Option<PathBuf>) -> Self {
        match path {
            None => Self {
                path: None,
                cache: HashMap::new(),
            },
            Some(path) => Self {
                path: Some(path.clone()),
                cache: match load_yaml(path.as_path()) {
                    Err(_) => HashMap::new(),
                    Ok(map) => map,
                },
            },
        }
    }
    fn save(&self) -> Result<(), YamlError> {
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
                    Ok(time) => Into::<SystemTime>::into(*cache_time) < time,
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
