use chrono::{DateTime, Utc};
use serde::de::DeserializeOwned;
use serde::Deserialize;
use std::collections::HashMap;
use std::fs::File;
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Deserialize)]
struct RawLibraryConfig {
    library: PathBuf,
    logs: Option<PathBuf>,
    config_folders: Vec<PathBuf>,
    extensions: Vec<String>,
    custom_fields: Vec<String>,
    cache: Option<PathBuf>,
}

struct LibraryConfig {
    library: PathBuf,
    logs: Option<PathBuf>,
    config_folders: Vec<PathBuf>,
    extensions: Vec<String>,
    custom_fields: Vec<String>,
    date_cache: DateCache,
}
impl LibraryConfig {
    pub fn new(folder: &Path, raw: RawLibraryConfig) -> Self {
        LibraryConfig {
            library: folder.join(raw.library),
            logs: raw.logs.map(|x| folder.join(x)),
            config_folders: raw.config_folders.iter().map(|x| folder.join(x)).collect(),
            extensions: raw.extensions,
            custom_fields: raw.custom_fields,
            date_cache: DateCache::new(raw.cache.map(|x| folder.join(x))),
        }
    }
}

struct DateCache {
    path: Option<PathBuf>,
    cache: HashMap<PathBuf, DateTime<Utc>>,
}
impl DateCache {
    pub fn new(path: Option<PathBuf>) -> Self {
        match path {
            None => DateCache {
                path: None,
                cache: HashMap::new(),
            },
            Some(path) => DateCache {
                path: Some(path.clone()),
                cache: match load_yaml(path.as_path()) {
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
                serde_yaml::to_writer(file, &self.cache)?;
                Ok(())
            }
        }
    }
}

fn main() {
    let path = Path::new("/d/Music/.music-cache/library.yaml");
    let raw = load_yaml::<RawLibraryConfig>(path);
    match raw {
        Err(error) => {
            println!("{}", error);
            return;
        }
        Ok(raw) => {
            let config = LibraryConfig::new(path.parent().unwrap(), raw);
            for f in config.date_cache.cache {
                println!("{}: {}", f.0.display(), f.1);
            }
        }
    }
}

#[derive(Error, Debug)]
#[error("{0}")]
enum YamlError {
    Io(#[from] std::io::Error),
    Yaml(#[from] serde_yaml::Error),
}

fn load_yaml<T>(path: &Path) -> Result<T, YamlError>
where
    T: DeserializeOwned,
{
    let file = File::open(path)?;
    let yaml: T = serde_yaml::from_reader(file)?;
    Ok(yaml)
}
