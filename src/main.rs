use chrono::{DateTime, Utc};
use jwalk::WalkDir;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use std::collections::HashMap;
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::time::SystemTime;
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
    fn new(path: Option<PathBuf>) -> Self {
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
    fn save(&self) -> Result<(), YamlError> {
        match &self.path {
            None => Ok(()),
            Some(path) => {
                let file = File::create(path)?;
                serde_yaml::to_writer(file, &self.cache)?;
                Ok(())
            }
        }
    }
    fn changed_recently(&self, path: &Path) -> bool {
        match fs::canonicalize(path) {
            Err(_) => true,
            Ok(path) => match self.cache.get(&path) {
                None => true,
                Some(cache_time) => match fs::metadata(&path).and_then(|x| x.modified()) {
                    Err(_) => true,
                    Ok(time) => Into::<SystemTime>::into(*cache_time) < time,
                },
            },
        }
    }
}

fn main() {
    println!("NAIVE MUSIC UPDATER");
    let path = Path::new("/d/Music/.music-cache/library.yaml");
    let raw = load_yaml::<RawLibraryConfig>(path);
    match raw {
        Err(error) => {
            println!("{}", error);
            return;
        }
        Ok(raw) => {
            let config = LibraryConfig::new(path.parent().unwrap(), raw);
            for folder in config.config_folders {
                for entry in WalkDir::new(folder) {
                    let path = entry.unwrap().path();
                    if let Some(name) = path.file_name().and_then(|x| x.to_str()) {
                        if name == "config.yaml" && config.date_cache.changed_recently(&path) {
                            println!("{}", path.display());
                        }
                    }
                }
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
