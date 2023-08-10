use chrono::{DateTime, Utc};
use jwalk::WalkDir;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use thiserror::Error;

#[derive(Deserialize)]
struct RawLibraryConfig {
    library: PathBuf,
    logs: Option<PathBuf>,
    config_folders: Vec<PathBuf>,
    extensions: HashSet<String>,
    custom_fields: Vec<String>,
    cache: Option<PathBuf>,
}

struct LibraryConfig {
    library_folder: PathBuf,
    logs: Option<PathBuf>,
    config_folders: Vec<PathBuf>,
    extensions: HashSet<String>,
    custom_fields: Vec<String>,
    date_cache: DateCache,
}
impl LibraryConfig {
    pub fn new(folder: &Path, raw: RawLibraryConfig) -> Self {
        LibraryConfig {
            library_folder: folder.join(raw.library),
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
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, &self.cache)?;
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
    let library_config_path = Path::new("/d/Music/.music-cache/library.yaml");
    let raw = load_yaml::<RawLibraryConfig>(library_config_path);
    match raw {
        Err(error) => {
            println!("{}", error);
            return;
        }
        Ok(raw) => {
            let library_config_folder = library_config_path
                .parent()
                .unwrap_or_else(|| Path::new(""));
            let library_config = LibraryConfig::new(library_config_folder, raw);
            let mut scan_songs = HashSet::<PathBuf>::new();
            find_scan_songs(&mut scan_songs, &library_config);
            for scan in scan_songs {
                println!("{}", scan.display());
            }
        }
    }
}

fn find_scan_songs(scan_songs: &mut HashSet<PathBuf>, library_config: &LibraryConfig) {
    for song in WalkDir::new(&library_config.library_folder)
        .into_iter()
        .filter_map(|x| song_path(x, &library_config.extensions))
    {
        scan_songs.insert(song);
    }
    for config_root in &library_config.config_folders {
        for config_path in WalkDir::new(config_root)
            .into_iter()
            .filter_map(|x| recently_changed_config_path(x, &library_config.date_cache))
        {
            let config_folder = config_path.parent().unwrap_or_else(|| Path::new(""));
            for song in WalkDir::new(config_folder)
                .into_iter()
                .filter_map(|x| song_path(x, &library_config.extensions))
            {
                scan_songs.insert(song);
            }
        }
    }
}

fn song_path(
    item: jwalk::Result<jwalk::DirEntry<((), ())>>,
    extensions: &HashSet<String>,
) -> Option<PathBuf> {
    match item {
        Err(_) => None,
        Ok(entry) => {
            let path: PathBuf = entry.path();
            if let Some(ext) = path.extension().and_then(|x| x.to_str()) {
                if extensions.contains(ext) {
                    Some(entry);
                }
            }
            None
        }
    }
}

fn recently_changed_config_path(
    item: jwalk::Result<jwalk::DirEntry<((), ())>>,
    cache: &DateCache,
) -> Option<PathBuf> {
    match item {
        Err(_) => None,
        Ok(entry) => {
            let path: PathBuf = entry.path();
            if let Some(file_name) = path.file_name().and_then(|x| x.to_str()) {
                if file_name == "config.yaml" && cache.changed_recently(&path) {
                    Some(entry);
                }
            }
            None
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
    let reader = BufReader::new(file);
    let yaml: T = serde_yaml::from_reader(reader)?;
    Ok(yaml)
}
