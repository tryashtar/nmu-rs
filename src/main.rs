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
    art: Option<RawArtRepo>,
}

struct LibraryConfig {
    library_folder: PathBuf,
    log_folder: Option<PathBuf>,
    config_folders: Vec<PathBuf>,
    song_extensions: HashSet<String>,
    custom_fields: Vec<String>,
    date_cache: DateCache,
    art_repo: Option<ArtRepo>,
}
impl LibraryConfig {
    fn new(folder: &Path, raw: RawLibraryConfig) -> Self {
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

struct ArtRepo {
    templates_folder: PathBuf,
    cache_folder: Option<PathBuf>,
    icon_folder: Option<PathBuf>,
    used_templates: ArtCache,
    named_settings: HashMap<String, ArtTemplateSettings>,
    image_extensions: HashSet<String>,
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

struct ArtCache {
    path: Option<PathBuf>,
    cache: HashMap<PathBuf, Vec<PathBuf>>,
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
struct ArtTemplateSettings {}

struct DateCache {
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
        .filter_map(|x| file_path(x, &library_config.song_extensions))
    {
        scan_songs.insert(song);
    }
    for config_root in &library_config.config_folders {
        for config_path in WalkDir::new(config_root)
            .into_iter()
            .filter_map(|x| recently_changed_path(x, &library_config.date_cache, "config.yaml"))
        {
            let config_folder = config_path.parent().unwrap_or_else(|| Path::new(""));
            for song in WalkDir::new(config_folder)
                .into_iter()
                .filter_map(|x| file_path(x, &library_config.song_extensions))
            {
                scan_songs.insert(song);
            }
        }
    }
    if let Some(art_repo) = &library_config.art_repo {
        let mut scan_images = HashSet::<PathBuf>::new();
        for config_path in WalkDir::new(&art_repo.templates_folder)
            .into_iter()
            .filter_map(|x| recently_changed_path(x, &library_config.date_cache, "images.yaml"))
        {
            let config_folder = config_path.parent().unwrap_or_else(|| Path::new(""));
            for image in WalkDir::new(config_folder)
                .into_iter()
                .filter_map(|x| file_path(x, &art_repo.image_extensions))
            {
                scan_images.insert(image);
            }
        }
        for (image_path, songs) in &art_repo.used_templates.cache {
            if scan_images.contains(image_path)
                || library_config
                    .date_cache
                    .changed_recently(image_path.as_path())
            {
                for song in songs {
                    scan_songs.insert(song.clone());
                }
            }
        }
    }
}

fn file_path(
    item: jwalk::Result<jwalk::DirEntry<((), ())>>,
    filter_extensions: &HashSet<String>,
) -> Option<PathBuf> {
    match item {
        Err(_) => None,
        Ok(entry) => {
            let path: PathBuf = entry.path();
            if let Some(ext) = path.extension().and_then(|x| x.to_str()) {
                if filter_extensions.contains(ext) {
                    return Some(path);
                }
            }
            None
        }
    }
}

fn recently_changed_path(
    item: jwalk::Result<jwalk::DirEntry<((), ())>>,
    cache: &DateCache,
    filter_filename: &str,
) -> Option<PathBuf> {
    match item {
        Err(_) => None,
        Ok(entry) => {
            let path: PathBuf = entry.path();
            if let Some(file_name) = path.file_name().and_then(|x| x.to_str()) {
                if file_name == filter_filename && cache.changed_recently(&path) {
                    return Some(path);
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
