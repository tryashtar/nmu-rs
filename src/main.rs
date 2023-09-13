use colored::Colorize;
use itertools::Itertools;
use jwalk::WalkDir;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::env;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use thiserror::Error;

mod tests;

mod library_config;
use library_config::*;

mod song_config;
use song_config::*;

mod tag_interop;
use tag_interop::Tags;

fn main() {
    println!("NAIVE MUSIC UPDATER");
    let first_argument = env::args().nth(1);
    let library_config_path = Path::new(first_argument.as_deref().unwrap_or("library.yaml"));
    let raw_config = load_yaml::<RawLibraryConfig>(library_config_path);
    match raw_config {
        Err(YamlError::Io(error))
            if error.kind() == ErrorKind::NotFound && first_argument.is_none() =>
        {
            eprintln!("{}", error.to_string().red());
            if let Ok(dir) = std::env::current_dir() {
                eprintln!(
                    "Provide the path to a library.yaml or add one to '{}'",
                    dir.display()
                );
            }
        }
        Err(error) => {
            eprintln!("{}", error.to_string().red());
        }
        Ok(raw_config) => {
            let library_config_folder = library_config_path.parent().unwrap_or(Path::new(""));
            let library_config: LibraryConfig =
                LibraryConfig::new(library_config_folder, raw_config);
            do_scan(library_config);
        }
    }
}

fn do_scan(library_config: LibraryConfig) {
    let mut cached_configs: HashMap<PathBuf, Option<SongConfig>> = HashMap::new();
    let ScanResults {
        songs: scan_songs,
        images: scan_images,
    } = find_scan_songs(&library_config);
    for song_path in scan_songs {
        let nice_path = song_path
            .strip_prefix(&library_config.library_folder)
            .unwrap_or(song_path.as_path())
            .with_extension("");
        println!("{}", nice_path.display());
        let mut metadata = PendingMetadata::new();
        let tags = Tags::load(&song_path);
        let existing_metadata = tags.get_metadata();
        let relative_parent = song_path
            .strip_prefix(&library_config.library_folder)
            .unwrap_or(song_path.as_path())
            .parent()
            .unwrap_or(Path::new(""));
        for ancestor in relative_parent
            .ancestors()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
        {
            let select_song_path = nice_path
                .strip_prefix(ancestor)
                .unwrap_or(nice_path.as_path());
            for config_root in &library_config.config_folders {
                let config_path = config_root.join(ancestor).join("config.yaml");
                if let Some(config) = cached_configs
                    .entry(config_path)
                    .or_insert_with_key(|x| load_config(x.as_path(), &library_config).ok())
                {
                    for setter in &config.set {
                        if setter.names.matches(select_song_path) {
                            setter.set.apply(&mut metadata, &nice_path, &library_config);
                        }
                    }
                }
            }
        }
        let final_metadata = metadata.resolve();
        print_differences(&existing_metadata, &final_metadata);
    }
    if let Err(err) = library_config.date_cache.save() {
        eprintln!("{}", err.to_string().red());
    }
    if let Some(repo) = library_config.art_repo {
        if let Err(err) = repo.used_templates.save() {
            eprintln!("{}", err.to_string().red());
        }
    }
}

fn print_differences(existing: &Metadata, incoming: &Metadata) {
    for key in existing
        .fields
        .keys()
        .chain(incoming.fields.keys())
        .unique()
    {
        if let MetadataField::Builtin(BuiltinMetadataField::SimpleLyrics) = key {
            continue;
        }
        if let MetadataField::Builtin(BuiltinMetadataField::Art) = key {
            continue;
        }
        if let MetadataField::Custom(_) = key {
            continue;
        }
        if let Some(new) = incoming.fields.get(key) {
            let new = &new.canonicalize();
            let current = existing.fields.get(key).unwrap_or(&MetadataValue::Blank);
            if current != new {
                println!("\t{:?}: {:?} -> {:?}", key, current, new);
            }
        }
    }
}

#[derive(Error, Debug)]
#[error("{0}")]
pub enum ConfigError {
    Yaml(#[from] YamlError),
    Library(#[from] LibraryError),
}

fn load_config<'a>(
    path: &Path,
    library_config: &'a LibraryConfig<'a>,
) -> Result<SongConfig<'a>, ConfigError> {
    match load_yaml::<RawSongConfig>(path) {
        Err(YamlError::Io(error)) if error.kind() == ErrorKind::NotFound => {
            Err(ConfigError::Yaml(YamlError::Io(error)))
        }
        Err(error) => {
            eprintln!("{}", path.display().to_string().red());
            eprintln!("{}", error.to_string().red());
            Err(ConfigError::Yaml(error))
        }
        Ok(config) => library_config
            .resolve_config(config)
            .map_err(ConfigError::Library),
    }
}

struct ScanResults {
    songs: BTreeSet<PathBuf>,
    images: BTreeSet<PathBuf>,
}

fn find_scan_songs(library_config: &LibraryConfig) -> ScanResults {
    let mut scan_songs = BTreeSet::<PathBuf>::new();
    let mut scan_images = BTreeSet::<PathBuf>::new();
    // for every config that's changed, scan all songs it applies to
    for config_root in &library_config.config_folders {
        for config_path in WalkDir::new(config_root)
            .into_iter()
            .filter_map(file_path)
            .filter(|x| {
                match_name(x, "config.yaml") && library_config.date_cache.changed_recently(x)
            })
        {
            let config_folder = config_path.parent().unwrap_or(Path::new(""));
            let corresponding_folder = &library_config.library_folder.join(
                config_folder
                    .strip_prefix(config_root)
                    .unwrap_or(config_folder),
            );
            for song in WalkDir::new(corresponding_folder)
                .into_iter()
                .filter_map(file_path)
                .filter(|x| match_extension(x, &library_config.song_extensions))
            {
                scan_songs.insert(song);
                print!("\rFound {}", scan_songs.len());
            }
        }
    }
    if let Some(art_repo) = &library_config.art_repo {
        // for every config that's changed, find all templates it applies to
        for config_path in WalkDir::new(&art_repo.templates_folder)
            .into_iter()
            .filter_map(file_path)
            .filter(|x| {
                match_name(x, "images.yaml") && library_config.date_cache.changed_recently(x)
            })
        {
            let config_folder = config_path.parent().unwrap_or(Path::new(""));
            for image in WalkDir::new(config_folder)
                .into_iter()
                .filter_map(file_path)
                .filter(|x| match_extension(x, &art_repo.image_extensions))
            {
                scan_images.insert(image);
            }
        }
        // find all templates that have changed
        // scan all songs that used to use any of these templates
        for (image_path, songs) in &art_repo.used_templates.cache {
            if scan_images.contains(image_path)
                || library_config
                    .date_cache
                    .changed_recently(image_path.as_path())
            {
                for song in songs {
                    scan_songs.insert(song.clone());
                    print!("\rFound {}", scan_songs.len());
                }
            }
        }
    }
    // scan all songs that have changed
    let mut skipped = 0;
    for song in WalkDir::new(&library_config.library_folder)
        .into_iter()
        .filter_map(file_path)
        .filter(|x| match_extension(x, &library_config.song_extensions))
    {
        if library_config.date_cache.changed_recently(&song) {
            scan_songs.insert(song);
        } else if !scan_songs.contains(&song) {
            skipped += 1;
        }
        print!("\rFound {}, skipped {}", scan_songs.len(), skipped);
    }
    println!();
    ScanResults {
        songs: scan_songs,
        images: scan_images,
    }
}

fn match_name(path: &Path, name: &str) -> bool {
    match path.file_name().and_then(|x| x.to_str()) {
        Some(file_name) => file_name == name,
        None => false,
    }
}

fn match_extension(path: &Path, extensions: &HashSet<String>) -> bool {
    match path.extension().and_then(|x| x.to_str()) {
        Some(ext) => extensions.contains(ext),
        None => false,
    }
}

fn file_path(item: jwalk::Result<jwalk::DirEntry<((), ())>>) -> Option<PathBuf> {
    match item {
        Err(_) => None,
        Ok(entry) => {
            let path: PathBuf = entry.path();
            if path.extension().and_then(|x| x.to_str()).is_some() {
                Some(path)
            } else {
                None
            }
        }
    }
}
