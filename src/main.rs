use colored::Colorize;
use file_stuff::NicePath;
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap};
use std::env;
use std::io::{ErrorKind, IsTerminal};
use std::path::{Path, PathBuf};

mod strategy;
use strategy::*;
mod file_stuff;
mod library_config;
use library_config::*;
mod metadata;
use metadata::*;
mod modifier;
use modifier::*;
mod util;
use util::*;
mod song_config;
use song_config::*;
mod tag_interop;
use tag_interop::*;
#[cfg(test)]
mod tests;

use crate::file_stuff::{match_extension, YamlError};

fn main() {
    println!("NAIVE MUSIC UPDATER");
    let library_argument = env::args().nth(1);
    let library_config_path = Path::new(library_argument.as_deref().unwrap_or("library.yaml"));
    let raw_config = file_stuff::load_yaml::<RawLibraryConfig>(library_config_path);
    match raw_config {
        Err(YamlError::Io(error))
            if error.kind() == ErrorKind::NotFound && library_argument.is_none() =>
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
            eprintln!("{}", "Error loading library config:".red());
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
    let mut config_cache: HashMap<PathBuf, Option<SongConfig>> = HashMap::new();
    let ScanResults {
        songs: scan_songs,
        folders: scan_folders,
        images: scan_images,
    } = find_scan_songs(&library_config);
    for song_path in scan_songs {
        let nice_path = song_path
            .strip_prefix(&library_config.library_folder)
            .unwrap_or(&song_path)
            .with_extension("");
        println!("{}", nice_path.display());
        let tags = tag_interop::Tags::load(&song_path);
        let final_metadata = get_metadata(
            &NicePath::Song(nice_path),
            &library_config,
            &mut config_cache,
        );
        if let Some(id3) = tags.id3 {
            let existing_metadata = Tags::get_metadata_id3(&id3, &library_config);
            print_differences("ID3 Tag", &existing_metadata, &final_metadata);
        }
        if let Some(flac) = tags.flac {
            let existing_metadata = Tags::get_metadata_flac(&flac, &library_config);
            print_differences("Flac Tag", &existing_metadata, &final_metadata);
        }
    }
    if let Err(err) = library_config.date_cache.save() {
        eprintln!("{}", "Error saving date cache:".red());
        eprintln!("{}", err.to_string().red());
    }
    if let Some(repo) = library_config.art_repo {
        if let Err(err) = repo.used_templates.save() {
            eprintln!("{}", "Error saving art cache:".red());
            eprintln!("{}", err.to_string().red());
        }
    }
}

fn get_metadata(
    path: &NicePath,
    library_config: &LibraryConfig,
    config_cache: &mut HashMap<PathBuf, Option<SongConfig>>,
) -> Metadata {
    let nice_path = path.as_path();
    let mut metadata = PendingMetadata::new();
    for ancestor in nice_path
        .parent()
        .unwrap_or(Path::new(""))
        .ancestors()
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
    {
        let select_song_path = nice_path.strip_prefix(ancestor).unwrap_or(nice_path);
        for config_root in &library_config.config_folders {
            let config_path = config_root.join(ancestor).join("config.yaml");
            if let Some(config) = config_cache
                .entry(config_path)
                .or_insert_with_key(|x| file_stuff::load_config(x, ancestor, library_config).ok())
            {
                for setter in &config.set {
                    if setter.names.matches(select_song_path)
                        && MusicItemType::matches(&path.as_type(), setter.must_be.as_ref())
                    {
                        setter.apply(&mut metadata, nice_path, library_config);
                    }
                }
            }
        }
    }
    metadata.resolve(nice_path, library_config, config_cache)
}

fn print_differences(name: &str, existing: &Metadata, incoming: &Metadata) {
    let mut any = false;
    let blank = MetadataValue::blank();
    for key in existing
        .fields
        .keys()
        .chain(incoming.fields.keys())
        .unique()
    {
        match key {
            MetadataField::Builtin(BuiltinMetadataField::SimpleLyrics)
            | MetadataField::Builtin(BuiltinMetadataField::Art)
            | MetadataField::Custom(_) => {}
            _ => {
                if let Some(new) = incoming.fields.get(key) {
                    let current = existing.fields.get(key).unwrap_or(&blank);
                    if current != new {
                        if !any {
                            any = true;
                            println!("\t{name}:")
                        }
                        println!("\t\t{key}: {current} -> {new}");
                    }
                }
            }
        }
    }
}

struct ScanResults {
    songs: BTreeSet<PathBuf>,
    folders: BTreeSet<PathBuf>,
    images: BTreeSet<PathBuf>,
}

fn find_scan_songs(library_config: &LibraryConfig) -> ScanResults {
    let mut scan_songs = BTreeSet::<PathBuf>::new();
    let mut scan_folders = BTreeSet::<PathBuf>::new();
    let mut scan_images = BTreeSet::<PathBuf>::new();
    // for every config that's changed, scan all songs it applies to
    for config_root in &library_config.config_folders {
        for config_folder in walkdir::WalkDir::new(config_root)
            .into_iter()
            .filter_entry(|x| x.file_type().is_dir())
            .filter_map(|x| x.ok())
        {
            let config_folder_path = config_folder.path();
            let config_file_path = config_folder_path.join("config.yaml");
            if config_file_path.exists()
                && library_config
                    .date_cache
                    .changed_recently(&config_file_path)
            {
                let corresponding_folder = &library_config.library_folder.join(
                    config_folder_path
                        .strip_prefix(config_root)
                        .unwrap_or(config_folder_path),
                );
                for entry in walkdir::WalkDir::new(corresponding_folder)
                    .into_iter()
                    .filter_map(|x| x.ok())
                {
                    let is_dir = entry.file_type().is_dir();
                    let path = entry.into_path();
                    if is_dir {
                        scan_folders.insert(path);
                    } else if match_extension(&path, &library_config.song_extensions)
                        && scan_songs.insert(path)
                        && std::io::stdout().is_terminal()
                    {
                        print!("\rFound {}", scan_songs.len())
                    }
                }
            }
        }
    }
    if let Some(art_repo) = &library_config.art_repo {
        // for every config that's changed, find all templates it applies to
        for config_folder in walkdir::WalkDir::new(&art_repo.templates_folder)
            .into_iter()
            .filter_entry(|x| x.file_type().is_dir())
            .filter_map(|x| x.ok())
        {
            let config_folder_path = config_folder.path();
            let config_file_path = config_folder_path.join("images.yaml");
            if config_file_path.exists()
                && library_config
                    .date_cache
                    .changed_recently(&config_file_path)
            {
                for image in walkdir::WalkDir::new(config_folder_path)
                    .into_iter()
                    .filter_map(|x| x.ok())
                {
                    let is_file = image.file_type().is_file();
                    let path = image.into_path();
                    if is_file && match_extension(&path, &art_repo.image_extensions) {
                        scan_images.insert(path);
                    }
                }
            }
        }
        // find all templates that have changed
        // scan all songs that used to use any of these templates
        for (image_path, songs) in &art_repo.used_templates.cache {
            if scan_images.contains(image_path)
                || library_config.date_cache.changed_recently(image_path)
            {
                for song in songs.clone() {
                    if scan_songs.insert(song) && std::io::stdout().is_terminal() {
                        print!("\rFound {}", scan_songs.len())
                    }
                }
            }
        }
    }
    // scan all songs that have changed
    let mut skipped = 0;
    for entry in walkdir::WalkDir::new(&library_config.library_folder)
        .into_iter()
        .filter_map(|x| x.ok())
    {
        let is_dir = entry.file_type().is_dir();
        let path = entry.into_path();
        if library_config.date_cache.changed_recently(&path) {
            if is_dir {
                scan_folders.insert(path);
            } else if match_extension(&path, &library_config.song_extensions)
                && scan_songs.insert(path)
                && std::io::stdout().is_terminal()
            {
                print!("\rFound {}, skipped {}", scan_songs.len(), skipped);
            }
        } else if !is_dir
            && match_extension(&path, &library_config.song_extensions)
            && !scan_songs.contains(&path)
        {
            skipped += 1;
            if std::io::stdout().is_terminal() {
                print!("\rFound {}, skipped {}", scan_songs.len(), skipped);
            }
        }
    }
    if std::io::stdout().is_terminal() {
        print!("\r")
    }
    println!("Found {}, skipped {}", scan_songs.len(), skipped);
    ScanResults {
        songs: scan_songs,
        folders: scan_folders,
        images: scan_images,
    }
}
