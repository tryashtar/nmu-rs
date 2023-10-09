use colored::Colorize;
use itertools::Itertools;
use jwalk::WalkDir;
use std::collections::{BTreeSet, HashMap};
use std::env;
use std::io::{ErrorKind, IsTerminal};
use std::path::{Path, PathBuf};

mod library_config;
mod file_stuff;
#[cfg(test)]
mod tests;
use library_config::*;

mod song_config;
use song_config::*;

mod tag_interop;
use tag_interop::Tags;

use crate::file_stuff::{file_path, match_name, match_extension};

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
        let tags = Tags::load(&song_path);
        let existing_metadata = tags.get_metadata();
        let final_metadata = get_metadata(&nice_path, &library_config, &mut cached_configs);
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

fn get_metadata<'a>(
    nice_path: &Path,
    library_config: &'a LibraryConfig,
    config_cache: &mut HashMap<PathBuf, Option<SongConfig<'a>>>,
) -> Metadata {
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
                .or_insert_with_key(|x| load_config(x.as_path(), library_config).ok())
            {
                for setter in &config.set {
                    if setter
                        .names
                        .matches(select_song_path)
                    {
                        setter.set.apply(&mut metadata, nice_path, library_config);
                    }
                }
            }
        }
    }
    metadata.resolve(nice_path, library_config, config_cache)
}

fn print_differences(existing: &Metadata, incoming: &Metadata) {
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
                        println!("\t{:?}: {:?} -> {:?}", key, current, new);
                    }
                }
            }
        }
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
                if scan_songs.insert(song) && std::io::stdout().is_terminal() {
                    print!("\rFound {}", scan_songs.len())
                }
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
                    if scan_songs.insert(song.clone()) && std::io::stdout().is_terminal() {
                        print!("\rFound {}", scan_songs.len())
                    }
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
            if scan_songs.insert(song) && std::io::stdout().is_terminal() {
                print!("\rFound {}, skipped {}", scan_songs.len(), skipped);
            }
        } else if !scan_songs.contains(&song) {
            skipped += 1;
            if std::io::stdout().is_terminal() {
                print!("\rFound {}, skipped {}", scan_songs.len(), skipped);
            }
        }
    }
    if std::io::stdout().is_terminal() {
        print!("\r")
    }
    print!("Found {}, skipped {}", scan_songs.len(), skipped);
    println!();
    ScanResults {
        songs: scan_songs,
        images: scan_images,
    }
}
