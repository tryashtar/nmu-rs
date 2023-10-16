use color_print::cformat;
use file_stuff::ConfigError;
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap};
use std::env;
use std::io::{ErrorKind, IsTerminal};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use walkdir::DirEntry;

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
            eprintln!("{}", cformat!("<red>{}</>", error.to_string()));
            if let Ok(dir) = std::env::current_dir() {
                eprintln!(
                    "Provide the path to a library.yaml or add one to '{}'",
                    dir.display()
                );
            }
        }
        Err(error) => {
            eprintln!(
                "{}",
                cformat!("<red>Error loading library config:\n{}</>", error)
            );
        }
        Ok(raw_config) => {
            let library_config_folder = library_config_path.parent().unwrap_or(Path::new(""));
            let library_config: LibraryConfig =
                LibraryConfig::new(library_config_folder, raw_config);
            do_scan(library_config);
        }
    }
}

fn make_nice(path: &Path, root: &Path) -> PathBuf {
    path.strip_prefix(root).unwrap_or(path).with_extension("")
}

fn print_errors<T>(results: Results<T, ValueError>) -> T {
    for err in results.errors {
        match err {
            ValueError::ExitRequested => {}
            other => {
                eprintln!("{}", cformat!("\t<yellow>{}</>", other));
            }
        }
    }
    results.result
}

fn add_to_song(
    file_path: &Path,
    nice_path: &Path,
    metadata: Metadata,
    config: &mut LibraryConfig,
    progress: &mut WorkProgress,
) {
    let mut any = false;
    let mut existing_metadata: Option<Metadata> = None;
    match id3::Tag::read_from_path(file_path) {
        Ok(id3) => {
            any = true;
            let existing = get_metadata_id3(&id3, config).into();
            if print_differences("ID3 Tag", &existing, &metadata) {
                progress.changed += 1;
            }
            existing_metadata = Some(existing);
        }
        Err(err) if matches!(err.kind, id3::ErrorKind::NoTag) => {}
        Err(err) => {
            if !any {
                any = true;
                progress.failed += 1;
            }
            eprintln!("{}", cformat!("\t<red>ID3 Tag error: {}</>", err));
        }
    }
    match metaflac::Tag::read_from_path(file_path) {
        Ok(flac) => {
            any = true;
            let existing = get_metadata_flac(&flac, config).into();
            if print_differences("Flac Tag", &existing, &metadata) {
                progress.changed += 1;
            }
            existing_metadata = Some(existing);
        }
        Err(err) if matches!(err.kind, metaflac::ErrorKind::InvalidInput) => {}
        Err(err) => {
            if !any {
                any = true;
                progress.failed += 1;
            }
            eprintln!("{}", cformat!("\t<red>Flac Tag error: {}</>", err));
        }
    }
    for report in config.reports.iter_mut() {
        report.record(
            nice_path,
            &metadata,
            existing_metadata.as_ref(),
            &config.artist_separator,
        );
    }
    print_errors(FinalMetadata::create(metadata));
    if !any {
        eprintln!("{}", cformat!("\t<red>No tags found in file</>"));
        progress.failed += 1;
    }
}

struct WorkProgress {
    changed: u32,
    failed: u32,
}

fn do_scan(mut library_config: LibraryConfig) {
    let mut config_cache: ConfigCache = HashMap::new();
    let mut progress = WorkProgress {
        changed: 0,
        failed: 0,
    };
    let ScanResults {
        songs: scan_songs,
        folders: scan_folders,
        images: scan_images,
    } = find_scan_songs(&library_config);
    for folder_path in scan_folders {
        let nice_path = ItemPath::Folder(make_nice(&folder_path, &library_config.library_folder));
        if let Ok(results) = get_metadata(&nice_path, &library_config, &mut config_cache) {
            println!("{}", nice_path.display());
            let metadata = print_errors(results);
            for (field, value) in metadata {
                println!("\t{field}: {value}");
            }
        }
    }
    for song_path in scan_songs {
        let nice_path = ItemPath::Song(make_nice(&song_path, &library_config.library_folder));
        if let Ok(results) = get_metadata(&nice_path, &library_config, &mut config_cache) {
            println!("{}", nice_path.display());
            let final_metadata = print_errors(results);
            add_to_song(
                &song_path,
                &nice_path,
                final_metadata,
                &mut library_config,
                &mut progress,
            );
        } else {
            progress.failed += 1;
        }
    }
    if progress.failed == 0 {
        println!("Updated {}", progress.changed);
    } else {
        println!("Updated {}, errored {}", progress.changed, progress.failed);
    }
    if let Err(err) = library_config.date_cache.save() {
        eprintln!("{}", cformat!("<red>Error saving date cache:\n{}</>", err));
    }
    if let Some(repo) = library_config.art_repo {
        if let Err(err) = repo.used_templates.save() {
            eprintln!("{}", cformat!("<red>Error saving art cache:\n{}</>", err));
        }
    }
    for report in library_config.reports {
        if let Err(err) = report.save() {
            eprintln!("{}", cformat!("<red>Error saving report:\n{}</>", err));
        }
    }
}

type ConfigCache = HashMap<PathBuf, Result<SongConfig, Rc<ConfigError>>>;

fn is_not_found(result: &ConfigError) -> bool {
    matches!(result, ConfigError::Yaml(YamlError::Io(ref error)) if error.kind() == ErrorKind::NotFound)
}

fn find_unused_selectors<'a>(
    selector: &'a ItemSelector,
    start: &Path,
    config: &LibraryConfig,
) -> Vec<&'a ItemSelector> {
    match selector {
        ItemSelector::All
        | ItemSelector::This
        | ItemSelector::Path(_)
        | ItemSelector::Segmented { .. } => {
            if file_stuff::find_matches(selector, start, config).is_empty() {
                vec![selector]
            } else {
                vec![]
            }
        }
        ItemSelector::Multi(many) => many
            .iter()
            .flat_map(|x| find_unused_selectors(x, start, config))
            .collect(),
        ItemSelector::Subpath { subpath, select } => {
            let results = file_stuff::find_matches(subpath, start, config);
            if results.is_empty() {
                vec![subpath]
            } else {
                results
                    .into_iter()
                    .flat_map(|x| find_unused_selectors(select, &x, config))
                    .collect()
            }
        }
    }
}

fn load_new_config(
    full_path: &Path,
    nice_folder: &Path,
    library_config: &LibraryConfig,
) -> Result<SongConfig, Rc<ConfigError>> {
    let result = file_stuff::load_config(full_path, nice_folder, library_config);
    match result {
        Ok(ref config) => {
            for unused in config
                .set
                .iter()
                .flat_map(|x| find_unused_selectors(&x.names, nice_folder, library_config))
            {
                let display = inline_data(unused);
                eprintln!(
                    "{}",
                    cformat!("<yellow>Selector {} didn't find anything</>", display)
                );
            }
        }
        Err(ref error) if is_not_found(error) => {}
        Err(ref error) => {
            eprintln!(
                "{}",
                cformat!(
                    "<red>Error loading config: {}\n{}</>",
                    full_path.display(),
                    error
                )
            );
        }
    }
    result.map_err(Rc::new)
}

pub struct Results<T, E> {
    result: T,
    errors: Vec<E>,
}

fn get_metadata(
    nice_path: &ItemPath,
    library_config: &LibraryConfig,
    config_cache: &mut ConfigCache,
) -> Result<Results<Metadata, ValueError>, Rc<ConfigError>> {
    let mut metadata = PendingMetadata::new();
    let mut errors = vec![];
    for ancestor in nice_path
        .parent()
        .unwrap_or(Path::new(""))
        .ancestors()
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
    {
        let select_path = nice_path.strip_prefix(ancestor).unwrap_or(nice_path);
        for config_root in &library_config.config_folders {
            let config_path = config_root.join(ancestor).join("config.yaml");
            let config_load = config_cache
                .entry(config_path)
                .or_insert_with_key(|config_path| {
                    load_new_config(config_path, ancestor, library_config)
                });
            match config_load {
                Ok(config) => {
                    let mut more_errors =
                        config.apply(nice_path, select_path, &mut metadata, library_config);
                    errors.append(&mut more_errors);
                }
                Err(error) if is_not_found(error) => {}
                Err(error) => {
                    return Err(error.clone());
                }
            }
        }
    }
    let mut resolved = metadata.resolve(nice_path, library_config, config_cache);
    errors.append(&mut resolved.errors);
    Ok(Results {
        result: resolved.result,
        errors,
    })
}

fn print_differences(name: &str, existing: &Metadata, incoming: &Metadata) -> bool {
    let mut any = false;
    for key in existing.keys().chain(incoming.keys()).unique() {
        match key {
            MetadataField::Builtin(BuiltinMetadataField::SimpleLyrics)
            | MetadataField::Builtin(BuiltinMetadataField::Art)
            | MetadataField::Custom(_) => {}
            _ => {
                if let Some(new) = incoming.get(key) {
                    let current = existing.get(key).unwrap_or(&BLANK_VALUE);
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
    any
}

struct ScanResults {
    songs: BTreeSet<PathBuf>,
    folders: BTreeSet<PathBuf>,
    images: BTreeSet<PathBuf>,
}

fn is_hidden(entry: &DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| s.starts_with('.'))
        .unwrap_or(false)
}

fn find_scan_songs(library_config: &LibraryConfig) -> ScanResults {
    let mut scan_songs = BTreeSet::<PathBuf>::new();
    let mut scan_folders = BTreeSet::<PathBuf>::new();
    let mut scan_images = BTreeSet::<PathBuf>::new();
    // for every config that's changed, scan all songs it applies to
    for config_root in &library_config.config_folders {
        for config_folder in walkdir::WalkDir::new(config_root)
            .into_iter()
            .filter_entry(|entry| entry.file_type().is_dir() && !is_hidden(entry))
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
                    .filter_entry(|entry| entry.file_type().is_file() || !is_hidden(entry))
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
            .filter_entry(|entry| entry.file_type().is_dir() && !is_hidden(entry))
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
                    .filter_entry(|entry| entry.file_type().is_file() || !is_hidden(entry))
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
        .filter_entry(|entry| entry.file_type().is_file() || !is_hidden(entry))
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
