use color_print::cformat;
use file_stuff::ConfigError;
use image::DynamicImage;
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::env;
use std::io::{ErrorKind, IsTerminal};
use std::ops::Deref;
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
mod art;
use art::*;
mod lyrics;
use lyrics::*;

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
            eprintln!("{}", cformat!("❌ <red>{}</>", error.to_string()));
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
                cformat!("❌ <red>Error loading library config:\n{}</>", error)
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

fn add_to_song(
    file_path: &Path,
    nice_path: &Path,
    metadata: Metadata,
    art: Option<Rc<DynamicImage>>,
    config: &mut LibraryConfig,
    progress: &mut WorkProgress,
) -> bool {
    let mut any = false;
    let mut success = true;
    let mut existing_metadata: Option<Metadata> = None;
    let mut final_metadata = FinalMetadata::create(&metadata);
    final_metadata.result.art = SetValue::Set(art);
    match id3::Tag::read_from_path(file_path) {
        Ok(mut id3) => {
            any = true;
            let existing = get_metadata_id3(&id3, &config.artist_separator);
            if let Some(lyric_config) = &config.lyrics {
                lyric_config.handle(nice_path, &existing, &mut final_metadata.result);
            }
            let existing = existing.into();
            if print_differences("ID3 Tag", &existing, &metadata) {
                progress.changed += 1;
                set_metadata_id3(&mut id3, &final_metadata.result, &config.artist_separator);
            }
            existing_metadata = Some(existing);
        }
        Err(err) if matches!(err.kind, id3::ErrorKind::NoTag) => {}
        Err(err) => {
            if !any {
                any = true;
                success = false;
                progress.failed += 1;
            }
            eprintln!("{}", cformat!("\t❌ <red>ID3 Tag error: {}</>", err));
        }
    }
    match metaflac::Tag::read_from_path(file_path) {
        Ok(mut flac) => {
            any = true;
            let existing = get_metadata_flac(&flac);
            if let Some(lyric_config) = &config.lyrics {
                lyric_config.handle(nice_path, &existing, &mut final_metadata.result);
            }
            let existing = existing.into();
            if print_differences("Flac Tag", &existing, &metadata) {
                progress.changed += 1;
                set_metadata_flac(&mut flac, &final_metadata.result);
            }
            existing_metadata = Some(existing);
        }
        Err(err) if matches!(err.kind, metaflac::ErrorKind::InvalidInput) => {}
        Err(err) => {
            if !any {
                any = true;
                success = false;
                progress.failed += 1;
            }
            eprintln!("{}", cformat!("\t❌ <red>Flac Tag error: {}</>", err));
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
    print_value_errors(&final_metadata.errors);
    if !any {
        eprintln!("{}", cformat!("\t❌ <red>No tags found in file</>"));
        success = false;
        progress.failed += 1;
    }
    success
}

struct WorkProgress {
    changed: u32,
    failed: u32,
}

fn print_value_errors(errors: &[ValueError]) {
    for err in errors {
        match err {
            ValueError::ExitRequested => {}
            other => {
                eprintln!("{}", cformat!("\t⚠️ <yellow>{}</>", other));
            }
        }
    }
}

fn print_art_errors(result: &ProcessArtResult, library_config: &mut LibraryConfig) {
    match result {
        ProcessArtResult::NoArtNeeded => {}
        ProcessArtResult::NoTemplateFound { tried } => {
            let list = tried.iter().map(|x| x.to_string_lossy()).join(", ");
            eprintln!(
                "{}",
                cformat!("⚠️ <yellow>No matching templates found: [{}]</>", list)
            );
        }
        ProcessArtResult::Processed {
            full_path,
            newly_loaded,
            result,
        } => {
            match Rc::as_ref(result) {
                Ok(_) => {
                    library_config.date_cache.mark_updated(full_path.to_owned());
                }
                Err(ArtError::Image(error)) => {
                    eprintln!(
                        "{}",
                        cformat!(
                            "❌ <red>Error loading image {}:\n{}</>",
                            full_path.display(),
                            error
                        )
                    );
                }
                Err(_) => {}
            }
            for load in newly_loaded {
                match Rc::as_ref(&load.result) {
                    Err(error) => {
                        if !is_not_found(error) {
                            eprintln!(
                                "{}",
                                cformat!(
                                    "❌ <red>Error loading config: {}\n{}</>",
                                    load.full_path.display(),
                                    error
                                )
                            );
                        }
                    }
                    Ok(_) => {
                        library_config
                            .date_cache
                            .mark_updated(load.full_path.to_owned());
                    }
                }
            }
        }
    }
}

fn print_metadata_errors(results: &GetMetadataResults, library_config: &mut LibraryConfig) {
    for load in &results.newly_loaded {
        match Rc::as_ref(&load.result) {
            Ok(config) => {
                for unused in config
                    .set
                    .iter()
                    .flat_map(|x| {
                        find_unused_selectors(&x.names, &load.nice_folder, library_config)
                    })
                    .chain(
                        config
                            .order
                            .as_ref()
                            .map(|x| match x {
                                OrderingSetter::Discs {
                                    original_selectors, ..
                                } => original_selectors
                                    .iter()
                                    .flat_map(|x| {
                                        find_unused_selectors(x, &load.nice_folder, library_config)
                                    })
                                    .collect(),
                                OrderingSetter::Order {
                                    original_selector, ..
                                } => find_unused_selectors(
                                    original_selector,
                                    &load.nice_folder,
                                    library_config,
                                ),
                            })
                            .unwrap_or_default(),
                    )
                {
                    let display = inline_data(unused);
                    eprintln!(
                        "{}",
                        cformat!("⚠️ <yellow>Selector {} didn't find anything</>", display)
                    );
                }
                if let Some(order) = &config.order {
                    let found = match order {
                        OrderingSetter::Discs { map, .. } => {
                            map.keys().map(|x| x.to_owned()).collect::<HashSet<_>>()
                        }
                        OrderingSetter::Order { map, .. } => {
                            map.keys().map(|x| x.to_owned()).collect::<HashSet<_>>()
                        }
                    };
                    let parents = found
                        .iter()
                        .filter_map(|x| x.parent())
                        .collect::<HashSet<_>>();
                    let all_children = parents
                        .into_iter()
                        .flat_map(|x| {
                            let start = load.nice_folder.join(x);
                            file_stuff::find_matches(
                                &ItemSelector::All { recursive: false },
                                &start,
                                library_config,
                            )
                            .into_iter()
                            .filter_map(|y| match y {
                                ItemPath::Folder(_) => None,
                                ItemPath::Song(path) => Some(x.join(path)),
                            })
                        })
                        .collect::<HashSet<_>>();
                    for item in all_children.difference(&found) {
                        eprintln!(
                            "{}",
                            cformat!(
                                "⚠️ <yellow>Item \"{}\" wasn't included in track order</>",
                                item.display()
                            )
                        );
                    }
                }
                library_config
                    .date_cache
                    .mark_updated(load.full_path.to_owned());
            }
            Err(error) => {
                if !is_not_found(error) {
                    eprintln!(
                        "{}",
                        cformat!(
                            "❌ <red>Error loading config: {}\n{}</>",
                            load.full_path.display(),
                            error
                        )
                    );
                }
            }
        }
    }
    print_value_errors(&results.value_errors);
}

fn do_scan(mut library_config: LibraryConfig) {
    let mut config_cache: ConfigCache = HashMap::new();
    let mut art_config_cache: ArtConfigCache = HashMap::new();
    let mut processed_art_cache: ProcessedArtCache = HashMap::new();
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
        let nice_path = ItemPath::Folder(
            folder_path
                .strip_prefix(&library_config.library_folder)
                .unwrap_or(&folder_path)
                .to_owned(),
        );
        let results = get_metadata(&nice_path, &library_config, &mut config_cache);
        if results.result.is_some() {
            println!("{}", nice_path.display());
        }
        print_metadata_errors(&results, &mut library_config);
        if let Some(mut metadata) = results.result {
            if let Some(repo) = &mut library_config.art_repo {
                let art = repo.resolve_art(
                    &mut metadata,
                    &scan_images,
                    &mut art_config_cache,
                    &mut processed_art_cache,
                );
                repo.used_templates.add(&folder_path, &art);
                print_art_errors(&art, &mut library_config);
            }
            for (field, value) in metadata.iter().sorted() {
                println!("\t{field}: {value}");
            }
            library_config.date_cache.mark_updated(folder_path);
        }
    }
    for song_path in scan_songs {
        let nice_path = ItemPath::Song(
            song_path
                .strip_prefix(&library_config.library_folder)
                .unwrap_or(&song_path)
                .with_extension(""),
        );
        let results = get_metadata(&nice_path, &library_config, &mut config_cache);
        if results.result.is_some() {
            println!("{}", nice_path.display());
        }
        print_metadata_errors(&results, &mut library_config);
        if let Some(mut metadata) = results.result {
            let mut image = None;
            if let Some(repo) = &mut library_config.art_repo {
                let art = repo.resolve_art(
                    &mut metadata,
                    &scan_images,
                    &mut art_config_cache,
                    &mut processed_art_cache,
                );
                repo.used_templates.add(&song_path, &art);
                print_art_errors(&art, &mut library_config);
                if let ProcessArtResult::Processed { result, .. } = art {
                    image = Some(result.clone());
                }
            }
            if add_to_song(
                &song_path,
                &nice_path,
                metadata,
                image.and_then(|x| x.as_ref().as_ref().ok().cloned()),
                &mut library_config,
                &mut progress,
            ) {
                library_config.date_cache.mark_updated(song_path);
            }
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
        eprintln!(
            "{}",
            cformat!("❌ <red>Error saving date cache:\n{}</>", err)
        );
    }
    if let Some(repo) = &mut library_config.art_repo {
        repo.used_templates
            .template_to_users
            .retain(|x, y| x.exists() && !y.is_empty());
        if let Err(err) = repo.used_templates.save() {
            eprintln!(
                "{}",
                cformat!("❌ <red>Error saving art cache:\n{}</>", err)
            );
        }
    }
    for report in library_config.reports {
        if let Err(err) = report.save() {
            eprintln!("{}", cformat!("❌ <red>Error saving report:\n{}</>", err));
        }
    }
}

type ConfigCache = HashMap<PathBuf, Rc<Result<SongConfig, ConfigError>>>;

fn is_not_found(result: &ConfigError) -> bool {
    matches!(result, ConfigError::Yaml(YamlError::Io(ref error)) if error.kind() == ErrorKind::NotFound)
}

fn find_unused_selectors<'a>(
    selector: &'a ItemSelector,
    start: &Path,
    config: &LibraryConfig,
) -> Vec<&'a ItemSelector> {
    match selector {
        ItemSelector::All { .. }
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
                    .flat_map(|x| find_unused_selectors(select, &start.join(x.deref()), config))
                    .collect()
            }
        }
    }
}

pub struct Results<T, E> {
    result: T,
    errors: Vec<E>,
}

pub struct ConfigLoadResults<T> {
    nice_folder: PathBuf,
    full_path: PathBuf,
    result: Rc<Result<T, ConfigError>>,
}

struct GetMetadataResults {
    newly_loaded: Vec<ConfigLoadResults<SongConfig>>,
    value_errors: Vec<ValueError>,
    result: Option<Metadata>,
}

fn get_metadata(
    nice_path: &ItemPath,
    library_config: &LibraryConfig,
    config_cache: &mut ConfigCache,
) -> GetMetadataResults {
    let mut newly_loaded = vec![];
    let mut value_errors = vec![];
    let mut metadata = Some(PendingMetadata::new());
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
                    let result = Rc::new(file_stuff::load_config(
                        config_path,
                        ancestor,
                        library_config,
                    ));
                    newly_loaded.push(ConfigLoadResults {
                        nice_folder: ancestor.to_owned(),
                        full_path: config_path.to_owned(),
                        result: result.clone(),
                    });
                    result
                });
            match Rc::as_ref(config_load) {
                Ok(config) => {
                    if let Some(ref mut metadata) = metadata {
                        let mut more_errors =
                            config.apply(nice_path, select_path, metadata, library_config);
                        value_errors.append(&mut more_errors);
                    }
                }
                Err(error) => {
                    if !is_not_found(error) {
                        metadata = None;
                    }
                }
            }
        }
    }
    let mut resolved = metadata.map(|x| x.resolve(nice_path, library_config, config_cache));
    if let Some(ref mut results) = resolved {
        value_errors.append(&mut results.errors);
    }
    GetMetadataResults {
        newly_loaded,
        value_errors,
        result: resolved.map(|x| x.result),
    }
}

fn print_differences(name: &str, existing: &Metadata, incoming: &Metadata) -> bool {
    let mut any = false;
    for key in existing.keys().chain(incoming.keys()).unique().sorted() {
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
        // also find all templates that have changed
        for template_file in walkdir::WalkDir::new(&art_repo.templates_folder)
            .into_iter()
            .filter_entry(|entry| entry.file_type().is_file() || !is_hidden(entry))
            .filter_map(|x| x.ok())
            .filter(|x| x.file_type().is_file())
        {
            let is_config = template_file.file_name() == "images.yaml";
            let template_file_path = template_file.into_path();
            if is_config
                && library_config
                    .date_cache
                    .changed_recently(&template_file_path)
            {
                for image in
                    walkdir::WalkDir::new(template_file_path.parent().unwrap_or(Path::new("")))
                        .into_iter()
                        .filter_entry(|entry| entry.file_type().is_file() || !is_hidden(entry))
                        .filter_map(|x| x.ok())
                        .filter(|x| x.file_type().is_file())
                {
                    let path = image.into_path();
                    if match_extension(&path, &art_repo.image_extensions) {
                        scan_images.insert(path);
                    }
                }
            } else if match_extension(&template_file_path, &art_repo.image_extensions)
                && library_config
                    .date_cache
                    .changed_recently(&template_file_path)
            {
                scan_images.insert(template_file_path);
            }
        }
        // scan all songs that used to use an updated image
        for image in &scan_images {
            if let Some(songs) = art_repo.used_templates.template_to_users.get(image) {
                for path in songs {
                    if path.is_dir() {
                        scan_folders.insert(path.clone());
                    } else if scan_songs.insert(path.clone()) && std::io::stdout().is_terminal() {
                        print!("\rFound {}", scan_songs.len())
                    }
                }
            }
        }
        for (template, songs) in &art_repo.used_templates.template_to_users {
            if !template.exists() {
                for path in songs {
                    if path.is_dir() {
                        scan_folders.insert(path.clone());
                    } else if scan_songs.insert(path.clone()) && std::io::stdout().is_terminal() {
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
    scan_folders.remove(&library_config.library_folder);
    ScanResults {
        songs: scan_songs,
        folders: scan_folders,
        images: scan_images,
    }
}
