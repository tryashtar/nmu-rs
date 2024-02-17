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
            eprintln!("{}", cformat!("❌ <red>{}</>", error));
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
                cformat!("❌ <red>Error loading library config\n{}</>", error)
            );
        }
        Ok(raw_config) => {
            let library_config_folder = library_config_path.parent().unwrap_or(Path::new(""));
            let mut library_config: LibraryConfig =
                LibraryConfig::new(library_config_folder, raw_config);
            do_scan(&mut library_config);
        }
    }
}

type ConfigCache = HashMap<PathBuf, Result<SongConfig, ConfigError>>;
struct WorkProgress {
    changed: u32,
    failed: u32,
}

fn do_scan(library_config: &mut LibraryConfig) {
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
    } = find_scan_items(library_config);
    let mut copy_cache = HashMap::new();
    for folder_path in scan_folders {
        let nice_path = ItemPath::Folder(
            folder_path
                .strip_prefix(&library_config.library_folder)
                .unwrap_or(&folder_path)
                .to_owned(),
        );
        process_path(
            library_config,
            &nice_path,
            &mut config_cache,
            &mut copy_cache,
        );
    }
    for song_path in scan_songs {
        let nice_path = ItemPath::Song(
            song_path
                .strip_prefix(&library_config.library_folder)
                .unwrap_or(&song_path)
                .with_extension(""),
        );
        process_path(
            library_config,
            &nice_path,
            &mut config_cache,
            &mut copy_cache,
        );
    }
    if progress.failed == 0 {
        println!("Updated {}", progress.changed);
    } else {
        println!("Updated {}, errored {}", progress.changed, progress.failed);
    }
    if let Err(err) = library_config.date_cache.save() {
        eprintln!(
            "{}",
            cformat!("❌ <red>Error saving date cache\n{}</>", err)
        );
    }
    if let Some(repo) = &mut library_config.art_repo {
        repo.used_templates
            .template_to_users
            .retain(|x, y| x.exists() && !y.is_empty());
        if let Err(err) = repo.used_templates.save() {
            eprintln!("{}", cformat!("❌ <red>Error saving art cache\n{}</>", err));
        }
    }
    for report in &library_config.reports {
        let path = match report {
            LibraryReport::SplitFields { path, .. }
            | LibraryReport::MergedFields { path, .. }
            | LibraryReport::ItemData { path, .. } => path,
        };
        if let Err(err) = report.save() {
            eprintln!(
                "{}",
                cformat!(
                    "❌ <red>Error saving report\n{}\n{}</>",
                    path.display(),
                    err
                )
            );
        }
    }
}

fn process_path(
    library_config: &mut LibraryConfig,
    nice_path: &ItemPath,
    config_cache: &mut ConfigCache,
    copy_cache: &mut HashMap<PathBuf, PendingMetadata>,
) {
    let mut metadata;
    let mut config_reports;
    loop {
        metadata = Some(PendingMetadata::new());
        config_reports = vec![];
        for (config_path, nice_folder) in relevant_config_paths(nice_path, library_config) {
            let loaded = config_cache
                .entry(config_path.clone())
                .or_insert_with_key(|x| {
                    let loaded = load_config(x, nice_folder, library_config);
                    print_config_errors(&loaded, x, nice_folder, library_config);
                    if loaded.is_ok() {
                        library_config.date_cache.mark_updated(x.to_owned());
                    }
                    loaded
                });
            match loaded {
                Ok(config) => {
                    if let Some(metadata) = &mut metadata {
                        let select_path =
                            nice_path.strip_prefix(nice_folder).unwrap_or(nice_folder);
                        let report = config.apply(
                            nice_path,
                            select_path,
                            metadata,
                            library_config,
                            copy_cache,
                        );
                        config_reports.push((config_path, report));
                    }
                }
                Err(err) => {
                    if !is_not_found(err) {
                        metadata = None;
                    }
                }
            }
        }
        let mut redo = false;
        for error in config_reports.iter().flat_map(|x| &x.1.errors) {
            if let ValueError::CopyNotFound { field, paths } = error {
                for path in paths {
                    if !copy_cache.contains_key(path) {
                        redo = true;
                        if (nice_path as &Path) == path.as_path() {
                            if let Some(metadata) = &metadata {
                                copy_cache.insert(path.clone(), metadata.clone());
                            }
                        } else {
                            copy_cache.insert(path.clone(), HashMap::new());
                        }
                    }
                }
            }
        }
        if !redo {
            break;
        }
    }
    if let Some(metadata) = metadata {
        println!("{}", nice_path.display());
        for (path, mut report) in config_reports {
            report
                .errors
                .retain(|x| !matches!(x, ValueError::ExitRequested));
            if !report.errors.is_empty() {
                eprintln!(
                    "{}",
                    cformat!("⚠️ <yellow>Errors applying config\n{}</>", path.display())
                );
                for error in report.errors {
                    eprintln!("{}", cformat!("\t<yellow>{}</>", error));
                }
            }
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
            eprintln!("{}", cformat!("❌ <red>Error reading ID3 tag\n{}</>", err));
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
            eprintln!("{}", cformat!("❌ <red>Error reading flac tag\n{}</>", err));
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
        eprintln!("{}", cformat!("❌ <red>No tags found in file</>"));
        success = false;
        progress.failed += 1;
    }
    success
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

fn get_unused_selectors<'a>(
    config: &'a SongConfig,
    nice_folder: &Path,
    library_config: &LibraryConfig,
) -> Vec<&'a ItemSelector> {
    config
        .set
        .iter()
        .flat_map(|x| find_unused_selectors(&x.names, nice_folder, library_config))
        .chain(
            config
                .order
                .as_ref()
                .map(|x| match x {
                    OrderingSetter::Discs {
                        original_selectors, ..
                    } => original_selectors
                        .iter()
                        .flat_map(|x| find_unused_selectors(x, nice_folder, library_config))
                        .collect(),
                    OrderingSetter::Order {
                        original_selector, ..
                    } => find_unused_selectors(original_selector, nice_folder, library_config),
                })
                .unwrap_or_default(),
        )
        .collect()
}

fn get_unselected_items(
    order: &OrderingSetter,
    nice_folder: &Path,
    library_config: &LibraryConfig,
) -> BTreeSet<PathBuf> {
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
    let mut all_children = parents
        .into_iter()
        .flat_map(|x| {
            let start = nice_folder.join(x);
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
        .collect::<BTreeSet<_>>();
    for item in found {
        all_children.remove(&item);
    }
    all_children
}

fn print_config_errors(
    result: &Result<SongConfig, ConfigError>,
    full_path: &Path,
    nice_folder: &Path,
    library_config: &LibraryConfig,
) {
    match result {
        Err(error) => {
            if !is_not_found(error) {
                eprintln!(
                    "{}",
                    cformat!(
                        "❌ <red>Error loading config\n{}\n{}</>",
                        full_path.display(),
                        error
                    )
                );
            }
        }
        Ok(config) => {
            let mut warnings = vec![];
            let unused = get_unused_selectors(config, nice_folder, library_config);
            if !unused.is_empty() {
                warnings.push(cformat!("<yellow>Selectors that didn't find anything:</>"));
                for selector in unused {
                    warnings.push(cformat!("\t<yellow>{}</>", inline_data(selector)));
                }
            }
            if let Some(order) = &config.order {
                let unselected = get_unselected_items(order, nice_folder, library_config);
                if !unselected.is_empty() {
                    warnings.push(cformat!("<yellow>Items not included in track order:</>"));
                    for item in unselected {
                        warnings.push(cformat!("\t<yellow>{}</>", item.display()));
                    }
                }
            }
            if !warnings.is_empty() {
                eprintln!(
                    "{}",
                    cformat!(
                        "⚠️ <yellow>Warnings loading config\n{}</>",
                        full_path.display()
                    )
                );
                for warning in warnings {
                    eprintln!("{}", warning);
                }
            }
        }
    }
}

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

fn load_config(
    full_path: &Path,
    nice_folder: &Path,
    library_config: &LibraryConfig,
) -> Result<SongConfig, ConfigError> {
    match file_stuff::load_yaml::<RawSongConfig>(full_path) {
        Err(error) => Err(ConfigError::Yaml(error)),
        Ok(config) => library_config
            .resolve_config(config, nice_folder)
            .map_err(ConfigError::Library),
    }
}

fn relevant_config_paths<'a>(
    nice_path: &'a Path,
    library_config: &LibraryConfig,
) -> Vec<(PathBuf, &'a Path)> {
    let mut list = vec![];
    for ancestor in nice_path
        .parent()
        .unwrap_or(Path::new(""))
        .ancestors()
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
    {
        for config_root in &library_config.config_folders {
            let config_path = config_root.join(ancestor).join("config.yaml");
            list.push((config_path, ancestor));
        }
    }
    list
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

fn find_scan_items(library_config: &LibraryConfig) -> ScanResults {
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
