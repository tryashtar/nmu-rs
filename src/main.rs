use color_print::cformat;
use image::DynamicImage;
use itertools::Itertools;
use library_config::{ScanOptions, TagSettings};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    env,
    io::{ErrorKind, IsTerminal, Write},
    ops::Deref,
    path::{Path, PathBuf},
    rc::Rc,
};
use tag_interop::{FullMetadata, LyricsMetadata, SetValue};
use walkdir::DirEntry;

use crate::{
    art::{ArtError, GetArtResults},
    file_stuff::{ConfigError, YamlError},
    library_config::{LibraryConfig, LibraryReport, RawLibraryConfig, TagOptions},
    metadata::{Metadata, MetadataField, MetadataValue},
    modifier::ValueError,
    song_config::{OrderingSetter, RawSongConfig, SongConfig},
    strategy::ItemSelector,
    util::ItemPath,
};

mod art;
mod file_stuff;
mod library_config;
mod lyrics;
mod metadata;
mod modifier;
mod song_config;
mod strategy;
mod tag_interop;
mod util;

#[cfg(test)]
mod tests;

fn main() {
    println!("NAIVE MUSIC UPDATER");
    let args = env::args().collect::<Vec<_>>();
    let library_argument = args.get(1);
    let library_config_path = Path::new(
        library_argument
            .map(|x| x.as_str())
            .unwrap_or("library.yaml"),
    );
    let next_argument = args.get(2);
    if let Some(word) = next_argument {
        match word.as_str() {
            "generate" => {}
            _ => {
                eprintln!("Unknown mode '{word}', try 'generate'");
            }
        }
        return;
    }
    let raw_config = file_stuff::load_yaml::<RawLibraryConfig>(library_config_path);
    match raw_config {
        Err(YamlError::Io(error))
            if error.kind() == ErrorKind::NotFound && library_argument.is_none() =>
        {
            if let Ok(dir) = std::env::current_dir() {
                eprintln!(
                    "Provide the path to a library.yaml or add one to '{}'\nOr use '<path> generate' to create a template",
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
            match LibraryConfig::new(library_config_folder, raw_config) {
                Err(error) => {
                    eprintln!(
                        "{}",
                        cformat!("❌ <red>Error loading library config\n{}</>", error)
                    );
                }
                Ok(mut library_config) => {
                    do_scan(&mut library_config);
                }
            }
        }
    }
}

type ConfigCache = HashMap<PathBuf, Result<Rc<SongConfig>, Rc<ConfigError>>>;
struct WorkProgress {
    changed: u32,
    failed: u32,
}

fn do_scan(library_config: &mut LibraryConfig) {
    let mut config_cache: ConfigCache = HashMap::new();
    let mut progress = WorkProgress {
        changed: 0,
        failed: 0,
    };
    let ScanResults {
        songs: scan_songs,
        folders: scan_folders,
        images: scan_images,
    } = find_scan_items(library_config);
    if let Some(art_repo) = &mut library_config.art_repo {
        if let Some(disk) = &mut art_repo.disk_cache {
            let mut deleted = 0;
            for image in &scan_images {
                let nice = image
                    .strip_prefix(&art_repo.templates_folder)
                    .unwrap_or(image)
                    .with_extension("");
                let full = disk.get_path(&nice);
                match std::fs::remove_file(&full) {
                    Err(err) if err.kind() != ErrorKind::NotFound => {
                        eprintln!(
                            "{}",
                            cformat!(
                                "❌ <red>Error deleting cached image {}\n{}",
                                full.display(),
                                err
                            )
                        );
                        disk.nice_evicted.insert(nice);
                    }
                    Ok(_) => {
                        deleted += 1;
                    }
                    _ => {}
                }
            }
            if deleted > 0 {
                println!("Deleted {deleted} cached images");
            }
        }
    }
    for folder_path in scan_folders {
        let nice_path = ItemPath::Folder(
            folder_path
                .strip_prefix(&library_config.library_folder)
                .unwrap_or(&folder_path)
                .to_owned(),
        );
        match process_folder(&nice_path, &folder_path, library_config, &mut config_cache) {
            Some(results) => {
                library_config.date_cache.mark_updated(folder_path);
            }
            None => {
                library_config.date_cache.remove(&folder_path);
            }
        }
    }
    for (song_path, options) in scan_songs {
        let nice_path = ItemPath::Song(
            song_path
                .strip_prefix(&library_config.library_folder)
                .unwrap_or(&song_path)
                .with_extension(""),
        );
        let mut success = false;
        let results = process_song(&nice_path, &song_path, library_config, &mut config_cache);
        if let Some(results) = results {
            for (field, value) in results.metadata.iter().sorted() {
                println!("\t{field}: {value}");
            }
            let art_set = match results.art {
                GetArtResults::Keep | GetArtResults::NoTemplateFound { .. } => SetValue::Keep,
                GetArtResults::Remove => SetValue::Remove,
                GetArtResults::Processed { result, .. } => match result {
                    Ok(img) => SetValue::Replace(img),
                    Err(_) => SetValue::Keep,
                },
            };
            let add_results = add_to_song(
                &options,
                &song_path,
                &nice_path,
                results.metadata,
                art_set,
                library_config,
            );
            match add_results {
                Ok(()) => {
                    success = true;
                }
                Err(err) => {
                    eprintln!(
                        "{}",
                        cformat!(
                            "❌ <red>Error adding to file {}\n{}</>",
                            song_path.display(),
                            err
                        )
                    );
                }
            }
        }
        if success {
            progress.changed += 1;
            library_config.date_cache.mark_updated(song_path);
        } else {
            progress.failed += 1;
            library_config.date_cache.remove(&song_path);
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
    if let Some(art_repo) = &mut library_config.art_repo {
        if let Some(disk) = &mut art_repo.disk_cache {
            let mut wrote = 0;
            for (nice, image) in &art_repo.processed_cache {
                if let Ok(img) = image {
                    let full = disk.get_path(nice);
                    if !full.exists() || disk.nice_evicted.contains(nice) {
                        if let Some(parent) = full.parent() {
                            _ = std::fs::create_dir_all(parent);
                        }
                        match img.save_with_format(&full, image::ImageFormat::Png) {
                            Err(err) => {
                                eprintln!(
                                    "{}",
                                    cformat!(
                                        "❌ <red>Error saving cached image {}\n{}",
                                        full.display(),
                                        err
                                    )
                                );
                            }
                            Ok(_) => {
                                wrote += 1;
                            }
                        }
                    }
                }
            }
            if wrote > 0 {
                println!("Saved {wrote} cached images");
            }
        }
    }
}

pub struct GetMetadataResults {
    metadata: Metadata,
    reports: Vec<SourcedReport>,
}

pub struct SourcedReport {
    full_path: PathBuf,
    errors: Vec<ValueError>,
}

pub struct ConfigLoadResults {
    result: Result<Rc<SongConfig>, Rc<ConfigError>>,
    nice_folder: PathBuf,
    full_path: PathBuf,
}

pub struct LoadedConfig {
    config: Rc<SongConfig>,
    nice_folder: PathBuf,
    full_path: PathBuf,
}

struct GetConfigsResults {
    result: Result<Vec<LoadedConfig>, Rc<ConfigError>>,
    loaded: Vec<ConfigLoadResults>,
}

fn get_relevant_configs(
    library_config: &LibraryConfig,
    nice_path: &Path,
    config_cache: &mut ConfigCache,
) -> GetConfigsResults {
    let mut newly_loaded = vec![];
    let mut results = vec![];
    for (config_path, nice_folder) in relevant_config_paths(nice_path, library_config) {
        let loaded = config_cache
            .entry(config_path.clone())
            .or_insert_with_key(|x| {
                let config = load_config(x, nice_folder, library_config)
                    .map(Rc::new)
                    .map_err(Rc::new);
                newly_loaded.push(ConfigLoadResults {
                    result: config.clone(),
                    full_path: x.clone(),
                    nice_folder: nice_folder.to_owned(),
                });
                config
            });
        match loaded {
            Ok(config) => {
                results.push(LoadedConfig {
                    config: config.clone(),
                    full_path: config_path,
                    nice_folder: nice_folder.to_owned(),
                });
            }
            Err(err) => {
                if !is_not_found(err) {
                    return GetConfigsResults {
                        loaded: newly_loaded,
                        result: Err(err.clone()),
                    };
                }
            }
        }
    }
    GetConfigsResults {
        loaded: newly_loaded,
        result: Ok(results),
    }
}

pub fn get_metadata(
    nice_path: &ItemPath,
    configs: &[LoadedConfig],
    library_config: &LibraryConfig,
) -> GetMetadataResults {
    let mut metadata;
    let mut config_reports;
    loop {
        metadata = Metadata::new();
        config_reports = vec![];
        for config in configs {
            let select_path = nice_path
                .strip_prefix(&config.nice_folder)
                .unwrap_or(&config.nice_folder);
            let report = config
                .config
                .apply(nice_path, select_path, &mut metadata, library_config);
            config_reports.push(SourcedReport {
                full_path: config.full_path.clone(),
                errors: report.errors,
            });
        }
        let mut redo = false;
        for error in config_reports.iter().flat_map(|x| &x.errors) {
            if let ValueError::CopyNotFound { field } = error {
                if metadata.contains_key(field) {
                    redo = true;
                }
            }
        }
        if !redo {
            break;
        }
    }
    GetMetadataResults {
        metadata,
        reports: config_reports,
    }
}

fn handle_apply_reports(reports: &mut Vec<SourcedReport>) {
    for report in reports {
        report
            .errors
            .retain(|x| !matches!(x, ValueError::ExitRequested));
        if !report.errors.is_empty() {
            eprintln!(
                "{}",
                cformat!(
                    "⚠️ <yellow>Errors applying config\n{}</>",
                    report.full_path.display()
                )
            );
            for error in &report.errors {
                eprintln!("{}", cformat!("\t<yellow>{}</>", error));
            }
        }
    }
}

struct ProcessFolderResults {
    metadata: Metadata,
    icon: Option<PathBuf>,
}
fn process_folder(
    nice_path: &ItemPath,
    full_path: &Path,
    library_config: &mut LibraryConfig,
    config_cache: &mut ConfigCache,
) -> Option<ProcessFolderResults> {
    let configs = get_relevant_configs(library_config, nice_path, config_cache);
    for load in configs.loaded {
        handle_config_loaded(
            load.result.as_deref().map_err(|x| x.deref()),
            &load.full_path,
            &load.nice_folder,
            library_config,
        );
    }
    if let Ok(configs) = configs.result {
        let mut art_results = GetArtResults::Keep;
        let mut results = get_metadata(nice_path, &configs, library_config);
        if let Some(repo) = &mut library_config.art_repo {
            if let Some(MetadataValue::List(art)) = results.metadata.get_mut(&MetadataField::Art) {
                art_results = repo.get_image(art);
                repo.used_templates.add(full_path, &art_results);
                if let GetArtResults::Processed { nice_path, .. } = &art_results {
                    art.clear();
                    art.push(nice_path.to_string_lossy().into_owned());
                }
            }
            repo.used_templates.add(full_path, &art_results);
        }
        handle_art_loaded(&art_results, library_config);
        println!("{}", nice_path.display());
        handle_apply_reports(&mut results.reports);
        return Some(ProcessFolderResults {
            metadata: results.metadata,
            icon: None,
        });
    }
    None
}

struct ProcessSongResults {
    metadata: Metadata,
    art: GetArtResults,
}
fn process_song(
    nice_path: &ItemPath,
    full_path: &Path,
    library_config: &mut LibraryConfig,
    config_cache: &mut ConfigCache,
) -> Option<ProcessSongResults> {
    let configs = get_relevant_configs(library_config, nice_path, config_cache);
    for load in configs.loaded {
        handle_config_loaded(
            load.result.as_deref().map_err(|x| x.deref()),
            &load.full_path,
            &load.nice_folder,
            library_config,
        );
    }
    if let Ok(configs) = configs.result {
        let mut art_results = GetArtResults::Keep;
        let mut results = get_metadata(nice_path, &configs, library_config);
        if let Some(repo) = &mut library_config.art_repo {
            if let Some(MetadataValue::List(art)) = results.metadata.get_mut(&MetadataField::Art) {
                art_results = repo.get_image(art);
                if let GetArtResults::Processed { nice_path, .. } = &art_results {
                    art.clear();
                    art.push(nice_path.to_string_lossy().into_owned());
                }
            }
            repo.used_templates.add(full_path, &art_results);
        }
        handle_art_loaded(&art_results, library_config);
        println!("{}", nice_path.display());
        handle_apply_reports(&mut results.reports);
        return Some(ProcessSongResults {
            metadata: results.metadata,
            art: art_results,
        });
    }
    None
}

#[derive(thiserror::Error, Debug)]
pub enum AddToSongError {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Id3(#[from] id3::Error),
    #[error("{0}")]
    Flac(#[from] metaflac::Error),
}

enum TagType {
    Id3,
    Flac,
}
impl<'a> TryFrom<&'a str> for TagType {
    type Error = &'a str;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match value {
            "audio/mpeg" => Ok(TagType::Id3),
            "audio/x-flac" => Ok(TagType::Flac),
            _ => Err(value),
        }
    }
}

fn add_to_song(
    options: &TagOptions,
    file_path: &Path,
    nice_path: &Path,
    metadata: Metadata,
    art: SetValue<Rc<DynamicImage>>,
    config: &mut LibraryConfig,
) -> Result<(), AddToSongError> {
    match options.id3 {
        TagSettings::Ignore => {}
        TagSettings::Remove => {
            if id3::Tag::remove_from_path(file_path)? {
                println!("\tRemoved entire ID3 tag");
            }
        }
        TagSettings::Set {} => {
            let mut tag = id3::Tag::read_from_path(file_path)?;
            let existing = tag_interop::get_metadata_id3(&tag, &config.artist_separator);
        }
    }
    match options.ape {
        TagSettings::Ignore => {}
        TagSettings::Remove => {}
        TagSettings::Set {} => {}
    }
    match options.flac {
        TagSettings::Ignore => {}
        TagSettings::Remove => {}
        TagSettings::Set {} => {
            let mut tag = metaflac::Tag::read_from_path(file_path)?;
            let existing = tag_interop::get_metadata_flac(&tag);
        }
    }
    Ok(())
}

fn handle_reports(
    config: &mut LibraryConfig,
    nice_path: &Path,
    metadata: &Metadata,
    existing_metadata: &Metadata,
) {
    for report in &mut config.reports {
        report.record(
            nice_path,
            metadata,
            existing_metadata,
            &config.artist_separator,
        );
    }
}

fn handle_art_loaded(result: &GetArtResults, library_config: &mut LibraryConfig) {
    match result {
        GetArtResults::Keep | GetArtResults::Remove => {}
        GetArtResults::NoTemplateFound { tried } => {
            eprintln!(
                "{}",
                cformat!("⚠️ <yellow>No matching templates found: {:?}</>", tried)
            );
        }
        GetArtResults::Processed {
            result,
            full_path,
            loaded,
            ..
        } => {
            match result {
                Ok(_) => {
                    library_config.date_cache.mark_updated(full_path.to_owned());
                }
                Err(err) => {
                    library_config.date_cache.remove(full_path);
                    if let ArtError::Image(error) = Rc::deref(err) {
                        eprintln!(
                            "{}",
                            cformat!(
                                "❌ <red>Error loading image {}:\n{}</>",
                                full_path.display(),
                                error
                            )
                        );
                    }
                }
            }
            for load in loaded {
                match &load.result {
                    Err(error) => {
                        if !is_not_found(error) {
                            library_config.date_cache.remove(&load.full_path);
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
                            .mark_updated(load.full_path.clone());
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
        OrderingSetter::Discs { map, .. } => map.keys().collect::<HashSet<_>>(),
        OrderingSetter::Order { map, .. } => map.keys().collect::<HashSet<_>>(),
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
        all_children.remove(item);
    }
    all_children
}

fn handle_config_loaded(
    result: Result<&SongConfig, &ConfigError>,
    full_path: &Path,
    nice_folder: &Path,
    library_config: &mut LibraryConfig,
) {
    match result {
        Err(error) => {
            library_config.date_cache.remove(full_path);
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
            library_config.date_cache.mark_updated(full_path.to_owned());
            let mut warnings = vec![];
            let unused = get_unused_selectors(config, nice_folder, library_config);
            if !unused.is_empty() {
                warnings.push(cformat!("<yellow>Selectors that didn't find anything:</>"));
                for selector in unused {
                    warnings.push(cformat!("\t<yellow>{}</>", modifier::inline_data(selector)));
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
                    eprintln!("{warning}");
                }
            }
        }
    }
}

fn is_not_found(result: &ConfigError) -> bool {
    matches!(result, ConfigError::Yaml(YamlError::Io(error)) if error.kind() == ErrorKind::NotFound)
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

struct ScanResults {
    songs: BTreeMap<PathBuf, Rc<TagOptions>>,
    folders: BTreeSet<PathBuf>,
    images: BTreeSet<PathBuf>,
}

fn is_hidden(entry: &DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .is_some_and(|s| s.starts_with('.'))
}

fn find_scan_items(library_config: &LibraryConfig) -> ScanResults {
    let mut lock = std::io::stdout().lock();
    let is_terminal = std::io::stdout().is_terminal();
    let mut scan_songs = BTreeMap::new();
    let mut scan_folders = BTreeSet::new();
    let mut scan_images = BTreeSet::new();
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
                    } else if let Some(settings) = library_config.scan_settings(&path) {
                        if scan_songs.insert(path, settings).is_none() && is_terminal {
                            _ = write!(lock, "\rFound {}", scan_songs.len());
                        }
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
            if is_config {
                if library_config
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
                        let is_config = image.file_name() == "images.yaml";
                        if !is_config {
                            let path = image.into_path();
                            scan_images.insert(path);
                        }
                    }
                }
            } else if library_config
                .date_cache
                .changed_recently(&template_file_path)
                || !art_repo
                    .used_templates
                    .template_to_users
                    .contains_key(&template_file_path)
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
                    } else if path.is_file() {
                        if let Some(settings) = library_config.scan_settings(&path) {
                            if scan_songs.insert(path.clone(), settings).is_none() && is_terminal {
                                _ = write!(lock, "\rFound {}", scan_songs.len());
                            }
                        }
                    }
                }
            }
        }
        for (template, songs) in &art_repo.used_templates.template_to_users {
            if !template.exists() {
                for path in songs {
                    if path.is_dir() {
                        scan_folders.insert(path.clone());
                    } else if path.is_file() {
                        if let Some(settings) = library_config.scan_settings(&path) {
                            if scan_songs.insert(path.clone(), settings).is_none() && is_terminal {
                                _ = write!(lock, "\rFound {}", scan_songs.len());
                            }
                        }
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
            } else if let Some(settings) = library_config.scan_settings(&path) {
                if scan_songs.insert(path, settings).is_none() && is_terminal {
                    _ = write!(lock, "\rFound {}, skipped {}", scan_songs.len(), skipped);
                }
            }
        } else if !is_dir && !scan_songs.contains_key(&path) {
            skipped += 1;
            if is_terminal {
                _ = write!(lock, "\rFound {}, skipped {}", scan_songs.len(), skipped);
            }
        }
    }
    if is_terminal {
        _ = write!(lock, "\r");
    }
    println!("Found {}, skipped {}", scan_songs.len(), skipped);
    scan_folders.remove(&library_config.library_folder);
    ScanResults {
        songs: scan_songs,
        folders: scan_folders,
        images: scan_images,
    }
}
