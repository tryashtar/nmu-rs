use color_print::cformat;
use regex::Regex;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    env,
    io::{ErrorKind, IsTerminal, Write},
    ops::Deref,
    path::{Path, PathBuf},
    rc::Rc,
};
use walkdir::DirEntry;

use crate::{
    art::{
        ArtDiskCache, ArtError, GetArtResults, GetProcessedResult, ProcessedArtCache, RawArtRepo,
    },
    file_stuff::YamlError,
    library_config::{
        LibraryConfig, LibraryReport, LyricsConfig, LyricsReplaceMode, LyricsType,
        RawLibraryConfig, RawLibraryReport, ScanDecision, ScanOptions, SetLyricsReport,
        SetLyricsResult, TagOptions, TagSettings,
    },
    metadata::{Metadata, SourcedReport},
    setting::{
        AddToSongError, AssignResults, ProcessFolderResults, ProcessSongResults, TagChanges,
    },
    song_config::{ConfigCache, ConfigLoadResults},
    strategy::FieldSelector,
    util::ItemPath,
};

mod art;
mod file_stuff;
mod library_config;
mod lyrics;
mod metadata;
mod modifier;
mod setting;
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
    let mode = args.get(2).map(|x| x.as_str()).unwrap_or("scan");
    match mode {
        "generate" => match main_generate(library_config_path) {
            Ok(()) => {
                println!("Created template library configuration file");
            }
            Err(err) => {
                eprintln!(
                    "{}",
                    cformat!("❌ <red>Error saving library config\n{}</>", err)
                );
            }
        },
        "scan" => {
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
                    let library_config_folder =
                        library_config_path.parent().unwrap_or(Path::new(""));
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
        word => {
            eprintln!("Unknown mode '{word}', try 'generate' or 'scan'");
        }
    }
}

fn main_generate(path: &Path) -> Result<(), YamlError> {
    let library = RawLibraryConfig {
        library: env::current_dir()?,
        reports: vec![RawLibraryReport::Items {
            path: PathBuf::from("report.yaml"),
            values: FieldSelector::All,
            blanks: false,
        }],
        lyrics: Some(LyricsConfig {
            folder: PathBuf::from("lyrics"),
            priority: vec![
                LyricsType::RichEmbedded,
                LyricsType::SyncedEmbedded,
                LyricsType::SimpleEmbedded,
            ],
            config: HashMap::from([(LyricsType::RichFile, LyricsReplaceMode::Replace)]),
        }),
        config_folders: vec![env::current_dir()?, PathBuf::from("configs")],
        custom_fields: vec![],
        cache: Some(PathBuf::from("datecache.yaml")),
        art: Some(RawArtRepo {
            templates: PathBuf::from("art"),
            cache: Some(PathBuf::from("cache")),
            icons: Some(PathBuf::from("cache")),
            file_cache: Some(PathBuf::from("imagecache.yaml")),
            named_settings: HashMap::default(),
        }),
        named_strategies: HashMap::default(),
        find_replace: HashMap::default(),
        artist_separator: String::from("/"),
        scan: vec![
            ScanOptions {
                pattern: Regex::new("\\.mp3$").expect("valid hardcoded regex"),
                tags: ScanDecision::Set(Rc::new(TagOptions {
                    flac: TagSettings::Remove,
                    ape: TagSettings::Remove,
                    id3: TagSettings::Set {},
                })),
            },
            ScanOptions {
                pattern: Regex::new("\\.flac$").expect("valid hardcoded regex"),
                tags: ScanDecision::Set(Rc::new(TagOptions {
                    flac: TagSettings::Set {},
                    ape: TagSettings::Remove,
                    id3: TagSettings::Remove {},
                })),
            },
        ],
    };
    file_stuff::save_yaml(path, &library)?;
    Ok(())
}

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
            clean_processed_art(&scan_images, &art_repo.templates_folder, disk);
        }
    }
    for folder_path in scan_folders {
        let nice_path = ItemPath::Folder(
            folder_path
                .strip_prefix(&library_config.library_folder)
                .unwrap_or(&folder_path)
                .to_owned(),
        );
    }
    for (song_path, options) in scan_songs {
        let nice_path = ItemPath::Song(
            song_path
                .strip_prefix(&library_config.library_folder)
                .unwrap_or(&song_path)
                .with_extension(""),
        );
        let results = setting::process_song(
            &nice_path,
            &song_path,
            library_config,
            &mut config_cache,
            &options,
        );
        handle_song_results(&results, &nice_path, library_config);
        if results.has_fatal_shared_error() || results.has_fatal_local_error() {
            progress.failed += 1;
            library_config.date_cache.remove(&song_path);
        } else {
            progress.changed += 1;
            library_config.date_cache.mark_updated(song_path.clone());
            if let Some(assigned) = results.assigned {
                library_config.update_reports(&nice_path, &assigned.metadata.metadata);
                if let Some(art_repo) = &mut library_config.art_repo {
                    art_repo.used_templates.add(&song_path, &assigned.art);
                }
            }
        }
    }
    if progress.failed == 0 {
        println!("Updated {}", progress.changed);
    } else {
        println!("Updated {}, errored {}", progress.changed, progress.failed);
    }
    save_caches(library_config);
    save_reports(&library_config.reports);
    if let Some(art_repo) = &library_config.art_repo {
        if let Some(disk) = &art_repo.disk_cache {
            save_processed_art(disk, &art_repo.processed_cache);
        }
    }
}

fn handle_song_results(
    results: &ProcessSongResults,
    nice_path: &Path,
    library_config: &mut LibraryConfig,
) {
    for load in &results.configs.newly_loaded {
        handle_config_loaded(load, library_config);
    }
    if let Some(AssignResults {
        art:
            GetArtResults::Processed {
                processed: Some(result),
                full_path,
                ..
            },
        ..
    }) = &results.assigned
    {
        handle_art_loaded(result, full_path, library_config);
    }
    if results.has_fatal_shared_error() {
        return;
    }
    println!("{}", nice_path.display());
    if let Some(assigned) = &results.assigned {
        if let GetArtResults::NoTemplateFound { tried } = &assigned.art {
            eprintln!(
                "{}",
                cformat!("⚠️ <yellow>No matching templates found: {:?}</>", tried)
            );
        }
        handle_apply_reports(&assigned.metadata.reports);
        handle_tag_changes(
            assigned.added.id3.as_ref(),
            &assigned.metadata.metadata,
            "ID3",
        );
        handle_tag_changes(
            assigned.added.flac.as_ref(),
            &assigned.metadata.metadata,
            "FLAC",
        );
        handle_tag_changes(
            assigned.added.ape.as_ref(),
            &assigned.metadata.metadata,
            "APE",
        );
        if let Some(report) = &assigned.file_lyrics {
            if !report.results.is_empty() {
                println!("\tUpdated file cached lyrics");
            }
            handle_lyrics_report(report);
        }
    }
}

fn handle_lyrics_report(report: &SetLyricsReport) {
    for (lyrics_type, result) in &report.results {
        match result {
            SetLyricsResult::Replaced(existing) => match existing {
                Ok(existing) => {
                    println!(
                        "\t\tReplaced {:?} lyrics: {:?}",
                        lyrics_type,
                        lyrics::display(existing)
                    );
                }
                Err(err) => {
                    println!("\t\tReplaced {:?} lyrics ({})", lyrics_type, err);
                }
            },
            SetLyricsResult::Removed(existing) => match existing {
                Ok(existing) => {
                    println!(
                        "\t\tRemoved {:?} lyrics: {:?}",
                        lyrics_type,
                        lyrics::display(existing)
                    );
                }
                Err(err) => {
                    println!("\t\tRemoved {:?} lyrics ({})", lyrics_type, err);
                }
            },
            SetLyricsResult::Failed(err) => {
                eprintln!(
                    "{}",
                    cformat!(
                        "\t\t❌ <red>Error writing {:?} lyrics: {}</>",
                        lyrics_type,
                        err
                    )
                );
            }
        }
    }
}

fn handle_tag_changes(
    changes: Result<&TagChanges, &AddToSongError>,
    metadata: &Metadata,
    tag_type: &'static str,
) {
    match changes {
        Ok(TagChanges::None) => {}
        Ok(TagChanges::Removed) => {
            println!("\tRemoved {} tag", tag_type);
        }
        Ok(TagChanges::Set(changes)) => {
            if changes.any() {
                if changes.created {
                    println!("\tAdded {} tag", tag_type);
                } else {
                    println!("\tUpdated {} tag", tag_type);
                }
            }
            if let Some((lyrics, report)) = &changes.lyrics {
                handle_lyrics_report(report);
                if !report.results.is_empty() {
                    println!("\t\tNew lyrics: {:?}", lyrics::display(lyrics));
                }
            }
            for (field, change) in &changes.metadata.fields {
                let new = metadata.get(field).unwrap_or(&metadata::BLANK_VALUE);
                match change {
                    tag_interop::SetFieldResult::Replaced(existing) => {
                        if changes.created {
                            println!("\t\t{}: {}", field, new);
                        } else {
                            println!("\t\t{}: {} -> {}", field, existing, new);
                        }
                    }
                    tag_interop::SetFieldResult::Incompatible { expected } => {
                        eprintln!(
                            "{}",
                            cformat!("⚠️ <yellow>Invalid field value, expected {}</>", expected)
                        );
                    }
                }
            }
        }
        Err(err) => {
            eprintln!(
                "{}",
                cformat!("\t❌ <red>Error updating {} tag\n\t{}</>", tag_type, err)
            );
        }
    }
}

fn handle_folder_results(results: &ProcessFolderResults) {}

fn save_caches(library_config: &mut LibraryConfig) {
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
}

fn save_reports(reports: &[LibraryReport]) {
    for report in reports {
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

fn clean_processed_art(templates: &BTreeSet<PathBuf>, folder: &Path, disk: &mut ArtDiskCache) {
    let mut deleted = 0;
    for image in templates {
        let nice = image
            .strip_prefix(folder)
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

fn save_processed_art(disk: &ArtDiskCache, processed: &ProcessedArtCache) {
    let mut wrote = 0;
    for (nice, image) in processed {
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
                    Ok(()) => {
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

fn handle_apply_reports(reports: &Vec<SourcedReport>) {
    for report in reports {
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

fn handle_art_loaded(
    processed: &GetProcessedResult,
    full_path: &Path,
    library_config: &mut LibraryConfig,
) {
    for load in &processed.newly_loaded {
        match &load.result {
            Err(error) => {
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
            Ok(_) => {
                library_config
                    .date_cache
                    .mark_updated(load.full_path.clone());
            }
        }
    }
    match &processed.result {
        Err(error) => {
            if !matches!(error.deref(), ArtError::Config) {
                library_config.date_cache.remove(full_path);
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
        Ok(_) => {
            library_config.date_cache.mark_updated(full_path.to_owned());
        }
    }
}

fn handle_config_loaded(load: &ConfigLoadResults, library_config: &mut LibraryConfig) {
    match &load.result {
        Err(error) => {
            library_config.date_cache.remove(&load.full_path);
            eprintln!(
                "{}",
                cformat!(
                    "❌ <red>Error loading config\n{}\n{}</>",
                    load.full_path.display(),
                    error
                )
            );
        }
        Ok(config) => {
            library_config
                .date_cache
                .mark_updated(load.full_path.to_owned());
            let mut warnings = vec![];
            let unused =
                song_config::get_unused_selectors(config, &load.nice_folder, library_config);
            if !unused.is_empty() {
                warnings.push(cformat!("<yellow>Selectors that didn't find anything:</>"));
                for selector in unused {
                    warnings.push(cformat!("\t<yellow>{}</>", modifier::inline_data(selector)));
                }
            }
            if let Some(order) = &config.order {
                let unselected =
                    song_config::get_unselected_items(order, &load.nice_folder, library_config);
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
                        load.full_path.display()
                    )
                );
                for warning in warnings {
                    eprintln!("{warning}");
                }
            }
        }
    }
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
                        if let Some(settings) = library_config.scan_settings(path) {
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
                        if let Some(settings) = library_config.scan_settings(path) {
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
