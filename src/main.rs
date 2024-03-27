use art::{ArtDiskCache, ProcessedArtCache, RawArtRepo};
use color_print::cformat;
use image::DynamicImage;
use itertools::Itertools;
use library_config::{SetLyricsReport, SetLyricsResult};
use lyrics::{RichLyrics, SyncedLyrics};
use regex::Regex;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    env,
    io::{ErrorKind, IsTerminal, Write},
    ops::Deref,
    path::{Path, PathBuf},
    rc::Rc,
};
use tag_interop::GetLyrics;
use walkdir::DirEntry;

use crate::{
    art::{ArtError, GetArtResults},
    file_stuff::{ConfigError, YamlError},
    library_config::{
        LibraryConfig, LibraryReport, LyricsConfig, LyricsReplaceMode, LyricsType,
        RawLibraryConfig, RawLibraryReport, ScanDecision, ScanOptions, TagOptions, TagSettings,
    },
    metadata::{Metadata, MetadataField, MetadataValue, SourcedReport},
    modifier::ValueError,
    song_config::{ConfigCache, SongConfig},
    strategy::FieldSelector,
    tag_interop::{GetLyricsError, SetValue},
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
        if let Some(mut results) = results {
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
                &mut results.metadata,
                art_set,
                library_config,
            );
            match add_results {
                Ok(()) => {
                    library_config.update_reports(&nice_path, &results.metadata);
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
    save_caches(library_config);
    save_reports(&library_config.reports);
    if let Some(art_repo) = &library_config.art_repo {
        if let Some(disk) = &art_repo.disk_cache {
            save_processed_art(disk, &art_repo.processed_cache);
        }
    }
}

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
    let configs = song_config::get_relevant_configs(library_config, nice_path, config_cache);
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
        let mut results = metadata::get_metadata(nice_path, &configs, library_config);
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
    let configs = song_config::get_relevant_configs(library_config, nice_path, config_cache);
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
        let mut results = metadata::get_metadata(nice_path, &configs, library_config);
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
    #[error("{0}")]
    Ape(#[from] ape::Error),
    #[error("{0}")]
    Lyrics(#[from] GetLyricsError),
}

fn add_to_song(
    options: &TagOptions,
    file_path: &Path,
    nice_path: &Path,
    metadata: &mut Metadata,
    art: SetValue<Rc<DynamicImage>>,
    config: &mut LibraryConfig,
) -> Result<(), AddToSongError> {
    let mut best_lyrics = None;
    match options.id3 {
        TagSettings::Ignore => {}
        TagSettings::Remove => {
            if id3::Tag::remove_from_path(file_path)? {
                println!("\tRemoved entire ID3 tag");
            }
        }
        TagSettings::Set {} => {
            let mut changed = false;
            let mut lyrics_changes = None;
            let mut tag = id3::Tag::read_from_path(file_path);
            if let Err(id3::Error {
                kind: id3::ErrorKind::NoTag,
                ..
            }) = tag
            {
                tag = Ok(id3::Tag::new());
                changed = true;
            }
            let mut tag = tag?;
            if let Some(lyrics_config) = &config.lyrics {
                let best = lyrics_config.get_best(nice_path, &tag);
                match best {
                    Ok(lyrics) => {
                        let report = lyrics_config.set(&lyrics, &mut tag);
                        changed |= lyrics_changed_tag(&lyrics, &report);
                        lyrics_changes = Some(report);
                        best_lyrics = Some(lyrics);
                    }
                    Err(GetLyricsError::NotEmbedded) => {}
                    Err(err) => return Err(AddToSongError::Lyrics(err)),
                }
            }
            if changed {
                println!("\tUpdated ID3 tag:");
                if let Some(changes) = lyrics_changes {
                    handle_lyrics_changes(&changes);
                }
            }
        }
    }
    match options.ape {
        TagSettings::Ignore => {}
        TagSettings::Remove => {
            let existing = ape::read_from_path(file_path);
            if let Err(ape::Error::TagNotFound) = existing {
                return Ok(());
            }
            ape::remove_from_path(file_path)?;
            println!("\tRemoved entire APE tag");
        }
        TagSettings::Set {} => {
            let mut changed = false;
            let mut lyrics_changes = None;
            let mut tag = ape::read_from_path(file_path);
            if let Err(ape::Error::TagNotFound) = tag {
                tag = Ok(ape::Tag::new());
                changed = true;
            }
            let mut tag = tag?;
            if let Some(lyrics_config) = &config.lyrics {
                let best = lyrics_config.get_best(nice_path, &tag);
                match best {
                    Ok(lyrics) => {
                        let report = lyrics_config.set(&lyrics, &mut tag);
                        changed |= lyrics_changed_tag(&lyrics, &report);
                        lyrics_changes = Some(report);
                        best_lyrics = Some(lyrics);
                    }
                    Err(GetLyricsError::NotEmbedded) => {}
                    Err(err) => return Err(AddToSongError::Lyrics(err)),
                }
            }
            if changed {
                println!("\tUpdated APE tag:");
                if let Some(changes) = lyrics_changes {
                    handle_lyrics_changes(&changes);
                }
            }
        }
    }
    match options.flac {
        TagSettings::Ignore => {}
        TagSettings::Remove => {
            let existing = metaflac::Tag::read_from_path(file_path);
            if let Err(metaflac::Error {
                kind: metaflac::ErrorKind::InvalidInput,
                ..
            }) = existing
            {
                return Ok(());
            }
            if existing?.blocks().next().is_some() {
                let mut tag = metaflac::Tag::new();
                tag.write_to_path(file_path)?;
                println!("\tRemoved entire FLAC tag");
            }
        }
        TagSettings::Set {} => {
            let mut changed = false;
            let mut lyrics_changes = None;
            let mut tag = metaflac::Tag::read_from_path(file_path);
            if let Err(metaflac::Error {
                kind: metaflac::ErrorKind::InvalidInput,
                ..
            }) = tag
            {
                tag = Ok(metaflac::Tag::new());
                changed = true;
            }
            let mut tag = tag?;
            if let Some(lyrics_config) = &config.lyrics {
                let best = lyrics_config.get_best(nice_path, &tag);
                match best {
                    Ok(lyrics) => {
                        let report = lyrics_config.set(&lyrics, &mut tag);
                        changed |= lyrics_changed_tag(&lyrics, &report);
                        lyrics_changes = Some(report);
                        best_lyrics = Some(lyrics);
                    }
                    Err(GetLyricsError::NotEmbedded) => {}
                    Err(err) => return Err(AddToSongError::Lyrics(err)),
                }
            }
            if changed {
                println!("\tUpdated FLAC tag:");
                if let Some(changes) = lyrics_changes {
                    handle_lyrics_changes(&changes);
                }
            }
        }
    }
    if let Some(lyrics) = best_lyrics {
        if let Some(lyrics_config) = &config.lyrics {
            let report = lyrics_config.write(nice_path, &lyrics);
            handle_lyrics_changes(&report);
        }
        metadata.insert(
            MetadataField::SimpleLyrics,
            MetadataValue::string(lyrics.into()),
        );
    }
    Ok(())
}

struct TagChanges {
    lyrics: Option<SetLyricsReport>,
}
impl TagChanges {
    fn new() -> Self {
        Self { lyrics: None }
    }
}

fn lyrics_changed_tag(best: &RichLyrics, changes: &SetLyricsReport) -> bool {
    for lyrics_type in [
        LyricsType::RichEmbedded,
        LyricsType::SyncedEmbedded,
        LyricsType::SimpleEmbedded,
    ] {
        let changed = match changes.results.get(&lyrics_type) {
            None => false,
            Some(SetLyricsResult::Removed(Err(GetLyricsError::NotEmbedded))) => false,
            Some(SetLyricsResult::Replaced(Ok(existing))) => best != existing,
            Some(_) => true,
        };
        if changed {
            return true;
        }
    }
    false
}

fn handle_lyrics_changes(changes: &SetLyricsReport) {
    for (lyrics_type, result) in &changes.results {
        match result {
            SetLyricsResult::Replaced(existing) => {
                println!("{:?}", existing)
            }
            SetLyricsResult::Removed(existing) => {
                println!("{:?}", existing)
            }
            SetLyricsResult::Failed(_) => {}
        }
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
                        if !song_config::is_not_found(error) {
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

fn handle_config_loaded(
    result: Result<&SongConfig, &ConfigError>,
    full_path: &Path,
    nice_folder: &Path,
    library_config: &mut LibraryConfig,
) {
    match result {
        Err(error) => {
            library_config.date_cache.remove(full_path);
            if !song_config::is_not_found(error) {
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
            let unused = song_config::get_unused_selectors(config, nice_folder, library_config);
            if !unused.is_empty() {
                warnings.push(cformat!("<yellow>Selectors that didn't find anything:</>"));
                for selector in unused {
                    warnings.push(cformat!("\t<yellow>{}</>", modifier::inline_data(selector)));
                }
            }
            if let Some(order) = &config.order {
                let unselected =
                    song_config::get_unselected_items(order, nice_folder, library_config);
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
