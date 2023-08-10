use jwalk::WalkDir;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

mod library_config;
use library_config::*;

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
