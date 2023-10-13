use std::{
    collections::HashSet,
    fs::{DirEntry, File},
    io::{BufReader, ErrorKind},
    path::{Path, PathBuf}, ffi::OsStr,
};

use colored::Colorize;
use itertools::Itertools;
use serde::de::DeserializeOwned;
use thiserror::Error;

use crate::{
    library_config::{LibraryConfig, LibraryError},
    song_config::{RawSongConfig, SongConfig},
    strategy::{ItemSelector, PathSegment},
    util::ItemPath,
};

#[derive(Error, Debug)]
#[error("{0}")]
pub enum YamlError {
    Io(#[from] std::io::Error),
    Yaml(#[from] serde_yaml::Error),
}

#[derive(Error, Debug)]
#[error("{0}")]
pub enum ConfigError {
    Yaml(#[from] YamlError),
    Library(#[from] LibraryError),
}

pub fn load_yaml<T>(path: &Path) -> Result<T, YamlError>
where
    T: DeserializeOwned,
{
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let yaml: T = serde_yaml::from_reader(reader)?;
    Ok(yaml)
}

pub fn load_config(
    full_path: &Path,
    nice_folder: &Path,
    library_config: &LibraryConfig,
) -> Result<SongConfig, ConfigError> {
    match load_yaml::<RawSongConfig>(full_path) {
        Err(YamlError::Io(error)) if error.kind() == ErrorKind::NotFound => {
            Err(ConfigError::Yaml(YamlError::Io(error)))
        }
        Err(error) => {
            eprintln!(
                "{} {}",
                "Error loading config:".red(),
                full_path.display().to_string().red()
            );
            eprintln!("{}", error.to_string().red());
            Err(ConfigError::Yaml(error))
        }
        Ok(config) => library_config
            .resolve_config(config, nice_folder)
            .map_err(ConfigError::Library),
    }
}

pub fn match_extension(path: &Path, extensions: &HashSet<String>) -> bool {
    match path.extension().and_then(|x| x.to_str()) {
        Some(ext) => extensions.contains(ext),
        None => false,
    }
}

fn is_dir(entry: &DirEntry) -> bool {
    entry.file_type().map(|x| x.is_dir()).unwrap_or(false)
}

fn matches_segment(path: &Path, segment: &PathSegment) -> bool {
    path.file_name()
        .map(|x| segment.matches(x))
        .unwrap_or(false)
}

fn matches_name(path: &Path, name: &str) -> bool {
    path.file_name().map(|x| x == name).unwrap_or(false)
}

pub fn find_matches(
    selector: &ItemSelector,
    start: &Path,
    config: &LibraryConfig,
) -> Vec<ItemPath> {
    let full_start = config.library_folder.join(start);
    match selector {
        ItemSelector::This => vec![ItemPath::Folder(PathBuf::from(""))],
        ItemSelector::All => walkdir::WalkDir::new(&full_start)
            .into_iter()
            .filter_entry(|entry| {
                entry.file_type().is_dir() || match_extension(entry.path(), &config.song_extensions)
            })
            .filter_map(|x| x.ok())
            .filter_map(|entry| {
                let is_dir = entry.file_type().is_dir();
                let path = entry.into_path();
                let path = path.strip_prefix(&full_start).ok();
                if is_dir {
                    path.map(|x| ItemPath::Folder(x.to_owned()))
                } else {
                    path.map(|x| ItemPath::Song(x.with_extension("")))
                }
            })
            .sorted_by(|a, b| Ord::cmp(a.as_ref(), b.as_ref()))
            .collect(),
        ItemSelector::Multi(checks) => checks
            .iter()
            .flat_map(|selector| find_matches(selector, start, config))
            .collect(),
        ItemSelector::Path(path) => {
            if let Some(name) = path.file_name().and_then(|x| x.to_str()) {
                if let Ok(read) =
                    std::fs::read_dir(full_start.join(path).parent().unwrap_or(Path::new("")))
                {
                    return read
                        .into_iter()
                        .filter_map(|x| x.ok())
                        .filter_map(|entry| {
                            let path = entry.path();
                            let path = path.strip_prefix(&full_start).ok();
                            path.and_then(|path| {
                                if is_dir(&entry) {
                                    if matches_name(path, name) {
                                        return Some(ItemPath::Folder(path.to_owned()));
                                    }
                                } else if match_extension(path, &config.song_extensions) {
                                    let stripped = path.with_extension("");
                                    if matches_name(&stripped, name) {
                                        return Some(ItemPath::Song(stripped));
                                    }
                                }
                                None
                            })
                        })
                        .collect();
                }
            }
            vec![]
        }
        ItemSelector::Segmented { path } => {
            if let Some((last, segments)) = path.split_last() {
                let mut items = vec![full_start.clone()];
                for segment in segments {
                    let files = items
                        .into_iter()
                        .filter_map(|x| std::fs::read_dir(x).ok())
                        .flatten()
                        .filter_map(|x| x.ok());
                    items = files
                        .filter(|entry| is_dir(entry) && segment.matches(&entry.file_name()))
                        .map(|x| x.path())
                        .sorted()
                        .collect();
                }
                let files = items
                    .into_iter()
                    .filter_map(|x| std::fs::read_dir(x).ok())
                    .flatten()
                    .filter_map(|x| x.ok());
                return files
                    .filter_map(|entry| {
                        let path = entry.path();
                        path.strip_prefix(&full_start).ok().and_then(|path| {
                            if is_dir(&entry) {
                                if matches_segment(path, last) {
                                    return Some(ItemPath::Folder(path.to_owned()));
                                }
                            } else if match_extension(path, &config.song_extensions) {
                                let stripped = path.with_extension("");
                                if matches_segment(&stripped, last) {
                                    return Some(ItemPath::Song(stripped));
                                }
                            }
                            None
                        })
                    })
                    .sorted_by(|a, b| Ord::cmp(a.as_ref(), b.as_ref()))
                    .collect();
            }
            vec![]
        }
        ItemSelector::Subpath { subpath, select } => {
            let first = find_matches(subpath, start, config)
                .into_iter()
                .filter_map(|x| match x {
                    ItemPath::Folder(path) => Some(path),
                    ItemPath::Song(_) => None,
                });
            first
                .flat_map(|path| {
                    let second = find_matches(select, &start.join(&path), config);
                    second.into_iter().map(move |x| match x {
                        ItemPath::Folder(p) => ItemPath::Folder(path.join(p)),
                        ItemPath::Song(p) => ItemPath::Song(path.join(p)),
                    })
                })
                .collect()
        }
    }
}
