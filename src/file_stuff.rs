use std::{
    collections::HashSet,
    fs::File,
    io::{BufReader, ErrorKind},
    path::{Path, PathBuf},
};

use colored::Colorize;
use itertools::Itertools;
use serde::de::DeserializeOwned;
use thiserror::Error;

use crate::{
    library_config::{LibraryConfig, LibraryError},
    song_config::{RawSongConfig, SongConfig},
    strategy::{ItemSelector, MusicItemType},
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

pub fn match_name(path: &Path, name: &str) -> bool {
    match path.file_name().and_then(|x| x.to_str()) {
        Some(file_name) => file_name == name,
        None => false,
    }
}

pub struct PathResults {
    songs: Vec<PathBuf>,
    folders: Vec<PathBuf>,
}

pub fn find_matches(
    selector: &ItemSelector,
    must_be: Option<&MusicItemType>,
    start: &Path,
    config: &LibraryConfig,
) -> PathResults {
    let full_start = config.library_folder.join(start);
    match selector {
        ItemSelector::This => PathResults {
            songs: vec![],
            folders: vec![PathBuf::from("")],
        },
        ItemSelector::All => {
            let folders_and_files = walkdir::WalkDir::new(&full_start)
                .into_iter()
                .filter_entry(|x| {
                    x.file_type().is_dir() || match_extension(x.path(), &config.song_extensions)
                })
                .filter_map(|x| x.ok());
            match must_be {
                Some(MusicItemType::Folder) => folders_and_files
                    .filter(|x| x.file_type().is_dir())
                    .filter_map(|x| x.into_path().strip_prefix(&full_start).ok())
                    .map(|x| x.to_owned())
                    .collect(),
                Some(MusicItemType::Song) => folders_and_files
                    .filter(|x| x.file_type().is_file())
                    .filter_map(|x| x.into_path().strip_prefix(&full_start).ok())
                    .map(|x| x.with_extension(""))
                    .collect(),
                None => folders_and_files
                    .filter_map(|x| {
                        let path = x.into_path().strip_prefix(&full_start).ok();
                        if x.file_type().is_dir() {
                            path.map(|x| x.to_owned())
                        } else {
                            path.map(|x| x.with_extension(""))
                        }
                    })
                    .collect(),
            }
        }
        ItemSelector::Multi(checks) => checks
            .iter()
            .flat_map(|x| find_matches(x, must_be, start, config))
            .collect(),
        ItemSelector::Path(path) => match path.file_name().and_then(|x| x.to_str()) {
            Some(name) => {
                match std::fs::read_dir(full_start.join(path).parent().unwrap_or(Path::new(""))) {
                    Ok(read) => read
                        .filter_map(|x| x.ok())
                        .filter_map(|x| {
                            if x.file_type().map(|x| x.is_dir()).unwrap_or(false) {
                                match must_be {
                                    Some(MusicItemType::Song) => None,
                                    None | Some(MusicItemType::Folder) => Some(x.path()),
                                }
                            } else {
                                match must_be {
                                    Some(MusicItemType::Folder) => None,
                                    None | Some(MusicItemType::Song) => {
                                        let path = x.path();
                                        match_extension(&path, &config.song_extensions)
                                            .then(|| path.with_extension(""))
                                    }
                                }
                            }
                        })
                        .filter(|x| match_name(&x, name))
                        .filter_map(|x| x.strip_prefix(&full_start).ok().map(|x| x.to_owned()))
                        .sorted()
                        .collect(),
                    Err(_) => vec![],
                }
            }
            None => {
                vec![]
            }
        },
        ItemSelector::Segmented { path } => {
            let mut items = vec![full_start.clone()];
            if let Some((last, segments)) = path.split_last() {
                for segment in segments {
                    let files = items
                        .into_iter()
                        .filter_map(|x| std::fs::read_dir(x).ok())
                        .flatten()
                        .filter_map(|x| x.ok());
                    items = files
                        .filter(|x| x.file_type().map(|x| x.is_dir()).unwrap_or(false))
                        .map(|x| x.path())
                        .filter(|x| x.file_name().map(|y| segment.matches(y)).unwrap_or(false))
                        .collect();
                }
                let files = items
                    .into_iter()
                    .filter_map(|x| std::fs::read_dir(x).ok())
                    .flatten()
                    .filter_map(|x| x.ok());
                items = files
                    .filter_map(|x| {
                        if x.file_type().map(|x| x.is_dir()).unwrap_or(false) {
                            match must_be {
                                Some(MusicItemType::Song) => None,
                                None | Some(MusicItemType::Folder) => Some(x.path()),
                            }
                        } else {
                            match must_be {
                                Some(MusicItemType::Folder) => None,
                                None | Some(MusicItemType::Song) => {
                                    let path = x.path();
                                    match_extension(&path, &config.song_extensions)
                                        .then(|| path.with_extension(""))
                                }
                            }
                        }
                    })
                    .filter(|x| x.file_name().map(|y| last.matches(y)).unwrap_or(false))
                    .collect();
            }
            items
                .into_iter()
                .filter_map(|x| x.strip_prefix(&full_start).ok().map(|x| x.to_owned()))
                .sorted()
                .collect()
        }
        ItemSelector::Subpath { subpath, select } => {
            let first = find_matches(subpath, Some(&MusicItemType::Folder), start, config);
            first
                .into_iter()
                .flat_map(|path| {
                    let second = find_matches(select, must_be, &start.join(&path), config);
                    second.into_iter().map(move |x| path.join(x))
                })
                .collect()
        }
    }
}
