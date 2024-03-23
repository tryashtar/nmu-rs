use std::{
    fs::{DirEntry, File},
    io::{BufReader, BufWriter, Write},
    ops::Deref,
    path::{Path, PathBuf},
};

use itertools::Itertools;
use serde::{de::DeserializeOwned, Serialize};

use crate::{
    library_config::{LibraryConfig, LibraryError},
    strategy::{ItemSelector, PathSegment},
    util::ItemPath,
};

#[derive(thiserror::Error, Debug)]
#[error("{0}")]
pub enum YamlError {
    Io(#[from] std::io::Error),
    Yaml(#[from] serde_yaml::Error),
}

#[derive(thiserror::Error, Debug)]
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

pub fn save_yaml<T>(path: &Path, value: &T) -> Result<(), YamlError>
where
    T: Serialize,
{
    let file = File::create(path)?;
    let mut writer = BufWriter::new(file);
    serde_yaml::to_writer(&mut writer, value)?;
    writer.flush()?;
    Ok(())
}

pub fn is_dir(entry: &DirEntry) -> bool {
    entry.file_type().map(|x| x.is_dir()).unwrap_or(false)
}

fn matches_segment(path: &Path, segment: &PathSegment) -> bool {
    path.file_name().is_some_and(|x| segment.matches(x))
}

pub fn matches_name(path: &Path, name: &str) -> bool {
    path.file_name().is_some_and(|x| x == name)
}

pub fn find_matches(
    selector: &ItemSelector,
    nice_start: &Path,
    config: &LibraryConfig,
) -> Vec<ItemPath> {
    let full_start = config.library_folder.join(nice_start);
    match selector {
        ItemSelector::This => vec![ItemPath::Folder(PathBuf::from(""))],
        ItemSelector::All { recursive } => {
            if *recursive {
                walkdir::WalkDir::new(&full_start)
                    .into_iter()
                    .skip(1)
                    .filter_map(|x| x.ok())
                    .filter_map(|entry| {
                        let is_dir = entry.file_type().is_dir();
                        let full_path = entry.into_path();
                        let path = full_path.strip_prefix(&full_start).ok();
                        if is_dir {
                            path.map(|x| ItemPath::Folder(x.to_owned()))
                        } else {
                            if config.scan_settings(&full_path).is_none() {
                                return None;
                            }
                            path.map(|x| ItemPath::Song(x.with_extension("")))
                        }
                    })
                    .sorted_by(|a, b| Ord::cmp(a.deref(), b.deref()))
                    .collect()
            } else {
                if let Ok(read) = std::fs::read_dir(&full_start) {
                    return read
                        .into_iter()
                        .filter_map(|x| x.ok())
                        .filter_map(|entry| {
                            let full_path = entry.path();
                            let path = full_path.strip_prefix(&full_start).unwrap_or(&full_path);
                            if is_dir(&entry) {
                                Some(ItemPath::Folder(path.to_owned()))
                            } else {
                                if config.scan_settings(&full_path).is_none() {
                                    return None;
                                }
                                Some(ItemPath::Song(path.with_extension("")))
                            }
                        })
                        .sorted_by(|a, b| Ord::cmp(a.deref(), b.deref()))
                        .collect();
                }
                vec![]
            }
        }
        ItemSelector::Multi(checks) => checks
            .iter()
            .flat_map(|selector| find_matches(selector, nice_start, config))
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
                                } else {
                                    if config.scan_settings(&path).is_none() {
                                        return None;
                                    }
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
                            } else {
                                if config.scan_settings(&path).is_none() {
                                    return None;
                                }
                                let stripped = path.with_extension("");
                                if matches_segment(&stripped, last) {
                                    return Some(ItemPath::Song(stripped));
                                }
                            }
                            None
                        })
                    })
                    .sorted_by(|a, b| Ord::cmp(a.deref(), b.deref()))
                    .collect();
            }
            vec![]
        }
        ItemSelector::Subpath { subpath, select } => {
            let first = find_matches(subpath, nice_start, config)
                .into_iter()
                .filter_map(|x| match x {
                    ItemPath::Folder(path) => Some(path),
                    ItemPath::Song(_) => None,
                });
            first
                .flat_map(|path| {
                    let second = find_matches(select, &nice_start.join(&path), config);
                    second.into_iter().map(move |x| match x {
                        ItemPath::Folder(p) => ItemPath::Folder(path.join(p)),
                        ItemPath::Song(p) => ItemPath::Song(path.join(p)),
                    })
                })
                .collect()
        }
    }
}
