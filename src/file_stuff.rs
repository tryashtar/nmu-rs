use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use itertools::Itertools;

use crate::{library_config::LibraryConfig, song_config::ItemSelector};

pub fn file_path(item: jwalk::Result<jwalk::DirEntry<((), ())>>) -> Option<PathBuf> {
    match item {
        Err(_) => None,
        Ok(entry) => {
            let path: PathBuf = entry.path();
            if path.extension().and_then(|x| x.to_str()).is_some() {
                Some(path)
            } else {
                None
            }
        }
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

pub fn find_matches(selector: &ItemSelector, start: &Path, config: &LibraryConfig) -> Vec<PathBuf> {
    let full_start = config.library_folder.join(start);
    match selector {
        ItemSelector::All => jwalk::WalkDir::new(&full_start)
            .sort(true)
            .into_iter()
            .filter_map(file_path)
            .filter(|x| match_extension(x, &config.song_extensions))
            .filter_map(|x| {
                x.strip_prefix(&full_start)
                    .ok()
                    .map(|x| x.with_extension(""))
            })
            .collect(),
        ItemSelector::Multi(checks) => checks
            .iter()
            .flat_map(|x| find_matches(x, start, config))
            .collect(),
        ItemSelector::Path(path) => match path.file_name().and_then(|x| x.to_str()) {
            Some(name) => {
                match std::fs::read_dir(full_start.join(path).parent().unwrap_or(Path::new(""))) {
                    Ok(read) => read
                        .filter_map(|x| x.ok())
                        .filter_map(|x| {
                            let path = x.path();
                            let stripped = path.with_extension("");
                            (match_name(&stripped, name)
                                && (x.metadata().map(|x| x.is_dir()).unwrap_or(false)
                                    || match_extension(&path, &config.song_extensions)))
                            .then_some(stripped)
                        })
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
            for segment in path {
                let files = items
                    .into_iter()
                    .filter_map(|x| std::fs::read_dir(x).ok())
                    .flatten()
                    .filter_map(|x| x.ok());
                items = files
                    .filter_map(|x| {
                        let path = x.path();
                        let stripped = path.with_extension("");
                        let name = stripped.file_name();
                        let name_matches = name.map(|x| segment.matches(x)).unwrap_or(false);
                        let type_matches = x.metadata().unwrap().is_dir()
                            || match_extension(&path, &config.song_extensions);
                        (name_matches && type_matches).then_some(path)
                    })
                    .sorted()
                    .collect();
            }
            items
                .into_iter()
                .filter_map(|x| {
                    x.strip_prefix(&full_start)
                        .ok()
                        .map(|x| x.with_extension(""))
                })
                .collect()
        }
        ItemSelector::Subpath { subpath, select } => {
            let first = find_matches(subpath, start, config);
            first
                .into_iter()
                .flat_map(|path| {
                    let second = find_matches(select, &start.join(&path), config);
                    second.into_iter().map(move |x| path.join(x))
                })
                .collect()
        }
    }
}
