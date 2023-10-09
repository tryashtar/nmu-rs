use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

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
            Some(name) => std::fs::read_dir(full_start.parent().unwrap_or(Path::new("")))
                .unwrap()
                .map(|x| x.unwrap().path().with_extension(""))
                .filter(|x| match_name(x, name))
                .filter_map(|x| x.strip_prefix(&full_start).ok().map(|x| x.to_owned()))
                .collect(),
            None => {
                vec![]
            }
        },
        ItemSelector::Segmented { path } => {
            let mut items = vec![full_start.clone()];
            for segment in path {
                items = items
                    .into_iter()
                    .flat_map(|x| {
                        std::fs::read_dir(x).unwrap().filter_map(|x| {
                            let val = x.unwrap();
                            segment
                                .matches(val.file_name().as_os_str())
                                .then(|| val.path())
                        })
                    })
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
        ItemSelector::Subpath { subpath, select } => find_matches(subpath, start, config)
            .into_iter()
            .flat_map(|x| find_matches(select, x.as_path(), config))
            .collect(),
    }
}
