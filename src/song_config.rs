use std::{
    collections::{BTreeSet, HashMap, HashSet},
    io::ErrorKind,
    ops::Deref,
    path::{Path, PathBuf},
    rc::Rc,
};

use serde::{Deserialize, Serialize};

use crate::{
    file_stuff::{self, ConfigError, YamlError},
    library_config::LibraryConfig,
    metadata::{Metadata, MetadataField, MetadataValue},
    strategy::{ApplyReport, ItemSelector, MetadataOperation, MusicItemType, ValueGetter},
    util::ItemPath,
};

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RawSongConfig {
    pub songs: Option<ReferencableOperation>,
    pub folders: Option<ReferencableOperation>,
    pub this: Option<ReferencableOperation>,
    pub discs: Option<HashMap<u32, ItemSelector>>,
    pub order: Option<ItemSelector>,
    #[serde(rename = "set fields")]
    pub set_fields: Option<Vec<RawFieldSetter>>,
    #[serde(rename = "set all")]
    pub set_all: Option<Vec<RawAllSetter>>,
    pub set: Option<HashMap<PathBuf, ReferencableOperation>>,
    pub subconfigs: Option<HashMap<PathBuf, RawSongConfig>>,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ReferencableOperation {
    Reference(String),
    Direct(MetadataOperation),
    Many(Vec<ReferencableOperation>),
}

#[derive(Deserialize, Serialize)]
pub struct RawAllSetter {
    pub names: ItemSelector,
    pub set: ReferencableOperation,
}

#[derive(Deserialize, Serialize)]
pub struct RawFieldSetter {
    pub field: MetadataField,
    pub set: HashMap<PathBuf, ValueGetter>,
}

pub struct SongConfig {
    pub set: Vec<Rc<AllSetter>>,
    pub order: Option<OrderingSetter>,
    pub subconfigs: HashMap<PathBuf, SongConfig>,
}
pub enum OrderingSetter {
    Order {
        map: HashMap<PathBuf, u32>,
        total: u32,
        original_selector: ItemSelector,
    },
    Discs {
        map: HashMap<PathBuf, DiscSet>,
        disc_total: u32,
        original_selectors: Vec<ItemSelector>,
    },
}
pub struct DiscSet {
    pub disc: u32,
    pub track: u32,
    pub track_total: u32,
}
impl SongConfig {
    pub fn apply(
        &self,
        nice_path: &ItemPath,
        select: &Path,
        metadata: &mut Metadata,
        copy_source: &Metadata,
        library_config: &LibraryConfig,
    ) -> ApplyReport {
        let mut report = ApplyReport { errors: vec![] };
        if let Some(order) = &self.order {
            match order {
                OrderingSetter::Order { map, total, .. } => {
                    if let Some(track) = map.get(select) {
                        metadata.insert(MetadataField::Track, MetadataValue::Number(*track));
                        metadata.insert(MetadataField::TrackTotal, MetadataValue::Number(*total));
                    }
                }
                OrderingSetter::Discs {
                    map, disc_total, ..
                } => {
                    if let Some(values) = map.get(select) {
                        metadata.insert(MetadataField::Disc, MetadataValue::Number(values.disc));
                        metadata
                            .insert(MetadataField::DiscTotal, MetadataValue::Number(*disc_total));
                        metadata.insert(MetadataField::Track, MetadataValue::Number(values.track));
                        metadata.insert(
                            MetadataField::TrackTotal,
                            MetadataValue::Number(values.track_total),
                        );
                    }
                }
            }
        }
        for setter in &self.set {
            if setter.names.matches(select)
                && MusicItemType::matches(nice_path.as_type(), setter.must_be)
            {
                let more =
                    setter
                        .set
                        .apply(metadata, copy_source, nice_path.as_ref(), library_config);
                report.merge(more);
            }
        }
        for (path, sub) in &self.subconfigs {
            let select_path = nice_path.strip_prefix(path).unwrap_or(path);
            let more = sub.apply(
                nice_path,
                select_path,
                metadata,
                copy_source,
                library_config,
            );
            report.merge(more);
        }
        report
    }
}

pub struct AllSetter {
    pub names: ItemSelector,
    pub must_be: Option<MusicItemType>,
    pub set: Rc<MetadataOperation>,
}
impl AllSetter {
    pub fn new(selector: ItemSelector, set: MetadataOperation) -> Self {
        Self {
            names: selector,
            must_be: None,
            set: Rc::new(set),
        }
    }
}

pub struct GetConfigsResults {
    pub result: Result<Vec<LoadedConfig>, Rc<ConfigError>>,
    pub newly_loaded: Vec<ConfigLoadResults>,
}
pub struct ConfigLoadResults {
    pub result: Result<Rc<SongConfig>, Rc<ConfigError>>,
    pub nice_folder: PathBuf,
    pub full_path: PathBuf,
}
pub struct LoadedConfig {
    pub config: Rc<SongConfig>,
    pub nice_folder: PathBuf,
    pub full_path: PathBuf,
}
pub type ConfigCache = HashMap<PathBuf, Result<Rc<SongConfig>, Rc<ConfigError>>>;

pub fn is_not_found(result: &ConfigError) -> bool {
    matches!(result, ConfigError::Yaml(YamlError::Io(error)) if error.kind() == ErrorKind::NotFound)
}

pub fn get_relevant_configs(
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
                match &config {
                    Err(err) if is_not_found(err) => {}
                    _ => {
                        newly_loaded.push(ConfigLoadResults {
                            result: config.clone(),
                            full_path: x.clone(),
                            nice_folder: nice_folder.to_owned(),
                        });
                    }
                }
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
                        newly_loaded,
                        result: Err(err.clone()),
                    };
                }
            }
        }
    }
    GetConfigsResults {
        newly_loaded,
        result: Ok(results),
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

pub fn get_unused_selectors<'a>(
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

pub fn get_unselected_items(
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
