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
    metadata::{self, Metadata, MetadataField, MetadataValue, PendingMetadata},
    strategy::{ApplyReport, ItemSelector, MetadataOperation, MusicItemType, ValueGetter},
    util::ItemPath,
};

struct SlottedVec<T> {
    underlying: Vec<Option<T>>,
}
impl<T> Default for SlottedVec<T> {
    fn default() -> Self {
        Self {
            underlying: Default::default(),
        }
    }
}
impl<T> SlottedVec<T> {
    fn insert(&mut self, item: T, index: usize) -> Option<T> {
        self.underlying.resize_with(
            usize::max(self.underlying.len(), index + 1),
            Default::default,
        );
        std::mem::replace(&mut self.underlying[index], Some(item))
    }
    fn get_or_create(&mut self, index: usize, mut create: impl FnMut() -> T) -> &mut T {
        if self
            .underlying
            .get(index)
            .and_then(|x| x.as_ref())
            .is_none()
        {
            let new_item = create();
            self.insert(new_item, index);
        }
        self.underlying.get_mut(index).unwrap().as_mut().unwrap()
    }
    fn into_contiguous(self) -> Option<Vec<T>> {
        self.underlying.into_iter().collect()
    }
    fn is_empty(&self) -> bool {
        self.underlying.is_empty()
    }
}

enum NoOrderReason<'a> {
    TrackOutOfBounds(&'a Path, u32),
    DiscOutOfBounds(&'a Path, u32),
    NoTrackNumber(&'a Path),
    DuplicateTrackNumber(&'a Path, &'a Path, u32),
    UnexpectedDiscPresent(&'a Path, u32),
    UnexpectedDiscAbsent(&'a Path),
    NoTracks,
    NotContiguous,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum RawSongConfigFile {
    Direct(RawSongConfig),
    Reverse { reverse: ReverseMode },
}
impl RawSongConfigFile {
    pub fn finalize(
        self,
        folder: &Path,
        config: &LibraryConfig,
        others_so_far: &[LoadedConfig],
    ) -> Result<RawSongConfig, ConfigError> {
        match self {
            Self::Direct(raw) => Ok(raw),
            Self::Reverse { reverse } => {
                let files = walkdir::WalkDir::new(folder)
                    .into_iter()
                    .skip(1)
                    .filter_map(|x| x.ok())
                    .filter(|x| x.file_type().is_file())
                    .filter_map(|entry| {
                        let full_path = entry.into_path();
                        let nice_path = full_path
                            .strip_prefix(folder)
                            .unwrap_or(&full_path)
                            .with_extension("");
                        let settings = config.scan_settings(&full_path)?;
                        let embedded = metadata::get_embedded(&full_path, &settings);
                        let current = match reverse {
                            ReverseMode::Full => HashMap::new(),
                            ReverseMode::Minimal => {
                                metadata::get_metadata(
                                    &ItemPath::Song(nice_path.clone()),
                                    others_so_far,
                                    config,
                                )
                                .metadata
                            }
                        };
                        Some((nice_path, (current, embedded)))
                    })
                    .collect::<HashMap<_, _>>();
                Ok(Self::make_reverse(files))
            }
        }
    }
    fn get_order<'a>(tracks: HashMap<&'a Path, &Metadata>) -> Result<SongOrder, NoOrderReason<'a>> {
        let total_tracks = tracks.len();
        let mut track_list = SlottedVec::<&Path>::default();
        let mut disc_list = SlottedVec::<SlottedVec<&Path>>::default();
        for (path, metadata) in tracks {
            let track_number = metadata.get(&MetadataField::Track);
            if let Some(MetadataValue::Number(track_number)) = track_number {
                if *track_number == 0 || (*track_number as usize) > total_tracks {
                    return Err(NoOrderReason::TrackOutOfBounds(path, *track_number));
                }
                let disc_number = metadata.get(&MetadataField::Disc);
                if let Some(MetadataValue::Number(disc_number)) = disc_number {
                    if *disc_number == 0 || (*disc_number as usize) > total_tracks {
                        return Err(NoOrderReason::DiscOutOfBounds(path, *disc_number));
                    }
                    if !track_list.is_empty() {
                        return Err(NoOrderReason::UnexpectedDiscPresent(path, *disc_number));
                    }
                    let disc_vec =
                        disc_list.get_or_create(*disc_number as usize, SlottedVec::default);
                    let existing = disc_vec.insert(path, *track_number as usize);
                    if let Some(existing) = existing {
                        return Err(NoOrderReason::DuplicateTrackNumber(
                            path,
                            existing,
                            *track_number,
                        ));
                    }
                } else {
                    if !disc_list.is_empty() {
                        return Err(NoOrderReason::UnexpectedDiscAbsent(path));
                    }
                    let existing = track_list.insert(path, *track_number as usize);
                    if let Some(existing) = existing {
                        return Err(NoOrderReason::DuplicateTrackNumber(
                            path,
                            existing,
                            *track_number,
                        ));
                    }
                }
            } else {
                return Err(NoOrderReason::NoTrackNumber(path));
            }
        }
        if !track_list.is_empty() {
            let contiguous = track_list.into_contiguous();
            if let Some(contiguous) = contiguous {
                Ok(SongOrder::Order(Self::make_selector(
                    contiguous.into_iter().map(ToOwned::to_owned).collect(),
                )))
            } else {
                Err(NoOrderReason::NotContiguous)
            }
        } else if !disc_list.is_empty() {
            let contiguous = disc_list.into_contiguous();
            if let Some(contiguous) = contiguous {
                let sublists = contiguous
                    .into_iter()
                    .map(|x| x.into_contiguous())
                    .collect::<Option<Vec<_>>>();
                if let Some(sublists) = sublists {
                    Ok(SongOrder::Discs(
                        sublists
                            .into_iter()
                            .map(|x| {
                                Self::make_selector(x.into_iter().map(ToOwned::to_owned).collect())
                            })
                            .collect(),
                    ))
                } else {
                    Err(NoOrderReason::NotContiguous)
                }
            } else {
                Err(NoOrderReason::NotContiguous)
            }
        } else {
            Err(NoOrderReason::NoTracks)
        }
    }
    fn make_selector(mut paths: Vec<PathBuf>) -> ItemSelector {
        if paths.len() == 1 {
            ItemSelector::Path(paths.remove(0))
        } else {
            let common_prefix = common_path::common_path_all(paths.iter().map(AsRef::as_ref));
            if let Some(prefix) = common_prefix {
                let select = ItemSelector::Multi(
                    paths
                        .into_iter()
                        .map(|x| {
                            ItemSelector::Path(x.strip_prefix(&prefix).unwrap_or(&x).to_owned())
                        })
                        .collect(),
                );
                ItemSelector::Subpath {
                    subpath: Box::new(ItemSelector::Path(prefix)),
                    select: Box::new(select),
                }
            } else {
                ItemSelector::Multi(paths.into_iter().map(ItemSelector::Path).collect())
            }
        }
    }
    pub fn make_reverse(nice_files: HashMap<PathBuf, (Metadata, Metadata)>) -> RawSongConfig {
        let mut result = RawSongConfig::blank();
        result.order = Self::get_order(
            nice_files
                .iter()
                .map(|(path, (_, embedded))| (path.as_path(), embedded))
                .collect(),
        )
        .ok();
        result
    }
}

#[derive(Deserialize, Serialize, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum ReverseMode {
    Full,
    Minimal,
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(deny_unknown_fields)]
pub struct RawSongConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub songs: Option<ReferencableOperation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub folders: Option<ReferencableOperation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub this: Option<ReferencableOperation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(flatten)]
    pub order: Option<SongOrder>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "set fields")]
    pub set_fields: Option<Vec<RawFieldSetter>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "set all")]
    pub set_all: Option<Vec<RawAllSetter>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub set: Option<HashMap<PathBuf, ReferencableOperation>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub subconfigs: Option<HashMap<PathBuf, RawSongConfig>>,
}
#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "snake_case")]
pub enum SongOrder {
    Discs(Vec<ItemSelector>),
    Order(ItemSelector),
}
impl RawSongConfig {
    pub fn blank() -> Self {
        Self {
            songs: None,
            folders: None,
            this: None,
            order: None,
            set_fields: None,
            set_all: None,
            set: None,
            subconfigs: None,
        }
    }
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(untagged)]
pub enum ReferencableOperation {
    Reference(Rc<str>),
    Direct(MetadataOperation),
    Many(Vec<ReferencableOperation>),
}

#[derive(Deserialize, Serialize, Debug)]
pub struct RawAllSetter {
    pub names: ItemSelector,
    pub set: ReferencableOperation,
}

#[derive(Deserialize, Serialize, Debug)]
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
        metadata: &mut PendingMetadata,
        copy_source: &Metadata,
        library_config: &LibraryConfig,
    ) -> ApplyReport {
        let mut report = ApplyReport { errors: vec![] };
        if let Some(order) = &self.order {
            match order {
                OrderingSetter::Order { map, total, .. } => {
                    if let Some(track) = map.get(select) {
                        metadata.insert(MetadataField::Track, Ok(MetadataValue::Number(*track)));
                        metadata
                            .insert(MetadataField::TrackTotal, Ok(MetadataValue::Number(*total)));
                    }
                }
                OrderingSetter::Discs {
                    map, disc_total, ..
                } => {
                    if let Some(values) = map.get(select) {
                        metadata
                            .insert(MetadataField::Disc, Ok(MetadataValue::Number(values.disc)));
                        metadata.insert(
                            MetadataField::DiscTotal,
                            Ok(MetadataValue::Number(*disc_total)),
                        );
                        metadata.insert(
                            MetadataField::Track,
                            Ok(MetadataValue::Number(values.track)),
                        );
                        metadata.insert(
                            MetadataField::TrackTotal,
                            Ok(MetadataValue::Number(values.track_total)),
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
            if let Ok(select_path) = nice_path.strip_prefix(path) {
                let more = sub.apply(
                    nice_path,
                    select_path,
                    metadata,
                    copy_source,
                    library_config,
                );
                report.merge(more);
            }
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
    dry_run: bool,
) -> GetConfigsResults {
    let mut newly_loaded = vec![];
    let mut results = vec![];
    for (config_path, nice_folder) in relevant_config_paths(nice_path, library_config) {
        let loaded = config_cache
            .entry(config_path.clone())
            .or_insert_with_key(|x| {
                let config = load_config(x, nice_folder, library_config, &results, dry_run)
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
    others_so_far: &[LoadedConfig],
    dry_run: bool,
) -> Result<SongConfig, ConfigError> {
    match file_stuff::load_yaml::<RawSongConfigFile>(full_path) {
        Err(error) => Err(ConfigError::Yaml(error)),
        Ok(config) => {
            let resave = matches!(config, RawSongConfigFile::Reverse { .. });
            let raw = config.finalize(
                full_path.parent().unwrap_or(Path::new("")),
                library_config,
                others_so_far,
            )?;
            let save_result = if resave && !dry_run {
                file_stuff::save_yaml(full_path, &raw)
            } else {
                Ok(())
            };
            let result = library_config
                .resolve_config(raw, nice_folder)
                .map_err(ConfigError::Library);
            save_result?;
            result
        }
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
