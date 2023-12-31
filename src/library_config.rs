use chrono::{DateTime, Utc};
use id3::frame::Lyrics;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs::{self, File};
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::SystemTime;
use thiserror::Error;

use crate::art::{ArtRepo, RawArtRepo};
use crate::file_stuff::{self, load_yaml, YamlError};
use crate::lyrics::{RichLyrics, SyncedLyrics};
use crate::metadata::{
    FinalMetadata, Metadata, MetadataField, MetadataValue, SetValue, BLANK_VALUE,
};
use crate::song_config::{
    AllSetter, DiscSet, OrderingSetter, RawSongConfig, ReferencableOperation, SongConfig,
};
use crate::strategy::{FieldSelector, ItemSelector, MetadataOperation, MusicItemType};

#[derive(Deserialize)]
pub struct RawLibraryConfig {
    library: PathBuf,
    reports: Vec<RawLibraryReport>,
    lyrics: Option<LyricsConfig>,
    logs: Option<PathBuf>,
    config_folders: Vec<PathBuf>,
    extensions: HashSet<String>,
    custom_fields: Vec<String>,
    cache: Option<PathBuf>,
    art: Option<RawArtRepo>,
    named_strategies: HashMap<String, MetadataOperation>,
    find_replace: HashMap<String, String>,
    artist_separator: String,
}

#[derive(Deserialize)]
#[serde(untagged)]
pub enum RawLibraryReport {
    Fields {
        path: PathBuf,
        key: MetadataField,
        #[serde(default = "default_false")]
        split: bool,
        #[serde(default = "default_false")]
        blanks: bool,
    },
    Items {
        path: PathBuf,
        values: FieldSelector,
        #[serde(default = "default_false")]
        embedded: bool,
        #[serde(default = "default_false")]
        blanks: bool,
    },
}
fn default_false() -> bool {
    false
}

pub enum LibraryReport {
    SplitFields {
        path: PathBuf,
        key: MetadataField,
        include_blanks: bool,
        map: BTreeMap<Option<String>, PathList>,
    },
    MergedFields {
        path: PathBuf,
        key: MetadataField,
        include_blanks: bool,
        map: BTreeMap<Option<String>, PathList>,
    },
    ItemData {
        path: PathBuf,
        values: FieldSelector,
        embedded: bool,
        include_blanks: bool,
        map: BTreeMap<PathBuf, BTreeMap<MetadataField, Option<MetadataValue>>>,
    },
}

#[derive(Deserialize, Default)]
pub struct PathList(#[serde(deserialize_with = "crate::util::path_or_seq_path")] BTreeSet<PathBuf>);
impl Serialize for PathList {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self.0.first() {
            Some(item) if self.0.len() == 1 => item.serialize(serializer),
            _ => self.0.serialize(serializer),
        }
    }
}

impl LibraryReport {
    pub fn save(&self) -> Result<(), YamlError> {
        match self {
            Self::SplitFields { path, map, .. } => {
                let file = File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, map)?;
                Ok(())
            }
            Self::MergedFields { path, map, .. } => {
                let file = File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, map)?;
                Ok(())
            }
            Self::ItemData { path, map, .. } => {
                let file = File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, map)?;
                Ok(())
            }
        }
    }
    fn load(source: RawLibraryReport, folder: &Path) -> LibraryReport {
        match source {
            RawLibraryReport::Fields {
                path,
                key,
                split,
                blanks,
            } => {
                let file_path = folder.join(path);
                let map = load_yaml(&file_path).unwrap_or_default();
                if split {
                    LibraryReport::SplitFields {
                        path: file_path,
                        key,
                        include_blanks: blanks,
                        map,
                    }
                } else {
                    LibraryReport::MergedFields {
                        path: file_path,
                        key,
                        include_blanks: blanks,
                        map,
                    }
                }
            }
            RawLibraryReport::Items {
                path,
                values,
                embedded,
                blanks,
            } => {
                let file_path = folder.join(path);
                let map = load_yaml(&file_path).unwrap_or_default();
                LibraryReport::ItemData {
                    path: file_path,
                    values,
                    embedded,
                    include_blanks: blanks,
                    map,
                }
            }
        }
    }
    fn val_to_str(value: &MetadataValue, sep: &str) -> Option<String> {
        match value {
            MetadataValue::Number(n) => Some(n.to_string()),
            MetadataValue::List(l) if l.is_empty() => None,
            MetadataValue::List(l) => Some(l.join(sep)),
        }
    }
    fn val_to_strs(value: &MetadataValue) -> Vec<String> {
        match value {
            MetadataValue::Number(n) => vec![n.to_string()],
            MetadataValue::List(l) => l.clone(),
        }
    }
    fn is_blank(value: &MetadataValue) -> bool {
        match value {
            MetadataValue::Number(_) => false,
            MetadataValue::List(l) => l.is_empty(),
        }
    }
    pub fn record(
        &mut self,
        item_path: &Path,
        metadata: &Metadata,
        embedded_metadata: Option<&Metadata>,
        sep: &str,
    ) {
        match self {
            Self::MergedFields {
                key,
                include_blanks,
                map,
                ..
            } => {
                for list in map.values_mut() {
                    list.0.remove(item_path);
                }
                let value = metadata.get(key).unwrap_or(&BLANK_VALUE);
                if *include_blanks || !Self::is_blank(value) {
                    let list = map.entry(Self::val_to_str(value, sep)).or_default();
                    list.0.insert(item_path.to_owned());
                }
            }
            Self::SplitFields {
                key,
                include_blanks,
                map,
                ..
            } => {
                let value = metadata.get(key).unwrap_or(&BLANK_VALUE);
                if *include_blanks && Self::is_blank(value) {
                    let list = map.entry(None).or_default();
                    list.0.insert(item_path.to_owned());
                } else {
                    for entry in Self::val_to_strs(value) {
                        let list = map.entry(Some(entry)).or_default();
                        list.0.insert(item_path.to_owned());
                    }
                }
            }
            Self::ItemData {
                values,
                embedded,
                include_blanks,
                map,
                ..
            } => {
                let mut results = BTreeMap::new();
                if let Some(save) = {
                    if *embedded {
                        embedded_metadata
                    } else {
                        Some(metadata)
                    }
                } {
                    for (field, value) in save {
                        if values.is_match(field) && (*include_blanks || !Self::is_blank(value)) {
                            results.insert(
                                field.clone(),
                                match value {
                                    MetadataValue::List(list) if list.is_empty() => None,
                                    _ => Some(value.clone()),
                                },
                            );
                        }
                    }
                    if !results.is_empty() {
                        map.insert(item_path.into(), results);
                    }
                }
            }
        }
    }
}

#[derive(Deserialize, Serialize)]
pub struct LyricsConfig {
    folder: PathBuf,
    priority: Vec<LyricsType>,
    config: HashMap<LyricsType, LyricsReplaceMode>,
}
impl LyricsConfig {
    pub fn handle(&self, nice_path: &Path, source: &FinalMetadata, dest: &mut FinalMetadata) {
        let best = self
            .priority
            .iter()
            .find_map(|x| x.get(&self.folder, nice_path, source));
        for (lyric_type, replace) in &self.config {
            match replace {
                LyricsReplaceMode::Ignore => {}
                LyricsReplaceMode::Remove => {
                    lyric_type.set(&self.folder, nice_path, dest, None);
                }
                LyricsReplaceMode::Replace => {
                    lyric_type.set(&self.folder, nice_path, dest, best.as_ref());
                }
            }
        }
    }
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
pub enum LyricsType {
    RichEmbedded,
    RichFile,
    SyncedEmbedded,
    SyncedFile,
    SimpleEmbedded,
    SimpleFile,
}
impl LyricsType {
    fn set(
        &self,
        folder: &Path,
        nice_path: &Path,
        metadata: &mut FinalMetadata,
        value: Option<&RichLyrics>,
    ) {
        match self {
            LyricsType::RichEmbedded => metadata.rich_lyrics = SetValue::Set(value.cloned()),
            LyricsType::RichFile => {
                let mut path = folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc.json");
                let path = PathBuf::from(path);
                match value {
                    None => {
                        std::fs::remove_file(path);
                    }
                    Some(val) => {
                        if let Ok(file) = std::fs::File::create(path) {
                            let mut writer = std::io::BufWriter::new(file);
                            if let Ok(str) = serde_json::ser::to_string_pretty(val) {
                                write!(writer, "{}", str);
                            }
                        }
                    }
                }
            }
            LyricsType::SyncedEmbedded => {
                metadata.synced_lyrics = SetValue::Set(value.cloned().map(|x| x.into()))
            }
            LyricsType::SyncedFile => {
                let mut path = folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc");
                let path = PathBuf::from(path);
                match value {
                    None => {
                        std::fs::remove_file(path);
                    }
                    Some(val) => {
                        if let Ok(file) = std::fs::File::create(path) {
                            let mut writer = std::io::BufWriter::new(file);
                            for line in std::convert::Into::<SyncedLyrics>::into(val.clone()).save()
                            {
                                writeln!(writer, "{}", line);
                            }
                        }
                    }
                }
            }
            LyricsType::SimpleEmbedded => {
                metadata.simple_lyrics = SetValue::Set(value.cloned().map(|x| x.into()))
            }
            LyricsType::SimpleFile => {
                let mut path = folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc.txt");
                let path = PathBuf::from(path);
                match value {
                    None => {
                        std::fs::remove_file(path);
                    }
                    Some(val) => {
                        if let Ok(file) = std::fs::File::create(path) {
                            let mut writer = std::io::BufWriter::new(file);
                            let str = std::convert::Into::<String>::into(val.clone());
                            write!(writer, "{}", str);
                        }
                    }
                }
            }
        }
    }
    fn get(&self, folder: &Path, nice_path: &Path, metadata: &FinalMetadata) -> Option<RichLyrics> {
        match self {
            LyricsType::RichEmbedded => match &metadata.rich_lyrics {
                SetValue::Skip => None,
                SetValue::Set(val) => val.as_ref().cloned(),
            },
            LyricsType::RichFile => {
                let mut path = folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc.json");
                let path = PathBuf::from(path);
                let file = File::open(path).ok()?;
                let reader = BufReader::new(file);
                let lyrics: RichLyrics = serde_json::de::from_reader(reader).ok()?;
                Some(lyrics)
            }
            LyricsType::SyncedEmbedded => match &metadata.synced_lyrics {
                SetValue::Skip => None,
                SetValue::Set(val) => val.as_ref().cloned().map(|x| x.into()),
            },
            LyricsType::SyncedFile => {
                let mut path = folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc");
                let path = PathBuf::from(path);
                let file = File::open(path).ok()?;
                let reader: BufReader<File> = BufReader::new(file);
                let lyrics =
                    SyncedLyrics::parse(reader.lines().filter_map(|x| x.ok()).collect()).ok()?;
                Some(lyrics.into())
            }
            LyricsType::SimpleEmbedded => match &metadata.simple_lyrics {
                SetValue::Skip => None,
                SetValue::Set(val) => val.as_ref().cloned().map(|x| x.into()),
            },
            LyricsType::SimpleFile => {
                let mut path = folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc.txt");
                let path = PathBuf::from(path);
                let file = File::open(path).ok()?;
                let mut reader: BufReader<File> = BufReader::new(file);
                let mut buf = String::new();
                reader.read_to_string(&mut buf).ok()?;
                Some(buf.into())
            }
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum LyricsReplaceMode {
    Ignore,
    Remove,
    Replace,
}

pub struct LibraryConfig {
    pub library_folder: PathBuf,
    pub reports: Vec<LibraryReport>,
    pub lyrics: Option<LyricsConfig>,
    pub log_folder: Option<PathBuf>,
    pub config_folders: Vec<PathBuf>,
    pub song_extensions: HashSet<String>,
    pub custom_fields: Vec<MetadataField>,
    pub date_cache: DateCache,
    pub art_repo: Option<ArtRepo>,
    pub named_strategies: HashMap<String, Rc<MetadataOperation>>,
    pub find_replace: HashMap<String, String>,
    pub artist_separator: String,
}
impl LibraryConfig {
    pub fn new(folder: &Path, raw: RawLibraryConfig) -> Self {
        Self {
            library_folder: folder.join(raw.library),
            reports: raw
                .reports
                .into_iter()
                .map(|x| LibraryReport::load(x, folder))
                .collect(),
            lyrics: raw.lyrics.map(|x| LyricsConfig {
                folder: folder.join(x.folder),
                priority: x.priority,
                config: x.config,
            }),
            log_folder: raw.logs.map(|x| folder.join(x)),
            config_folders: raw
                .config_folders
                .into_iter()
                .map(|x| folder.join(x))
                .collect(),
            song_extensions: raw
                .extensions
                .into_iter()
                .map(|x| match x.strip_prefix('.') {
                    Some(stripped) => stripped.to_owned(),
                    None => x,
                })
                .collect(),
            custom_fields: raw
                .custom_fields
                .into_iter()
                .map(MetadataField::Custom)
                .collect(),
            date_cache: DateCache::new(raw.cache.map(|x| folder.join(x))),
            art_repo: raw.art.map(|x| ArtRepo::new(folder, x)),
            named_strategies: raw
                .named_strategies
                .into_iter()
                .map(|(k, v)| (k, Rc::new(v)))
                .collect(),
            find_replace: raw.find_replace,
            artist_separator: raw.artist_separator,
        }
    }
    pub fn resolve_config(
        &self,
        raw_config: RawSongConfig,
        folder: &Path,
    ) -> Result<SongConfig, LibraryError> {
        let songs = raw_config
            .songs
            .map(|x| {
                self.resolve_operation(x).map(|ops| AllSetter {
                    names: ItemSelector::All { recursive: true },
                    must_be: Some(MusicItemType::Song),
                    set: ops,
                })
            })
            .transpose()?;
        let folders = raw_config
            .folders
            .map(|x| {
                self.resolve_operation(x).map(|ops| AllSetter {
                    names: ItemSelector::All { recursive: true },
                    must_be: Some(MusicItemType::Folder),
                    set: ops,
                })
            })
            .transpose()?;
        let this = raw_config
            .this
            .map(|x| {
                self.resolve_operation(x).map(|ops| AllSetter {
                    names: ItemSelector::This,
                    must_be: None,
                    set: ops,
                })
            })
            .transpose()?;
        let set_fields = raw_config
            .set_fields
            .map(|sets| {
                sets.into_iter()
                    .flat_map(|setter| {
                        setter
                            .set
                            .into_iter()
                            .map(|(path, getter)| {
                                AllSetter::new(
                                    ItemSelector::Path(path),
                                    MetadataOperation::Set(HashMap::from([(
                                        setter.field.clone(),
                                        getter,
                                    )])),
                                )
                            })
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let set_all = raw_config
            .set_all
            .map(|x| {
                x.into_iter()
                    .map(|y| {
                        self.resolve_operation(y.set).map(|ops| AllSetter {
                            names: y.names,
                            must_be: None,
                            set: ops,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .unwrap_or_default();
        let set = raw_config
            .set
            .map(|map| {
                map.into_iter()
                    .map(|(path, ops)| {
                        self.resolve_operation(ops).map(|ops| AllSetter {
                            names: ItemSelector::Path(path),
                            must_be: None,
                            set: ops,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .unwrap_or(vec![]);
        let merged = songs
            .into_iter()
            .chain(folders)
            .chain(this)
            .chain(set_fields)
            .chain(set_all)
            .chain(set)
            .collect::<Vec<_>>();
        let ordering = {
            if let Some(discs) = raw_config.discs {
                let mut map = HashMap::new();
                for (disc, sel) in &discs {
                    let matches = file_stuff::find_matches(sel, folder, self);
                    let track_total = matches.len();
                    for (track, path) in matches.into_iter().enumerate() {
                        map.insert(
                            PathBuf::from(path),
                            DiscSet {
                                disc: *disc,
                                track: (track + 1) as u32,
                                track_total: track_total as u32,
                            },
                        );
                    }
                }
                Some(OrderingSetter::Discs {
                    map,
                    disc_total: *(discs.keys().max().unwrap_or(&1)),
                    original_selectors: discs.into_values().collect(),
                })
            } else if let Some(order) = raw_config.order {
                let map = file_stuff::find_matches(&order, folder, self)
                    .into_iter()
                    .enumerate()
                    .map(|(track, path)| (PathBuf::from(path), (track + 1) as u32))
                    .collect::<HashMap<_, _>>();
                let total = map.len() as u32;
                Some(OrderingSetter::Order {
                    map,
                    total,
                    original_selector: order,
                })
            } else {
                None
            }
        };
        Ok(SongConfig {
            set: merged,
            order: ordering,
        })
    }
    fn resolve_operation(
        &self,
        operation: ReferencableOperation,
    ) -> Result<Rc<MetadataOperation>, LibraryError> {
        match operation {
            ReferencableOperation::Direct(direct) => Ok(Rc::new(direct)),
            ReferencableOperation::Reference(reference) => self
                .named_strategies
                .get(&reference)
                .cloned()
                .ok_or(LibraryError::MissingNamedStrategy(reference)),
            ReferencableOperation::Many(many) => many
                .into_iter()
                .map(|x| self.resolve_operation(x))
                .collect::<Result<Vec<_>, _>>()
                .map(|x| Rc::new(MetadataOperation::Many(x))),
        }
    }
}

#[derive(Error, Debug)]
#[error("{0}")]
pub enum LibraryError {
    MissingNamedStrategy(String),
}

pub struct DateCache {
    path: Option<PathBuf>,
    cache: HashMap<PathBuf, DateTime<Utc>>,
    updated: HashSet<PathBuf>,
}
impl DateCache {
    pub fn new(path: Option<PathBuf>) -> Self {
        match path {
            None => Self {
                path: None,
                cache: HashMap::new(),
                updated: HashSet::new(),
            },
            Some(path) => Self {
                path: Some(path.clone()),
                cache: match file_stuff::load_yaml(&path) {
                    Err(_) => HashMap::new(),
                    Ok(map) => map,
                },
                updated: HashSet::new(),
            },
        }
    }
    pub fn mark_updated(&mut self, path: PathBuf) {
        self.updated.insert(path);
    }
    pub fn save(&mut self) -> Result<(), YamlError> {
        match &self.path {
            None => Ok(()),
            Some(path) => {
                let time = SystemTime::now().into();
                for entry in self.updated.drain() {
                    self.cache.insert(entry, time);
                }
                let ordered: BTreeMap<_, _> = self.cache.iter().collect();
                let file = File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, &ordered)?;
                Ok(())
            }
        }
    }
    pub fn changed_recently(&self, path: &Path) -> bool {
        match self.cache.get(path) {
            None => true,
            Some(cache_time) => match fs::metadata(path).and_then(|x| x.modified()) {
                Err(_) => true,
                Ok(file_time) => *cache_time < std::convert::Into::<DateTime<Utc>>::into(file_time),
            },
        }
    }
}
