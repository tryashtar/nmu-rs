use chrono::{DateTime, Utc};
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader, BufWriter, Read, Write},
    path::{Path, PathBuf},
    rc::Rc,
    time::SystemTime,
};

use crate::{
    art::{ArtRepo, RawArtRepo},
    file_stuff::{self, YamlError},
    lyrics::{RichLyrics, SyncedLyrics},
    metadata::{Metadata, MetadataField, MetadataValue, BLANK_VALUE},
    modifier::ValueModifier,
    song_config::{
        AllSetter, DiscSet, OrderingSetter, RawSongConfig, ReferencableOperation, SongConfig,
    },
    strategy::{FieldSelector, ItemSelector, MetadataOperation, MusicItemType, ValueGetter},
    tag_interop::{LyricsMetadata, SetValue},
};

#[derive(Deserialize, Serialize)]
pub struct RawLibraryConfig {
    pub library: PathBuf,
    pub reports: Vec<RawLibraryReport>,
    pub lyrics: Option<LyricsConfig>,
    pub config_folders: Vec<PathBuf>,
    pub custom_fields: Vec<String>,
    pub cache: Option<PathBuf>,
    pub art: Option<RawArtRepo>,
    pub named_strategies: HashMap<String, MetadataOperation>,
    pub find_replace: HashMap<String, String>,
    pub artist_separator: String,
    pub scan: Vec<ScanOptions>,
}

#[derive(Deserialize, Serialize)]
pub struct ScanOptions {
    #[serde(with = "serde_regex")]
    pub pattern: Regex,
    pub tags: ScanDecision,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ScanDecision {
    Ignore,
    #[serde(untagged)]
    Set(Rc<TagOptions>),
}
const fn default_ignore() -> TagSettings {
    TagSettings::Ignore
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum TagSettings {
    Ignore,
    Remove,
    #[serde(untagged)]
    Set {},
}

#[derive(Deserialize, Serialize)]
pub struct TagOptions {
    #[serde(default = "default_ignore")]
    pub flac: TagSettings,
    #[serde(default = "default_ignore")]
    pub id3: TagSettings,
    #[serde(default = "default_ignore")]
    pub ape: TagSettings,
}

#[derive(Deserialize, Serialize)]
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
const fn default_false() -> bool {
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
    fn load(source: RawLibraryReport, folder: &Path) -> Self {
        match source {
            RawLibraryReport::Fields {
                path,
                key,
                split,
                blanks,
            } => {
                let file_path = folder.join(path);
                let map = file_stuff::load_yaml(&file_path).unwrap_or_default();
                if split {
                    Self::SplitFields {
                        path: file_path,
                        key,
                        include_blanks: blanks,
                        map,
                    }
                } else {
                    Self::MergedFields {
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
                let map = file_stuff::load_yaml(&file_path).unwrap_or_default();
                Self::ItemData {
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
            MetadataValue::RegexMatches { .. } => None,
        }
    }
    fn val_to_strs(value: &MetadataValue) -> Vec<String> {
        match value {
            MetadataValue::Number(n) => vec![n.to_string()],
            MetadataValue::List(l) => l.clone(),
            MetadataValue::RegexMatches { .. } => vec![],
        }
    }
    fn is_blank(value: &MetadataValue) -> bool {
        match value {
            MetadataValue::List(l) => l.is_empty(),
            _ => false,
        }
    }
    pub fn record(
        &mut self,
        item_path: &Path,
        metadata: &Metadata,
        embedded_metadata: &Metadata,
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
                let meta = if *embedded {
                    embedded_metadata
                } else {
                    metadata
                };
                for (field, value) in meta {
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

#[derive(Deserialize, Serialize)]
pub struct LyricsConfig {
    folder: PathBuf,
    priority: Vec<LyricsType>,
    config: HashMap<LyricsType, LyricsReplaceMode>,
}
impl LyricsConfig {
    pub fn handle(&self, nice_path: &Path, source: &LyricsMetadata) -> LyricsMetadata {
        let mut result = LyricsMetadata::keep();
        let best = self
            .priority
            .iter()
            .find_map(|x| x.get(&self.folder, nice_path, source));
        for (lyric_type, replace) in &self.config {
            match replace {
                LyricsReplaceMode::Ignore => {}
                LyricsReplaceMode::Remove => {
                    lyric_type.set(&self.folder, nice_path, &mut result, None);
                }
                LyricsReplaceMode::Replace => {
                    lyric_type.set(&self.folder, nice_path, &mut result, best.as_ref());
                }
            }
        }
        result
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
        metadata: &mut LyricsMetadata,
        value: Option<&RichLyrics>,
    ) {
        match self {
            Self::RichEmbedded => {
                metadata.rich = match value {
                    None => SetValue::Remove,
                    Some(val) => SetValue::Replace(val.clone()),
                }
            }
            Self::RichFile => {
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
            Self::SyncedEmbedded => {
                metadata.synced = match value {
                    None => SetValue::Remove,
                    Some(val) => SetValue::Replace(val.clone().into()),
                }
            }
            Self::SyncedFile => {
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
            Self::SimpleEmbedded => {
                metadata.simple = match value {
                    None => SetValue::Remove,
                    Some(val) => SetValue::Replace(val.clone().into()),
                }
            }
            Self::SimpleFile => {
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
    fn get(
        &self,
        folder: &Path,
        nice_path: &Path,
        metadata: &LyricsMetadata,
    ) -> Option<RichLyrics> {
        match self {
            Self::RichEmbedded => match &metadata.rich {
                SetValue::Keep | SetValue::Remove => None,
                SetValue::Replace(val) => Some(val.clone()),
            },
            Self::RichFile => {
                let mut path = folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc.json");
                let path = PathBuf::from(path);
                let file = File::open(path).ok()?;
                let reader = BufReader::new(file);
                let lyrics: RichLyrics = serde_json::de::from_reader(reader).ok()?;
                Some(lyrics)
            }
            Self::SyncedEmbedded => match &metadata.synced {
                SetValue::Keep | SetValue::Remove => None,
                SetValue::Replace(val) => Some(val.clone().into()),
            },
            Self::SyncedFile => {
                let mut path = folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc");
                let path = PathBuf::from(path);
                let file = File::open(path).ok()?;
                let reader: BufReader<File> = BufReader::new(file);
                let lyrics =
                    SyncedLyrics::parse(reader.lines().filter_map(|x| x.ok()).collect()).ok()?;
                Some(lyrics.into())
            }
            Self::SimpleEmbedded => match &metadata.simple {
                SetValue::Keep | SetValue::Remove => None,
                SetValue::Replace(val) => Some(val.clone().into()),
            },
            Self::SimpleFile => {
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
    pub config_folders: Vec<PathBuf>,
    pub custom_fields: Vec<MetadataField>,
    pub date_cache: DateCache,
    pub art_repo: Option<ArtRepo>,
    pub named_strategies: HashMap<String, Rc<MetadataOperation>>,
    pub find_replace: HashMap<String, String>,
    pub artist_separator: String,
    pub scan: Vec<ScanOptions>,
}
impl LibraryConfig {
    pub fn new(folder: &Path, raw: RawLibraryConfig) -> Result<Self, LibraryError> {
        let result = Self {
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
            config_folders: raw
                .config_folders
                .into_iter()
                .map(|x| folder.join(x))
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
            scan: raw.scan,
        };
        for strat in result.named_strategies.values() {
            result.check_operation(strat)?;
        }
        Ok(result)
    }
    pub fn scan_settings(&self, full_path: &Path) -> Option<Rc<TagOptions>> {
        let relative = full_path
            .strip_prefix(&self.library_folder)
            .unwrap_or(full_path);
        for entry in &self.scan {
            if entry.pattern.is_match(&relative.to_string_lossy()) {
                return match &entry.tags {
                    ScanDecision::Ignore => None,
                    ScanDecision::Set(result) => Some(result.clone()),
                };
            }
        }
        None
    }
    pub fn resolve_config(
        &self,
        raw_config: RawSongConfig,
        nice_folder: &Path,
    ) -> Result<SongConfig, LibraryError> {
        let mut setters = vec![];
        if let Some(songs) = raw_config.songs {
            let resolved = self.resolve_operation(songs)?;
            setters.push(Rc::new(AllSetter {
                names: ItemSelector::All { recursive: true },
                must_be: Some(MusicItemType::Song),
                set: resolved,
            }));
        }
        if let Some(folders) = raw_config.folders {
            let resolved = self.resolve_operation(folders)?;
            setters.push(Rc::new(AllSetter {
                names: ItemSelector::All { recursive: true },
                must_be: Some(MusicItemType::Folder),
                set: resolved,
            }));
        }
        if let Some(this) = raw_config.this {
            let resolved = self.resolve_operation(this)?;
            setters.push(Rc::new(AllSetter {
                names: ItemSelector::This,
                must_be: None,
                set: resolved,
            }));
        }
        if let Some(set_fields) = raw_config.set_fields {
            for setter in set_fields {
                self.check_field(&setter.field)?;
                for (path, getter) in setter.set {
                    self.check_getter(&getter)?;
                    setters.push(Rc::new(AllSetter::new(
                        ItemSelector::Path(path),
                        MetadataOperation::Set(HashMap::from([(setter.field.clone(), getter)])),
                    )));
                }
            }
        }
        if let Some(set_all) = raw_config.set_all {
            for setter in set_all {
                let resolved = self.resolve_operation(setter.set)?;
                setters.push(Rc::new(AllSetter {
                    names: setter.names,
                    must_be: None,
                    set: resolved,
                }));
            }
        }
        if let Some(set) = raw_config.set {
            for (path, op) in set {
                let resolved = self.resolve_operation(op)?;
                setters.push(Rc::new(AllSetter {
                    names: ItemSelector::Path(path),
                    must_be: None,
                    set: resolved,
                }));
            }
        }
        let ordering = {
            if let Some(discs) = raw_config.discs {
                let mut map = HashMap::new();
                for (disc, sel) in &discs {
                    let matches = file_stuff::find_matches(sel, nice_folder, self);
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
                let map = file_stuff::find_matches(&order, nice_folder, self)
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
        let mut subconfigs = HashMap::new();
        if let Some(configs) = raw_config.subconfigs {
            for (path, config) in configs {
                let mut full_path = nice_folder.to_owned();
                full_path.push(path);
                let converted = self.resolve_config(config, &full_path)?;
                subconfigs.insert(full_path, converted);
            }
        }
        Ok(SongConfig {
            set: setters,
            order: ordering,
            subconfigs,
        })
    }
    fn check_fields(&self, selector: &FieldSelector) -> Result<(), LibraryError> {
        match selector {
            FieldSelector::All => Ok(()),
            FieldSelector::Single(field) => self.check_field(field),
            FieldSelector::Multiple(fields) => {
                for field in fields {
                    self.check_field(field)?;
                }
                Ok(())
            }
        }
    }
    fn check_field(&self, field: &MetadataField) -> Result<(), LibraryError> {
        match field {
            MetadataField::Custom(str) => {
                if self.custom_fields.contains(field) {
                    Ok(())
                } else {
                    Err(LibraryError::UnlistedCustomField(str.clone()))
                }
            }
            _ => Ok(()),
        }
    }
    fn check_operation(&self, operation: &MetadataOperation) -> Result<(), LibraryError> {
        match &operation {
            MetadataOperation::Blank { remove } => self.check_fields(remove),
            MetadataOperation::Keep { keep } => self.check_fields(keep),
            MetadataOperation::Shared { fields, set } => {
                self.check_fields(fields)?;
                self.check_getter(set)?;
                Ok(())
            }
            MetadataOperation::SharedModify { fields, modify } => {
                self.check_fields(fields)?;
                self.check_modifier(modify)?;
                Ok(())
            }
            MetadataOperation::Context { source, modify } => {
                self.check_getter(source)?;
                for (field, modify) in modify {
                    self.check_field(field)?;
                    self.check_modifier(modify)?;
                }
                Ok(())
            }
            MetadataOperation::Modify { modify } => {
                for (field, modify) in modify {
                    self.check_field(field)?;
                    self.check_modifier(modify)?;
                }
                Ok(())
            }
            MetadataOperation::Set(map) => {
                for (field, getter) in map {
                    self.check_field(field)?;
                    self.check_getter(getter)?;
                }
                Ok(())
            }
            MetadataOperation::Many(list) => {
                for op in list {
                    self.check_operation(op)?;
                }
                Ok(())
            }
        }
    }
    fn check_getter(&self, getter: &ValueGetter) -> Result<(), LibraryError> {
        match getter {
            ValueGetter::Direct(_) => Ok(()),
            ValueGetter::Copy { copy, modify, .. } => {
                self.check_field(copy)?;
                if let Some(modify) = modify {
                    self.check_modifier(modify)?;
                }
                Ok(())
            }
            ValueGetter::From { modify, .. } => {
                if let Some(modify) = modify {
                    self.check_modifier(modify)?;
                }
                Ok(())
            }
        }
    }
    fn check_modifier(&self, modifier: &ValueModifier) -> Result<(), LibraryError> {
        match modifier {
            ValueModifier::Prepend { prepend, .. } => self.check_getter(prepend),
            ValueModifier::Append { append, .. } => self.check_getter(append),
            ValueModifier::InsertBefore { insert, .. }
            | ValueModifier::InsertAfter { insert, .. } => self.check_getter(insert),
            ValueModifier::Join { join } => self.check_getter(join),
            ValueModifier::Split { .. }
            | ValueModifier::Regex { .. }
            | ValueModifier::Replace { .. }
            | ValueModifier::Take { .. } => Ok(()),
            ValueModifier::Multiple(list) => {
                for item in list {
                    self.check_modifier(item)?;
                }
                Ok(())
            }
        }
    }
    fn resolve_operation(
        &self,
        operation: ReferencableOperation,
    ) -> Result<Rc<MetadataOperation>, LibraryError> {
        match operation {
            ReferencableOperation::Direct(direct) => {
                self.check_operation(&direct)?;
                Ok(Rc::new(direct))
            }
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

#[derive(thiserror::Error, Debug)]
pub enum LibraryError {
    MissingNamedStrategy(String),
    UnlistedCustomField(String),
}
impl std::fmt::Display for LibraryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingNamedStrategy(str) => {
                write!(f, "No named strategy with name '{str}'")
            }
            Self::UnlistedCustomField(str) => {
                write!(f, "No field with name '{str}'")
            }
        }
    }
}

pub struct DateCache {
    path: Option<PathBuf>,
    cache: HashMap<PathBuf, DateTime<Utc>>,
    updated: HashSet<PathBuf>,
}
impl DateCache {
    pub fn new(path: Option<PathBuf>) -> Self {
        path.map_or_else(
            || Self {
                path: None,
                cache: HashMap::new(),
                updated: HashSet::new(),
            },
            |path| Self {
                path: Some(path.clone()),
                cache: file_stuff::load_yaml(&path).unwrap_or_default(),
                updated: HashSet::new(),
            },
        )
    }
    pub fn mark_updated(&mut self, path: PathBuf) {
        self.updated.insert(path);
    }
    pub fn remove(&mut self, path: &Path) {
        self.updated.remove(path);
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
        self.cache.get(path).map_or(true, |cache_time| {
            std::fs::metadata(path)
                .and_then(|x| x.modified())
                .map_or(true, |file_time| {
                    *cache_time < std::convert::Into::<DateTime<Utc>>::into(file_time)
                })
        })
    }
}
