use chrono::{DateTime, Utc};
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    io::{BufRead, BufReader, BufWriter, Read, Write},
    path::{Path, PathBuf},
    rc::Rc,
    time::SystemTime,
};

use crate::{
    art::{ArtRepo, RawArtRepo},
    file_stuff::{self, YamlError},
    lyrics::{self, RichLyrics, SomeLyrics, SyncedLyrics},
    metadata::{self, Metadata, MetadataField, MetadataValue},
    modifier::ValueModifier,
    song_config::{
        AllSetter, DiscSet, OrderingSetter, RawSongConfig, ReferencableOperation, SongConfig,
        SongOrder,
    },
    strategy::{FieldSelector, ItemSelector, MetadataOperation, MusicItemType, ValueGetter},
    tag_interop::{GetLyrics, GetLyricsError},
};

#[derive(Deserialize, Serialize)]
pub struct RawLibraryConfig {
    pub library: PathBuf,
    pub reports: Vec<RawLibraryReport>,
    pub lyrics: Option<LyricsConfig>,
    pub config_folders: Vec<PathBuf>,
    pub custom_fields: Vec<Rc<str>>,
    pub cache: Option<PathBuf>,
    pub art: Option<RawArtRepo>,
    pub named_strategies: HashMap<Rc<str>, MetadataOperation>,
    pub find_replace: HashMap<String, String>,
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
    pub fn clean(&mut self, valid_names: &HashSet<PathBuf>) {
        // when extract_if is stabilized, use it here instead of retain, so we can print out removed entries
        match self {
            LibraryReport::SplitFields { map, .. } | LibraryReport::MergedFields { map, .. } => {
                for entry in map.values_mut() {
                    entry.0.retain(|x| valid_names.contains(x));
                }
                map.retain(|_, v| !v.0.is_empty());
            }
            LibraryReport::ItemData { map, .. } => {
                map.retain(|k, _| valid_names.contains(k));
            }
        }
    }
    pub fn save(&self) -> Result<(), YamlError> {
        match self {
            Self::SplitFields { path, map, .. } => {
                let file = std::fs::File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, map)?;
                Ok(())
            }
            Self::MergedFields { path, map, .. } => {
                let file = std::fs::File::create(path)?;
                let writer = BufWriter::new(file);
                serde_yaml::to_writer(writer, map)?;
                Ok(())
            }
            Self::ItemData { path, map, .. } => {
                let file = std::fs::File::create(path)?;
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
                blanks,
            } => {
                let file_path = folder.join(path);
                let map = file_stuff::load_yaml(&file_path).unwrap_or_default();
                Self::ItemData {
                    path: file_path,
                    values,
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
    pub fn record(&mut self, item_path: &Path, metadata: &Metadata) {
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
                let value = metadata.get(key).unwrap_or(&metadata::BLANK_VALUE);
                if *include_blanks || !Self::is_blank(value) {
                    let list = map.entry(Self::val_to_str(value, "/")).or_default();
                    list.0.insert(item_path.to_owned());
                }
            }
            Self::SplitFields {
                key,
                include_blanks,
                map,
                ..
            } => {
                let value = metadata.get(key).unwrap_or(&metadata::BLANK_VALUE);
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
                include_blanks,
                map,
                ..
            } => {
                let mut results = BTreeMap::new();
                for (field, value) in metadata {
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
    pub folder: PathBuf,
    pub priority: Vec<LyricsType>,
    pub config: HashMap<LyricsType, LyricsReplaceMode>,
}
impl LyricsConfig {
    pub fn get_best(
        &self,
        nice_path: &Path,
        source: &impl GetLyrics,
    ) -> Result<SomeLyrics, GetLyricsError> {
        for lyrics in &self.priority {
            let result = self.get(*lyrics, nice_path, source);
            match result {
                Ok(lyrics) => return Ok(lyrics),
                Err(GetLyricsError::NotEmbedded) => {}
                Err(GetLyricsError::Io(io)) if io.kind() == std::io::ErrorKind::NotFound => {}
                Err(err) => return Err(err),
            };
        }
        Err(GetLyricsError::NotEmbedded)
    }
    pub fn get(
        &self,
        lyrics: LyricsType,
        nice_path: &Path,
        source: &impl GetLyrics,
    ) -> Result<SomeLyrics, GetLyricsError> {
        match lyrics {
            LyricsType::RichEmbedded => source.get_rich_lyrics().map(SomeLyrics::Rich),
            LyricsType::SyncedEmbedded => source.get_synced_lyrics().map(SomeLyrics::Synced),
            LyricsType::SimpleEmbedded => source.get_simple_lyrics().map(SomeLyrics::Simple),
            LyricsType::RichFile => {
                let mut path = self.folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc.json");
                let path = PathBuf::from(path);
                Self::read_rich(&path).map(SomeLyrics::Rich)
            }
            LyricsType::SyncedFile => {
                let mut path = self.folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc");
                let path = PathBuf::from(path);
                Self::read_synced(&path).map(SomeLyrics::Synced)
            }
            LyricsType::SimpleFile => {
                let mut path = self.folder.join(nice_path).to_string_lossy().into_owned();
                path.push_str(".lrc.txt");
                let path = PathBuf::from(path);
                Self::read_simple(&path).map(SomeLyrics::Simple)
            }
        }
    }

    fn read_rich(path: &Path) -> Result<RichLyrics, GetLyricsError> {
        let file = std::fs::File::open(path)?;
        let reader = BufReader::new(file);
        let lyrics: RichLyrics = serde_json::de::from_reader(reader)?;
        Ok(lyrics)
    }

    fn read_synced(path: &Path) -> Result<SyncedLyrics, GetLyricsError> {
        let file = std::fs::File::open(path)?;
        let reader = BufReader::new(file);
        let lines = reader.lines().collect::<Result<Vec<_>, _>>()?;
        let lyrics = SyncedLyrics::parse(lines)?;
        Ok(lyrics)
    }

    fn read_simple(path: &Path) -> Result<String, GetLyricsError> {
        let file = std::fs::File::open(path)?;
        let mut reader = BufReader::new(file);
        let mut buf = String::new();
        reader.read_to_string(&mut buf)?;
        Ok(buf)
    }

    fn write_rich(path: &Path, lyrics: &RichLyrics) -> Result<(), std::io::Error> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let file = std::fs::File::create(path)?;
        let writer = std::io::BufWriter::new(file);
        serde_json::to_writer_pretty(writer, lyrics)?;
        Ok(())
    }

    fn write_synced(path: &Path, lyrics: &SyncedLyrics) -> Result<(), std::io::Error> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let file = std::fs::File::create(path)?;
        let mut writer = std::io::LineWriter::new(file);
        writer.write_all(lyrics.save().join("\n").as_bytes())?;
        Ok(())
    }

    fn write_simple(path: &Path, lyrics: &str) -> Result<(), std::io::Error> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let file = std::fs::File::create(path)?;
        let mut writer = std::io::LineWriter::new(file);
        writer.write_all(lyrics.as_bytes())?;
        Ok(())
    }

    pub fn write(&self, nice_path: &Path, lyrics: &SomeLyrics) -> SetLyricsReport {
        let mut report = SetLyricsReport {
            results: HashMap::new(),
        };
        for (lyrics_type, mode) in &self.config {
            match lyrics_type {
                LyricsType::RichFile => {
                    let mut path = self.folder.join(nice_path).to_string_lossy().into_owned();
                    path.push_str(".lrc.json");
                    let path = PathBuf::from(path);
                    match mode {
                        LyricsReplaceMode::Ignore => {}
                        LyricsReplaceMode::Remove => {
                            let existing = Self::read_rich(&path);
                            match std::fs::remove_file(&path) {
                                Ok(()) => {
                                    report.results.insert(
                                        *lyrics_type,
                                        SetLyricsResult::Removed(existing.map(SomeLyrics::Rich)),
                                    );
                                }
                                Err(err) => {
                                    if err.kind() != std::io::ErrorKind::NotFound {
                                        report
                                            .results
                                            .insert(*lyrics_type, SetLyricsResult::Failed(err));
                                    }
                                }
                            }
                        }
                        LyricsReplaceMode::Replace => {
                            let existing = Self::read_rich(&path).map(SomeLyrics::Rich);
                            match Self::write_rich(&path, &lyrics.clone().into_rich()) {
                                Ok(()) => {
                                    if !lyrics::matches(lyrics, existing.as_ref()) {
                                        report.results.insert(
                                            *lyrics_type,
                                            SetLyricsResult::Replaced(existing),
                                        );
                                    }
                                }
                                Err(err) => {
                                    report
                                        .results
                                        .insert(*lyrics_type, SetLyricsResult::Failed(err));
                                }
                            }
                        }
                    }
                }
                LyricsType::SyncedFile => {
                    let mut path = self.folder.join(nice_path).to_string_lossy().into_owned();
                    path.push_str(".lrc");
                    let path = PathBuf::from(path);
                    match mode {
                        LyricsReplaceMode::Ignore => {}
                        LyricsReplaceMode::Remove => {
                            let existing = Self::read_synced(&path);
                            match std::fs::remove_file(&path) {
                                Ok(()) => {
                                    report.results.insert(
                                        *lyrics_type,
                                        SetLyricsResult::Removed(existing.map(SomeLyrics::Synced)),
                                    );
                                }
                                Err(err) => {
                                    if err.kind() != std::io::ErrorKind::NotFound {
                                        report
                                            .results
                                            .insert(*lyrics_type, SetLyricsResult::Failed(err));
                                    }
                                }
                            }
                        }
                        LyricsReplaceMode::Replace => {
                            let existing = Self::read_synced(&path).map(SomeLyrics::Synced);
                            match Self::write_synced(&path, &lyrics.clone().into_synced()) {
                                Ok(()) => {
                                    if !lyrics::matches(lyrics, existing.as_ref()) {
                                        report.results.insert(
                                            *lyrics_type,
                                            SetLyricsResult::Replaced(existing),
                                        );
                                    }
                                }
                                Err(err) => {
                                    report
                                        .results
                                        .insert(*lyrics_type, SetLyricsResult::Failed(err));
                                }
                            }
                        }
                    }
                }
                LyricsType::SimpleFile => {
                    let mut path = self.folder.join(nice_path).to_string_lossy().into_owned();
                    path.push_str(".lrc.txt");
                    let path = PathBuf::from(path);
                    match mode {
                        LyricsReplaceMode::Ignore => {}
                        LyricsReplaceMode::Remove => {
                            let existing = Self::read_simple(&path);
                            match std::fs::remove_file(&path) {
                                Ok(()) => {
                                    report.results.insert(
                                        *lyrics_type,
                                        SetLyricsResult::Removed(existing.map(SomeLyrics::Simple)),
                                    );
                                }
                                Err(err) => {
                                    if err.kind() != std::io::ErrorKind::NotFound {
                                        report
                                            .results
                                            .insert(*lyrics_type, SetLyricsResult::Failed(err));
                                    }
                                }
                            }
                        }
                        LyricsReplaceMode::Replace => {
                            let existing = Self::read_simple(&path).map(SomeLyrics::Simple);
                            match Self::write_simple(&path, &lyrics.clone().into_simple()) {
                                Ok(()) => {
                                    if !lyrics::matches(lyrics, existing.as_ref()) {
                                        report.results.insert(
                                            *lyrics_type,
                                            SetLyricsResult::Replaced(existing),
                                        );
                                    }
                                }
                                Err(err) => {
                                    report
                                        .results
                                        .insert(*lyrics_type, SetLyricsResult::Failed(err));
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        report
    }

    pub fn set(&self, lyrics: &SomeLyrics, tag: &mut impl GetLyrics) -> SetLyricsReport {
        let mut report = SetLyricsReport {
            results: HashMap::new(),
        };
        for (lyrics_type, mode) in &self.config {
            match lyrics_type {
                LyricsType::RichEmbedded => match mode {
                    LyricsReplaceMode::Ignore => {}
                    LyricsReplaceMode::Remove => {
                        let existing = tag.remove_rich_lyrics();
                        match existing {
                            Err(GetLyricsError::NotEmbedded) => {}
                            _ => {
                                report.results.insert(
                                    *lyrics_type,
                                    SetLyricsResult::Removed(existing.map(SomeLyrics::Rich)),
                                );
                            }
                        }
                    }
                    LyricsReplaceMode::Replace => {
                        let existing = tag
                            .set_rich_lyrics(lyrics.clone().into_rich())
                            .map(SomeLyrics::Rich);
                        if !lyrics::matches(lyrics, existing.as_ref()) {
                            report
                                .results
                                .insert(*lyrics_type, SetLyricsResult::Replaced(existing));
                        }
                    }
                },
                LyricsType::SyncedEmbedded => match mode {
                    LyricsReplaceMode::Ignore => {}
                    LyricsReplaceMode::Remove => {
                        let existing = tag.remove_synced_lyrics();
                        match existing {
                            Err(GetLyricsError::NotEmbedded) => {}
                            _ => {
                                report.results.insert(
                                    *lyrics_type,
                                    SetLyricsResult::Removed(existing.map(SomeLyrics::Synced)),
                                );
                            }
                        }
                    }
                    LyricsReplaceMode::Replace => {
                        let existing = tag
                            .set_synced_lyrics(lyrics.clone().into_synced())
                            .map(SomeLyrics::Synced);
                        if !lyrics::matches(lyrics, existing.as_ref()) {
                            report
                                .results
                                .insert(*lyrics_type, SetLyricsResult::Replaced(existing));
                        }
                    }
                },
                LyricsType::SimpleEmbedded => match mode {
                    LyricsReplaceMode::Ignore => {}
                    LyricsReplaceMode::Remove => {
                        let existing = tag.remove_simple_lyrics();
                        match existing {
                            Err(GetLyricsError::NotEmbedded) => {}
                            _ => {
                                report.results.insert(
                                    *lyrics_type,
                                    SetLyricsResult::Removed(existing.map(SomeLyrics::Simple)),
                                );
                            }
                        }
                    }
                    LyricsReplaceMode::Replace => {
                        let existing = tag
                            .set_simple_lyrics(lyrics.clone().into_simple())
                            .map(SomeLyrics::Simple);
                        if !lyrics::matches(lyrics, existing.as_ref()) {
                            report
                                .results
                                .insert(*lyrics_type, SetLyricsResult::Replaced(existing));
                        }
                    }
                },
                _ => {}
            }
        }
        report
    }
}

pub struct SetLyricsReport {
    pub results: HashMap<LyricsType, SetLyricsResult>,
}

pub enum SetLyricsResult {
    Replaced(Result<SomeLyrics, GetLyricsError>),
    Removed(Result<SomeLyrics, GetLyricsError>),
    Failed(std::io::Error),
}

#[derive(Deserialize, Serialize, Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[serde(rename_all = "snake_case")]
pub enum LyricsType {
    RichEmbedded,
    RichFile,
    SyncedEmbedded,
    SyncedFile,
    SimpleEmbedded,
    SimpleFile,
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
    pub lyrics: Option<LyricsConfig>,
    pub config_folders: Vec<PathBuf>,
    pub custom_fields: Vec<MetadataField>,
    pub named_strategies: HashMap<Rc<str>, Rc<MetadataOperation>>,
    pub find_replace: HashMap<String, String>,
    pub scan: Vec<ScanOptions>,
}

pub struct LibraryCache {
    pub reports: Vec<LibraryReport>,
    pub date_cache: DateCache,
    pub art_repo: Option<ArtRepo>,
}

fn full_path(path: PathBuf) -> PathBuf {
    path.canonicalize().unwrap_or(path)
}

impl LibraryConfig {
    pub fn new(folder: &Path, raw: RawLibraryConfig) -> Result<(Self, LibraryCache), LibraryError> {
        let config = Self {
            library_folder: full_path(folder.join(raw.library)),
            lyrics: raw.lyrics.map(|x| LyricsConfig {
                folder: folder.join(x.folder),
                priority: x.priority,
                config: x.config,
            }),
            config_folders: raw
                .config_folders
                .into_iter()
                .map(|x| full_path(folder.join(x)))
                .collect(),
            custom_fields: raw
                .custom_fields
                .into_iter()
                .map(MetadataField::Custom)
                .collect(),
            named_strategies: raw
                .named_strategies
                .into_iter()
                .map(|(k, v)| (k, Rc::new(v)))
                .collect(),
            find_replace: raw.find_replace,
            scan: raw.scan,
        };
        for strat in config.named_strategies.values() {
            config.check_operation(strat)?;
        }
        let cache = LibraryCache {
            reports: raw
                .reports
                .into_iter()
                .map(|x| LibraryReport::load(x, folder))
                .collect(),
            date_cache: DateCache::new(raw.cache.map(|x| folder.join(x))),
            art_repo: raw.art.map(|x| ArtRepo::new(folder, x)),
        };
        Ok((config, cache))
    }
    pub fn get_all_fields(&self) -> impl Iterator<Item = MetadataField> {
        let builtin = MetadataField::iter_default();
        let custom = self.custom_fields.clone().into_iter();
        builtin.chain(custom)
    }
    pub fn get_fields<'a>(
        &'a self,
        selector: &'a FieldSelector,
    ) -> impl Iterator<Item = MetadataField> + 'a {
        self.get_all_fields().filter(|x| selector.is_match(x))
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
        let ordering = raw_config.order.map(|x| self.make_ordering(x, nice_folder));
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
    fn make_ordering(&self, order: SongOrder, nice_folder: &Path) -> OrderingSetter {
        match order {
            SongOrder::Discs(discs) => {
                let mut map = HashMap::new();
                for (disc, sel) in discs.iter().enumerate() {
                    let matches = file_stuff::find_matches(sel, nice_folder, self);
                    let track_total = matches.len();
                    for (track, path) in matches.into_iter().enumerate() {
                        map.insert(
                            PathBuf::from(path),
                            DiscSet {
                                disc: (disc + 1) as u32,
                                track: (track + 1) as u32,
                                track_total: track_total as u32,
                            },
                        );
                    }
                }
                OrderingSetter::Discs {
                    map,
                    disc_total: discs.len() as u32,
                    original_selectors: discs,
                }
            }
            SongOrder::Order(order) => {
                let map = file_stuff::find_matches(&order, nice_folder, self)
                    .into_iter()
                    .enumerate()
                    .map(|(track, path)| (PathBuf::from(path), (track + 1) as u32))
                    .collect::<HashMap<_, _>>();
                let total = map.len() as u32;
                OrderingSetter::Order {
                    map,
                    total,
                    original_selector: order,
                }
            }
        }
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
            FieldSelector::AllExcept { exclude } => {
                for field in exclude {
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
                    Err(LibraryError::UnlistedCustomField(Rc::clone(str)))
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

impl LibraryCache {
    pub fn update_reports(&mut self, nice_path: &Path, metadata: &Metadata) {
        for report in &mut self.reports {
            report.record(nice_path, metadata);
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LibraryError {
    MissingNamedStrategy(Rc<str>),
    UnlistedCustomField(Rc<str>),
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

#[derive(Default)]
pub struct DateCache {
    path: Option<PathBuf>,
    pub cache: HashMap<PathBuf, DateTime<Utc>>,
    pub removed: Vec<PathBuf>,
    updated: HashSet<PathBuf>,
}
impl DateCache {
    pub fn new(path: Option<PathBuf>) -> Self {
        path.map_or_else(
            || Self {
                path: None,
                cache: HashMap::new(),
                updated: HashSet::new(),
                removed: vec![],
            },
            |path| {
                let mut cache = file_stuff::load_yaml::<HashMap<PathBuf, DateTime<Utc>>>(&path)
                    .unwrap_or_default();
                let mut removed = vec![];
                // replace with extract_if when stable
                cache.retain(|k, _| {
                    if !k.exists() {
                        removed.push(k.clone());
                        false
                    } else {
                        true
                    }
                });
                Self {
                    cache,
                    path: Some(path),
                    updated: HashSet::new(),
                    removed,
                }
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
                let file = std::fs::File::create(path)?;
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
