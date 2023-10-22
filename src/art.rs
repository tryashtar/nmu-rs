use std::{
    collections::{BTreeSet, HashMap, HashSet},
    path::{Path, PathBuf},
    rc::Rc,
};

use image::{DynamicImage, ImageError};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{
    file_stuff::{self, ConfigError, YamlError},
    is_not_found,
    library_config::LibraryError,
    metadata::{BuiltinMetadataField, Metadata, MetadataValue},
    ConfigLoadResults,
};

pub type ArtConfigCache = HashMap<PathBuf, Rc<Result<ArtConfig, ConfigError>>>;
pub type ProcessedArtCache = HashMap<PathBuf, Rc<Result<DynamicImage, ArtError>>>;

#[derive(Error, Debug)]
pub enum ArtError {
    #[error("Config failed")]
    Config,
    #[error("{0}")]
    Image(#[from] ImageError),
}

#[derive(Deserialize, Serialize, Default)]
pub struct ArtSettings {
    width: Option<ArtLength>,
    height: Option<ArtLength>,
    interpolation: Option<Interpolation>,
    integer_scale: Option<bool>,
    buffer: Option<ArtList>,
    background: Option<ArtList>,
    scale: Option<ArtScale>,
}
#[derive(Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum ArtList {
    #[serde(deserialize_with = "art_list_disabled")]
    Disabled,
    Present([u32; 4]),
}
fn art_list_disabled<'de, D>(deserializer: D) -> Result<(), D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    struct Visitor;

    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = ();

        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
            formatter.write_str("bool")
        }

        fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            if !v {
                Ok(())
            } else {
                Err(serde::de::Error::custom("invalid"))
            }
        }
    }

    deserializer.deserialize_bool(Visitor)
}
#[derive(Deserialize, Serialize, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum Interpolation {
    Bicubic,
    NearestNeighbor,
}
#[derive(Deserialize, Serialize, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum ArtScale {
    Pad,
    Max,
}
#[derive(Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum ArtLength {
    #[serde(deserialize_with = "art_length_original")]
    Original,
    Number(u32),
}
fn art_length_original<'de, D>(deserializer: D) -> Result<(), D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    struct Visitor;

    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = ();

        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
            formatter.write_str("string")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            if value == "original" {
                Ok(())
            } else {
                Err(serde::de::Error::custom("invalid"))
            }
        }
    }

    deserializer.deserialize_str(Visitor)
}
impl ArtSettings {
    fn merge_in(&mut self, other: &Self) {
        if let Some(width) = &other.width {
            self.width = Some(width.clone());
        }
        if let Some(height) = &other.height {
            self.height = Some(height.clone());
        }
        if let Some(interpolation) = &other.interpolation {
            self.interpolation = Some(*interpolation);
        }
        if let Some(integer_scale) = &other.integer_scale {
            self.integer_scale = Some(*integer_scale);
        }
        if let Some(buffer) = &other.buffer {
            self.buffer = Some(buffer.clone());
        }
        if let Some(background) = &other.background {
            self.background = Some(background.clone());
        }
        if let Some(scale) = &other.scale {
            self.scale = Some(*scale);
        }
    }
    fn finalize(self) -> FinalArtSettings {
        FinalArtSettings {
            width: match self.width {
                None | Some(ArtLength::Original) => None,
                Some(ArtLength::Number(num)) => Some(num),
            },
            height: match self.height {
                None | Some(ArtLength::Original) => None,
                Some(ArtLength::Number(num)) => Some(num),
            },
            interpolation: self.interpolation.unwrap_or(Interpolation::Bicubic),
            integer_scale: self.integer_scale.unwrap_or(false),
            buffer: match self.buffer {
                None | Some(ArtList::Disabled) => None,
                Some(ArtList::Present(list)) => Some(list),
            },
            background: match self.background {
                None | Some(ArtList::Disabled) => None,
                Some(ArtList::Present(list)) => Some(list),
            },
            scale: self.scale.unwrap_or(ArtScale::Pad),
        }
    }
}
pub struct FinalArtSettings {
    width: Option<u32>,
    height: Option<u32>,
    interpolation: Interpolation,
    integer_scale: bool,
    buffer: Option<[u32; 4]>,
    background: Option<[u32; 4]>,
    scale: ArtScale,
}
impl FinalArtSettings {
    fn apply(&self, image: &mut DynamicImage) {}
}

#[derive(Deserialize, Serialize)]
pub struct RawArtConfig {
    pub all: Option<ReferencableArtSettings>,
    #[serde(rename = "set all")]
    pub set_all: Option<Vec<RawArtSettingsSetter>>,
    pub set: Option<HashMap<PathBuf, ReferencableArtSettings>>,
}

pub struct ArtConfig {
    pub all: Vec<Rc<ArtSettings>>,
    pub set: HashMap<PathBuf, Vec<Rc<ArtSettings>>>,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ReferencableArtSettings {
    Reference(String),
    Direct(ArtSettings),
    Many(Vec<ReferencableArtSettings>),
}

#[derive(Deserialize, Serialize)]
pub struct RawArtSettingsSetter {
    pub names: Vec<PathBuf>,
    pub set: ReferencableArtSettings,
}

#[derive(Deserialize)]
pub struct RawArtRepo {
    templates: PathBuf,
    cache: Option<PathBuf>,
    icons: Option<PathBuf>,
    file_cache: Option<PathBuf>,
    named_settings: HashMap<String, ArtSettings>,
    extensions: HashSet<String>,
}

pub enum ProcessArtResult {
    NoArtNeeded,
    NoTemplateFound,
    Processed {
        full_path: PathBuf,
        newly_loaded: Vec<ConfigLoadResults<ArtConfig>>,
        result: Rc<Result<DynamicImage, ArtError>>,
    },
}

pub struct GetProcessedResult {
    newly_loaded: Vec<ConfigLoadResults<ArtConfig>>,
    result: Result<DynamicImage, ArtError>,
}

pub struct GetSettingsResults {
    newly_loaded: Vec<ConfigLoadResults<ArtConfig>>,
    result: Option<FinalArtSettings>,
}

pub struct ArtRepo {
    pub templates_folder: PathBuf,
    pub cache_folder: Option<PathBuf>,
    pub icon_folder: Option<PathBuf>,
    pub used_templates: ArtCache,
    pub named_settings: HashMap<String, Rc<ArtSettings>>,
    pub image_extensions: HashSet<String>,
}
impl ArtRepo {
    pub fn new(folder: &Path, raw: RawArtRepo) -> Self {
        Self {
            templates_folder: folder.join(raw.templates),
            cache_folder: raw.cache.map(|x| folder.join(x)),
            icon_folder: raw.icons.map(|x| folder.join(x)),
            used_templates: ArtCache::new(raw.file_cache.map(|x| folder.join(x))),
            named_settings: raw
                .named_settings
                .into_iter()
                .map(|(k, v)| (k, Rc::new(v)))
                .collect(),
            image_extensions: raw
                .extensions
                .into_iter()
                .map(|x| match x.strip_prefix('.') {
                    Some(stripped) => stripped.to_owned(),
                    None => x,
                })
                .collect(),
        }
    }
    pub fn resolve_art(
        &self,
        metadata: &mut Metadata,
        scan_paths: &BTreeSet<PathBuf>,
        config_cache: &mut ArtConfigCache,
        processed_cache: &mut ProcessedArtCache,
    ) -> ProcessArtResult {
        let mut newly_loaded = vec![];
        if let Some(MetadataValue::List(art)) = metadata.get_mut(&BuiltinMetadataField::Art.into())
        {
            if art.is_empty() {
                return ProcessArtResult::NoArtNeeded;
            }
            let template = self.find_first_template(art);
            if let Some((nice, template)) = template {
                art.clear();
                art.push(nice.to_string_lossy().into_owned());
                let result = processed_cache.entry(nice).or_insert_with_key(|nice| {
                    let mut result = self.get_processed(
                        &template,
                        nice,
                        config_cache,
                        scan_paths.contains(&template),
                    );
                    newly_loaded.append(&mut result.newly_loaded);
                    Rc::new(result.result)
                });
                ProcessArtResult::Processed {
                    full_path: template,
                    newly_loaded,
                    result: result.clone(),
                }
            } else {
                ProcessArtResult::NoTemplateFound
            }
        } else {
            ProcessArtResult::NoArtNeeded
        }
    }
    fn get_processed(
        &self,
        template: &Path,
        nice: &Path,
        config_cache: &mut ArtConfigCache,
        full_load: bool,
    ) -> GetProcessedResult {
        let cached_path = self.cache_folder.as_ref().map(|x| {
            let mut joined = x
                .join(nice)
                .to_string_lossy()
                .replace(|c: char| !c.is_ascii(), "_");
            joined.push_str(".png");
            PathBuf::from(joined)
        });
        if !full_load {
            if let Some(cached_path) = cached_path {
                return GetProcessedResult {
                    newly_loaded: vec![],
                    result: image::open(cached_path).map_err(ArtError::Image),
                };
            }
        }
        let mut template = image::open(template);
        let settings = self.get_settings(nice, config_cache);
        match &settings.result {
            None => GetProcessedResult {
                newly_loaded: settings.newly_loaded,
                result: Err(ArtError::Config),
            },
            Some(config) => {
                if let Ok(ref mut template) = template {
                    config.apply(template);
                    if let Some(cached_path) = cached_path {
                        let _ = template.save(cached_path);
                    }
                }
                GetProcessedResult {
                    newly_loaded: settings.newly_loaded,
                    result: template.map_err(ArtError::Image),
                }
            }
        }
    }
    fn resolve_config(&self, raw_config: RawArtConfig) -> Result<ArtConfig, LibraryError> {
        let all = raw_config
            .all
            .map(|x| self.resolve_settings(x))
            .transpose()?
            .unwrap_or_default();
        let mut set: HashMap<PathBuf, Vec<Rc<ArtSettings>>> = HashMap::new();
        if let Some(raw_set) = raw_config.set_all {
            for setter in raw_set {
                let mut resolved = self.resolve_settings(setter.set)?;
                for name in setter.names {
                    set.entry(name).or_default().append(&mut resolved);
                }
            }
        }
        if let Some(raw_set) = raw_config.set {
            for (path, settings) in raw_set {
                let mut resolved = self.resolve_settings(settings)?;
                set.entry(path).or_default().append(&mut resolved);
            }
        }
        Ok(ArtConfig { all, set })
    }
    fn resolve_settings(
        &self,
        settings: ReferencableArtSettings,
    ) -> Result<Vec<Rc<ArtSettings>>, LibraryError> {
        match settings {
            ReferencableArtSettings::Direct(direct) => Ok(vec![Rc::new(direct)]),
            ReferencableArtSettings::Reference(reference) => self
                .named_settings
                .get(&reference)
                .cloned()
                .map(|x| vec![x])
                .ok_or(LibraryError::MissingNamedStrategy(reference)),
            ReferencableArtSettings::Many(many) => many
                .into_iter()
                .map(|x| self.resolve_settings(x))
                .collect::<Result<Vec<_>, _>>()
                .map(|x| x.into_iter().flatten().collect()),
        }
    }
    fn find_first_template(&self, list: &Vec<String>) -> Option<(PathBuf, PathBuf)> {
        for entry in list {
            let nice = PathBuf::from(entry.clone());
            if let Some(template) = self.find_template(&nice) {
                return Some((nice, template));
            }
        }
        None
    }
    fn find_template(&self, nice_path: &Path) -> Option<PathBuf> {
        if let Some(name) = nice_path.file_name().and_then(|x| x.to_str()) {
            if let Ok(read) = std::fs::read_dir(
                self.templates_folder
                    .join(nice_path)
                    .parent()
                    .unwrap_or(Path::new("")),
            ) {
                for file in read.filter_map(|x| x.ok()) {
                    let path = file.path();
                    if path.file_stem().map(|x| x == name).unwrap_or(false)
                        && file_stuff::match_extension(&path, &self.image_extensions)
                    {
                        return Some(path);
                    }
                }
            }
        }
        None
    }
    fn get_settings(
        &self,
        nice_path: &Path,
        config_cache: &mut ArtConfigCache,
    ) -> GetSettingsResults {
        let mut settings = Some(ArtSettings::default());
        let mut newly_loaded = vec![];
        for ancestor in nice_path
            .parent()
            .unwrap_or(Path::new(""))
            .ancestors()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
        {
            let select_path = nice_path.strip_prefix(ancestor).unwrap_or(nice_path);
            let config_path = self.templates_folder.join(ancestor).join("images.yaml");
            let config_load = config_cache
                .entry(config_path)
                .or_insert_with_key(|config_path| {
                    let result = Rc::new(self.load_new_config(config_path));
                    newly_loaded.push(ConfigLoadResults {
                        nice_folder: ancestor.to_owned(),
                        full_path: config_path.to_owned(),
                        result: result.clone(),
                    });
                    result
                });
            match Rc::as_ref(config_load) {
                Ok(config) => {
                    if let Some(ref mut settings) = settings {
                        for replace in &config.all {
                            settings.merge_in(replace);
                        }
                        if let Some(more) = config.set.get(select_path) {
                            for replace in more {
                                settings.merge_in(replace);
                            }
                        }
                    }
                }
                Err(error) => {
                    if !is_not_found(error) {
                        settings = None;
                    }
                }
            }
        }
        GetSettingsResults {
            newly_loaded,
            result: settings.map(|x| x.finalize()),
        }
    }
    fn load_new_config(&self, full_path: &Path) -> Result<ArtConfig, ConfigError> {
        match file_stuff::load_yaml::<RawArtConfig>(full_path) {
            Err(error) => Err(ConfigError::Yaml(error)),
            Ok(config) => self.resolve_config(config).map_err(ConfigError::Library),
        }
    }
}

pub struct ArtCache {
    path: Option<PathBuf>,
    pub cache: HashMap<PathBuf, Vec<PathBuf>>,
}
impl ArtCache {
    fn new(path: Option<PathBuf>) -> Self {
        match path {
            None => Self {
                path: None,
                cache: HashMap::new(),
            },
            Some(path) => Self {
                path: Some(path.clone()),
                cache: match file_stuff::load_yaml(&path) {
                    Err(_) => HashMap::new(),
                    Ok(map) => map,
                },
            },
        }
    }
    pub fn save(&self) -> Result<(), YamlError> {
        match &self.path {
            None => Ok(()),
            Some(path) => {
                let file = std::fs::File::create(path)?;
                let writer = std::io::BufWriter::new(file);
                serde_yaml::to_writer(writer, &self.cache)?;
                Ok(())
            }
        }
    }
}
