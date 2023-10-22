use std::{
    collections::{BTreeSet, HashMap, HashSet},
    path::{Path, PathBuf},
    rc::Rc,
};

use image::{DynamicImage, ImageError};
use serde::{Deserialize, Serialize};

use crate::{
    file_stuff::{self, ConfigError, YamlError},
    library_config::LibraryError,
    metadata::{BuiltinMetadataField, Metadata, MetadataValue},
};

pub type ArtConfigCache = HashMap<PathBuf, Rc<Result<ArtConfig, ConfigError>>>;
pub type ProcessedArtCache = HashMap<PathBuf, Rc<Result<DynamicImage, ImageError>>>;

#[derive(Deserialize, Serialize, Default)]
pub struct ArtSettings {}
impl ArtSettings {
    fn merge_in(&mut self, other: &Self) {}
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
    Processed(Rc<Result<DynamicImage, ImageError>>),
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
        if let Some(MetadataValue::List(art)) = metadata.get_mut(&BuiltinMetadataField::Art.into())
        {
            let template = self.find_first_template(art);
            if let Some((nice, template)) = template {
                art.clear();
                art.push(nice.to_string_lossy().into_owned());
                let result = processed_cache.entry(nice).or_insert_with_key(|nice| {
                    Rc::new(self.get_processed(
                        &template,
                        nice,
                        config_cache,
                        scan_paths.contains(&template),
                    ))
                });
                ProcessArtResult::Processed(result.clone())
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
    ) -> Result<DynamicImage, ImageError> {
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
                return image::open(cached_path);
            }
        }
        let mut template = image::open(template)?;
        let settings = self.get_settings(nice, config_cache);
        settings.apply(&mut template);
        if let Some(cached_path) = cached_path {
            template.save(cached_path)?;
        }
        Ok(template)
    }
    pub fn resolve_config(&self, raw_config: RawArtConfig) -> Result<ArtConfig, LibraryError> {
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
    fn get_settings(&self, nice_path: &Path, config_cache: &mut ArtConfigCache) -> ArtSettings {
        let mut settings = ArtSettings::default();
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
                .or_insert_with_key(|config_path| Rc::new(self.load_new_config(config_path)));
            if let Ok(config) = Rc::as_ref(config_load) {
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
        settings
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
