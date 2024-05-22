use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    io,
    path::{Path, PathBuf},
    rc::Rc,
};

use image::{DynamicImage, GenericImageView, ImageError, ImageResult};
use serde::{Deserialize, Serialize};

use crate::{
    file_stuff::{self, ConfigError, YamlError},
    library_config::LibraryError,
    song_config,
};

pub type ArtConfigCache = HashMap<PathBuf, Result<Rc<ArtConfig>, Rc<ConfigError>>>;
pub type ProcessedArtCache = HashMap<PathBuf, Result<Rc<DynamicImage>, Rc<ArtError>>>;

#[derive(thiserror::Error, Debug)]
pub enum ArtError {
    #[error("Config failed")]
    Config,
    #[error(transparent)]
    Image(#[from] ImageError),
}

#[derive(Deserialize, Serialize, Default)]
pub struct ArtSettings {
    width: Option<ArtLength>,
    height: Option<ArtLength>,
    interpolation: Option<Interpolation>,
    integer_scale: Option<bool>,
    buffer: Option<ArtList<[u32; 4]>>,
    background: Option<ArtList<[u8; 4]>>,
    scale: Option<ArtScale>,
}
#[derive(Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum ArtList<T> {
    #[serde(deserialize_with = "art_list_disabled")]
    Disabled,
    Present(T),
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
            if v {
                Err(serde::de::Error::custom("invalid"))
            } else {
                Ok(())
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
    Stretch,
}
#[derive(Deserialize, Serialize, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum ArtLength {
    Original,
    #[serde(untagged)]
    Number(u32),
}
impl ArtSettings {
    fn merge_in(&mut self, other: &Self) {
        if let Some(width) = other.width {
            self.width = Some(width);
        }
        if let Some(height) = other.height {
            self.height = Some(height);
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
    background: Option<[u8; 4]>,
    scale: ArtScale,
}
impl FinalArtSettings {
    fn apply(&self, mut image: DynamicImage) -> DynamicImage {
        if self.buffer.is_some() {
            let (x, y, width, height) = Self::bounding_rectangle(&image);
            image = image.crop(x, y, width, height);
        }
        if self.width.is_some() || self.height.is_some() {
            let (current_width, current_height) = image.dimensions();
            let mut width = self.width.unwrap_or(current_width);
            let mut height = self.height.unwrap_or(current_height);
            if self.integer_scale {
                width = width / current_width * current_width;
                height = height / current_height * current_height;
            }
            if let Some(buffer) = self.buffer {
                width -= buffer[0] + buffer[2];
                height -= buffer[1] + buffer[3];
            }
            let filter = match self.interpolation {
                Interpolation::NearestNeighbor => image::imageops::FilterType::Nearest,
                Interpolation::Bicubic => image::imageops::FilterType::CatmullRom,
            };
            image = match self.scale {
                ArtScale::Stretch => image.resize_exact(width, height, filter),
                ArtScale::Pad | ArtScale::Max => image.resize(width, height, filter),
            };
            if matches!(self.scale, ArtScale::Pad) {
                let mut field = self.empty_field(width, height);
                let (current_width, current_height) = image.dimensions();
                image::imageops::overlay(
                    &mut field,
                    &image,
                    i64::from((width - current_width) / 2),
                    i64::from((height - current_height) / 2),
                );
                image = field.into();
            }
        }
        if let Some(buffer) = self.buffer {
            let (width, height) = image.dimensions();
            let mut field = self.empty_field(
                width + buffer[0] + buffer[2],
                height + buffer[1] + buffer[3],
            );
            image::imageops::overlay(
                &mut field,
                &image,
                i64::from(buffer[0]),
                i64::from(buffer[1]),
            );
            image = field.into();
        } else if self.background.is_some() {
            let (width, height) = image.dimensions();
            let mut field = self.empty_field(width, height);
            image::imageops::overlay(&mut field, &image, 0, 0);
        }
        image
    }
    fn empty_field(&self, width: u32, height: u32) -> image::RgbaImage {
        self.background.map_or_else(
            || image::RgbaImage::new(width, height),
            |bg| image::RgbaImage::from_pixel(width, height, image::Rgba::<u8>(bg)),
        )
    }
    fn bounding_rectangle(image: &DynamicImage) -> (u32, u32, u32, u32) {
        let (mut left, mut top) = image.dimensions();
        let (mut right, mut bottom) = (0, 0);
        for (x, y, color) in image.pixels() {
            if color[3] > 0 {
                left = std::cmp::min(x, left);
                top = std::cmp::min(y, top);
                right = std::cmp::max(x, right);
                bottom = std::cmp::max(y, bottom);
            }
        }
        (left, top, right - left + 1, bottom - top + 1)
    }
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
    Reference(Rc<str>),
    Direct(ArtSettings),
    Many(Vec<ReferencableArtSettings>),
}

#[derive(Deserialize, Serialize)]
pub struct RawArtSettingsSetter {
    pub names: Vec<PathBuf>,
    pub set: ReferencableArtSettings,
}

#[derive(Deserialize, Serialize)]
pub struct RawArtRepo {
    pub templates: PathBuf,
    pub cache: Option<PathBuf>,
    pub icons: Option<PathBuf>,
    pub file_cache: Option<PathBuf>,
    pub named_settings: HashMap<Rc<str>, ArtSettings>,
}

pub struct GetProcessedResult {
    pub newly_loaded: Vec<ArtConfigLoadResults>,
    pub result: Result<Rc<DynamicImage>, Rc<ArtError>>,
}

pub struct GetSettingsResults {
    newly_loaded: Vec<ArtConfigLoadResults>,
    result: Option<FinalArtSettings>,
}

pub struct ArtConfigLoadResults {
    pub result: Result<Rc<ArtConfig>, Rc<ConfigError>>,
    pub nice_folder: PathBuf,
    pub full_path: PathBuf,
}

pub enum GetArtResults {
    Keep,
    Remove,
    NoTemplateFound {
        tried: Vec<PathBuf>,
    },
    Processed {
        result: Result<Rc<DynamicImage>, Rc<ArtError>>,
        nice_path: PathBuf,
        full_path: PathBuf,
        processed: Option<GetProcessedResult>,
    },
}

pub struct ArtDiskCache {
    pub folder: PathBuf,
    pub nice_evicted: HashSet<PathBuf>,
}
impl ArtDiskCache {
    pub fn get_path(&self, nice: &Path) -> PathBuf {
        let mut joined = self
            .folder
            .join(nice)
            .to_string_lossy()
            .replace(|c: char| !c.is_ascii(), "_");
        joined.push_str(".png");
        PathBuf::from(joined)
    }
    pub fn get(&self, nice: &Path) -> ImageResult<DynamicImage> {
        if self.nice_evicted.contains(nice) {
            return Err(ImageError::IoError(io::Error::new(
                io::ErrorKind::NotFound,
                "evicted from cache",
            )));
        }
        image::open(self.get_path(nice))
    }
}

pub struct ArtRepo {
    pub templates_folder: PathBuf,
    pub disk_cache: Option<ArtDiskCache>,
    pub icon_folder: Option<PathBuf>,
    pub used_templates: ArtUsageCache,
    pub named_settings: HashMap<Rc<str>, Rc<ArtSettings>>,
    pub processed_cache: ProcessedArtCache,
    config_cache: ArtConfigCache,
}
impl ArtRepo {
    pub fn new(folder: &Path, raw: RawArtRepo) -> Self {
        Self {
            templates_folder: folder.join(raw.templates),
            disk_cache: raw.cache.map(|x| ArtDiskCache {
                folder: folder.join(x),
                nice_evicted: HashSet::new(),
            }),
            icon_folder: raw.icons.map(|x| folder.join(x)),
            used_templates: ArtUsageCache::new(raw.file_cache.map(|x| folder.join(x))),
            named_settings: raw
                .named_settings
                .into_iter()
                .map(|(k, v)| (k, Rc::new(v)))
                .collect(),
            config_cache: HashMap::new(),
            processed_cache: HashMap::new(),
        }
    }
    pub fn get_image(&mut self, art: &[String]) -> GetArtResults {
        if art.is_empty() {
            return GetArtResults::Remove;
        }
        let template = self.find_first_template(art);
        match template {
            Some((nice, template)) => {
                let mut processed = None;
                // partial borrow hack
                let mut cache = std::mem::take(&mut self.processed_cache);
                let result = cache
                    .entry(nice.clone())
                    .or_insert_with_key(|nice| {
                        let load = self.get_processed(&template, nice);
                        let result = load.result.clone();
                        processed = Some(load);
                        result
                    })
                    .clone();
                self.processed_cache = cache;
                GetArtResults::Processed {
                    result,
                    nice_path: nice,
                    full_path: template,
                    processed,
                }
            }
            None => GetArtResults::NoTemplateFound {
                tried: art.iter().map(PathBuf::from).collect(),
            },
        }
    }
    pub fn get_processed(&mut self, template: &Path, nice: &Path) -> GetProcessedResult {
        if let Some(disk) = &self.disk_cache {
            if let Ok(img) = disk.get(nice) {
                return GetProcessedResult {
                    newly_loaded: vec![],
                    result: Ok(Rc::new(img)),
                };
            }
        }
        match image::open(template) {
            Err(err) => GetProcessedResult {
                newly_loaded: vec![],
                result: Err(Rc::new(ArtError::Image(err))),
            },
            Ok(img) => {
                let settings = self.get_settings(nice);
                match &settings.result {
                    None => GetProcessedResult {
                        newly_loaded: settings.newly_loaded,
                        result: Err(Rc::new(ArtError::Config)),
                    },
                    Some(config) => {
                        let modified = config.apply(img);
                        GetProcessedResult {
                            newly_loaded: settings.newly_loaded,
                            result: Ok(Rc::new(modified)),
                        }
                    }
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
                let resolved = self.resolve_settings(setter.set)?;
                for name in setter.names {
                    set.entry(name).or_default().append(&mut resolved.clone());
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
    pub fn find_first_template(&self, list: &[String]) -> Option<(PathBuf, PathBuf)> {
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
                for entry in read.filter_map(|x| x.ok()) {
                    let path = entry.path();
                    if !file_stuff::is_dir(&entry)
                        && path.file_stem().is_some_and(|x| x == name)
                        && !path.file_name().is_some_and(|x| x == "images.yaml")
                    {
                        return Some(path);
                    }
                }
            }
        }
        None
    }
    fn get_settings(&mut self, nice_path: &Path) -> GetSettingsResults {
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
            // partial borrow hack
            let mut cache = std::mem::take(&mut self.config_cache);
            let config_load = cache
                .entry(config_path)
                .or_insert_with_key(|config_path| {
                    let result = self
                        .load_new_config(config_path)
                        .map(Rc::new)
                        .map_err(Rc::new);
                    match &result {
                        Err(err) if song_config::is_not_found(err) => {}
                        _ => {
                            newly_loaded.push(ArtConfigLoadResults {
                                nice_folder: ancestor.to_owned(),
                                full_path: config_path.to_owned(),
                                result: result.clone(),
                            });
                        }
                    }
                    result
                })
                .clone();
            self.config_cache = cache;
            match config_load {
                Ok(config) => {
                    if let Some(settings) = &mut settings {
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
                    if !song_config::is_not_found(&error) {
                        settings = None;
                    }
                }
            }
        }
        GetSettingsResults {
            newly_loaded,
            result: settings.map(ArtSettings::finalize),
        }
    }
    fn load_new_config(&self, full_path: &Path) -> Result<ArtConfig, ConfigError> {
        match file_stuff::load_yaml::<RawArtConfig>(full_path) {
            Err(error) => Err(ConfigError::Yaml(error)),
            Ok(config) => self.resolve_config(config).map_err(ConfigError::Library),
        }
    }
}

pub struct ArtUsageCache {
    path: Option<PathBuf>,
    pub template_to_users: HashMap<PathBuf, HashSet<PathBuf>>,
    user_to_template: HashMap<PathBuf, PathBuf>,
    pub removed: HashMap<PathBuf, HashSet<PathBuf>>,
}
impl ArtUsageCache {
    fn new(path: Option<PathBuf>) -> Self {
        match path {
            None => Self {
                path: None,
                template_to_users: HashMap::new(),
                user_to_template: HashMap::new(),
                removed: HashMap::new(),
            },
            Some(path) => {
                let map = file_stuff::load_yaml::<HashMap<PathBuf, HashSet<PathBuf>>>(&path);
                match map {
                    Err(_) => Self {
                        path: Some(path),
                        template_to_users: HashMap::new(),
                        user_to_template: HashMap::new(),
                        removed: HashMap::new(),
                    },
                    Ok(mut map) => {
                        let mut removed = HashMap::new();
                        // replace with extract_if when stable
                        map.retain(|k, v| {
                            if !k.exists() {
                                removed.insert(k.clone(), v.clone());
                                false
                            } else {
                                true
                            }
                        });
                        let mut reverse = HashMap::new();
                        for (template, songs) in &map {
                            for song in songs {
                                reverse.insert(song.to_path_buf(), template.to_path_buf());
                            }
                        }
                        Self {
                            path: Some(path),
                            template_to_users: map,
                            user_to_template: reverse,
                            removed,
                        }
                    }
                }
            }
        }
    }
    pub fn add(&mut self, song: &Path, art: &GetArtResults) {
        match art {
            GetArtResults::Keep | GetArtResults::Remove | GetArtResults::NoTemplateFound { .. } => {
                if let Some(old) = self.user_to_template.remove(song) {
                    if let Some(set) = self.template_to_users.get_mut(&old) {
                        set.remove(song);
                    }
                }
            }
            GetArtResults::Processed { full_path, .. } => {
                if let Some(old) = self.user_to_template.remove(song) {
                    if let Some(set) = self.template_to_users.get_mut(&old) {
                        set.remove(song);
                    }
                }
                self.user_to_template
                    .insert(song.to_owned(), full_path.to_owned());
                self.template_to_users
                    .entry(full_path.to_owned())
                    .or_default()
                    .insert(song.to_owned());
            }
        }
    }
    pub fn save(&self) -> Result<(), YamlError> {
        match &self.path {
            None => Ok(()),
            Some(path) => {
                let ordered: BTreeMap<_, BTreeSet<_>> = self
                    .template_to_users
                    .iter()
                    .map(|(x, y)| (x, y.iter().collect()))
                    .collect();
                let file = std::fs::File::create(path)?;
                let writer = std::io::BufWriter::new(file);
                serde_yaml::to_writer(writer, &ordered)?;
                Ok(())
            }
        }
    }
}
