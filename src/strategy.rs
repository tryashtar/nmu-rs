use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    ffi::OsStr,
    path::{Path, PathBuf},
    rc::Rc,
};

use regex::Regex;
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;

use crate::{
    file_stuff,
    library_config::LibraryConfig,
    metadata::{Metadata, MetadataField, MetadataValue},
    modifier::{ValueError, ValueModifier},
    util::{OutOfBoundsDecision, Range},
    CopyCache,
};

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum MetadataOperation {
    Blank {
        remove: FieldSelector,
    },
    Keep {
        keep: FieldSelector,
    },
    Shared {
        fields: FieldSelector,
        set: ValueGetter,
    },
    SharedModify {
        fields: FieldSelector,
        modify: Rc<ValueModifier>,
    },
    Context {
        source: ValueGetter,
        modify: HashMap<MetadataField, Rc<ValueModifier>>,
    },
    Modify {
        modify: HashMap<MetadataField, Rc<ValueModifier>>,
    },
    Set(HashMap<MetadataField, ValueGetter>),
    Many(Vec<Rc<MetadataOperation>>),
}
pub struct ApplyReport {
    pub errors: Vec<ValueError>,
}
impl ApplyReport {
    pub fn merge(&mut self, mut other: ApplyReport) {
        self.errors.append(&mut other.errors);
    }
}
impl MetadataOperation {
    pub fn apply(
        &self,
        metadata: &mut Metadata,
        nice_path: &Path,
        config: &LibraryConfig,
        copy_cache: &CopyCache,
    ) -> ApplyReport {
        let mut report = ApplyReport { errors: vec![] };
        match self {
            Self::Many(items) => {
                for item in items {
                    let more = item.apply(metadata, nice_path, config, copy_cache);
                    report.merge(more);
                }
            }
            Self::Blank { remove } => {
                let builtin = MetadataField::iter().map(|x| x.into()).collect::<Vec<_>>();
                for field in config.custom_fields.iter().chain(builtin.iter()) {
                    if remove.is_match(field) {
                        metadata.insert(field.clone(), MetadataValue::blank().into());
                    }
                }
            }
            Self::Keep { keep } => {
                metadata.retain(|k, _| !keep.is_match(k));
            }
            Self::Shared { fields, set } => match set.get(nice_path, config, copy_cache) {
                Ok(value) => {
                    let builtin = MetadataField::iter().map(|x| x.into()).collect::<Vec<_>>();
                    for field in config.custom_fields.iter().chain(builtin.iter()) {
                        if fields.is_match(field) {
                            metadata.insert(field.clone(), value.clone());
                        }
                    }
                }
                Err(err) => {
                    report.errors.push(err);
                }
            },
            Self::SharedModify { fields, modify } => {
                let builtin = MetadataField::iter().map(|x| x.into()).collect::<Vec<_>>();
                for field in config.custom_fields.iter().chain(builtin.iter()) {
                    if fields.is_match(field) {
                        if let Some(existing) = metadata.get(field) {
                            match modify.modify(existing.clone(), nice_path, config, copy_cache) {
                                Ok(modified) => {
                                    metadata.insert(field.clone(), modified);
                                }
                                Err(err) => {
                                    report.errors.push(err);
                                }
                            };
                        } else {
                            report.errors.push(ValueError::MissingField {
                                modifier: modify.clone(),
                                field: field.clone(),
                            });
                        }
                    }
                }
            }
            Self::Set(set) => {
                for (field, value) in set {
                    match value.get(nice_path, config, copy_cache) {
                        Ok(value) => {
                            metadata.insert(field.clone(), value);
                        }
                        Err(err) => {
                            report.errors.push(err);
                        }
                    }
                }
            }
            Self::Context { source, modify } => match source.get(nice_path, config, copy_cache) {
                Ok(value) => {
                    for (field, modifier) in modify {
                        match modifier.modify(value.clone(), nice_path, config, copy_cache) {
                            Ok(modified) => {
                                metadata.insert(field.clone(), modified);
                            }
                            Err(err) => {
                                report.errors.push(err);
                            }
                        }
                    }
                }
                Err(err) => {
                    report.errors.push(err);
                }
            },
            Self::Modify { modify } => {
                for (field, modifier) in modify {
                    if let Some(existing) = metadata.get(field) {
                        match modifier.modify(existing.clone(), nice_path, config, copy_cache) {
                            Ok(modified) => {
                                metadata.insert(field.clone(), modified);
                            }
                            Err(err) => {
                                report.errors.push(err);
                            }
                        };
                    } else {
                        report.errors.push(ValueError::MissingField {
                            modifier: modifier.clone(),
                            field: field.clone(),
                        });
                    }
                }
            }
        }
        report
    }
}

#[derive(Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum ItemSelector {
    #[serde(skip)]
    All {
        recursive: bool,
    },
    #[serde(skip)]
    This,
    Path(PathBuf),
    Multi(Vec<ItemSelector>),
    Segmented {
        path: Vec<PathSegment>,
    },
    Subpath {
        subpath: Box<ItemSelector>,
        select: Box<ItemSelector>,
    },
}
impl ItemSelector {
    pub fn matches(&self, check_path: &Path) -> bool {
        match self {
            Self::All { recursive } => {
                if *recursive {
                    true
                } else {
                    check_path.components().take(2).collect::<Vec<_>>().len() == 1
                }
            }
            Self::This => check_path.as_os_str().is_empty(),
            Self::Multi(checks) => checks.iter().any(|x| x.matches(check_path)),
            Self::Path(path) => check_path.starts_with(path),
            Self::Segmented { path } => {
                let components = check_path.components().collect::<Vec<_>>();
                if path.len() > components.len() {
                    return false;
                };
                path.iter()
                    .zip(components.iter())
                    .all(|(x, y)| x.matches(y.as_os_str()))
            }
            Self::Subpath { .. } => !self.consume(check_path).is_empty(),
        }
    }
    fn consume<'a>(&self, check_path: &'a Path) -> Vec<&'a Path> {
        match self {
            Self::All { .. } => vec![check_path],
            Self::This => vec![],
            Self::Path(path) => check_path.strip_prefix(path).into_iter().collect(),
            Self::Multi(multi) => multi.iter().flat_map(|x| x.consume(check_path)).collect(),
            Self::Segmented { path } => {
                let components = check_path.components().collect::<Vec<_>>();
                if path.len() > components.len() {
                    return vec![];
                }
                for (segment, component) in path.iter().zip(components.iter()) {
                    if !segment.matches(component.as_os_str()) {
                        return vec![];
                    }
                }
                check_path
                    .strip_prefix(components.into_iter().take(path.len()).collect::<PathBuf>())
                    .into_iter()
                    .collect()
            }
            Self::Subpath { subpath, select } => subpath
                .consume(check_path)
                .into_iter()
                .flat_map(|x| select.consume(x))
                .collect(),
        }
    }
}

#[derive(Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum PathSegment {
    Literal(String),
    Regex {
        #[serde(with = "serde_regex")]
        regex: Regex,
    },
}
impl PathSegment {
    pub fn matches(&self, component: &OsStr) -> bool {
        match component.to_str() {
            None => false,
            Some(str) => match self {
                Self::Literal(literal) => str == literal,
                Self::Regex { regex } => regex.is_match(str),
            },
        }
    }
}

#[derive(Deserialize, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
#[serde(untagged)]
pub enum LocalItemSelector {
    #[serde(deserialize_with = "item_selector_this")]
    This,
    DrillUp {
        up: Range,
    },
    DrillDown {
        from_root: Range,
        #[serde(skip_serializing_if = "Option::is_none")]
        must_be: Option<MusicItemType>,
    },
    Selector {
        selector: ItemSelector,
        #[serde(skip_serializing_if = "Option::is_none")]
        must_be: Option<MusicItemType>,
    },
}
fn item_selector_this<'de, D>(deserializer: D) -> Result<(), D::Error>
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
            if value == "this" || value == "self" {
                Ok(())
            } else {
                Err(serde::de::Error::custom("invalid"))
            }
        }
    }

    deserializer.deserialize_str(Visitor)
}
impl LocalItemSelector {
    fn get(&self, start: &Path, config: &LibraryConfig) -> Vec<PathBuf> {
        match self {
            Self::This => vec![start.to_owned()],
            Self::DrillUp { up } => {
                let ancestors = start.ancestors().collect::<Vec<_>>();
                match up.slice(ancestors.as_slice(), OutOfBoundsDecision::Clamp) {
                    None => vec![],
                    Some(slice) => slice.iter().map(|x| (*x).to_owned()).collect(),
                }
            }
            Self::DrillDown { must_be, from_root } => {
                let ancestors = start.ancestors().collect::<Vec<_>>();
                let last = ancestors.len() - 1;
                let ancestors = ancestors
                    .into_iter()
                    .rev()
                    .enumerate()
                    .map(|(i, x)| {
                        let item_type = if i == last {
                            MusicItemType::Song
                        } else {
                            MusicItemType::Folder
                        };
                        if MusicItemType::matches(&item_type, must_be.as_ref()) {
                            Some(x.to_owned())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                match from_root.slice(ancestors.as_slice(), OutOfBoundsDecision::Clamp) {
                    None => vec![],
                    Some(slice) => slice
                        .iter()
                        .filter_map(|x| x.as_ref())
                        .map(|x| x.to_owned())
                        .collect(),
                }
            }
            Self::Selector { selector, must_be } => {
                file_stuff::find_matches(selector, start, config)
                    .into_iter()
                    .filter(|x| MusicItemType::matches(&x.as_type(), must_be.as_ref()))
                    .map(|x| x.into())
                    .collect()
            }
        }
    }
}

#[derive(Deserialize, Serialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum MusicItemType {
    Song,
    Folder,
}
impl MusicItemType {
    pub fn matches(check: &MusicItemType, against: Option<&MusicItemType>) -> bool {
        match against {
            None => true,
            Some(required) => check == required,
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum FieldSelector {
    #[serde(deserialize_with = "field_selector_all")]
    All,
    Single(MetadataField),
    Multiple(HashSet<MetadataField>),
}
fn field_selector_all<'de, D>(deserializer: D) -> Result<(), D::Error>
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
            if value == "*" {
                Ok(())
            } else {
                Err(serde::de::Error::custom("invalid"))
            }
        }
    }

    deserializer.deserialize_str(Visitor)
}
impl FieldSelector {
    pub fn is_match(&self, field: &MetadataField) -> bool {
        match self {
            Self::All => true,
            Self::Single(single) => field == single,
            Self::Multiple(set) => set.contains(field),
        }
    }
}

#[derive(Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum ValueGetter {
    Direct(MetadataValue),
    Copy {
        from: Rc<LocalItemSelector>,
        copy: MetadataField,
        #[serde(skip_serializing_if = "Option::is_none")]
        modify: Option<Rc<ValueModifier>>,
    },
    From {
        from: Rc<LocalItemSelector>,
        #[serde(default = "default_value")]
        value: FieldValueGetter,
        #[serde(default = "default_missing")]
        if_missing: WarnBehavior,
        #[serde(skip_serializing_if = "Option::is_none")]
        modify: Option<Rc<ValueModifier>>,
    },
}
#[derive(Deserialize, Serialize, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum WarnBehavior {
    Warn,
    Exit,
}
fn default_missing() -> WarnBehavior {
    WarnBehavior::Warn
}
impl ValueGetter {
    pub fn get(
        &self,
        path: &Path,
        config: &LibraryConfig,
        copy_cache: &CopyCache,
    ) -> Result<MetadataValue, ValueError> {
        match self {
            Self::Direct(value) => Ok(value.clone()),
            Self::From {
                from,
                value,
                if_missing,
                modify,
            } => {
                let items = from.get(path, config);
                if items.is_empty() {
                    return match if_missing {
                        WarnBehavior::Warn => Err(ValueError::ItemNotFound {
                            selector: from.clone(),
                        }),
                        WarnBehavior::Exit => Err(ValueError::ExitRequested),
                    };
                }
                let result = MetadataValue::List(
                    items
                        .into_iter()
                        .map(|x| value.get(x.as_ref(), config).to_string())
                        .collect(),
                )
                .into();
                match modify {
                    None => Ok(result),
                    Some(modify) => modify.modify(result, path, config, copy_cache),
                }
            }
            Self::Copy { from, copy, modify } => {
                let sources = from.get(path, config);
                if sources.is_empty() {
                    return Err(ValueError::ItemNotFound {
                        selector: from.clone(),
                    });
                }
                let mut missing = vec![];
                let mut present = vec![];
                for source in sources {
                    match copy_cache.get(&source).and_then(|x| x.get(copy)).cloned() {
                        None => {
                            missing.push(source);
                        }
                        Some(value) => {
                            present.push(value);
                        }
                    }
                }
                if !missing.is_empty() {
                    return Err(ValueError::CopyNotFound {
                        field: copy.clone(),
                        paths: missing,
                    });
                }
                let result = Self::combine(present)?;
                match modify {
                    None => Ok(result),
                    Some(modify) => modify.modify(result, path, config, copy_cache),
                }
            }
        }
    }
    fn combine(values: Vec<MetadataValue>) -> Result<MetadataValue, ValueError> {
        match values.len() {
            0 => Err(ValueError::Uncombinable { values }),
            1 => Ok(values[0].clone()),
            _ => {
                let mut list = vec![];
                for value in values.clone() {
                    match value {
                        MetadataValue::List(mut vals) => list.append(&mut vals),
                        _ => return Err(ValueError::Uncombinable { values }),
                    }
                }
                Ok(MetadataValue::List(list).into())
            }
        }
    }
}

#[derive(Deserialize, Serialize, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum FieldValueGetter {
    FileName,
    CleanName,
    Path,
}
impl FieldValueGetter {
    fn file_name(path: &Path) -> Cow<str> {
        path.file_name()
            .map(|x| x.to_string_lossy())
            .unwrap_or_else(|| path.to_string_lossy())
    }
    fn clean<'a>(name: Cow<'a, str>, config: &LibraryConfig) -> Cow<'a, str> {
        let mut result = name;
        for (find, replace) in &config.find_replace {
            result = result.replace(find, replace).into();
        }
        result
    }
    fn get<'a>(&self, path: &'a Path, config: &LibraryConfig) -> Cow<'a, str> {
        match self {
            Self::CleanName => Self::clean(Self::file_name(path), config),
            Self::FileName => Self::file_name(path),
            Self::Path => path.to_string_lossy(),
        }
    }
}
fn default_value() -> FieldValueGetter {
    FieldValueGetter::CleanName
}
