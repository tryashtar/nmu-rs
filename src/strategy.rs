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
    file_stuff::{self, NicePath},
    library_config::LibraryConfig,
    metadata::{BuiltinMetadataField, MetadataField, MetadataValue, PendingMetadata, PendingValue},
    modifier::{ValueError, ValueModifier},
    util::{Listable, OutOfBoundsDecision, Range},
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
    Context {
        source: ValueGetter,
        modify: HashMap<MetadataField, Listable<Rc<ValueModifier>>>,
    },
    Modify {
        modify: HashMap<MetadataField, Listable<Rc<ValueModifier>>>,
    },
    Set(HashMap<MetadataField, ValueGetter>),
}
impl MetadataOperation {
    pub fn apply(&self, metadata: &mut PendingMetadata, path: &Path, config: &LibraryConfig) {
        match self {
            Self::Blank { remove } => {
                for field in &config.custom_fields {
                    if remove.is_match(field) {
                        metadata
                            .fields
                            .insert(field.clone(), MetadataValue::blank().into());
                    }
                }
                for field in BuiltinMetadataField::iter() {
                    let builtin = field.into();
                    if remove.is_match(&builtin) {
                        metadata
                            .fields
                            .insert(builtin, MetadataValue::blank().into());
                    }
                }
            }
            Self::Keep { keep } => {
                metadata.fields.retain(|k, _| !keep.is_match(k));
            }
            Self::Set(set) => {
                for (field, value) in set {
                    if let Ok(value) = value.get(path, config) {
                        metadata.fields.insert(field.clone(), value);
                    }
                }
            }
            Self::Context { source, modify } => {
                if let Ok(value) = source.get(path, config) {
                    for (field, modifier) in modify {
                        if let Ok(modified) = ValueModifier::modify_all(
                            modifier.as_slice(),
                            value.clone(),
                            path,
                            config,
                        ) {
                            metadata.fields.insert(field.clone(), modified);
                        }
                    }
                }
            }
            Self::Modify { modify } => {
                for (field, modifier) in modify {
                    if let Some(existing) = metadata.fields.get(field) {
                        if let Ok(modified) = ValueModifier::modify_all(
                            modifier.as_slice(),
                            existing.clone(),
                            path,
                            config,
                        ) {
                            metadata.fields.insert(field.clone(), modified);
                        }
                    }
                }
            }
        }
    }
}

#[derive(Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum ItemSelector {
    All,
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
            Self::All => true,
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
            Self::All => vec![check_path],
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
        must_be: Option<MusicItemType>,
    },
    Selector {
        selector: ItemSelector,
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
    fn get(&self, start: &Path, config: &LibraryConfig) -> Vec<NicePath> {
        match self {
            Self::This => vec![NicePath::Folder(start.to_owned())],
            Self::DrillUp { up } => {
                let ancestors = start.ancestors().collect::<Vec<_>>();
                up.slice(ancestors.as_slice(), OutOfBoundsDecision::Clamp)
                    .iter()
                    .map(|x| NicePath::Folder((*x).to_owned()))
                    .collect()
            }
            Self::DrillDown { must_be, from_root } => {
                let ancestors = start.ancestors().collect::<Vec<_>>();
                let last = ancestors.len() - 1;
                let ancestors = ancestors
                    .into_iter()
                    .rev()
                    .enumerate()
                    .map(|(i, x)| {
                        let nice = if i == last {
                            NicePath::Song(x.to_owned())
                        } else {
                            NicePath::Folder(x.to_owned())
                        };
                        if MusicItemType::matches(&nice.as_type(), must_be.as_ref()) {
                            Some(nice)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                from_root
                    .slice(ancestors.as_slice(), OutOfBoundsDecision::Clamp)
                    .iter()
                    .filter_map(|x| x.as_ref())
                    .map(|x| x.to_owned())
                    .collect()
            }
            Self::Selector { selector, must_be } => {
                file_stuff::find_matches(selector, start, config)
                    .into_iter()
                    .filter(|x| MusicItemType::matches(&x.as_type(), must_be.as_ref()))
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
            Some(required) => check == required
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
    fn is_match(&self, field: &MetadataField) -> bool {
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
        from: LocalItemSelector,
        copy: MetadataField,
        #[serde(default = "empty_list")]
        modify: Listable<Rc<ValueModifier>>,
    },
    From {
        from: LocalItemSelector,
        #[serde(default = "default_value")]
        value: FieldValueGetter,
        #[serde(default = "empty_list")]
        modify: Listable<Rc<ValueModifier>>,
    },
}
fn empty_list<T>() -> Listable<T> {
    Listable::List(vec![])
}
impl ValueGetter {
    pub fn get(&self, path: &Path, config: &LibraryConfig) -> Result<PendingValue, ValueError> {
        match self {
            Self::Direct(value) => Ok(value.clone().into()),
            Self::From {
                from,
                value,
                modify,
            } => {
                let items = from.get(path, config);
                if items.is_empty() {
                    return Err(ValueError::ItemNotFound);
                }
                let result = MetadataValue::List(
                    items
                        .into_iter()
                        .map(|x| value.get(x.as_path(), config).to_string())
                        .collect(),
                );
                ValueModifier::modify_all(modify.as_slice(), result.into(), path, config)
            }
            Self::Copy { from, copy, modify } => Ok(PendingValue::CopyField {
                field: copy.clone(),
                modify: modify.clone(),
                sources: from.get(path, config),
            }),
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
