use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

use serde::{Deserialize, Serialize};

use crate::{
    library_config::LibraryConfig,
    metadata::{MetadataField, PendingMetadata},
    strategy::{ItemSelector, MetadataOperation, MusicItemType, ValueGetter},
    util::Listable,
};

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RawSongConfig {
    pub songs: Option<Listable<ReferencableOperation>>,
    pub folders: Option<Listable<ReferencableOperation>>,
    pub this: Option<Listable<ReferencableOperation>>,
    pub discs: Option<HashMap<u32, ItemSelector>>,
    pub order: Option<ItemSelector>,
    #[serde(rename = "set fields")]
    pub set_fields: Option<Vec<RawFieldSetter>>,
    #[serde(rename = "set all")]
    pub set_all: Option<Vec<RawAllSetter>>,
    pub set: Option<HashMap<PathBuf, Listable<ReferencableOperation>>>,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ReferencableOperation {
    Reference(String),
    Direct(MetadataOperation),
}

#[derive(Deserialize, Serialize)]
pub struct RawAllSetter {
    pub names: ItemSelector,
    pub set: Listable<ReferencableOperation>,
}

#[derive(Deserialize, Serialize)]
pub struct RawFieldSetter {
    pub field: MetadataField,
    pub set: HashMap<PathBuf, ValueGetter>,
}

pub struct SongConfig {
    pub set: Vec<AllSetter>,
}

pub struct AllSetter {
    pub names: ItemSelector,
    pub must_be: Option<MusicItemType>,
    pub set: Vec<Rc<MetadataOperation>>,
}
impl AllSetter {
    pub fn new(selector: ItemSelector, set: MetadataOperation) -> Self {
        Self {
            names: selector,
            must_be: None,
            set: vec![Rc::new(set)],
        }
    }
    pub fn apply(&self, metadata: &mut PendingMetadata, path: &Path, config: &LibraryConfig) {
        for op in &self.set {
            op.apply(metadata, path, config);
        }
    }
}
