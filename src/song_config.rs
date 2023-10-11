use std::{collections::HashMap, path::{PathBuf, Path}, rc::Rc};

use serde::{Deserialize, Serialize};

use crate::{
    strategy::{ItemSelector, MetadataOperation},
    util::Listable, metadata::PendingMetadata, library_config::LibraryConfig,
};

#[derive(Deserialize, Serialize)]
pub struct RawSongConfig {
    pub songs: Option<Listable<ReferencableOperation>>,
    pub set: Option<HashMap<PathBuf, Listable<ReferencableOperation>>>,
    #[serde(rename = "set all")]
    pub set_all: Option<Vec<RawAllSetter>>,
    pub order: Option<ItemSelector>,
    pub discs: Option<HashMap<u32, ItemSelector>>,
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

pub struct SongConfig {
    pub set: Vec<AllSetter>,
}

pub struct AllSetter {
    pub names: ItemSelector,
    pub set: Vec<Rc<MetadataOperation>>,
}
impl AllSetter {
    pub fn new(selector: ItemSelector, set: MetadataOperation) -> Self {
        Self {
            names: selector,
            set: vec![Rc::new(set)],
        }
    }
    pub fn apply(&self, metadata: &mut PendingMetadata, path: &Path, config: &LibraryConfig) {
        for op in &self.set {
            op.apply(metadata, path, config);
        }
    }
}
