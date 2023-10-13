use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

use serde::{Deserialize, Serialize};

use crate::{
    library_config::LibraryConfig,
    metadata::{MetadataField, PendingMetadata},
    strategy::{ItemSelector, MetadataOperation, MusicItemType, ValueGetter}, util::ItemPath,
};

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RawSongConfig {
    pub songs: Option<ReferencableOperation>,
    pub folders: Option<ReferencableOperation>,
    pub this: Option<ReferencableOperation>,
    pub discs: Option<HashMap<u32, ItemSelector>>,
    pub order: Option<ItemSelector>,
    #[serde(rename = "set fields")]
    pub set_fields: Option<Vec<RawFieldSetter>>,
    #[serde(rename = "set all")]
    pub set_all: Option<Vec<RawAllSetter>>,
    pub set: Option<HashMap<PathBuf, ReferencableOperation>>,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ReferencableOperation {
    Reference(String),
    Direct(MetadataOperation),
    Many(Vec<ReferencableOperation>),
}

#[derive(Deserialize, Serialize)]
pub struct RawAllSetter {
    pub names: ItemSelector,
    pub set: ReferencableOperation,
}

#[derive(Deserialize, Serialize)]
pub struct RawFieldSetter {
    pub field: MetadataField,
    pub set: HashMap<PathBuf, ValueGetter>,
}

pub struct SongConfig {
    pub set: Vec<AllSetter>,
}
impl SongConfig {
    pub fn apply(
        &self,
        nice_path: &ItemPath,
        select: &Path,
        metadata: &mut PendingMetadata,
        library_config: &LibraryConfig,
    ) {
        for setter in &self.set {
            if setter.names.matches(select)
                && MusicItemType::matches(&nice_path.as_type(), setter.must_be.as_ref())
            {
                setter
                    .set
                    .apply(metadata, nice_path.as_ref(), library_config);
            }
        }
    }
}

pub struct AllSetter {
    pub names: ItemSelector,
    pub must_be: Option<MusicItemType>,
    pub set: Rc<MetadataOperation>,
}
impl AllSetter {
    pub fn new(selector: ItemSelector, set: MetadataOperation) -> Self {
        Self {
            names: selector,
            must_be: None,
            set: Rc::new(set),
        }
    }
}
