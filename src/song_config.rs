use std::{collections::HashMap, path::PathBuf};

use serde::{Deserialize, Serialize};

use crate::{strategy::{ItemSelector, MetadataOperation}, util::Borrowable};

#[derive(Deserialize, Serialize)]
pub struct RawSongConfig<'a> {
    pub songs: Option<ReferencableOperation<'a>>,
    pub set: Option<HashMap<PathBuf, ReferencableOperation<'a>>>,
    #[serde(rename = "set all")]
    pub set_all: Option<Vec<RawAllSetter<'a>>>,
    pub order: Option<ItemSelector>,
    pub discs: Option<HashMap<u32, ItemSelector>>,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
pub enum ReferencableOperation<'a> {
    Reference(String),
    Sequence(Vec<ReferencableOperation<'a>>),
    Direct(MetadataOperation<'a>),
}

#[derive(Deserialize, Serialize)]
pub struct RawAllSetter<'a> {
    pub names: ItemSelector,
    pub set: ReferencableOperation<'a>,
}

pub struct SongConfig<'a> {
    pub set: Vec<AllSetter<'a>>,
}

pub struct AllSetter<'a> {
    pub names: ItemSelector,
    pub set: Borrowable<'a, MetadataOperation<'a>>,
}
