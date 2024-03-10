use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    rc::Rc,
};

use serde::{Deserialize, Serialize};

use crate::{
    library_config::LibraryConfig,
    metadata::{Metadata, MetadataField, MetadataValue},
    strategy::{ApplyReport, ItemSelector, MetadataOperation, MusicItemType, ValueGetter},
    util::ItemPath,
    CopyCache,
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
    pub set: Vec<Rc<AllSetter>>,
    pub order: Option<OrderingSetter>,
}
pub enum OrderingSetter {
    Order {
        map: HashMap<PathBuf, u32>,
        total: u32,
        original_selector: ItemSelector,
    },
    Discs {
        map: HashMap<PathBuf, DiscSet>,
        disc_total: u32,
        original_selectors: Vec<ItemSelector>,
    },
}
pub struct DiscSet {
    pub disc: u32,
    pub track: u32,
    pub track_total: u32,
}
impl SongConfig {
    pub fn apply(
        &self,
        nice_path: &ItemPath,
        select: &Path,
        metadata: &mut Metadata,
        library_config: &LibraryConfig,
        copy_cache: &CopyCache,
    ) -> ApplyReport {
        let mut report = ApplyReport { errors: vec![] };
        if let Some(order) = &self.order {
            match order {
                OrderingSetter::Order { map, total, .. } => {
                    if let Some(track) = map.get(select) {
                        metadata.insert(MetadataField::Track, MetadataValue::Number(*track));
                        metadata.insert(MetadataField::TrackTotal, MetadataValue::Number(*total));
                    }
                }
                OrderingSetter::Discs {
                    map, disc_total, ..
                } => {
                    if let Some(values) = map.get(select) {
                        metadata.insert(MetadataField::Disc, MetadataValue::Number(values.disc));
                        metadata
                            .insert(MetadataField::DiscTotal, MetadataValue::Number(*disc_total));
                        metadata.insert(MetadataField::Track, MetadataValue::Number(values.track));
                        metadata.insert(
                            MetadataField::TrackTotal,
                            MetadataValue::Number(values.track_total),
                        );
                    }
                }
            }
        }
        for setter in &self.set {
            if setter.names.matches(select)
                && MusicItemType::matches(nice_path.as_type(), setter.must_be)
            {
                let more =
                    setter
                        .set
                        .apply(metadata, nice_path.as_ref(), library_config, copy_cache);
                report.merge(more);
            }
        }
        report
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
