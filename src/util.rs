use std::path::{PathBuf, Path};

use serde::{Deserialize, Serialize};

use crate::strategy::MusicItemType;

#[derive(Serialize, Clone)]
pub struct Range {
    pub start: i32,
    pub stop: i32,
}
impl<'de> Deserialize<'de> for Range {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Start,
            Stop,
        }

        struct RangeVisitor;

        impl<'de> serde::de::Visitor<'de> for RangeVisitor {
            type Value = Range;

            fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
                formatter.write_str("Range")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Range, V::Error>
            where
                V: serde::de::SeqAccess<'de>,
            {
                let start = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;
                let stop = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::invalid_length(1, &self))?;
                Ok(Range::new(start, stop))
            }

            fn visit_map<V>(self, mut map: V) -> Result<Range, V::Error>
            where
                V: serde::de::MapAccess<'de>,
            {
                let mut start = None;
                let mut stop = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Start => {
                            if start.is_some() {
                                return Err(serde::de::Error::duplicate_field("start"));
                            }
                            start = Some(map.next_value()?);
                        }
                        Field::Stop => {
                            if stop.is_some() {
                                return Err(serde::de::Error::duplicate_field("stop"));
                            }
                            stop = Some(map.next_value()?);
                        }
                    }
                }
                Ok(Range::new(start.unwrap_or(0), stop.unwrap_or(-1)))
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok((v as i32).into())
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok((v as i32).into())
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match v {
                    "all" => Ok(Range::from_start(0)),
                    "first" => Ok(0.into()),
                    "last" => Ok((-1).into()),
                    _ => Err(serde::de::Error::unknown_variant(
                        v,
                        &["all", "first", "last"],
                    )),
                }
            }
        }

        deserializer.deserialize_any(RangeVisitor)
    }
}

impl Range {
    pub fn new(start: i32, stop: i32) -> Self {
        Self { start, stop }
    }
    pub fn from_start(start: i32) -> Self {
        Self { start, stop: -1 }
    }
}
impl From<i32> for Range {
    fn from(value: i32) -> Self {
        Self {
            start: value,
            stop: value,
        }
    }
}

#[derive(Deserialize, Serialize, Clone, Copy)]
#[serde(rename_all = "lowercase")]
pub enum OutOfBoundsDecision {
    Exit,
    Clamp,
}
impl Range {
    pub fn in_range(&self, index: usize, length: usize, decision: OutOfBoundsDecision) -> bool {
        match self.to_range(length, decision) {
            None => false,
            Some(range) => range.contains(&index),
        }
    }
    pub fn slice<'a, T>(&self, items: &'a [T], decision: OutOfBoundsDecision) -> &'a [T] {
        match self.to_range(items.len(), decision) {
            Some(range) => &items[range],
            None => &[],
        }
    }
    pub fn to_range(
        &self,
        length: usize,
        decision: OutOfBoundsDecision,
    ) -> Option<std::ops::RangeInclusive<usize>> {
        self.wrap_both(length, decision)
            .map(|(start, stop)| start..=stop)
    }
    pub fn wrap_both(
        &self,
        length: usize,
        decision: OutOfBoundsDecision,
    ) -> Option<(usize, usize)> {
        Option::zip(
            Self::wrap(self.start, length, decision),
            Self::wrap(self.stop, length, decision),
        )
    }
    pub fn wrap(index: i32, length: usize, decision: OutOfBoundsDecision) -> Option<usize> {
        let result = if index >= 0 {
            index as usize
        } else {
            ((length as i32) + index) as usize
        };
        match decision {
            OutOfBoundsDecision::Exit => (result < length).then_some(result),
            OutOfBoundsDecision::Clamp => Some(std::cmp::min(result, length - 1)),
        }
    }
}

#[derive(Clone)]
pub enum ItemPath {
    Song(PathBuf),
    Folder(PathBuf),
}
impl ItemPath {
    pub fn as_type(&self) -> MusicItemType {
        match self {
            Self::Song(_) => MusicItemType::Song,
            Self::Folder(_) => MusicItemType::Folder,
        }
    }
}
impl From<ItemPath> for PathBuf {
    fn from(value: ItemPath) -> Self {
        match value {
            ItemPath::Song(path) | ItemPath::Folder(path) => path,
        }
    }
}
impl AsRef<Path> for ItemPath {
    fn as_ref(&self) -> &Path {
        match self {
            Self::Song(path) | Self::Folder(path) => path,
        }
    }
}