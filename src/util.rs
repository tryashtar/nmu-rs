use std::{
    collections::BTreeSet,
    ops::Deref,
    path::{Path, PathBuf},
};

use serde::{ser::SerializeTuple, Deserialize, Serialize};

use crate::strategy::MusicItemType;

#[derive(Clone)]
pub struct Range {
    pub start: i32,
    pub stop: i32,
}
impl Serialize for Range {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if self.start == self.stop {
            serializer.serialize_i32(self.stop)
        } else {
            let mut seq = serializer.serialize_tuple(2)?;
            seq.serialize_element(&self.start)?;
            seq.serialize_element(&self.stop)?;
            seq.end()
        }
    }
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
    pub fn slice<'a, T>(&self, items: &'a [T], decision: OutOfBoundsDecision) -> Option<&'a [T]> {
        self.to_range(items.len(), decision)
            .map(|range| &items[range])
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

#[derive(Clone, PartialEq, Eq, Hash)]
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
impl Deref for ItemPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Song(path) | Self::Folder(path) => path,
        }
    }
}

pub fn string_or_seq_string<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    struct StringOrVec(std::marker::PhantomData<Vec<String>>);

    impl<'de> serde::de::Visitor<'de> for StringOrVec {
        type Value = Vec<String>;

        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
            formatter.write_str("string or list of strings")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(vec![value.to_owned()])
        }

        fn visit_seq<S>(self, visitor: S) -> Result<Self::Value, S::Error>
        where
            S: serde::de::SeqAccess<'de>,
        {
            Deserialize::deserialize(serde::de::value::SeqAccessDeserializer::new(visitor))
        }
    }

    deserializer.deserialize_any(StringOrVec(std::marker::PhantomData))
}

pub fn path_or_seq_path<'de, D>(deserializer: D) -> Result<BTreeSet<PathBuf>, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    struct PathOrList(std::marker::PhantomData<BTreeSet<PathBuf>>);

    impl<'de> serde::de::Visitor<'de> for PathOrList {
        type Value = BTreeSet<PathBuf>;

        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
            formatter.write_str("string or list of strings")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            Ok(BTreeSet::from([PathBuf::from(value)]))
        }

        fn visit_seq<S>(self, visitor: S) -> Result<Self::Value, S::Error>
        where
            S: serde::de::SeqAccess<'de>,
        {
            Deserialize::deserialize(serde::de::value::SeqAccessDeserializer::new(visitor))
        }
    }

    deserializer.deserialize_any(PathOrList(std::marker::PhantomData))
}
