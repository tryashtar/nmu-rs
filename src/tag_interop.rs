use std::{collections::HashMap, rc::Rc};

use id3::{
    frame::{Lyrics, SynchronisedLyrics},
    TagLike,
};
use image::DynamicImage;
use itertools::Itertools;

use crate::{
    lyrics::{RichLyrics, SyncedLine, SyncedLyrics},
    metadata::{Metadata, MetadataField, MetadataValue},
};

pub enum SetValue<T> {
    Keep,
    Remove,
    Replace(T),
}

pub struct FullMetadata {
    pub normal: Metadata,
    pub art: SetValue<Rc<DynamicImage>>,
    pub lyrics: LyricsMetadata,
}

pub struct LyricsMetadata {
    pub rich: SetValue<RichLyrics>,
    pub synced: SetValue<SyncedLyrics>,
    pub simple: SetValue<String>,
}
impl LyricsMetadata {
    pub const fn keep() -> LyricsMetadata {
        LyricsMetadata {
            rich: SetValue::Keep,
            synced: SetValue::Keep,
            simple: SetValue::Keep,
        }
    }
}

pub fn get_metadata_flac(tag: &metaflac::Tag) -> FullMetadata {
    FullMetadata {
        normal: HashMap::new(),
        art: SetValue::Keep,
        lyrics: LyricsMetadata::keep(),
    }
}
pub fn set_metadata_flac(tag: &mut metaflac::Tag, metadata: &FullMetadata) {}
pub fn get_metadata_id3(tag: &id3::Tag, sep: &str) -> FullMetadata {
    FullMetadata {
        normal: HashMap::new(),
        art: SetValue::Keep,
        lyrics: LyricsMetadata::keep(),
    }
}
fn convert_id3_synced_lyrics(lyrics: &SynchronisedLyrics) -> Vec<SyncedLine> {
    match lyrics.timestamp_format {
        id3::frame::TimestampFormat::Mpeg => lyrics
            .content
            .iter()
            .map(|(_, y)| SyncedLine {
                timestamp: std::time::Duration::ZERO,
                text: y.to_owned(),
            })
            .collect(),
        id3::frame::TimestampFormat::Ms => lyrics
            .content
            .iter()
            .map(|(x, y)| SyncedLine {
                timestamp: std::time::Duration::from_millis(u64::from(*x)),
                text: y.to_owned(),
            })
            .collect(),
    }
}
pub fn set_metadata_id3(tag: &mut id3::Tag, metadata: &FullMetadata, sep: &str) {}

fn flac_set(comment: &mut metaflac::block::VorbisComment, field: &str, value: Option<String>) {
    match value {
        None => comment.remove(field),
        Some(value) => comment.set(field, vec![value]),
    };
}

fn flac_set_number(comment: &mut metaflac::block::VorbisComment, field: &str, value: Option<u32>) {
    match value {
        None => comment.remove(field),
        Some(value) => comment.set(field, vec![value.to_string()]),
    };
}

fn flac_set_list(comment: &mut metaflac::block::VorbisComment, field: &str, value: Vec<String>) {
    if value.is_empty() {
        comment.remove(field);
    } else {
        comment.set(field, value);
    }
}

fn id3_comments(item: Vec<&id3::frame::Comment>) -> Vec<String> {
    item.into_iter().map(|x| x.text.clone()).collect()
}

fn id3_lyrics(item: Vec<&id3::frame::Lyrics>) -> Option<String> {
    if item.is_empty() {
        None
    } else {
        Some(item.into_iter().map(|x| x.text.clone()).join("\n"))
    }
}

fn id3_str(item: Option<&str>) -> Option<String> {
    item.map(|x| x.replace('\0', "/"))
}

fn id3_str_sep(item: Option<&str>, separator: &str) -> Vec<String> {
    item.map(|x| x.split(separator).map(|y| y.replace('\0', "/")).collect())
        .unwrap_or_default()
}
