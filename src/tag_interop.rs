use std::path::Path;

use id3::TagLike;
use metaflac::{block::VorbisComment, Block};
use strum::IntoEnumIterator;

use crate::song_config::{BuiltinMetadataField, Metadata, MetadataValue};

pub struct Tags {
    flac: Option<metaflac::Tag>,
    id3: Option<id3::Tag>,
}
impl Tags {
    pub fn load(path: &Path) -> Self {
        let flac = metaflac::Tag::read_from_path(path).ok();
        let id3 = id3::Tag::read_from_path(path).ok();
        Self { flac, id3 }
    }
    pub fn get_metadata(&self) -> Metadata {
        let mut metadata = Metadata::new();
        if let Some(flac) = &self.flac {
            for block in flac.blocks() {
                if let Block::VorbisComment(comment) = block {
                    for field in BuiltinMetadataField::iter() {
                        if let Some(value) = get_flac(comment, &field) {
                            metadata.fields.insert(field.into(), value);
                        }
                    }
                }
            }
        }
        if let Some(id3) = &self.id3 {
            for field in BuiltinMetadataField::iter() {
                if let Some(value) = get_id3(id3, &field) {
                    metadata.fields.insert(field.into(), value);
                }
            }
        }
        metadata
    }
}

fn get_id3(tag: &id3::Tag, field: &BuiltinMetadataField) -> Option<MetadataValue> {
    match field {
        BuiltinMetadataField::Album => convert_str(tag.album()),
        BuiltinMetadataField::AlbumArtists => convert_str(tag.album_artist()),
        BuiltinMetadataField::Arranger => convert_str(tag.text_for_frame_id("TPE4")),
        BuiltinMetadataField::Comment => convert_comments(tag.comments().collect()),
        BuiltinMetadataField::Composers => convert_list_str(tag.text_values_for_frame_id("TCOM")),
        BuiltinMetadataField::Track => tag.track().map(MetadataValue::Number),
        BuiltinMetadataField::TrackTotal => tag.total_tracks().map(MetadataValue::Number),
        BuiltinMetadataField::Title => convert_str(tag.title()),
        BuiltinMetadataField::Language => convert_str(tag.text_for_frame_id("TLAN")),
        BuiltinMetadataField::Genres => convert_list_str(tag.genres()),
        BuiltinMetadataField::Performers => convert_list_str(tag.artists()),
        BuiltinMetadataField::Year => tag.year().map(|x| MetadataValue::Number(x as u32)),
        BuiltinMetadataField::Disc => tag.disc().map(MetadataValue::Number),
        BuiltinMetadataField::DiscTotal => tag.total_discs().map(MetadataValue::Number),
        BuiltinMetadataField::SimpleLyrics => convert_lyrics(tag.lyrics().collect()),
        BuiltinMetadataField::Art => None,
    }
}

fn get_flac(tag: &VorbisComment, field: &BuiltinMetadataField) -> Option<MetadataValue> {
    match field {
        BuiltinMetadataField::Album => convert_list(tag.album()),
        BuiltinMetadataField::AlbumArtists => convert_list(tag.album_artist()),
        BuiltinMetadataField::Arranger => convert_list(tag.get("REMIXEDBY")),
        BuiltinMetadataField::Comment => convert_list(tag.get("COMMENT")),
        BuiltinMetadataField::Composers => convert_list(tag.get("COMPOSER")),
        BuiltinMetadataField::Track => tag.track().map(MetadataValue::Number),
        BuiltinMetadataField::TrackTotal => tag.total_tracks().map(MetadataValue::Number),
        BuiltinMetadataField::Title => convert_list(tag.title()),
        BuiltinMetadataField::Language => convert_list(tag.get("LANGUAGE")),
        BuiltinMetadataField::Genres => convert_list(tag.genre()),
        BuiltinMetadataField::Performers => convert_list(tag.artist()),
        BuiltinMetadataField::Year => convert_num(tag.get("YEAR")),
        BuiltinMetadataField::Disc => convert_num(tag.get("DISCNUMBER")),
        BuiltinMetadataField::DiscTotal => convert_num(tag.get("DISCTOTAL")),
        BuiltinMetadataField::SimpleLyrics => convert_list(tag.lyrics()),
        BuiltinMetadataField::Art => None,
    }
}

fn convert_comments(item: Vec<&id3::frame::Comment>) -> Option<MetadataValue> {
    Some(item.into_iter().map(|x| x.text.clone()).collect::<Vec<_>>())
        .and_then(MetadataValue::from_list)
}

fn convert_lyrics(item: Vec<&id3::frame::Lyrics>) -> Option<MetadataValue> {
    Some(item.into_iter().map(|x| x.text.clone()).collect::<Vec<_>>())
        .and_then(MetadataValue::from_list)
}

fn convert_str(item: Option<&str>) -> Option<MetadataValue> {
    item.map(|x| MetadataValue::String(x.to_owned()))
}

fn convert_num(item: Option<&Vec<String>>) -> Option<MetadataValue> {
    item.and_then(|s| {
        if !s.is_empty() {
            s[0].parse::<u32>().ok()
        } else {
            None
        }
    })
    .map(MetadataValue::Number)
}

fn convert_list_str(item: Option<Vec<&str>>) -> Option<MetadataValue> {
    item.map(|x| x.into_iter().map(|y| y.to_owned()).collect())
        .and_then(MetadataValue::from_list)
}

fn convert_list(item: Option<&Vec<String>>) -> Option<MetadataValue> {
    item.and_then(|x| MetadataValue::from_list(x.clone()))
}
