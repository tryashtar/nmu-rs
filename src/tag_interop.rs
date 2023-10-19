use id3::TagLike;
use itertools::Itertools;
use metaflac::Block;

use crate::metadata::{FinalMetadata, SetValue};

pub fn get_metadata_flac(tag: &metaflac::Tag) -> FinalMetadata {
    let mut metadata = FinalMetadata {
        title: SetValue::Skip,
        album: SetValue::Skip,
        performers: SetValue::Skip,
        album_artist: SetValue::Skip,
        composers: SetValue::Skip,
        arranger: SetValue::Skip,
        comments: SetValue::Skip,
        track: SetValue::Skip,
        track_total: SetValue::Skip,
        disc: SetValue::Skip,
        disc_total: SetValue::Skip,
        year: SetValue::Skip,
        language: SetValue::Skip,
        genres: SetValue::Skip,
        art: SetValue::Skip,
        simple_lyrics: SetValue::Skip,
    };
    for block in tag.blocks() {
        if let Block::VorbisComment(comment) = block {
            if let SetValue::Set(v) = flac_str(comment.title()) {
                metadata.title = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_str(comment.album()) {
                metadata.album = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_list(comment.artist()) {
                metadata.performers = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_str(comment.album_artist()) {
                metadata.album_artist = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_list(comment.get("COMPOSER")) {
                metadata.composers = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_str(comment.get("REMIXEDBY")) {
                metadata.arranger = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_list(comment.get("COMMENT")) {
                metadata.comments = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_num(comment.track()) {
                metadata.track = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_num_str(comment.get("TRACKTOTAL")) {
                metadata.track_total = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_num_str(comment.get("DISCNUMBER")) {
                metadata.disc = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_num_str(comment.get("DISCTOTAL")) {
                metadata.disc_total = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_num_str(comment.get("YEAR")) {
                metadata.year = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_str(comment.get("LANGUAGE")) {
                metadata.language = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_list(comment.genre()) {
                metadata.genres = SetValue::Set(v);
            }
            if let SetValue::Set(v) = flac_list(comment.get("UNSYNCED LYRICS")) {
                metadata.simple_lyrics = SetValue::Set(Some(v.join("\n")));
            }
        }
    }
    metadata
}
pub fn get_metadata_id3(tag: &id3::Tag, sep: &str) -> FinalMetadata {
    FinalMetadata {
        title: SetValue::Set(id3_str(tag.title())),
        album: SetValue::Set(id3_str(tag.album())),
        performers: SetValue::Set(id3_str_sep(tag.artist(), sep)),
        album_artist: SetValue::Set(id3_str(tag.album_artist())),
        composers: SetValue::Set(id3_str_sep(tag.text_for_frame_id("TCOM"), sep)),
        arranger: SetValue::Set(id3_str(tag.text_for_frame_id("TPE4"))),
        comments: SetValue::Set(id3_comments(tag.comments().collect())),
        track: SetValue::Set(tag.track()),
        track_total: SetValue::Set(tag.total_tracks()),
        disc: SetValue::Set(tag.disc()),
        disc_total: SetValue::Set(tag.total_discs()),
        year: SetValue::Set(tag.year().map(|x| x as u32)),
        language: SetValue::Set(id3_str(tag.text_for_frame_id("TLAN"))),
        genres: SetValue::Set(id3_str_sep(tag.genre(), sep)),
        art: SetValue::Skip,
        simple_lyrics: SetValue::Set(id3_lyrics(tag.lyrics().collect())),
    }
}

fn id3_comments(item: Vec<&id3::frame::Comment>) -> Vec<String> {
    item.into_iter().map(|x| x.text.clone()).collect()
}

fn id3_lyrics(item: Vec<&id3::frame::Lyrics>) -> Option<String> {
    if item.is_empty() {
        None
    } else {
        Some(item.into_iter().map(|x| x.text.to_owned()).join("\n"))
    }
}

fn id3_str(item: Option<&str>) -> Option<String> {
    item.map(|x| x.replace('\0', "/"))
}

fn id3_str_sep(item: Option<&str>, separator: &str) -> Vec<String> {
    item.map(|x| x.split(separator).map(|y| y.replace('\0', "/")).collect())
        .unwrap_or_default()
}

fn flac_num(item: Option<u32>) -> SetValue<Option<u32>> {
    match item {
        None => SetValue::Skip,
        Some(value) => SetValue::Set(Some(value)),
    }
}

fn flac_num_str(item: Option<&Vec<String>>) -> SetValue<Option<u32>> {
    match item {
        None => SetValue::Skip,
        Some(list) if list.is_empty() => SetValue::Set(None),
        Some(list) => SetValue::Set(list[0].parse::<u32>().ok()),
    }
}

fn flac_str(item: Option<&Vec<String>>) -> SetValue<Option<String>> {
    match item {
        None => SetValue::Skip,
        Some(list) if list.is_empty() => SetValue::Set(None),
        Some(list) => SetValue::Set(Some(list[0].clone())),
    }
}

fn flac_list(item: Option<&Vec<String>>) -> SetValue<Vec<String>> {
    match item {
        None => SetValue::Skip,
        Some(list) => SetValue::Set(list.clone()),
    }
}
