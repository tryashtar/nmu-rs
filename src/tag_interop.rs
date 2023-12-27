use id3::TagLike;
use itertools::Itertools;

use crate::{
    lyrics::SyncedLyrics,
    metadata::{FinalMetadata, SetValue},
};

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
        synced_lyrics: SetValue::Skip,
        rich_lyrics: SetValue::Skip,
    };
    if let Some(comment) = tag.vorbis_comments() {
        if let SetValue::Set(v) = flac_str(comment.get("TITLE")) {
            metadata.title = SetValue::Set(v);
        }
        if let SetValue::Set(v) = flac_str(comment.get("ALBUM")) {
            metadata.album = SetValue::Set(v);
        }
        if let SetValue::Set(v) = flac_list(comment.get("ARTIST")) {
            metadata.performers = SetValue::Set(v);
        }
        if let SetValue::Set(v) = flac_str(comment.get("ALBUMARTIST")) {
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
        if let SetValue::Set(v) = flac_num_str(comment.get("TRACKNUMBER")) {
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
        if let SetValue::Set(v) = flac_list(comment.get("GENRE")) {
            metadata.genres = SetValue::Set(v);
        }
        if let SetValue::Set(v) = flac_list(comment.get("UNSYNCED LYRICS")) {
            metadata.simple_lyrics = SetValue::Set(Some(v.join("\n")));
        }
        if let SetValue::Set(v) = flac_list(comment.get("LYRICS")) {
            if let Ok(lyrics) = SyncedLyrics::parse(v) {
                metadata.synced_lyrics = SetValue::Set(Some(lyrics));
            }
        }
        if let SetValue::Set(v) = flac_str(comment.get("RICH LYRICS")) {
            metadata.rich_lyrics = SetValue::Set(v.and_then(|x| serde_json::de::from_str(&x).ok()))
        }
    }
    metadata
}
pub fn set_metadata_flac(tag: &mut metaflac::Tag, metadata: &FinalMetadata) {
    let comment = tag.vorbis_comments_mut();
    if let SetValue::Set(v) = &metadata.title {
        flac_set(comment, "TITLE", v.clone());
    }
    if let SetValue::Set(v) = &metadata.album {
        flac_set(comment, "ALBUM", v.clone());
    }
    if let SetValue::Set(v) = &metadata.performers {
        flac_set_list(comment, "ARTIST", v.clone());
    }
    if let SetValue::Set(v) = &metadata.album_artist {
        flac_set(comment, "ALBUMARTIST", v.clone());
    }
    if let SetValue::Set(v) = &metadata.composers {
        flac_set_list(comment, "COMPOSER", v.clone());
    }
    if let SetValue::Set(v) = &metadata.arranger {
        flac_set(comment, "REMIXEDBY", v.clone());
    }
    if let SetValue::Set(v) = &metadata.comments {
        flac_set_list(comment, "COMMENT", v.clone());
    }
    if let SetValue::Set(v) = &metadata.track {
        flac_set_number(comment, "TRACK", *v);
    }
    if let SetValue::Set(v) = &metadata.track_total {
        flac_set_number(comment, "TRACKTOTAL", *v);
    }
    if let SetValue::Set(v) = &metadata.disc {
        flac_set_number(comment, "DISCNUMBER", *v);
    }
    if let SetValue::Set(v) = &metadata.disc_total {
        flac_set_number(comment, "DISCTOTAL", *v);
    }
    if let SetValue::Set(v) = &metadata.year {
        flac_set_number(comment, "YEAR", *v);
    }
    if let SetValue::Set(v) = &metadata.language {
        flac_set(comment, "LANGUAGE", v.clone());
    }
    if let SetValue::Set(v) = &metadata.genres {
        flac_set_list(comment, "GENRE", v.clone());
    }
    if let SetValue::Set(v) = &metadata.simple_lyrics {
        flac_set(comment, "UNSYNCED LYRICS", v.clone());
    }
    if let SetValue::Set(v) = &metadata.synced_lyrics {
        flac_set_list(
            comment,
            "LYRICS",
            match v {
                None => vec![],
                Some(lyrics) => lyrics.lines.iter().map(|x| x.to_str()).collect(),
            },
        );
    }
    if let SetValue::Set(v) = &metadata.rich_lyrics {
        flac_set(
            comment,
            "RICH LYRICS",
            v.as_ref().and_then(|x| serde_json::ser::to_string(&x).ok()),
        );
    }
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
        synced_lyrics: SetValue::Skip,
        rich_lyrics: SetValue::Skip,
    }
}
pub fn set_metadata_id3(tag: &mut id3::Tag, metadata: &FinalMetadata, sep: &str) {
    if let SetValue::Set(v) = &metadata.title {
        match v {
            None => tag.remove_title(),
            Some(v) => tag.set_title(v),
        };
    }
    if let SetValue::Set(v) = &metadata.album {
        match v {
            None => tag.remove_album(),
            Some(v) => tag.set_album(v),
        };
    }
    if let SetValue::Set(v) = &metadata.performers {
        if v.is_empty() {
            tag.remove_artist();
        } else {
            tag.set_artist(v.join(sep));
        }
    }
    if let SetValue::Set(v) = &metadata.album_artist {
        match v {
            None => tag.remove_album_artist(),
            Some(v) => tag.set_album_artist(v),
        };
    }
    if let SetValue::Set(v) = &metadata.composers {
        if v.is_empty() {
            tag.remove("TCOM");
        } else {
            tag.set_text("TCOM", v.join(sep));
        }
    }
    if let SetValue::Set(v) = &metadata.arranger {
        match v {
            None => {
                tag.remove("TPE4");
            }
            Some(v) => {
                tag.set_text("TPE4", v);
            }
        };
    }
    if let SetValue::Set(v) = &metadata.comments {
        if v.is_empty() {
            tag.remove("COMM");
        } else {
            let lang = match &metadata.language {
                SetValue::Skip => id3_str(tag.text_for_frame_id("TLAN")),
                SetValue::Set(val) => val.clone(),
            }
            .unwrap_or_default();
            for comment in v {
                tag.add_frame(id3::frame::Comment {
                    lang: lang.clone(),
                    description: String::from(""),
                    text: comment.to_owned(),
                });
            }
        }
    }
    if let SetValue::Set(v) = &metadata.track {
        match v {
            None => tag.remove_track(),
            Some(v) => tag.set_track(*v),
        };
    }
    if let SetValue::Set(v) = &metadata.track_total {
        match v {
            None => tag.remove_total_tracks(),
            Some(v) => tag.set_total_tracks(*v),
        };
    }
    if let SetValue::Set(v) = &metadata.disc {
        match v {
            None => tag.remove_disc(),
            Some(v) => tag.set_disc(*v),
        };
    }
    if let SetValue::Set(v) = &metadata.disc_total {
        match v {
            None => tag.remove_total_discs(),
            Some(v) => tag.set_total_discs(*v),
        };
    }
    if let SetValue::Set(v) = &metadata.year {
        match v {
            None => tag.remove_year(),
            Some(v) => tag.set_year(*v as i32),
        };
    }
    if let SetValue::Set(v) = &metadata.language {
        match v {
            None => {
                tag.remove("TLAN");
            }
            Some(v) => {
                tag.set_text("TLAN", v);
            }
        };
    }
    if let SetValue::Set(v) = &metadata.genres {
        if v.is_empty() {
            tag.remove_genre();
        } else {
            tag.set_genre(v.join(sep));
        }
    }
}

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
