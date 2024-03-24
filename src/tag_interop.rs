use id3::frame::SynchronisedLyrics;

use crate::lyrics::{ParseError, RichLyrics, SyncedLine, SyncedLyrics};

pub enum SetValue<T> {
    Keep,
    Remove,
    Replace(T),
}

#[derive(thiserror::Error, Debug)]
pub enum GetLyricsError {
    #[error("Lyrics not present in tag")]
    NotEmbedded,
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Json(#[from] serde_json::Error),
    #[error("{0}")]
    Parsing(#[from] ParseError),
}

pub fn get_id3_simple_lyrics(tag: &id3::Tag) -> Result<String, GetLyricsError> {
    let lyrics = tag.lyrics().map(|x| x.text.clone()).collect::<Vec<_>>();
    if lyrics.is_empty() {
        Err(GetLyricsError::NotEmbedded)
    } else {
        Ok(lyrics.join("\n"))
    }
}

pub fn get_id3_synced_lyrics(tag: &id3::Tag) -> Result<SyncedLyrics, GetLyricsError> {
    let lyrics = tag
        .synchronised_lyrics()
        .flat_map(convert_id3_synced_lyrics)
        .collect::<Vec<_>>();
    if lyrics.is_empty() {
        Err(GetLyricsError::NotEmbedded)
    } else {
        Ok(SyncedLyrics { lines: lyrics })
    }
}

pub fn get_id3_rich_lyrics(tag: &id3::Tag) -> Result<RichLyrics, GetLyricsError> {
    Ok(tag
        .frames()
        .filter_map(|frame| frame.content().extended_text())
        .filter(|x| x.description == "RICH LYRICS")
        .map(|x| serde_json::from_str::<RichLyrics>(&x.value))
        .next()
        .ok_or(GetLyricsError::NotEmbedded)??)
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
