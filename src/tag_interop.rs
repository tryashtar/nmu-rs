use id3::{frame::SynchronisedLyrics, TagLike};
use itertools::Itertools;

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

pub trait GetLyrics {
    fn get_simple_lyrics(&self) -> Result<String, GetLyricsError>;
    fn remove_simple_lyrics(&mut self) -> Result<String, GetLyricsError>;
    fn set_simple_lyrics(&mut self, lyrics: String) -> Result<String, GetLyricsError>;
    fn get_synced_lyrics(&self) -> Result<SyncedLyrics, GetLyricsError>;
    fn remove_synced_lyrics(&mut self) -> Result<SyncedLyrics, GetLyricsError>;
    fn set_synced_lyrics(&mut self, lyrics: SyncedLyrics) -> Result<SyncedLyrics, GetLyricsError>;
    fn get_rich_lyrics(&self) -> Result<RichLyrics, GetLyricsError>;
    fn remove_rich_lyrics(&mut self) -> Result<RichLyrics, GetLyricsError>;
    fn set_rich_lyrics(&mut self, lyrics: RichLyrics) -> Result<RichLyrics, GetLyricsError>;
}

impl GetLyrics for id3::Tag {
    fn get_simple_lyrics(&self) -> Result<String, GetLyricsError> {
        let lyrics = self.lyrics().map(|x| x.text.clone()).collect::<Vec<_>>();
        if lyrics.is_empty() {
            Err(GetLyricsError::NotEmbedded)
        } else {
            Ok(lyrics.join("\n"))
        }
    }

    fn remove_simple_lyrics(&mut self) -> Result<String, GetLyricsError> {
        let frames = self.frames_vec_mut();
        let mut lyrics = vec![];
        let mut i = 0;
        while i < frames.len() {
            if let Some(entry) = frames[i].content().lyrics() {
                lyrics.push(entry.text.clone());
                frames.remove(i);
            } else {
                i += 1;
            }
        }
        if lyrics.is_empty() {
            Err(GetLyricsError::NotEmbedded)
        } else {
            Ok(lyrics.join("\n"))
        }
    }

    fn set_simple_lyrics(&mut self, lyrics: String) -> Result<String, GetLyricsError> {
        let result = self.remove_simple_lyrics();
        self.add_frame(id3::frame::Lyrics {
            text: lyrics,
            lang: String::from(""),
            description: String::from(""),
        });
        result
    }

    fn get_synced_lyrics(&self) -> Result<SyncedLyrics, GetLyricsError> {
        let lyrics = self
            .synchronised_lyrics()
            .flat_map(convert_id3_synced_lyrics)
            .collect::<Vec<_>>();
        if lyrics.is_empty() {
            Err(GetLyricsError::NotEmbedded)
        } else {
            Ok(SyncedLyrics { lines: lyrics })
        }
    }

    fn remove_synced_lyrics(&mut self) -> Result<SyncedLyrics, GetLyricsError> {
        let frames = self.frames_vec_mut();
        let mut lyrics = vec![];
        let mut i = 0;
        while i < frames.len() {
            if let Some(entry) = frames[i].content().synchronised_lyrics() {
                lyrics.append(&mut convert_id3_synced_lyrics(entry));
                frames.remove(i);
            } else {
                i += 1;
            }
        }
        if lyrics.is_empty() {
            Err(GetLyricsError::NotEmbedded)
        } else {
            Ok(SyncedLyrics { lines: lyrics })
        }
    }

    fn set_synced_lyrics(&mut self, lyrics: SyncedLyrics) -> Result<SyncedLyrics, GetLyricsError> {
        let result = self.remove_synced_lyrics();
        self.add_frame(id3::frame::SynchronisedLyrics {
            lang: String::from(""),
            timestamp_format: id3::frame::TimestampFormat::Ms,
            content_type: id3::frame::SynchronisedLyricsType::Lyrics,
            description: String::from(""),
            content: lyrics
                .lines
                .into_iter()
                .map(|x| (x.timestamp.as_millis() as u32, x.text))
                .collect(),
        });
        result
    }

    fn get_rich_lyrics(&self) -> Result<RichLyrics, GetLyricsError> {
        let mut lyrics = self.frames().filter_map(|frame| {
            frame
                .content()
                .extended_text()
                .filter(|x| x.description == "RICH LYRICS")
        });
        match lyrics.next() {
            None => Err(GetLyricsError::NotEmbedded),
            Some(lyrics) => Ok(serde_json::from_str::<RichLyrics>(&lyrics.value)?),
        }
    }

    fn remove_rich_lyrics(&mut self) -> Result<RichLyrics, GetLyricsError> {
        let frames = self.frames_vec_mut();
        let mut lyrics = Err(GetLyricsError::NotEmbedded);
        let mut i = 0;
        while i < frames.len() {
            if let Some(entry) = frames[i]
                .content()
                .extended_text()
                .filter(|x| x.description == "RICH LYRICS")
            {
                if lyrics.is_err() {
                    lyrics = serde_json::from_str::<RichLyrics>(&entry.value)
                        .map_err(GetLyricsError::Json);
                }
                frames.remove(i);
            } else {
                i += 1;
            }
        }
        lyrics
    }

    fn set_rich_lyrics(&mut self, lyrics: RichLyrics) -> Result<RichLyrics, GetLyricsError> {
        let result = self.remove_rich_lyrics();
        self.add_frame(id3::frame::ExtendedText {
            description: String::from("RICH LYRICS"),
            value: serde_json::to_string(&lyrics)?,
        });
        result
    }
}

impl GetLyrics for metaflac::Tag {
    fn get_simple_lyrics(&self) -> Result<String, GetLyricsError> {
        let lyrics = self
            .blocks()
            .filter_map(|block| match block {
                metaflac::Block::VorbisComment(comment) => Some(comment),
                _ => None,
            })
            .filter_map(|x| x.get("UNSYNCED LYRICS"))
            .flatten()
            .collect::<Vec<_>>();
        if lyrics.is_empty() {
            Err(GetLyricsError::NotEmbedded)
        } else {
            Ok(lyrics.into_iter().join("\n"))
        }
    }

    fn remove_simple_lyrics(&mut self) -> Result<String, GetLyricsError> {
        let lyrics = self
            .blocks()
            .filter_map(|block| match block {
                metaflac::Block::VorbisComment(comment) => Some(comment),
                _ => None,
            })
            .filter_map(|x| x.get("UNSYNCED LYRICS"))
            .collect::<Vec<_>>();
        match lyrics.len() {
            0 => Err(GetLyricsError::NotEmbedded),
            1 => {
                let result = lyrics[0].join("\n");
                self.vorbis_comments_mut().remove("UNSYNCED LYRICS");
                Ok(result)
            }
            2.. => {
                // psycho path
                let result = lyrics.into_iter().flatten().join("\n");
                let blocks = self
                    .blocks()
                    .filter_map(|block| match block {
                        metaflac::Block::VorbisComment(comment) => Some(comment.clone()),
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                self.remove_blocks(metaflac::BlockType::VorbisComment);
                for mut block in blocks {
                    block.remove("UNSYNCED LYRICS");
                    self.push_block(metaflac::Block::VorbisComment(block));
                }
                Ok(result)
            }
        }
    }

    fn set_simple_lyrics(&mut self, lyrics: String) -> Result<String, GetLyricsError> {
        let result = self.remove_simple_lyrics();
        self.set_vorbis("UNSYNCED LYRICS", lyrics.split('\n').collect());
        result
    }

    fn get_synced_lyrics(&self) -> Result<SyncedLyrics, GetLyricsError> {
        let lyrics = self
            .blocks()
            .filter_map(|block| match block {
                metaflac::Block::VorbisComment(comment) => Some(comment),
                _ => None,
            })
            .filter_map(|x| x.get("LYRICS"))
            .flatten()
            .cloned()
            .collect::<Vec<_>>();
        if lyrics.is_empty() {
            Err(GetLyricsError::NotEmbedded)
        } else {
            Ok(SyncedLyrics::parse(lyrics)?)
        }
    }

    fn remove_synced_lyrics(&mut self) -> Result<SyncedLyrics, GetLyricsError> {
        let lyrics = self
            .blocks()
            .filter_map(|block| match block {
                metaflac::Block::VorbisComment(comment) => Some(comment),
                _ => None,
            })
            .filter_map(|x| x.get("LYRICS"))
            .collect::<Vec<_>>();
        match lyrics.len() {
            0 => Err(GetLyricsError::NotEmbedded),
            1 => {
                let result = SyncedLyrics::parse(lyrics[0].clone());
                self.vorbis_comments_mut().remove("LYRICS");
                Ok(result?)
            }
            2.. => {
                // psycho path
                let result = SyncedLyrics::parse(lyrics.into_iter().flatten().cloned().collect());
                let blocks = self
                    .blocks()
                    .filter_map(|block| match block {
                        metaflac::Block::VorbisComment(comment) => Some(comment.clone()),
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                self.remove_blocks(metaflac::BlockType::VorbisComment);
                for mut block in blocks {
                    block.remove("LYRICS");
                    self.push_block(metaflac::Block::VorbisComment(block));
                }
                Ok(result?)
            }
        }
    }

    fn set_synced_lyrics(&mut self, lyrics: SyncedLyrics) -> Result<SyncedLyrics, GetLyricsError> {
        let result = self.remove_synced_lyrics();
        self.set_vorbis("LYRICS", lyrics.save());
        result
    }

    fn get_rich_lyrics(&self) -> Result<RichLyrics, GetLyricsError> {
        let lyrics = self
            .blocks()
            .filter_map(|block| match block {
                metaflac::Block::VorbisComment(comment) => Some(comment),
                _ => None,
            })
            .filter_map(|x| x.get("RICH LYRICS"))
            .flatten()
            .next();
        match lyrics {
            None => Err(GetLyricsError::NotEmbedded),
            Some(str) => Ok(serde_json::from_str(str)?),
        }
    }

    fn remove_rich_lyrics(&mut self) -> Result<RichLyrics, GetLyricsError> {
        let lyrics = self
            .blocks()
            .filter_map(|block| match block {
                metaflac::Block::VorbisComment(comment) => Some(comment),
                _ => None,
            })
            .filter_map(|x| x.get("RICH LYRICS"))
            .collect::<Vec<_>>();
        match lyrics.len() {
            0 => Err(GetLyricsError::NotEmbedded),
            1 => {
                if lyrics[0].is_empty() {
                    self.vorbis_comments_mut().remove("RICH LYRICS");
                    Err(GetLyricsError::NotEmbedded)
                } else {
                    let result = serde_json::from_str(&lyrics[0][0]);
                    self.vorbis_comments_mut().remove("RICH LYRICS");
                    Ok(result?)
                }
            }
            2.. => {
                // psycho path
                let result = lyrics
                    .into_iter()
                    .flatten()
                    .next()
                    .map(|x| serde_json::from_str::<RichLyrics>(x));
                let blocks = self
                    .blocks()
                    .filter_map(|block| match block {
                        metaflac::Block::VorbisComment(comment) => Some(comment.clone()),
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                self.remove_blocks(metaflac::BlockType::VorbisComment);
                for mut block in blocks {
                    block.remove("RICH LYRICS");
                    self.push_block(metaflac::Block::VorbisComment(block));
                }
                match result {
                    None => Err(GetLyricsError::NotEmbedded),
                    Some(result) => Ok(result?),
                }
            }
        }
    }

    fn set_rich_lyrics(&mut self, lyrics: RichLyrics) -> Result<RichLyrics, GetLyricsError> {
        let result = self.remove_rich_lyrics();
        self.set_vorbis(
            "RICH LYRICS",
            vec![serde_yaml::to_string(&lyrics).unwrap_or_default()],
        );
        result
    }
}

impl GetLyrics for ape::Tag {
    fn get_simple_lyrics(&self) -> Result<String, GetLyricsError> {
        let lyrics = self
            .items("LYRICS")
            .into_iter()
            .filter_map(|x| match &x.value {
                ape::ItemValue::Text(text) => Some(text.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        if lyrics.is_empty() {
            Err(GetLyricsError::NotEmbedded)
        } else {
            Ok(lyrics.join("\n"))
        }
    }

    fn remove_simple_lyrics(&mut self) -> Result<String, GetLyricsError> {
        let result = self.get_simple_lyrics();
        self.remove_items("LYRICS");
        result
    }

    fn set_simple_lyrics(&mut self, lyrics: String) -> Result<String, GetLyricsError> {
        let result = self.remove_simple_lyrics();
        self.add_item(ape::Item::from_text("LYRICS", lyrics).expect("lyrics is a valid key"));
        result
    }

    fn get_synced_lyrics(&self) -> Result<SyncedLyrics, GetLyricsError> {
        let lyrics = self
            .items("SYNCED LYRICS")
            .into_iter()
            .filter_map(|x| match &x.value {
                ape::ItemValue::Text(text) => Some(text.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        if lyrics.is_empty() {
            Err(GetLyricsError::NotEmbedded)
        } else {
            Ok(SyncedLyrics::parse(lyrics)?)
        }
    }

    fn remove_synced_lyrics(&mut self) -> Result<SyncedLyrics, GetLyricsError> {
        let result = self.get_synced_lyrics();
        self.remove_items("SYNCED LYRICS");
        result
    }

    fn set_synced_lyrics(&mut self, lyrics: SyncedLyrics) -> Result<SyncedLyrics, GetLyricsError> {
        let result = self.remove_synced_lyrics();
        self.add_item(
            ape::Item::from_text("SYNCED LYRICS", lyrics.save().join("\n"))
                .expect("lyrics is a valid key"),
        );
        result
    }

    fn get_rich_lyrics(&self) -> Result<RichLyrics, GetLyricsError> {
        let lyrics = self
            .items("RICH LYRICS")
            .into_iter()
            .filter_map(|x| match &x.value {
                ape::ItemValue::Text(text) => Some(text.clone()),
                _ => None,
            })
            .next();
        match lyrics {
            None => Err(GetLyricsError::NotEmbedded),
            Some(lyrics) => Ok(serde_json::from_str(&lyrics)?),
        }
    }

    fn remove_rich_lyrics(&mut self) -> Result<RichLyrics, GetLyricsError> {
        let result = self.get_rich_lyrics();
        self.remove_items("RICH LYRICS");
        result
    }

    fn set_rich_lyrics(&mut self, lyrics: RichLyrics) -> Result<RichLyrics, GetLyricsError> {
        let result = self.remove_rich_lyrics();
        self.add_item(
            ape::Item::from_text(
                "RICH LYRICS",
                serde_json::to_string(&lyrics).unwrap_or_default(),
            )
            .expect("lyrics is a valid key"),
        );
        result
    }
}

fn convert_id3_synced_lyrics(lyrics: &SynchronisedLyrics) -> Vec<SyncedLine> {
    match lyrics.timestamp_format {
        id3::frame::TimestampFormat::Mpeg => lyrics
            .content
            .iter()
            .map(|(_, text)| SyncedLine {
                timestamp: std::time::Duration::ZERO,
                text: text.to_owned(),
            })
            .collect(),
        id3::frame::TimestampFormat::Ms => lyrics
            .content
            .iter()
            .map(|(ms, time)| SyncedLine {
                timestamp: std::time::Duration::from_millis(u64::from(*ms)),
                text: time.to_owned(),
            })
            .collect(),
    }
}
