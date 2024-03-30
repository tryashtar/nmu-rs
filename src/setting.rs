use std::{path::Path, rc::Rc};

use image::DynamicImage;

use crate::{
    art::{GetArtResults, GetProcessedResult},
    library_config::{
        LibraryConfig, LyricsConfig, SetLyricsReport, SetLyricsResult, TagOptions, TagSettings,
    },
    lyrics::SomeLyrics,
    metadata::{self, GetMetadataResults, Metadata, MetadataField, MetadataValue},
    song_config::{self, ConfigCache, GetConfigsResults},
    tag_interop::{GetLyricsError, SetValue},
    util::ItemPath,
};

pub struct ProcessFolderResults {
    pub configs: GetConfigsResults,
    pub metadata: Option<GetMetadataResults>,
    pub art: Option<GetArtResults>,
}
pub fn process_folder(
    nice_path: &ItemPath,
    full_path: &Path,
    library_config: &mut LibraryConfig,
    config_cache: &mut ConfigCache,
) -> ProcessFolderResults {
    let configs = song_config::get_relevant_configs(library_config, nice_path, config_cache);
    if let Ok(loaded) = &configs.result {
        let mut art_results = GetArtResults::Keep;
        let mut results = metadata::get_metadata(nice_path, loaded, library_config);
        if let Some(repo) = &mut library_config.art_repo {
            if let Some(MetadataValue::List(art)) = results.metadata.get_mut(&MetadataField::Art) {
                art_results = repo.get_image(art);
                repo.used_templates.add(full_path, &art_results);
                if let GetArtResults::Processed { nice_path, .. } = &art_results {
                    art.clear();
                    art.push(nice_path.to_string_lossy().into_owned());
                }
            }
            repo.used_templates.add(full_path, &art_results);
        }
        return ProcessFolderResults {
            configs,
            metadata: Some(results),
            art: Some(art_results),
        };
    }
    ProcessFolderResults {
        configs,
        metadata: None,
        art: None,
    }
}

pub struct ProcessSongResults {
    pub configs: GetConfigsResults,
    pub metadata: Option<GetMetadataResults>,
    pub art: Option<GetArtResults>,
    pub added: Option<AddToSongReport>,
    pub file_lyrics: Option<SetLyricsReport>,
}
impl ProcessSongResults {
    pub fn has_fatal_shared_error(&self) -> bool {
        if self.configs.result.is_err() {
            return true;
        }
        if let Some(GetArtResults::Processed {
            processed: Some(GetProcessedResult { result: Err(_), .. }),
            ..
        }) = &self.art
        {
            return true;
        }
        false
    }
    pub fn has_fatal_local_error(&self) -> bool {
        if let Some(added) = &self.added {
            if added.id3.is_err() || added.ape.is_err() || added.flac.is_err() {
                return true;
            }
        }
        if let Some(lyrics) = &self.file_lyrics {
            if lyrics
                .results
                .values()
                .any(|x| matches!(x, SetLyricsResult::Failed(_)))
            {
                return true;
            }
        }
        false
    }
}
pub fn process_song(
    nice_path: &ItemPath,
    full_path: &Path,
    library_config: &mut LibraryConfig,
    config_cache: &mut ConfigCache,
    options: &TagOptions,
) -> ProcessSongResults {
    let configs = song_config::get_relevant_configs(library_config, nice_path, config_cache);
    if let Ok(loaded) = &configs.result {
        let mut art_results = GetArtResults::Keep;
        let mut results = metadata::get_metadata(nice_path, loaded, library_config);
        if let Some(repo) = &mut library_config.art_repo {
            if let Some(MetadataValue::List(art)) = results.metadata.get_mut(&MetadataField::Art) {
                art_results = repo.get_image(art);
                if let GetArtResults::Processed { nice_path, .. } = &art_results {
                    art.clear();
                    art.push(nice_path.to_string_lossy().into_owned());
                }
            }
            repo.used_templates.add(full_path, &art_results);
        }
        let art_set = match &art_results {
            GetArtResults::Keep | GetArtResults::NoTemplateFound { .. } => SetValue::Keep,
            GetArtResults::Remove => SetValue::Remove,
            GetArtResults::Processed { result, .. } => match result {
                Ok(img) => SetValue::Replace(img.clone()),
                Err(_) => SetValue::Keep,
            },
        };
        let added = add_to_song(
            options,
            full_path,
            nice_path,
            &results.metadata,
            art_set,
            library_config,
        );
        let mut file_lyrics = None;
        if let Some(lyrics) = get_tag_lyrics(&added) {
            results.metadata.insert(
                MetadataField::SimpleLyrics,
                MetadataValue::string(lyrics.clone().into_simple()),
            );
            if let Some(lyrics_config) = &library_config.lyrics {
                let report = lyrics_config.write(nice_path, lyrics);
                file_lyrics = Some(report);
            }
        }
        return ProcessSongResults {
            configs,
            metadata: Some(results),
            art: Some(art_results),
            added: Some(added),
            file_lyrics,
        };
    }
    ProcessSongResults {
        configs,
        metadata: None,
        art: None,
        added: None,
        file_lyrics: None,
    }
}

pub fn get_tag_lyrics(changes: &AddToSongReport) -> Option<&SomeLyrics> {
    for change in [&changes.id3, &changes.flac, &changes.ape]
        .into_iter()
        .flatten()
    {
        if let TagChanges::Set(TagSpecificChanges {
            lyrics: Some((lyrics, _)),
            ..
        }) = change
        {
            return Some(lyrics);
        }
    }
    None
}

#[derive(thiserror::Error, Debug)]
pub enum AddToSongError {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Id3(#[from] id3::Error),
    #[error("{0}")]
    Flac(#[from] metaflac::Error),
    #[error("{0}")]
    Ape(#[from] ape::Error),
    #[error("{0}")]
    Lyrics(#[from] GetLyricsError),
}

pub struct AddToSongReport {
    pub id3: Result<TagChanges, AddToSongError>,
    pub flac: Result<TagChanges, AddToSongError>,
    pub ape: Result<TagChanges, AddToSongError>,
}

pub enum TagChanges {
    None,
    Removed,
    Set(TagSpecificChanges),
}
pub struct TagSpecificChanges {
    pub lyrics: Option<(SomeLyrics, SetLyricsReport)>,
    pub created: bool,
}
impl TagSpecificChanges {
    fn new() -> Self {
        Self {
            lyrics: None,
            created: false,
        }
    }
    pub fn any(&self) -> bool {
        if self.created {
            return true;
        }
        if let Some((_, report)) = &self.lyrics {
            if !report.results.is_empty() {
                return true;
            }
        }
        false
    }
}

fn set_id3(
    mode: &TagSettings,
    file_path: &Path,
    nice_path: &Path,
    lyrics_config: Option<&LyricsConfig>,
) -> Result<TagChanges, AddToSongError> {
    match mode {
        TagSettings::Ignore => Ok(TagChanges::None),
        TagSettings::Remove => {
            if id3::Tag::remove_from_path(file_path)? {
                Ok(TagChanges::Removed)
            } else {
                Ok(TagChanges::None)
            }
        }
        TagSettings::Set {} => {
            let mut changes = TagSpecificChanges::new();
            let mut tag = id3::Tag::read_from_path(file_path);
            if let Err(id3::Error {
                kind: id3::ErrorKind::NoTag,
                ..
            }) = tag
            {
                tag = Ok(id3::Tag::new());
                changes.created = true;
            }
            let mut tag = tag?;
            if let Some(lyrics_config) = lyrics_config {
                let best = lyrics_config.get_best(nice_path, &tag);
                match best {
                    Ok(lyrics) => {
                        let report = lyrics_config.set(&lyrics, &mut tag);
                        changes.lyrics = Some((lyrics, report));
                    }
                    Err(GetLyricsError::NotEmbedded) => {}
                    Err(err) => return Err(AddToSongError::Lyrics(err)),
                }
            }
            Ok(TagChanges::Set(changes))
        }
    }
}

fn set_flac(
    mode: &TagSettings,
    file_path: &Path,
    nice_path: &Path,
    lyrics_config: Option<&LyricsConfig>,
) -> Result<TagChanges, AddToSongError> {
    match mode {
        TagSettings::Ignore => Ok(TagChanges::None),
        TagSettings::Remove => {
            let existing = metaflac::Tag::read_from_path(file_path);
            if let Err(metaflac::Error {
                kind: metaflac::ErrorKind::InvalidInput,
                ..
            }) = existing
            {
                Ok(TagChanges::None)
            } else if existing?.blocks().next().is_some() {
                let mut tag = metaflac::Tag::new();
                tag.write_to_path(file_path)?;
                Ok(TagChanges::Removed)
            } else {
                Ok(TagChanges::None)
            }
        }
        TagSettings::Set {} => {
            let mut changes = TagSpecificChanges::new();
            let mut tag = metaflac::Tag::read_from_path(file_path);
            if let Err(metaflac::Error {
                kind: metaflac::ErrorKind::InvalidInput,
                ..
            }) = tag
            {
                tag = Ok(metaflac::Tag::new());
                changes.created = true;
            }
            let mut tag = tag?;
            if let Some(lyrics_config) = lyrics_config {
                let best = lyrics_config.get_best(nice_path, &tag);
                match best {
                    Ok(lyrics) => {
                        let report = lyrics_config.set(&lyrics, &mut tag);
                        changes.lyrics = Some((lyrics, report));
                    }
                    Err(GetLyricsError::NotEmbedded) => {}
                    Err(err) => return Err(AddToSongError::Lyrics(err)),
                }
            }
            Ok(TagChanges::Set(changes))
        }
    }
}

fn set_ape(
    mode: &TagSettings,
    file_path: &Path,
    nice_path: &Path,
    lyrics_config: Option<&LyricsConfig>,
) -> Result<TagChanges, AddToSongError> {
    match mode {
        TagSettings::Ignore => Ok(TagChanges::None),
        TagSettings::Remove => {
            let existing = ape::read_from_path(file_path);
            if let Err(ape::Error::TagNotFound) = existing {
                Ok(TagChanges::None)
            } else {
                ape::remove_from_path(file_path)?;
                Ok(TagChanges::Removed)
            }
        }
        TagSettings::Set {} => {
            let mut changes = TagSpecificChanges::new();
            let mut tag = ape::read_from_path(file_path);
            if let Err(ape::Error::TagNotFound) = tag {
                tag = Ok(ape::Tag::new());
                changes.created = true;
            }
            let mut tag = tag?;
            if let Some(lyrics_config) = lyrics_config {
                let best = lyrics_config.get_best(nice_path, &tag);
                match best {
                    Ok(lyrics) => {
                        let report = lyrics_config.set(&lyrics, &mut tag);
                        changes.lyrics = Some((lyrics, report));
                    }
                    Err(GetLyricsError::NotEmbedded) => {}
                    Err(err) => return Err(AddToSongError::Lyrics(err)),
                }
            }
            Ok(TagChanges::Set(changes))
        }
    }
}

fn add_to_song(
    options: &TagOptions,
    file_path: &Path,
    nice_path: &Path,
    metadata: &Metadata,
    art: SetValue<Rc<DynamicImage>>,
    config: &LibraryConfig,
) -> AddToSongReport {
    let id3 = set_id3(&options.id3, file_path, nice_path, config.lyrics.as_ref());
    let flac = set_flac(&options.flac, file_path, nice_path, config.lyrics.as_ref());
    let ape = set_ape(&options.ape, file_path, nice_path, config.lyrics.as_ref());
    AddToSongReport { id3, flac, ape }
}
