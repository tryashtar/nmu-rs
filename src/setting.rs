use std::{path::Path, rc::Rc};

use image::DynamicImage;

use crate::tag_interop::{GetLyrics, SetMetadata};
use crate::{
    art::{GetArtResults, GetProcessedResult},
    library_config::{LibraryConfig, SetLyricsReport, SetLyricsResult, TagOptions, TagSettings},
    lyrics::SomeLyrics,
    metadata::{self, GetMetadataResults, Metadata, MetadataField, MetadataValue},
    song_config::{self, ConfigCache, GetConfigsResults},
    tag_interop::{self, GetLyricsError, SetMetadataReport, SetValue},
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
    pub assigned: Option<AssignResults>,
}
pub struct AssignResults {
    pub metadata: GetMetadataResults,
    pub art: GetArtResults,
    pub added: AddToSongReport,
    pub file_lyrics: Option<SetLyricsReport>,
}
impl ProcessSongResults {
    pub fn has_fatal_shared_error(&self) -> bool {
        if self.configs.result.is_err() {
            return true;
        }
        if let Some(AssignResults {
            art:
                GetArtResults::Processed {
                    processed: Some(GetProcessedResult { result: Err(_), .. }),
                    ..
                },
            ..
        }) = &self.assigned
        {
            return true;
        }
        false
    }
    pub fn has_fatal_local_error(&self) -> bool {
        if let Some(assigned) = &self.assigned {
            if assigned.added.id3.is_err()
                || assigned.added.ape.is_err()
                || assigned.added.flac.is_err()
            {
                return true;
            }
            if let Some(lyrics) = &assigned.file_lyrics {
                if lyrics
                    .results
                    .values()
                    .any(|x| matches!(x, SetLyricsResult::Failed(_)))
                {
                    return true;
                }
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
            assigned: Some(AssignResults {
                metadata: results,
                art: art_results,
                added,
                file_lyrics,
            }),
        };
    }
    ProcessSongResults {
        configs,
        assigned: None,
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
    pub metadata: SetMetadataReport,
    pub created: bool,
}
impl TagSpecificChanges {
    pub fn any(&self) -> bool {
        if self.created {
            return true;
        }
        if let Some((_, report)) = &self.lyrics {
            if !report.results.is_empty() {
                return true;
            }
        }
        if !self.metadata.fields.is_empty() {
            return true;
        }
        false
    }
}

fn set_id3(
    mode: &TagSettings,
    file_path: &Path,
    nice_path: &Path,
    metadata: Metadata,
    art: &SetValue<Rc<DynamicImage>>,
    config: &LibraryConfig,
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
            let mut created = false;
            let mut tag = id3::Tag::read_from_path(file_path);
            if let Err(id3::Error {
                kind: id3::ErrorKind::NoTag,
                ..
            }) = tag
            {
                tag = Ok(id3::Tag::new());
                created = true;
            }
            let mut tag = tag?;
            let result = set_generic(&mut tag, nice_path, metadata, config, created)?;
            Ok(TagChanges::Set(result))
        }
    }
}

fn set_flac(
    mode: &TagSettings,
    file_path: &Path,
    nice_path: &Path,
    metadata: Metadata,
    art: &SetValue<Rc<DynamicImage>>,
    config: &LibraryConfig,
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
            let mut created = false;
            let mut tag = metaflac::Tag::read_from_path(file_path);
            if let Err(metaflac::Error {
                kind: metaflac::ErrorKind::InvalidInput,
                ..
            }) = tag
            {
                tag = Ok(metaflac::Tag::new());
                created = true;
            }
            let mut tag = tag?;
            let result = set_generic(&mut tag, nice_path, metadata, config, created)?;
            Ok(TagChanges::Set(result))
        }
    }
}

fn set_ape(
    mode: &TagSettings,
    file_path: &Path,
    nice_path: &Path,
    metadata: Metadata,
    art: &SetValue<Rc<DynamicImage>>,
    config: &LibraryConfig,
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
            let mut created = false;
            let mut tag = ape::read_from_path(file_path);
            if let Err(ape::Error::TagNotFound) = tag {
                tag = Ok(ape::Tag::new());
                created = true;
            }
            let mut tag = tag?;
            let result = set_generic(&mut tag, nice_path, metadata, config, created)?;
            Ok(TagChanges::Set(result))
        }
    }
}

fn set_generic<T>(
    tag: &mut T,
    nice_path: &Path,
    metadata: Metadata,
    config: &LibraryConfig,
    created: bool,
) -> Result<TagSpecificChanges, AddToSongError>
where
    T: SetMetadata + GetLyrics,
{
    let mut lyrics_report = None;
    if let Some(lyrics_config) = &config.lyrics {
        let best = lyrics_config.get_best(nice_path, tag);
        match best {
            Ok(lyrics) => {
                let report = lyrics_config.set(&lyrics, tag);
                lyrics_report = Some((lyrics, report));
            }
            Err(GetLyricsError::NotEmbedded) => {}
            Err(err) => return Err(AddToSongError::Lyrics(err)),
        }
    }
    let metadata_report = tag_interop::set_metadata(tag, metadata, &config.artist_separator);
    Ok(TagSpecificChanges {
        created,
        lyrics: lyrics_report,
        metadata: metadata_report,
    })
}

fn add_to_song(
    options: &TagOptions,
    file_path: &Path,
    nice_path: &Path,
    metadata: &Metadata,
    art: SetValue<Rc<DynamicImage>>,
    config: &LibraryConfig,
) -> AddToSongReport {
    let id3 = set_id3(
        &options.id3,
        file_path,
        nice_path,
        metadata.clone(),
        &art,
        config,
    );
    let flac = set_flac(
        &options.flac,
        file_path,
        nice_path,
        metadata.clone(),
        &art,
        config,
    );
    let ape = set_ape(
        &options.ape,
        file_path,
        nice_path,
        metadata.clone(),
        &art,
        config,
    );
    AddToSongReport { id3, flac, ape }
}
