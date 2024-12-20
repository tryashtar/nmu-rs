use regex::Regex;

use crate::{
    library_config::{
        DateCache, LibraryConfig, ScanDecision, ScanOptions, TagOptions, TagSettings,
    },
    lyrics::{parse_duration, TimeParseError},
    metadata::{self, MetadataField, MetadataValue},
    modifier::{ValueError, ValueModifier},
    song_config::{AllSetter, LoadedConfig, RawSongConfigFile, ReverseMode, SongConfig},
    strategy::{
        FieldSelector, FieldValueGetter, ItemSelector, LocalItemSelector, MetadataOperation,
        PathSegment, ValueGetter, WarnBehavior,
    },
    util::{ItemPath, OutOfBoundsDecision, Range},
};
use std::{
    collections::{HashMap, HashSet},
    num::IntErrorKind,
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

// replace assert!(matches!()) with assert_matches!() when stable
mod interop {
    use id3::TagLike;

    use crate::{
        lyrics::{self, Channel, RichLine, RichLyrics, SomeLyrics, SyncedLine, SyncedLyrics},
        tag_interop::{GetLyrics, GetLyricsError},
    };

    #[test]
    fn lyric_equality() {
        let rich1 = RichLyrics {
            channels: vec![
                Channel {
                    name: Some(String::from("channel1")),
                    lyrics: vec![
                        RichLine {
                            start: std::time::Duration::from_secs(5),
                            end: std::time::Duration::from_secs(10),
                            text: String::from("line1"),
                        },
                        RichLine {
                            start: std::time::Duration::from_secs(10),
                            end: std::time::Duration::from_secs(15),
                            text: String::from("line2"),
                        },
                    ],
                },
                Channel {
                    name: Some(String::from("channel2")),
                    lyrics: vec![
                        RichLine {
                            start: std::time::Duration::from_secs(3),
                            end: std::time::Duration::from_secs(6),
                            text: String::from("line3"),
                        },
                        RichLine {
                            start: std::time::Duration::from_secs(6),
                            end: std::time::Duration::from_secs(10),
                            text: String::from("line4"),
                        },
                    ],
                },
            ],
        };
        let rich2 = rich1.clone();
        let synced1 = SyncedLyrics::from(rich1.clone());
        let synced2 = synced1.clone();
        let simple1 = String::from(rich1.clone());
        let simple2 = simple1.clone();
        let rich1 = SomeLyrics::Rich(rich1);
        let rich2 = SomeLyrics::Rich(rich2);
        let synced1 = SomeLyrics::Synced(synced1);
        let synced2 = SomeLyrics::Synced(synced2);
        let simple1 = SomeLyrics::Simple(simple1);
        let simple2 = SomeLyrics::Simple(simple2);
        assert!(lyrics::matches(&rich1, Ok(&rich1)));
        assert!(lyrics::matches(&rich1, Ok(&rich2)));
        assert!(lyrics::matches(&rich1, Ok(&synced1)));
        assert!(lyrics::matches(&rich1, Ok(&synced2)));
        assert!(lyrics::matches(&rich1, Ok(&simple1)));
        assert!(lyrics::matches(&rich1, Ok(&simple2)));
        assert!(lyrics::matches(&rich2, Ok(&rich1)));
        assert!(lyrics::matches(&rich2, Ok(&rich2)));
        assert!(lyrics::matches(&rich2, Ok(&synced1)));
        assert!(lyrics::matches(&rich2, Ok(&synced2)));
        assert!(lyrics::matches(&rich2, Ok(&simple1)));
        assert!(lyrics::matches(&rich2, Ok(&simple2)));
        assert!(lyrics::matches(&synced1, Ok(&rich1)));
        assert!(lyrics::matches(&synced1, Ok(&rich2)));
        assert!(lyrics::matches(&synced1, Ok(&synced1)));
        assert!(lyrics::matches(&synced1, Ok(&synced2)));
        assert!(lyrics::matches(&synced1, Ok(&simple1)));
        assert!(lyrics::matches(&synced1, Ok(&simple2)));
        assert!(lyrics::matches(&synced2, Ok(&rich1)));
        assert!(lyrics::matches(&synced2, Ok(&rich2)));
        assert!(lyrics::matches(&synced2, Ok(&synced1)));
        assert!(lyrics::matches(&synced2, Ok(&synced2)));
        assert!(lyrics::matches(&synced2, Ok(&simple1)));
        assert!(lyrics::matches(&synced2, Ok(&simple2)));
        assert!(lyrics::matches(&simple1, Ok(&rich1)));
        assert!(lyrics::matches(&simple1, Ok(&rich2)));
        assert!(lyrics::matches(&simple1, Ok(&synced1)));
        assert!(lyrics::matches(&simple1, Ok(&synced2)));
        assert!(lyrics::matches(&simple1, Ok(&simple1)));
        assert!(lyrics::matches(&simple1, Ok(&simple2)));
        assert!(lyrics::matches(&simple2, Ok(&rich1)));
        assert!(lyrics::matches(&simple2, Ok(&rich2)));
        assert!(lyrics::matches(&simple2, Ok(&synced1)));
        assert!(lyrics::matches(&simple2, Ok(&synced2)));
        assert!(lyrics::matches(&simple2, Ok(&simple1)));
        assert!(lyrics::matches(&simple2, Ok(&simple2)));
    }

    #[test]
    fn id3_simple_lyrics() {
        let mut tag = id3::Tag::new();
        tag.add_frame(id3::frame::Lyrics {
            lang: String::from(""),
            description: String::from("first"),
            text: String::from("one\ntwo"),
        });
        tag.add_frame(id3::frame::Lyrics {
            lang: String::from(""),
            description: String::from("second"),
            text: String::from("three\nfour"),
        });
        let lyrics = tag.get_simple_lyrics().unwrap();
        assert_eq!(lyrics, "one\ntwo\nthree\nfour");
        let removed = tag.remove_simple_lyrics().unwrap();
        assert_eq!(removed, "one\ntwo\nthree\nfour");
        let lyrics = tag.get_simple_lyrics();
        assert!(matches!(lyrics, Err(GetLyricsError::NotEmbedded)));
        let removed = tag.set_simple_lyrics(String::from("line1\nline2"));
        assert!(matches!(removed, Err(GetLyricsError::NotEmbedded)));
        let lyrics = tag.get_simple_lyrics().unwrap();
        assert_eq!(lyrics, "line1\nline2");
    }

    #[test]
    fn id3_synced_lyrics() {
        let mut tag = id3::Tag::new();
        tag.add_frame(id3::frame::SynchronisedLyrics {
            lang: String::from("lang1"),
            timestamp_format: id3::frame::TimestampFormat::Ms,
            content_type: id3::frame::SynchronisedLyricsType::Lyrics,
            description: String::from("first"),
            content: vec![(0, String::from("line 1")), (10, String::from("line 2"))],
        });
        tag.add_frame(id3::frame::SynchronisedLyrics {
            lang: String::from("lang2"),
            timestamp_format: id3::frame::TimestampFormat::Ms,
            content_type: id3::frame::SynchronisedLyricsType::Lyrics,
            description: String::from("second"),
            content: vec![(5, String::from("line 3")), (15, String::from("line 4"))],
        });
        let lyrics = tag.get_synced_lyrics().unwrap();
        assert_eq!(lyrics.lines.len(), 4);
        assert_eq!(lyrics.lines[0].text, "line 1");
        assert_eq!(lyrics.lines[1].text, "line 2");
        assert_eq!(lyrics.lines[2].text, "line 3");
        assert_eq!(lyrics.lines[3].text, "line 4");
        let removed = tag.remove_synced_lyrics().unwrap();
        assert_eq!(removed.lines.len(), 4);
        assert_eq!(removed.lines[0].text, "line 1");
        assert_eq!(removed.lines[1].text, "line 2");
        assert_eq!(removed.lines[2].text, "line 3");
        assert_eq!(removed.lines[3].text, "line 4");
        let lyrics = tag.get_synced_lyrics();
        assert!(matches!(lyrics, Err(GetLyricsError::NotEmbedded)));
        let removed = tag.set_synced_lyrics(SyncedLyrics {
            lines: vec![
                SyncedLine {
                    timestamp: std::time::Duration::ZERO,
                    text: String::from("aaa"),
                },
                SyncedLine {
                    timestamp: std::time::Duration::from_secs(1),
                    text: String::from("bbb"),
                },
            ],
        });
        assert!(matches!(removed, Err(GetLyricsError::NotEmbedded)));
        let lyrics = tag.get_synced_lyrics().unwrap();
        assert_eq!(lyrics.lines.len(), 2);
        assert_eq!(lyrics.lines[0].text, "aaa");
        assert_eq!(lyrics.lines[1].text, "bbb");
    }

    #[test]
    fn flac_simple_lyrics() {
        let mut tag = metaflac::Tag::new();
        let mut com1 = metaflac::block::VorbisComment::new();
        com1.set("UNSYNCED LYRICS", vec!["one\ntwo", "three\nfour"]);
        let mut com2 = metaflac::block::VorbisComment::new();
        com2.set("UNSYNCED LYRICS", vec!["five\nsix", "seven\neight"]);
        tag.push_block(metaflac::Block::VorbisComment(com1));
        tag.push_block(metaflac::Block::VorbisComment(com2));
        let lyrics = tag.get_simple_lyrics().unwrap();
        assert_eq!(lyrics, "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight");
        let removed = tag.remove_simple_lyrics().unwrap();
        assert_eq!(removed, "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight");
        let lyrics = tag.get_simple_lyrics();
        assert!(matches!(lyrics, Err(GetLyricsError::NotEmbedded)));
        let removed = tag.set_simple_lyrics(String::from("line1\nline2"));
        assert!(matches!(removed, Err(GetLyricsError::NotEmbedded)));
        let lyrics = tag.get_simple_lyrics().unwrap();
        assert_eq!(lyrics, "line1\nline2");
    }

    #[test]
    fn ape_simple_lyrics() {
        let mut tag = ape::Tag::new();
        tag.add_item(ape::Item {
            key: String::from("Lyrics"),
            value: ape::ItemValue::Text(String::from("one\ntwo")),
        });
        tag.add_item(ape::Item {
            key: String::from("Lyrics"),
            value: ape::ItemValue::Text(String::from("three\nfour")),
        });
        let lyrics = tag.get_simple_lyrics().unwrap();
        assert_eq!(lyrics, "one\ntwo\nthree\nfour");
        let removed = tag.remove_simple_lyrics().unwrap();
        assert_eq!(removed, "one\ntwo\nthree\nfour");
        let lyrics = tag.get_simple_lyrics();
        assert!(matches!(lyrics, Err(GetLyricsError::NotEmbedded)));
        let removed = tag.set_simple_lyrics(String::from("line1\nline2"));
        assert!(matches!(removed, Err(GetLyricsError::NotEmbedded)));
        let lyrics = tag.get_simple_lyrics().unwrap();
        assert_eq!(lyrics, "line1\nline2");
    }
}

mod deserialize {
    use super::*;

    #[test]
    fn meta_op_blank() {
        let result = serde_yaml::from_str::<MetadataOperation>("remove: [title]").unwrap();
        assert!(matches!(
            result,
            MetadataOperation::Blank {
                remove: FieldSelector::Multiple(x)
            }
            if x == HashSet::from([MetadataField::Title])
        ));
    }

    #[test]
    fn meta_op_keep() {
        let result = serde_yaml::from_str::<MetadataOperation>("keep: [title]").unwrap();
        assert!(matches!(
            result,
            MetadataOperation::Keep {
                keep: FieldSelector::Multiple(x)
            }
            if x == HashSet::from([MetadataField::Title])
        ));
    }

    #[test]
    fn meta_op_set() {
        let result = serde_yaml::from_str::<MetadataOperation>("{title: 'test'}").unwrap();
        assert!(matches!(result, MetadataOperation::Set { .. }));
    }

    #[test]
    fn item_sel_path() {
        let result = serde_yaml::from_str::<ItemSelector>("a/b/c").unwrap();
        assert!(matches!(result, ItemSelector::Path { .. }));
    }

    #[test]
    fn item_sel_multi() {
        let result = serde_yaml::from_str::<ItemSelector>("[a/b/c, d/e/f]").unwrap();
        assert!(matches!(result, ItemSelector::Multi { .. }));
    }

    #[test]
    fn item_sel_seg() {
        let result = serde_yaml::from_str::<ItemSelector>("path: [a, b, c]").unwrap();
        assert!(matches!(result, ItemSelector::Segmented { .. }));
    }

    #[test]
    fn item_sel_subpath() {
        let result =
            serde_yaml::from_str::<ItemSelector>("{subpath: a/b/c, select: [d, e, f]}").unwrap();
        assert!(matches!(result, ItemSelector::Subpath { .. }));
    }

    #[test]
    fn segment_literal() {
        let result = serde_yaml::from_str::<PathSegment>("abc").unwrap();
        assert!(matches!(result, PathSegment::Literal { .. }));
    }

    #[test]
    fn segment_regex() {
        let result = serde_yaml::from_str::<PathSegment>("regex: '^abc$'").unwrap();
        assert!(matches!(result, PathSegment::Regex { .. }));
    }

    #[test]
    fn local_item_self() {
        let result = serde_yaml::from_str::<LocalItemSelector>("this").unwrap();
        assert!(matches!(result, LocalItemSelector::This { .. }));
    }

    #[test]
    fn local_item_this() {
        let result = serde_yaml::from_str::<LocalItemSelector>("self").unwrap();
        assert!(matches!(result, LocalItemSelector::This { .. }));
    }

    #[test]
    fn local_item_up() {
        let result = serde_yaml::from_str::<LocalItemSelector>("up: 2").unwrap();
        assert!(matches!(result, LocalItemSelector::DrillUp { .. }));
    }

    #[test]
    fn local_item_down() {
        let result = serde_yaml::from_str::<LocalItemSelector>("from_root: 2").unwrap();
        assert!(matches!(result, LocalItemSelector::DrillDown { .. }));
    }

    #[test]
    fn field_title() {
        let result = serde_yaml::from_str::<MetadataField>("title").unwrap();
        assert!(matches!(result, MetadataField::Title));
    }

    #[test]
    fn field_custom() {
        let result = serde_yaml::from_str::<MetadataField>("source").unwrap();
        assert!(matches!(result, MetadataField::Custom { .. }));
    }

    #[test]
    fn field_select_title() {
        let result = serde_yaml::from_str::<FieldSelector>("title").unwrap();
        assert!(matches!(result, FieldSelector::Single { .. }));
    }

    #[test]
    fn field_select_multiple() {
        let result = serde_yaml::from_str::<FieldSelector>("[title, album]").unwrap();
        assert!(matches!(result, FieldSelector::Multiple { .. }));
    }

    #[test]
    fn field_select_all() {
        let result = serde_yaml::from_str::<FieldSelector>("'*'").unwrap();
        assert!(matches!(result, FieldSelector::All { .. }));
    }

    #[test]
    fn meta_str() {
        let result = serde_yaml::from_str::<MetadataValue>("test").unwrap();
        assert!(matches!(result, MetadataValue::List { .. }));
    }

    #[test]
    fn meta_list() {
        let result = serde_yaml::from_str::<MetadataValue>("[test1, test2]").unwrap();
        assert!(matches!(result, MetadataValue::List { .. }));
    }

    #[test]
    fn value_direct() {
        let result = serde_yaml::from_str::<ValueGetter>("test").unwrap();
        assert!(matches!(result, ValueGetter::Direct { .. }));
    }

    #[test]
    fn value_from() {
        let result = serde_yaml::from_str::<ValueGetter>("from: this").unwrap();
        assert!(matches!(result, ValueGetter::From { .. }));
    }

    #[test]
    fn value_get_name() {
        let result = serde_yaml::from_str::<FieldValueGetter>("clean_name").unwrap();
        assert!(matches!(result, FieldValueGetter::CleanName));
    }

    #[test]
    fn meta_op_modify() {
        let result =
            serde_yaml::from_str::<MetadataOperation>("modify: {title: {split: ' '}}").unwrap();
        assert!(matches!(result, MetadataOperation::Modify { .. }));
    }

    #[test]
    fn meta_op_context() {
        let result = serde_yaml::from_str::<MetadataOperation>(
            "{source: 'test', modify: {title: {split: ' '}}}",
        )
        .unwrap();
        assert!(matches!(result, MetadataOperation::Context { .. }));
    }

    #[test]
    fn range_index() {
        let result = serde_yaml::from_str::<Range>("5").unwrap();
        assert!(matches!(result, Range { start: 5, stop: 5 }));
    }

    #[test]
    fn range_tuple() {
        let result = serde_yaml::from_str::<Range>("[0, 5]").unwrap();
        assert!(matches!(result, Range { start: 0, stop: 5 }));
    }

    #[test]
    fn range_start() {
        let result = serde_yaml::from_str::<Range>("{start: 5}").unwrap();
        assert!(matches!(result, Range { start: 5, stop: -1 }));
    }

    #[test]
    fn range_literal() {
        let result = serde_yaml::from_str::<Range>("'first'").unwrap();
        assert!(matches!(result, Range { start: 0, stop: 0 }));
    }

    #[test]
    fn modifier_append() {
        let result = serde_yaml::from_str::<ValueModifier>("append: 'test'").unwrap();
        assert!(matches!(result, ValueModifier::Append { .. }));
    }

    #[test]
    fn modifier_prepend() {
        let result = serde_yaml::from_str::<ValueModifier>("prepend: 'test'").unwrap();
        assert!(matches!(result, ValueModifier::Prepend { .. }));
    }

    #[test]
    fn modifier_join() {
        let result = serde_yaml::from_str::<ValueModifier>("join: 'test'").unwrap();
        assert!(matches!(result, ValueModifier::Join { .. }));
    }

    #[test]
    fn modifier_split() {
        let result = serde_yaml::from_str::<ValueModifier>("split: 'test'").unwrap();
        assert!(matches!(result, ValueModifier::Split { .. }));
    }

    #[test]
    fn take_simple() {
        let result = serde_yaml::from_str::<ValueModifier>("take: first").unwrap();
        assert!(matches!(result, ValueModifier::Take { .. }));
    }

    #[test]
    fn take_struct() {
        let result = serde_yaml::from_str::<ValueModifier>("take: {index: first}").unwrap();
        assert!(matches!(result, ValueModifier::Take { .. }));
    }

    #[test]
    fn this_ser() {
        let str = serde_yaml::to_string(&LocalItemSelector::This).unwrap();
        assert_eq!(str, "this\n");
    }
}

#[test]
fn range_int_first() {
    let range: Range = 0.into();
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Exit).unwrap(),
        ['a']
    );
}

#[test]
fn range_int_last() {
    let range: Range = 3.into();
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Exit).unwrap(),
        ['d']
    );
}

#[test]
fn range_int_from_back() {
    let range: Range = (-2).into();
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Exit).unwrap(),
        ['c']
    );
}

#[test]
fn range_int_too_big() {
    let range: Range = 5.into();
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(range.slice(&values, OutOfBoundsDecision::Exit), None);
}

#[test]
fn range_int_too_small() {
    let range: Range = (-5).into();
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(range.slice(&values, OutOfBoundsDecision::Exit), None);
}

#[test]
fn range_mult_all() {
    let range: Range = Range::new(0, -1);
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Exit).unwrap(),
        ['a', 'b', 'c', 'd']
    );
}

#[test]
fn range_mult_backwards() {
    let range: Range = Range::new(1, 0);
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(range.slice(&values, OutOfBoundsDecision::Exit).unwrap(), []);
}

#[test]
fn range_mult_some() {
    let range: Range = Range::new(1, 2);
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Exit).unwrap(),
        ['b', 'c']
    );
}

#[test]
fn range_mult_clamp() {
    let range: Range = Range::new(2, 5);
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Clamp).unwrap(),
        ['c', 'd']
    );
}

#[test]
fn select_path_full() {
    assert!(ItemSelector::Path(PathBuf::from("a/b")).matches(Path::new("a/b")));
}

#[test]
fn select_path_partial() {
    assert!(ItemSelector::Path(PathBuf::from("a/b")).matches(Path::new("a/b/c")));
}

#[test]
fn select_path_too_far() {
    assert!(!ItemSelector::Path(PathBuf::from("a/b")).matches(Path::new("a")));
}

#[test]
fn select_segment_full() {
    assert!(ItemSelector::Segmented {
        path: vec![
            PathSegment::Literal(String::from("a")),
            PathSegment::Literal(String::from("b"))
        ]
    }
    .matches(Path::new("a/b")));
}

#[test]
fn select_segment_partial() {
    assert!(ItemSelector::Segmented {
        path: vec![
            PathSegment::Literal(String::from("a")),
            PathSegment::Literal(String::from("b"))
        ]
    }
    .matches(Path::new("a/b/c")));
}

#[test]
fn select_segment_too_far() {
    assert!(!ItemSelector::Segmented {
        path: vec![
            PathSegment::Literal(String::from("a")),
            PathSegment::Literal(String::from("b"))
        ]
    }
    .matches(Path::new("a")));
}

#[test]
fn select_subpath_simple() {
    assert!(ItemSelector::Subpath {
        subpath: Box::new(ItemSelector::Path(PathBuf::from("a"))),
        select: Box::new(ItemSelector::Path(PathBuf::from("b")))
    }
    .matches(Path::new("a/b")));
}

#[test]
fn select_subpath_complex() {
    let selector = ItemSelector::Subpath {
        subpath: Box::new(ItemSelector::Multi(vec![
            ItemSelector::Path(PathBuf::from("a")),
            ItemSelector::Path(PathBuf::from("b")),
        ])),
        select: Box::new(ItemSelector::Multi(vec![
            ItemSelector::Path(PathBuf::from("c")),
            ItemSelector::Path(PathBuf::from("d")),
        ])),
    };
    assert!(selector.matches(Path::new("a/c")));
    assert!(selector.matches(Path::new("a/d")));
    assert!(selector.matches(Path::new("b/c")));
    assert!(selector.matches(Path::new("b/d")));
    assert!(!selector.matches(Path::new("a")));
    assert!(!selector.matches(Path::new("b")));
    assert!(!selector.matches(Path::new("c")));
    assert!(!selector.matches(Path::new("d")));
}

fn dummy_config() -> LibraryConfig {
    LibraryConfig {
        library_folder: PathBuf::from("a/b/c"),
        lyrics: None,
        config_folders: vec![],
        custom_fields: vec![],
        named_strategies: HashMap::new(),
        find_replace: HashMap::new(),
        scan: vec![ScanOptions {
            pattern: Regex::new("\\.mp3$").unwrap(),
            tags: ScanDecision::Set(Rc::new(TagOptions {
                flac: TagSettings::Ignore,
                id3: TagSettings::Ignore,
                ape: TagSettings::Ignore,
            })),
        }],
    }
}

#[test]
fn copy_field_simple() {
    let path = PathBuf::from("a/b/c");
    let config = dummy_config();
    let getter = ValueGetter::Copy {
        copy: MetadataField::Performers,
        modify: None,
    };
    let mut copy_source = HashMap::new();
    copy_source.insert(
        MetadataField::Performers,
        MetadataValue::List(vec![String::from("item")]),
    );
    let result = getter.get(&copy_source, &path, &config).ok().unwrap();
    assert!(matches!(result, MetadataValue::List(x) if x.as_slice() == ["item"]));
}

#[test]
fn copy_field_nested() {
    let path = PathBuf::from("a/b/c");
    let config = dummy_config();
    let getter = ValueGetter::From {
        from: Rc::new(LocalItemSelector::This),
        value: FieldValueGetter::FileName,
        if_missing: WarnBehavior::Exit,
        modify: Some(Rc::new(ValueModifier::Append {
            append: Box::new(ValueGetter::Copy {
                copy: MetadataField::Performers,
                modify: None,
            }),
            index: None,
        })),
    };
    let mut copy_source = HashMap::new();
    copy_source.insert(
        MetadataField::Performers,
        MetadataValue::List(vec![String::from("item")]),
    );
    let result = getter.get(&copy_source, &path, &config).ok().unwrap();
    assert!(matches!(result, MetadataValue::List(x) if x.as_slice() == ["citem"]));
}

#[test]
fn copy_field_missing() {
    let path = PathBuf::from("a/b/c");
    let config = dummy_config();
    let getter = ValueGetter::Copy {
        copy: MetadataField::Performers,
        modify: None,
    };
    let copy_source = HashMap::new();
    let result = getter.get(&copy_source, &path, &config).unwrap_err();
    assert!(matches!(result, ValueError::CopyNotFound { .. }));
}

fn fast_config(selector: ItemSelector, op: MetadataOperation) -> LoadedConfig {
    LoadedConfig {
        config: Rc::new(SongConfig {
            set: vec![Rc::new(AllSetter {
                names: selector,
                must_be: None,
                set: Rc::new(op),
            })],
            order: None,
            subconfigs: HashMap::new(),
        }),
        nice_folder: PathBuf::new(),
        full_path: PathBuf::new(),
    }
}

fn fast_copy(field: MetadataField) -> ValueGetter {
    ValueGetter::Copy {
        copy: field,
        modify: None,
    }
}

#[test]
fn copy_full_simple() {
    let config = dummy_config();
    let path = ItemPath::Song(PathBuf::from("a/b/c"));
    let configs = [
        fast_config(
            ItemSelector::All { recursive: true },
            MetadataOperation::Set(HashMap::from([(
                MetadataField::Title,
                ValueGetter::Direct(MetadataValue::string(String::from("test"))),
            )])),
        ),
        fast_config(
            ItemSelector::All { recursive: true },
            MetadataOperation::Set(HashMap::from([(
                MetadataField::Performers,
                fast_copy(MetadataField::Title),
            )])),
        ),
    ];
    let results = metadata::get_metadata(&path, &configs, &config);
    let field = results.metadata.get(&MetadataField::Performers).unwrap();
    assert!(matches!(field, MetadataValue::List(x) if x.as_slice() == ["test"]));
    assert!(results.reports.into_iter().all(|x| x.errors.is_empty()));
}

#[test]
fn copy_full_self() {
    let config = dummy_config();
    let path = ItemPath::Song(PathBuf::from("a/b/c"));
    let configs = [fast_config(
        ItemSelector::All { recursive: true },
        MetadataOperation::Set(HashMap::from([(
            MetadataField::Title,
            fast_copy(MetadataField::Title),
        )])),
    )];
    let results = metadata::get_metadata(&path, &configs, &config);
    let field = results.metadata.get(&MetadataField::Title);
    assert!(field.is_none());
    assert!(matches!(
        results.reports[0].errors[0],
        ValueError::CopyNotFound {
            field: MetadataField::Title,
            ..
        }
    ));
}

#[test]
fn copy_full_missing() {
    let config = dummy_config();
    let path = ItemPath::Song(PathBuf::from("a/b/c"));
    let configs = [fast_config(
        ItemSelector::All { recursive: true },
        MetadataOperation::Set(HashMap::from([(
            MetadataField::Title,
            fast_copy(MetadataField::Performers),
        )])),
    )];
    let results = metadata::get_metadata(&path, &configs, &config);
    let field = results.metadata.get(&MetadataField::Title);
    assert!(field.is_none());
    assert!(matches!(
        results.reports[0].errors[0],
        ValueError::CopyNotFound {
            field: MetadataField::Performers,
            ..
        }
    ));
}

#[test]
fn copy_full_chain() {
    let config = dummy_config();
    let path = ItemPath::Song(PathBuf::from("a/b/c"));
    let configs = [
        fast_config(
            ItemSelector::All { recursive: true },
            MetadataOperation::Set(HashMap::from([(
                MetadataField::Title,
                ValueGetter::Direct(MetadataValue::string(String::from("test"))),
            )])),
        ),
        fast_config(
            ItemSelector::All { recursive: true },
            MetadataOperation::Set(HashMap::from([(
                MetadataField::Performers,
                fast_copy(MetadataField::Title),
            )])),
        ),
        fast_config(
            ItemSelector::All { recursive: true },
            MetadataOperation::Set(HashMap::from([(
                MetadataField::Composers,
                fast_copy(MetadataField::Performers),
            )])),
        ),
        fast_config(
            ItemSelector::All { recursive: true },
            MetadataOperation::Set(HashMap::from([(
                MetadataField::Genres,
                fast_copy(MetadataField::Composers),
            )])),
        ),
    ];
    let results = metadata::get_metadata(&path, &configs, &config);
    assert!(
        matches!(results.metadata.get(&MetadataField::Title).unwrap(), MetadataValue::List(x) if x.as_slice() == ["test"])
    );
    assert!(
        matches!(results.metadata.get(&MetadataField::Performers).unwrap(), MetadataValue::List(x) if x.as_slice() == ["test"])
    );
    assert!(
        matches!(results.metadata.get(&MetadataField::Composers).unwrap(), MetadataValue::List(x) if x.as_slice() == ["test"])
    );
    assert!(
        matches!(results.metadata.get(&MetadataField::Genres).unwrap(), MetadataValue::List(x) if x.as_slice() == ["test"])
    );
    assert!(results.reports.into_iter().all(|x| x.errors.is_empty()));
}

#[test]
fn copy_full_modify() {
    let config = dummy_config();
    let path = ItemPath::Song(PathBuf::from("a/b/c"));
    let configs = [
        fast_config(
            ItemSelector::All { recursive: true },
            MetadataOperation::Set(HashMap::from([(
                MetadataField::Title,
                ValueGetter::Direct(MetadataValue::string(String::from("test"))),
            )])),
        ),
        fast_config(
            ItemSelector::All { recursive: true },
            MetadataOperation::Set(HashMap::from([(
                MetadataField::Performers,
                ValueGetter::Direct(MetadataValue::string(String::from("before-"))),
            )])),
        ),
        fast_config(
            ItemSelector::All { recursive: true },
            MetadataOperation::Modify {
                modify: HashMap::from([(
                    MetadataField::Performers,
                    Rc::new(ValueModifier::Append {
                        index: None,
                        append: Box::new(fast_copy(MetadataField::Title)),
                    }),
                )]),
            },
        ),
    ];
    let results = metadata::get_metadata(&path, &configs, &config);
    let field = results.metadata.get(&MetadataField::Performers).unwrap();
    assert!(matches!(field, MetadataValue::List(x) if x.as_slice() == ["before-test"]));
    assert!(results.reports.into_iter().all(|x| x.errors.is_empty()));
}

#[test]
fn path_equality() {
    let path1 = ItemPath::Song(PathBuf::from_str("test thing").unwrap());
    let path2 = ItemPath::Song(PathBuf::from_str("test thing").unwrap());
    assert_eq!(path1, path2);
    assert!(path1 == path2);
}

#[test]
fn selector_matches() {
    let tmp_dir = tempdir::TempDir::new("nmu-tests").unwrap();
    let config = dummy_config();
    let make_file = |str: &str| {
        let full = tmp_dir
            .path()
            .join(PathBuf::from(str))
            .with_extension("mp3");
        std::fs::create_dir_all(full.parent().unwrap()).unwrap();
        std::fs::File::create(&full).unwrap();
        std::fs::File::create(full.with_extension("fake")).unwrap();
    };
    let assert_results = |selector: ItemSelector, start: &str, desired: &[&str]| {
        let desired = desired.iter().map(PathBuf::from).collect::<Vec<_>>();
        let actual: Vec<PathBuf> = crate::file_stuff::find_matches(
            &selector,
            &if start.is_empty() {
                tmp_dir.path().to_owned()
            } else {
                tmp_dir.path().join(start)
            },
            &config,
        )
        .into_iter()
        .map(|x| x.into())
        .collect::<Vec<_>>();
        assert_eq!(actual, desired);
        assert!(desired.into_iter().all(|x| selector.matches(&x)));
    };
    make_file("a");
    make_file("b");
    make_file("c");
    make_file("sub/a");
    make_file("sub/e");
    make_file("sub/f");
    make_file("sub/deep1/one");
    make_file("sub/deep2/one");
    assert_results(
        ItemSelector::All { recursive: true },
        "",
        &[
            "a",
            "b",
            "c",
            "sub",
            "sub/a",
            "sub/deep1",
            "sub/deep1/one",
            "sub/deep2",
            "sub/deep2/one",
            "sub/e",
            "sub/f",
        ],
    );
    assert_results(
        ItemSelector::All { recursive: false },
        "",
        &["a", "b", "c", "sub"],
    );
    assert_results(
        ItemSelector::All { recursive: true },
        "sub",
        &["a", "deep1", "deep1/one", "deep2", "deep2/one", "e", "f"],
    );
    assert_results(
        ItemSelector::All { recursive: false },
        "sub",
        &["a", "deep1", "deep2", "e", "f"],
    );
    assert_results(ItemSelector::Path(PathBuf::from("b")), "", &["b"]);
    assert_results(ItemSelector::Path(PathBuf::from("b")), "sub", &[]);
    assert_results(ItemSelector::Path(PathBuf::from("sub")), "", &["sub"]);
    assert_results(
        ItemSelector::Multi(vec![
            ItemSelector::Path(PathBuf::from("a")),
            ItemSelector::Path(PathBuf::from("sub/e")),
        ]),
        "",
        &["a", "sub/e"],
    );
    assert_results(
        ItemSelector::Segmented {
            path: vec![
                PathSegment::Literal(String::from("sub")),
                PathSegment::Regex {
                    regex: regex::Regex::new(r"deep\d").unwrap(),
                },
                PathSegment::Literal(String::from("one")),
            ],
        },
        "",
        &["sub/deep1/one", "sub/deep2/one"],
    );
    assert_results(
        ItemSelector::Subpath {
            subpath: Box::new(ItemSelector::Path(PathBuf::from("sub"))),
            select: Box::new(ItemSelector::Multi(vec![
                ItemSelector::Path(PathBuf::from("a")),
                ItemSelector::Path(PathBuf::from("deep1/one")),
            ])),
        },
        "",
        &["sub/a", "sub/deep1/one"],
    );
}

#[test]
fn reverse_config() {
    let files = HashMap::from([
        (
            PathBuf::from("a"),
            (
                HashMap::new(),
                HashMap::from([
                    (
                        MetadataField::Title,
                        MetadataValue::string(String::from("a")),
                    ),
                    (
                        MetadataField::Album,
                        MetadataValue::string(String::from("test")),
                    ),
                    (
                        MetadataField::Performers,
                        MetadataValue::string(String::from("guy 1")),
                    ),
                ]),
            ),
        ),
        (
            PathBuf::from("b"),
            (
                HashMap::new(),
                HashMap::from([
                    (
                        MetadataField::Title,
                        MetadataValue::string(String::from("b")),
                    ),
                    (
                        MetadataField::Album,
                        MetadataValue::string(String::from("test")),
                    ),
                    (
                        MetadataField::Performers,
                        MetadataValue::string(String::from("guy 1")),
                    ),
                ]),
            ),
        ),
        (
            PathBuf::from("c"),
            (
                HashMap::new(),
                HashMap::from([
                    (
                        MetadataField::Title,
                        MetadataValue::string(String::from("c")),
                    ),
                    (
                        MetadataField::Album,
                        MetadataValue::string(String::from("test")),
                    ),
                    (
                        MetadataField::Performers,
                        MetadataValue::string(String::from("guy 1")),
                    ),
                ]),
            ),
        ),
        (
            PathBuf::from("d"),
            (
                HashMap::new(),
                HashMap::from([
                    (
                        MetadataField::Title,
                        MetadataValue::string(String::from("d")),
                    ),
                    (
                        MetadataField::Album,
                        MetadataValue::string(String::from("test")),
                    ),
                    (
                        MetadataField::Performers,
                        MetadataValue::string(String::from("guy 2")),
                    ),
                ]),
            ),
        ),
        (
            PathBuf::from("e"),
            (
                HashMap::new(),
                HashMap::from([
                    (
                        MetadataField::Title,
                        MetadataValue::string(String::from("e")),
                    ),
                    (
                        MetadataField::Album,
                        MetadataValue::string(String::from("test")),
                    ),
                    (
                        MetadataField::Performers,
                        MetadataValue::string(String::from("guy 2")),
                    ),
                ]),
            ),
        ),
        (
            PathBuf::from("f"),
            (
                HashMap::new(),
                HashMap::from([
                    (
                        MetadataField::Title,
                        MetadataValue::string(String::from("f")),
                    ),
                    (
                        MetadataField::Album,
                        MetadataValue::string(String::from("test")),
                    ),
                    (
                        MetadataField::Performers,
                        MetadataValue::string(String::from("guy 2")),
                    ),
                ]),
            ),
        ),
        (
            PathBuf::from("g"),
            (
                HashMap::new(),
                HashMap::from([
                    (
                        MetadataField::Title,
                        MetadataValue::string(String::from("g")),
                    ),
                    (
                        MetadataField::Album,
                        MetadataValue::string(String::from("test")),
                    ),
                    (
                        MetadataField::Performers,
                        MetadataValue::string(String::from("guy 3")),
                    ),
                ]),
            ),
        ),
    ]);
    let config = RawSongConfigFile::make_reverse(files);
    println!("{}", serde_yaml::to_string(&config).unwrap());
}

#[test]
fn good_durations() {
    let durations = [
        ("0:00", 0.0),
        ("00:00", 0.0),
        ("1:00", 60.0),
        ("1:20", 80.0),
        ("2:33", 153.0),
        ("04:10", 250.0),
        ("3:02.5", 182.5),
        ("2:20:20", 8420.0),
        ("1:00:00.250", 3600.25),
        ("03:15:04.0", 11704.0),
        ("500:00:00", 1800000.0),
        ("00:00:00.0", 0.0),
    ];
    for (str, secs) in durations {
        assert_eq!(
            parse_duration(str).expect(str),
            core::time::Duration::from_secs_f32(secs),
            "{str}"
        );
    }
}

#[test]
fn bad_durations() {
    assert!(matches!(
        parse_duration("").unwrap_err(),
        TimeParseError::MissingColon
    ));
    assert!(matches!(
        parse_duration("aa").unwrap_err(),
        TimeParseError::MissingColon
    ));
    assert!(matches!(
        parse_duration("a:a").unwrap_err(),
        TimeParseError::Int(x) if *x.kind() == IntErrorKind::InvalidDigit
    ));
    assert!(matches!(
        parse_duration("0:0").unwrap_err(),
        TimeParseError::WrongLength
    ));
    assert!(matches!(
        parse_duration("00:0").unwrap_err(),
        TimeParseError::WrongLength
    ));
    assert!(matches!(
        parse_duration("000:00").unwrap_err(),
        TimeParseError::WrongLength
    ));
    assert!(matches!(
        parse_duration("0:000").unwrap_err(),
        TimeParseError::WrongLength
    ));
    assert!(matches!(
        parse_duration("0:5.55").unwrap_err(),
        TimeParseError::WrongLength
    ));
    assert!(matches!(
        parse_duration("0:-3.5").unwrap_err(),
        TimeParseError::ExceedsBounds
    ));
    assert!(matches!(
        parse_duration("70:00").unwrap_err(),
        TimeParseError::ExceedsBounds
    ));
    assert!(matches!(
        parse_duration("10:60").unwrap_err(),
        TimeParseError::ExceedsBounds
    ));
    assert!(matches!(
        parse_duration("20:00.123asdf").unwrap_err(),
        TimeParseError::Float(_)
    ));
    assert!(matches!(
        parse_duration("10:2:30").unwrap_err(),
        TimeParseError::WrongLength
    ));
    assert!(matches!(
        parse_duration("10:20:3").unwrap_err(),
        TimeParseError::WrongLength
    ));
    assert!(matches!(
        parse_duration("10:20:30:40").unwrap_err(),
        TimeParseError::WrongLength
    ));
    assert!(matches!(
        parse_duration("10:20:").unwrap_err(),
        TimeParseError::WrongLength
    ));
    assert!(matches!(
        parse_duration(":10:20").unwrap_err(),
        TimeParseError::Int(x) if *x.kind() == IntErrorKind::Empty
    ));
    assert!(matches!(
        parse_duration(":").unwrap_err(),
        TimeParseError::Int(x) if *x.kind() == IntErrorKind::Empty
    ));
}
