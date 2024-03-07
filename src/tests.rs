use crate::{
    library_config::{DateCache, LibraryConfig},
    metadata::{MetadataField, MetadataValue},
    modifier::ValueModifier,
    strategy::{
        FieldSelector, FieldValueGetter, ItemSelector, LocalItemSelector, MetadataOperation,
        PathSegment, ValueGetter,
    },
    util::{ItemPath, OutOfBoundsDecision, Range},
};
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

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
    )
}

#[test]
fn range_int_last() {
    let range: Range = 3.into();
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Exit).unwrap(),
        ['d']
    )
}

#[test]
fn range_int_from_back() {
    let range: Range = (-2).into();
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Exit).unwrap(),
        ['c']
    )
}

#[test]
fn range_int_too_big() {
    let range: Range = 5.into();
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(range.slice(&values, OutOfBoundsDecision::Exit), None)
}

#[test]
fn range_int_too_small() {
    let range: Range = (-5).into();
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(range.slice(&values, OutOfBoundsDecision::Exit), None)
}

#[test]
fn range_mult_all() {
    let range: Range = Range::new(0, -1);
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Exit).unwrap(),
        ['a', 'b', 'c', 'd']
    )
}

#[test]
fn range_mult_backwards() {
    let range: Range = Range::new(1, 0);
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(range.slice(&values, OutOfBoundsDecision::Exit).unwrap(), [])
}

#[test]
fn range_mult_some() {
    let range: Range = Range::new(1, 2);
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Exit).unwrap(),
        ['b', 'c']
    )
}

#[test]
fn range_mult_clamp() {
    let range: Range = Range::new(2, 5);
    let values = ['a', 'b', 'c', 'd'];
    assert_eq!(
        range.slice(&values, OutOfBoundsDecision::Clamp).unwrap(),
        ['c', 'd']
    )
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
            PathSegment::Literal("a".to_owned()),
            PathSegment::Literal("b".to_owned())
        ]
    }
    .matches(Path::new("a/b")));
}

#[test]
fn select_segment_partial() {
    assert!(ItemSelector::Segmented {
        path: vec![
            PathSegment::Literal("a".to_owned()),
            PathSegment::Literal("b".to_owned())
        ]
    }
    .matches(Path::new("a/b/c")));
}

#[test]
fn select_segment_too_far() {
    assert!(!ItemSelector::Segmented {
        path: vec![
            PathSegment::Literal("a".to_owned()),
            PathSegment::Literal("b".to_owned())
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
        reports: vec![],
        lyrics: None,
        log_folder: None,
        config_folders: vec![],
        song_extensions: HashSet::from(["a".to_owned()]),
        custom_fields: vec![],
        date_cache: DateCache::new(None),
        art_repo: None,
        named_strategies: HashMap::new(),
        find_replace: HashMap::new(),
        artist_separator: ";".to_owned(),
    }
}

#[test]
fn copy_field_resolution() {
    let path = PathBuf::from("a/b/c");
    let config = dummy_config();
    let getter = ValueGetter::Copy {
        from: Rc::new(LocalItemSelector::This),
        copy: MetadataField::Performers,
        modify: None,
    };
    let copy_cache = HashMap::new();
    let result = getter
        .get(&path, &config, &copy_cache)
        .unwrap_or_else(|_| panic!("should resolve"));
    assert!(matches!(result, MetadataValue::List(x) if x.as_slice() == ["item"]));
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
            .with_extension(config.song_extensions.iter().next().unwrap());
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
        assert!(desired.into_iter().all(|x| selector.matches(&x)))
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
                PathSegment::Literal("sub".to_owned()),
                PathSegment::Regex {
                    regex: regex::Regex::new(r"deep\d").unwrap(),
                },
                PathSegment::Literal("one".to_owned()),
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
