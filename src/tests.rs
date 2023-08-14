#[cfg(test)]
use super::*;

#[test]
fn meta_op_blank() {
    let result = serde_yaml::from_str::<MetadataOperation>("remove: [title]").unwrap();
    assert!(matches!(result, MetadataOperation::Blank { .. }));
}

#[test]
fn meta_op_keep() {
    let result = serde_yaml::from_str::<MetadataOperation>("keep: [title]").unwrap();
    assert!(matches!(result, MetadataOperation::Keep { .. }));
}

#[test]
fn meta_op_seq() {
    let result =
        serde_yaml::from_str::<MetadataOperation>("[{title: 'test'},{title: 'test'}]").unwrap();
    assert!(matches!(result, MetadataOperation::Sequence { .. }));
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
fn local_item_select() {
    let result = serde_yaml::from_str::<LocalItemSelector>("selector: 'a/b/c'").unwrap();
    assert!(matches!(result, LocalItemSelector::Select { .. }));
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
    assert!(matches!(
        result,
        MetadataField::Builtin(BuiltinMetadataField::Title)
    ));
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
    assert!(matches!(result, MetadataValue::String { .. }));
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
    assert!(matches!(result, ValueGetter::Copy { .. }));
}

#[test]
fn value_get_name() {
    let result = serde_yaml::from_str::<ItemValueGetter>("clean_name").unwrap();
    assert!(matches!(
        result,
        ItemValueGetter::Field(FieldValueGetter::CleanName)
    ));
}

#[test]
fn value_get_file() {
    let result = serde_yaml::from_str::<ItemValueGetter>("file_name").unwrap();
    assert!(matches!(
        result,
        ItemValueGetter::Field(FieldValueGetter::FileName)
    ));
}

#[test]
fn value_get_path() {
    let result = serde_yaml::from_str::<ItemValueGetter>("path").unwrap();
    assert!(matches!(
        result,
        ItemValueGetter::Field(FieldValueGetter::Path)
    ));
}

#[test]
fn value_get_meta() {
    let result = serde_yaml::from_str::<ItemValueGetter>("copy: title").unwrap();
    assert!(matches!(result, ItemValueGetter::Copy { .. }));
}
