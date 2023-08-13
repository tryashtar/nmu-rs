#[cfg(test)]

use crate::song_config::MetadataOperation;
use crate::song_config::FieldSelector;

#[test]
fn map_strat() {
    let result = serde_yaml::from_str::<MetadataOperation>("album artists: 'test'").expect("Should parse");
    assert!(matches!(result, MetadataOperation::Set { .. }));
}

#[test]
fn remove_strat() {
    let result = serde_yaml::from_str::<MetadataOperation>("remove: [title]").expect("Should parse");
    assert!(matches!(result, MetadataOperation::Blank { .. }));
}

#[test]
fn find_all() {
    let result = serde_yaml::from_str::<FieldSelector>("'*'").expect("Should parse");
    assert!(matches!(result, FieldSelector::All { .. }));
}