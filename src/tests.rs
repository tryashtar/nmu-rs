#[cfg(test)]

use crate::song_config::MetadataOperation;

#[test]
fn map_strat() {
    let result = serde_yaml::from_str::<MetadataOperation>("title: \"test\"").expect("Should parse");
    assert!(matches!(result, MetadataOperation::Set { .. }));
}

#[test]
fn remove_strat() {
    let result = serde_yaml::from_str::<MetadataOperation>("remove: [title]").expect("Should parse");
    assert!(matches!(result, MetadataOperation::Blank { .. }));
}