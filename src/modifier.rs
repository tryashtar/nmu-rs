use std::{path::Path, rc::Rc};

use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::{
    library_config::LibraryConfig,
    metadata::{Metadata, MetadataField, MetadataValue, RegexWrap},
    strategy::{LocalItemSelector, ValueGetter},
    util::{OutOfBoundsDecision, Range},
};

#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(untagged)]
pub enum ValueModifier {
    Prepend {
        prepend: Box<ValueGetter>,
        #[serde(skip_serializing_if = "Option::is_none")]
        index: Option<Range>,
    },
    Append {
        append: Box<ValueGetter>,
        #[serde(skip_serializing_if = "Option::is_none")]
        index: Option<Range>,
    },
    InsertBefore {
        insert: Box<ValueGetter>,
        before: Range,
        #[serde(default = "default_oob")]
        out_of_bounds: OutOfBoundsDecision,
    },
    InsertAfter {
        insert: Box<ValueGetter>,
        after: Range,
        #[serde(default = "default_oob")]
        out_of_bounds: OutOfBoundsDecision,
    },
    Join {
        join: Box<ValueGetter>,
    },
    Split {
        split: String,
    },
    Regex {
        #[serde(with = "serde_regex")]
        regex: Regex,
    },
    Replace {
        replace: String,
    },
    Take {
        take: TakeModifier,
    },
    Multiple(Vec<Rc<ValueModifier>>),
}

#[derive(Clone, Debug)]
pub enum ValueError {
    ExitRequested,
    CopyNotFound {
        field: MetadataField,
    },
    UnexpectedType {
        modifier: Rc<ValueModifier>,
        got: MetadataValue,
        expected: &'static str,
    },
    MissingField {
        modifier: Rc<ValueModifier>,
        field: MetadataField,
    },
    ItemNotFound {
        selector: Rc<LocalItemSelector>,
    },
}
pub fn inline_data<T>(item: &T) -> String
where
    T: serde::Serialize,
{
    serde_json::to_string(item).unwrap_or_else(|_| String::from("???"))
}
impl std::fmt::Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedType {
                modifier,
                got,
                expected,
            } => {
                let mod_str = inline_data(&modifier);
                write!(f, "Modifier {mod_str} expected {expected}, but got {got}")
            }
            Self::MissingField { modifier, field } => {
                let mod_str = inline_data(&modifier);
                write!(
                    f,
                    "Modifier {mod_str} tried to modify {field}, but no value was found",
                )
            }
            Self::ItemNotFound { selector } => {
                let sel_str = inline_data(&selector);
                write!(f, "Selector {sel_str} didn't find anything")
            }
            Self::ExitRequested => f.write_str("Conditions not met, skipping"),
            Self::CopyNotFound { field } => {
                write!(f, "Tried to copy {field}, but no value was found",)
            }
        }
    }
}

impl ValueModifier {
    fn take(
        list: &[String],
        range: &Range,
        oob: OutOfBoundsDecision,
        min_length: Option<usize>,
    ) -> Result<MetadataValue, ValueError> {
        match range.slice(list, oob) {
            None => Err(ValueError::ExitRequested),
            Some(slice) => {
                if let Some(min) = min_length {
                    if list.len() < min {
                        return Err(ValueError::ExitRequested);
                    }
                }
                Ok(MetadataValue::List(slice.to_vec()))
            }
        }
    }
    fn checked_append(
        self: &Rc<Self>,
        copy_source: &Metadata,
        value: MetadataValue,
        append: &ValueGetter,
        path: &Path,
        config: &LibraryConfig,
        index: Option<&Range>,
        appending: bool,
    ) -> Result<MetadataValue, ValueError> {
        if let MetadataValue::List(mut list) = value {
            let extra = append.get(copy_source, path, config)?;
            if let Some(str) = extra.as_string() {
                Self::append(&mut list, str, index, appending);
                return Ok(MetadataValue::List(list));
            }
            Err(ValueError::UnexpectedType {
                modifier: self.clone(),
                got: extra,
                expected: "single string",
            })
        } else {
            Err(ValueError::UnexpectedType {
                modifier: self.clone(),
                got: value,
                expected: "list",
            })
        }
    }
    fn append(list: &mut [String], extra: &str, index: Option<&Range>, appending: bool) {
        for i in 0..list.len() {
            let should_modify = index.map_or(true, |index| {
                index.in_range(i, list.len(), OutOfBoundsDecision::Clamp)
            });
            if should_modify {
                if appending {
                    list[i].push_str(extra);
                } else {
                    list[i].insert_str(0, extra);
                }
            }
        }
    }
    fn checked_insert(
        self: &Rc<Self>,
        copy_source: &Metadata,
        value: MetadataValue,
        insert: &ValueGetter,
        point: &Range,
        out_of_bounds: OutOfBoundsDecision,
        add: usize,
        path: &Path,
        config: &LibraryConfig,
    ) -> Result<MetadataValue, ValueError> {
        if let MetadataValue::List(val) = insert.get(copy_source, path, config)? {
            if let MetadataValue::List(mut list) = value {
                return match Range::wrap_both(point, list.len(), out_of_bounds) {
                    None => Err(ValueError::ExitRequested),
                    Some((start, stop)) => {
                        let range = (start + add)..(stop + add);
                        list.splice(range, val);
                        Ok(MetadataValue::List(list))
                    }
                };
            }
        }
        Err(ValueError::UnexpectedType {
            modifier: self.clone(),
            got: value,
            expected: "list",
        })
    }
    pub fn modify(
        self: &Rc<Self>,
        copy_source: &Metadata,
        mut value: MetadataValue,
        path: &Path,
        config: &LibraryConfig,
    ) -> Result<MetadataValue, ValueError> {
        match Rc::as_ref(self) {
            Self::Multiple(items) => {
                for item in items {
                    value = item.modify(copy_source, value, path, config)?;
                }
                Ok(value)
            }
            Self::Replace { replace } => {
                if let MetadataValue::RegexMatches { source, regex } = value {
                    return Ok(MetadataValue::string(
                        regex.0.replace(&source, replace).into_owned(),
                    ));
                }
                Err(ValueError::UnexpectedType {
                    modifier: self.clone(),
                    got: value,
                    expected: "regex",
                })
            }
            Self::Regex { regex } => {
                if let Some(str) = value.as_string() {
                    return Ok(MetadataValue::RegexMatches {
                        source: str.to_owned(),
                        regex: RegexWrap(regex.clone()),
                    });
                }
                Err(ValueError::UnexpectedType {
                    modifier: self.clone(),
                    got: value,
                    expected: "single string",
                })
            }
            Self::InsertBefore {
                insert,
                before,
                out_of_bounds,
            } => self.checked_insert(
                copy_source,
                value,
                insert,
                before,
                *out_of_bounds,
                0,
                path,
                config,
            ),
            Self::InsertAfter {
                insert,
                after,
                out_of_bounds,
            } => self.checked_insert(
                copy_source,
                value,
                insert,
                after,
                *out_of_bounds,
                1,
                path,
                config,
            ),
            Self::Take { take } => {
                if let MetadataValue::List(list) = value {
                    return match take {
                        TakeModifier::Defined {
                            index,
                            out_of_bounds,
                            min_length,
                        } => Self::take(&list, index, *out_of_bounds, min_length.as_ref().copied()),
                        TakeModifier::Simple(range) => {
                            Self::take(&list, range, OutOfBoundsDecision::Exit, None)
                        }
                    };
                }
                Err(ValueError::UnexpectedType {
                    modifier: self.clone(),
                    got: value,
                    expected: "list",
                })
            }
            Self::Append { append, index } => self.checked_append(
                copy_source,
                value,
                append,
                path,
                config,
                index.as_ref(),
                true,
            ),
            Self::Prepend { prepend, index } => self.checked_append(
                copy_source,
                value,
                prepend,
                path,
                config,
                index.as_ref(),
                false,
            ),
            Self::Join { join } => {
                let extra = join.get(copy_source, path, config)?;
                if let Some(str) = extra.as_string() {
                    if let MetadataValue::List(list) = value {
                        return Ok(MetadataValue::string(list.join(str)));
                    }
                }
                Err(ValueError::UnexpectedType {
                    modifier: self.clone(),
                    got: value,
                    expected: "list",
                })
            }
            Self::Split { split } => {
                if let MetadataValue::List(list) = value {
                    let vec = list
                        .iter()
                        .flat_map(|x| x.split(split))
                        .map(|x| x.to_owned())
                        .collect::<Vec<_>>();
                    return Ok(MetadataValue::List(vec));
                }
                Err(ValueError::UnexpectedType {
                    modifier: self.clone(),
                    got: value,
                    expected: "list",
                })
            }
        }
    }
}

#[derive(Deserialize, Serialize, Clone, Debug)]
#[serde(untagged)]
pub enum TakeModifier {
    Simple(Range),
    Defined {
        index: Range,
        #[serde(default = "default_oob")]
        out_of_bounds: OutOfBoundsDecision,
        min_length: Option<usize>,
    },
}
const fn default_oob() -> OutOfBoundsDecision {
    OutOfBoundsDecision::Exit
}
