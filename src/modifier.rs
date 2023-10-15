use std::{path::Path, rc::Rc};

use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::{
    library_config::LibraryConfig,
    metadata::{MetadataField, MetadataValue, PendingValue},
    strategy::{LocalItemSelector, ValueGetter},
    util::{OutOfBoundsDecision, Range},
};

#[derive(Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum ValueModifier {
    Prepend {
        prepend: Box<ValueGetter>,
        index: Option<Range>,
    },
    Append {
        append: Box<ValueGetter>,
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

pub enum ValueError {
    UnexpectedType {
        modifier: Rc<ValueModifier>,
        got: PendingValue,
        expected: &'static str,
    },
    MissingField {
        modifier: Rc<ValueModifier>,
        field: MetadataField,
    },
    ItemNotFound {
        selector: Rc<LocalItemSelector>,
    },
    ExitRequested,
    ResolutionFailed {
        field: MetadataField,
        value: PendingValue,
    },
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
        self: &Rc<ValueModifier>,
        value: PendingValue,
        append: &ValueGetter,
        path: &Path,
        config: &LibraryConfig,
        index: Option<&Range>,
        appending: bool,
    ) -> Result<PendingValue, ValueError> {
        if let PendingValue::Ready(MetadataValue::List(mut list)) = value {
            let extra = append.get(path, config)?;
            match extra {
                PendingValue::Ready(ref val) => {
                    if let Some(str) = val.as_string() {
                        Self::append(&mut list, str, index, appending);
                        return Ok(MetadataValue::List(list).into());
                    }
                }
                PendingValue::CopyField {
                    field,
                    sources,
                    modify,
                } => {
                    let switched = Rc::new(if appending {
                        ValueModifier::Prepend {
                            prepend: Box::new(ValueGetter::Direct(MetadataValue::List(list))),
                            index: None,
                        }
                    } else {
                        ValueModifier::Append {
                            append: Box::new(ValueGetter::Direct(MetadataValue::List(list))),
                            index: None,
                        }
                    });
                    let new_modify = match modify {
                        None => switched,
                        Some(modify) => Rc::new(ValueModifier::Multiple(vec![modify, switched])),
                    };
                    return Ok(PendingValue::CopyField {
                        field,
                        sources,
                        modify: Some(new_modify),
                    });
                }
                _ => {}
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
    fn append(list: &mut Vec<String>, extra: &str, index: Option<&Range>, appending: bool) {
        let formatter = if appending {
            |str: &str, extra: &str| format!("{str}{extra}")
        } else {
            |str: &str, extra: &str| format!("{extra}{str}")
        };
        for i in 0..list.len() {
            match index {
                None => {
                    list[i] = formatter(&list[i], extra);
                }
                Some(index) if index.in_range(i, list.len(), OutOfBoundsDecision::Clamp) => {
                    list[i] = formatter(&list[i], extra);
                }
                Some(_) => {}
            };
        }
    }
    fn checked_insert(
        self: &Rc<ValueModifier>,
        value: PendingValue,
        insert: &ValueGetter,
        point: &Range,
        out_of_bounds: OutOfBoundsDecision,
        add: usize,
        path: &Path,
        config: &LibraryConfig,
    ) -> Result<PendingValue, ValueError> {
        if let PendingValue::Ready(MetadataValue::List(val)) = insert.get(path, config)? {
            if let PendingValue::Ready(MetadataValue::List(mut list)) = value {
                return match Range::wrap_both(point, list.len(), out_of_bounds) {
                    None => Err(ValueError::ExitRequested),
                    Some((start, stop)) => {
                        let range = (start + add)..(stop + add);
                        list.splice(range, val);
                        Ok(MetadataValue::List(list).into())
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
        mut value: PendingValue,
        path: &Path,
        config: &LibraryConfig,
    ) -> Result<PendingValue, ValueError> {
        if let PendingValue::CopyField {
            field,
            sources,
            modify,
        } = value
        {
            let new_modify = match modify {
                None => self.clone(),
                Some(modify) => Rc::new(ValueModifier::Multiple(vec![modify, self.clone()])),
            };
            return Ok(PendingValue::CopyField {
                field,
                sources,
                modify: Some(new_modify),
            });
        }
        match Rc::as_ref(self) {
            Self::Multiple(items) => {
                for item in items {
                    value = item.modify(value, path, config)?;
                }
                Ok(value)
            }
            Self::Replace { replace } => {
                if let PendingValue::RegexMatches { source, regex } = value {
                    return Ok(
                        MetadataValue::string(regex.replace(&source, replace).into_owned()).into(),
                    );
                }
                Err(ValueError::UnexpectedType {
                    modifier: self.clone(),
                    got: value,
                    expected: "regex",
                })
            }
            Self::Regex { regex } => {
                if let PendingValue::Ready(val) = &value {
                    if let Some(str) = val.as_string() {
                        return Ok(PendingValue::RegexMatches {
                            source: str.to_owned(),
                            regex: regex.clone(),
                        });
                    }
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
            } => self.checked_insert(value, insert, before, *out_of_bounds, 0, path, config),
            Self::InsertAfter {
                insert,
                after,
                out_of_bounds,
            } => self.checked_insert(value, insert, after, *out_of_bounds, 1, path, config),
            Self::Take { take } => {
                if let PendingValue::Ready(MetadataValue::List(list)) = value {
                    return match take {
                        TakeModifier::Defined {
                            index,
                            out_of_bounds,
                            min_length,
                        } => Self::take(&list, index, *out_of_bounds, min_length.as_ref().copied())
                            .map(|x| x.into()),
                        TakeModifier::Simple(range) => {
                            Self::take(&list, range, OutOfBoundsDecision::Exit, None)
                                .map(|x| x.into())
                        }
                    };
                }
                Err(ValueError::UnexpectedType {
                    modifier: self.clone(),
                    got: value,
                    expected: "list",
                })
            }
            Self::Append { append, index } => {
                self.checked_append(value, append, path, config, index.as_ref(), true)
            }
            Self::Prepend { prepend, index } => {
                self.checked_append(value, prepend, path, config, index.as_ref(), false)
            }
            Self::Join { join } => {
                if let PendingValue::Ready(extra) = join.get(path, config)? {
                    if let Some(str) = extra.as_string() {
                        if let PendingValue::Ready(MetadataValue::List(list)) = value {
                            return Ok(MetadataValue::string(list.join(str)).into());
                        }
                    }
                }
                Err(ValueError::UnexpectedType {
                    modifier: self.clone(),
                    got: value,
                    expected: "list",
                })
            }
            Self::Split { split } => {
                if let PendingValue::Ready(MetadataValue::List(list)) = value {
                    let vec = list
                        .iter()
                        .flat_map(|x| x.split(split))
                        .map(|x| x.to_owned())
                        .collect::<Vec<_>>();
                    return Ok(MetadataValue::List(vec).into());
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

#[derive(Deserialize, Serialize, Clone)]
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
fn default_oob() -> OutOfBoundsDecision {
    OutOfBoundsDecision::Exit
}
