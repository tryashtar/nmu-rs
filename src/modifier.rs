use std::{path::Path, rc::Rc};

use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::{
    library_config::LibraryConfig,
    metadata::{MetadataValue, PendingValue},
    strategy::ValueGetter,
    util::{Listable, OutOfBoundsDecision, Range},
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
}

pub enum ValueError {
    UnexpectedType,
    ItemNotFound,
    NoMatchFound,
    ConditionsNotMet,
}

impl ValueModifier {
    fn take(
        list: &[String],
        range: &Range,
        oob: OutOfBoundsDecision,
        min_length: Option<usize>,
    ) -> Result<MetadataValue, ValueError> {
        let result = range.slice(list, oob);
        if let Some(min) = min_length {
            if list.len() < min {
                return Err(ValueError::ConditionsNotMet);
            }
        }
        Ok(MetadataValue::List(result.to_vec()))
    }
    fn append(
        value: &MetadataValue,
        extra: &str,
        index: Option<&Range>,
        appending: bool,
    ) -> Result<MetadataValue, ValueError> {
        let formatter = if appending {
            |str: &str, extra: &str| format!("{str}{extra}")
        } else {
            |str: &str, extra: &str| format!("{extra}{str}")
        };
        match value {
            MetadataValue::List(list) => Ok(MetadataValue::List(
                list.iter()
                    .enumerate()
                    .map(|(i, x)| match index {
                        None => formatter(x, extra),
                        Some(index) => {
                            if index.in_range(i, list.len(), OutOfBoundsDecision::Clamp) {
                                formatter(x, extra)
                            } else {
                                x.clone()
                            }
                        }
                    })
                    .collect(),
            )),
            MetadataValue::Number(_) => Err(ValueError::UnexpectedType),
        }
    }
    pub fn modify_all(
        items: &[Rc<Self>],
        mut value: PendingValue,
        path: &Path,
        config: &LibraryConfig,
    ) -> Result<PendingValue, ValueError> {
        for item in items {
            value = item.modify(value, path, config)?;
        }
        Ok(value)
    }
    pub fn modify(
        self: &Rc<Self>,
        value: PendingValue,
        path: &Path,
        config: &LibraryConfig,
    ) -> Result<PendingValue, ValueError> {
        if let PendingValue::CopyField {
            field,
            sources,
            mut modify,
        } = value
        {
            match modify {
                Listable::List(ref mut modify_list) => modify_list.push(self.clone()),
                Listable::Single(single) => modify = Listable::List(vec![single, self.clone()]),
            };
            return Ok(PendingValue::CopyField {
                field,
                sources,
                modify,
            });
        }
        match Rc::as_ref(self) {
            Self::Replace { replace } => {
                if let PendingValue::RegexMatches { source, regex } = value {
                    return Ok(
                        MetadataValue::string(regex.replace(&source, replace).into_owned()).into(),
                    );
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Regex { regex } => {
                if let PendingValue::Ready(val) = value {
                    if let Some(str) = val.as_string() {
                        return Ok(PendingValue::RegexMatches {
                            source: str.to_owned(),
                            regex: regex.clone(),
                        });
                    } else {
                        return Err(ValueError::NoMatchFound);
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::InsertBefore {
                insert,
                before,
                out_of_bounds,
            } => {
                if let PendingValue::Ready(MetadataValue::List(val)) = insert.get(path, config)? {
                    if let PendingValue::Ready(MetadataValue::List(list)) = value {
                        return match Range::wrap_both(before, list.len(), *out_of_bounds) {
                            None => Err(ValueError::ConditionsNotMet),
                            Some((start, stop)) => {
                                let mut result = list.clone();
                                let range = start..stop;
                                result.splice(range, val);
                                Ok(MetadataValue::List(result).into())
                            }
                        };
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::InsertAfter {
                insert,
                after,
                out_of_bounds,
            } => {
                if let PendingValue::Ready(MetadataValue::List(val)) = insert.get(path, config)? {
                    if let PendingValue::Ready(MetadataValue::List(list)) = value {
                        return match Range::wrap_both(after, list.len(), *out_of_bounds) {
                            None => Err(ValueError::ConditionsNotMet),
                            Some((start, stop)) => {
                                let mut result = list.clone();
                                let range = (start + 1)..(stop + 1);
                                result.splice(range, val);
                                Ok(MetadataValue::List(result).into())
                            }
                        };
                    }
                }
                Err(ValueError::UnexpectedType)
            }
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
                Err(ValueError::UnexpectedType)
            }
            Self::Append { append, index } => {
                if let PendingValue::Ready(val) = append.get(path, config)? {
                    if let Some(str) = val.as_string() {
                        if let PendingValue::Ready(value) = value {
                            return Ok(Self::append(&value, str, index.as_ref(), true)?.into());
                        }
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Prepend { prepend, index } => {
                if let PendingValue::Ready(val) = prepend.get(path, config)? {
                    if let Some(str) = val.as_string() {
                        if let PendingValue::Ready(value) = value {
                            return Ok(Self::append(&value, str, index.as_ref(), false)?.into());
                        }
                    }
                }
                Err(ValueError::UnexpectedType)
            }
            Self::Join { join } => {
                if let PendingValue::Ready(extra) = join.get(path, config)? {
                    if let Some(str) = extra.as_string() {
                        if let PendingValue::Ready(MetadataValue::List(list)) = value {
                            return Ok(MetadataValue::string(list.join(str)).into());
                        }
                    }
                }
                Err(ValueError::UnexpectedType)
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
                Err(ValueError::UnexpectedType)
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
