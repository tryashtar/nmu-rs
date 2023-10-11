use std::path::Path;

use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::{
    library_config::LibraryConfig,
    metadata::{PendingValue, MetadataValue}, strategy::ValueGetter, util::{Range, OutOfBoundsDecision, Borrowable},
};

#[derive(Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum ValueModifier<'a> {
    Prepend {
        prepend: Box<ValueGetter<'a>>,
        index: Option<Range>,
    },
    Append {
        append: Box<ValueGetter<'a>>,
        index: Option<Range>,
    },
    InsertBefore {
        insert: Box<ValueGetter<'a>>,
        before: Range,
        #[serde(default = "default_oob")]
        out_of_bounds: OutOfBoundsDecision,
    },
    InsertAfter {
        insert: Box<ValueGetter<'a>>,
        after: Range,
        #[serde(default = "default_oob")]
        out_of_bounds: OutOfBoundsDecision,
    },
    Join {
        join: Box<ValueGetter<'a>>,
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
    Sequence(Vec<Borrowable<'a, ValueModifier<'a>>>)
}

pub enum ValueError {
    UnexpectedType,
    ItemNotFound,
    NoMatchFound,
    EmptyList,
    ConditionsNotMet,
}

impl ValueModifier<'_> {
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
    pub fn modify<'a>(
        &'a self,
        value: PendingValue<'a>,
        path: &Path,
        config: &LibraryConfig,
    ) -> Result<PendingValue, ValueError> {
        if let PendingValue::CopyField {
            field,
            sources,
            modify,
        } = value
        {
            let mut modifiers = vec![];
            if let Some(md) = modify {
                modifiers.push(md);
            }
            modifiers.push(Borrowable::Borrowed(self));
            return Ok(PendingValue::CopyField {
                field,
                sources,
                modify: Some(Borrowable::Owned(ValueModifier::Sequence(modifiers))),
            });
        }
        match self {
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
            Self::Sequence(seq) => {
                if seq.is_empty() {
                    Err(ValueError::EmptyList)
                } else {
                    let value = seq[0].modify(value, path, config)?;
                    seq.iter()
                        .skip(1)
                        .try_fold(value, |x, y| y.modify(x, path, config))
                }
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
