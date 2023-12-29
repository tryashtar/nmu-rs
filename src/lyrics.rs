use std::{
    num::{ParseFloatError, ParseIntError},
    time::Duration,
};

use itertools::Itertools;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Serialize, Deserialize, Clone)]
pub struct SyncedLine {
    pub timestamp: std::time::Duration,
    pub text: String,
}
impl SyncedLine {
    pub fn to_str(&self) -> String {
        format!("[{}]{}", duration_to_str(&self.timestamp), self.text)
    }
    pub fn parse(line: &str) -> Result<Self, ParseError> {
        if !line.starts_with('[') {
            return Err(ParseError::MissingColon);
        }
        let close = line.find(']').ok_or(ParseError::MissingColon)?;
        let timestamp = parse_duration(&line[1..close])?;
        Ok(SyncedLine {
            timestamp,
            text: line[close + 1..].to_owned(),
        })
    }
}
impl From<RichLyrics> for String {
    fn from(value: RichLyrics) -> Self {
        value
            .channels
            .into_iter()
            .flat_map(|x| x.lyrics)
            .map(|x| x.text)
            .join("\n")
    }
}
impl From<String> for RichLyrics {
    fn from(value: String) -> Self {
        Self {
            channels: vec![Channel {
                name: None,
                lyrics: value
                    .split('\n')
                    .map(|x| RichLine {
                        start: Duration::ZERO,
                        end: Duration::ZERO,
                        text: x.to_owned(),
                    })
                    .collect(),
            }],
        }
    }
}
impl From<RichLyrics> for SyncedLyrics {
    fn from(value: RichLyrics) -> Self {
        SyncedLyrics {
            lines: value
                .channels
                .into_iter()
                .flat_map(|x| x.lyrics)
                .map(|x| SyncedLine {
                    timestamp: x.start,
                    text: x.text,
                })
                .collect(),
        }
    }
}
impl From<SyncedLyrics> for RichLyrics {
    fn from(value: SyncedLyrics) -> Self {
        let mut lines: Vec<RichLine> = vec![];
        for line in value.lines {
            if let Some(last) = lines.last_mut() {
                last.end = line.timestamp;
            }
            lines.push(RichLine {
                start: line.timestamp,
                end: line.timestamp,
                text: line.text,
            });
        }
        Self {
            channels: vec![Channel {
                name: None,
                lyrics: lines,
            }],
        }
    }
}

fn duration_to_str(duration: &std::time::Duration) -> String {
    let total_secs = duration.as_secs();
    let hours = total_secs / 60 / 60;
    let minutes = total_secs / 60 % 60;
    let secs = (total_secs % 60) as f32 + duration.as_secs_f32().fract();
    let secs_str = if secs >= 10.0 {
        format!("{secs:.2}")
    } else {
        format!("0{secs:.2}")
    };
    if hours > 0 {
        format!("{hours}:{minutes:0>2}:{secs_str}")
    } else {
        format!("{minutes:0>2}:{secs_str}")
    }
}
fn parse_duration(string: &str) -> Result<std::time::Duration, ParseError> {
    if string.len() < 4 {
        return Err(ParseError::WrongLength);
    }
    let mut colon_index = 0;
    for (i, char) in string.chars().enumerate().skip(1).take(2) {
        if char == ':' {
            colon_index = i;
        }
    }
    if colon_index == 0 {
        return Err(ParseError::MissingColon);
    }
    let first_number = string[..colon_index]
        .parse::<u8>()
        .map_err(ParseError::Int)? as u64;
    let remaining = &string[colon_index + 1..];
    if remaining.len() < 2 {
        return Err(ParseError::WrongLength);
    }
    let second_number = remaining[..2].parse::<u8>().map_err(ParseError::Int)? as u64;
    let remaining = &remaining[2..];
    match remaining.chars().next() {
        None => Ok(std::time::Duration::from_secs(
            first_number * 60 + second_number,
        )),
        Some('.') => {
            let fraction = remaining.parse::<f64>().map_err(ParseError::Float)?;
            Ok(std::time::Duration::from_secs_f64(
                (first_number as f64 * 60.0) + (second_number as f64 + fraction),
            ))
        }
        Some(':') => {
            let remaining = &remaining[1..];
            if remaining.len() < 2 {
                return Err(ParseError::WrongLength);
            }
            let third_number = remaining[..2].parse::<u8>().map_err(ParseError::Int)? as u64;
            let remaining = &remaining[2..];
            match remaining.chars().next() {
                None => Ok(std::time::Duration::from_secs(
                    first_number * 60 * 60 + second_number * 60 + third_number,
                )),
                Some('.') => {
                    let fraction = remaining.parse::<f64>().map_err(ParseError::Float)?;
                    Ok(std::time::Duration::from_secs_f64(
                        (first_number as f64 * 60.0 * 60.0)
                            + (second_number as f64 * 60.0)
                            + (third_number as f64 + fraction),
                    ))
                }
                Some(_) => Err(ParseError::WrongLength),
            }
        }
        Some(_) => Err(ParseError::WrongLength),
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct SyncedLyrics {
    pub lines: Vec<SyncedLine>,
}
impl SyncedLyrics {
    pub fn parse(lines: Vec<String>) -> Result<Self, ParseError> {
        let lines = lines
            .into_iter()
            .map(|x| SyncedLine::parse(&x))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self { lines })
    }
    pub fn save(&self) -> Vec<String> {
        self.lines
            .iter()
            .map(|x| format!("[{}]{}", duration_to_str(&x.timestamp), x.text))
            .collect()
    }
}

#[derive(Error, Debug)]
#[error("parsing")]
pub enum ParseError {
    MissingColon,
    WrongLength,
    Int(#[from] ParseIntError),
    Float(#[from] ParseFloatError),
}

#[derive(Serialize, Deserialize, Clone)]
pub struct RichLyrics {
    channels: Vec<Channel>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Channel {
    name: Option<String>,
    lyrics: Vec<RichLine>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct RichLine {
    #[serde(deserialize_with = "deserialize_duration")]
    #[serde(serialize_with = "serialize_duration")]
    start: std::time::Duration,
    #[serde(deserialize_with = "deserialize_duration")]
    #[serde(serialize_with = "serialize_duration")]
    end: std::time::Duration,
    text: String,
}
fn serialize_duration<S>(duration: &std::time::Duration, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::ser::Serializer,
{
    serializer.serialize_str(&duration_to_str(duration))
}
fn deserialize_duration<'de, D>(deserializer: D) -> Result<std::time::Duration, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    struct Visitor;

    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = std::time::Duration;

        fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
            formatter.write_str("string")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            parse_duration(value).map_err(|_| serde::de::Error::custom("invalid"))
        }
    }

    deserializer.deserialize_str(Visitor)
}
