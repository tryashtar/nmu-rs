use std::num::{ParseFloatError, ParseIntError};

use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Serialize, Deserialize)]
pub struct SyncedLine {
    pub timestamp: std::time::Duration,
    pub text: String,
}
impl SyncedLine {
    pub fn to_str(&self) -> String {
        format!("[{}]{}", Self::duration_to_str(&self.timestamp), self.text)
    }
    pub fn parse(line: &str) -> Result<Self, ParseError> {
        if !line.starts_with('[') {
            return Err(ParseError::MissingColon);
        }
        let close = line.find(']').ok_or(ParseError::MissingColon)?;
        let timestamp = Self::parse_duration(&line[1..close])?;
        Ok(SyncedLine {
            timestamp,
            text: line[close + 1..].to_owned(),
        })
    }
    fn duration_to_str(duration: &std::time::Duration) -> String {
        let total_secs = duration.as_secs();
        let hours = total_secs / 60 / 60;
        let minutes = total_secs / 60 % 60;
        let secs = total_secs % 60;
        let frac = duration.as_secs_f32().fract();
        if hours > 0 {
            if frac == 0.0 {
                let secs = secs as u8;
                format!("{hours}:{minutes:0>2}:{secs:0>2}")
            } else {
                format!("{hours}:{minutes:0>2}:{secs:0>2}{frac:.2}")
            }
        } else if frac == 0.0 {
            let secs = secs as u8;
            format!("{minutes:0>2}:{secs:0>2}")
        } else {
            format!("{minutes:0>2}:{secs:0>2}{frac:.2}")
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
}

#[derive(Serialize, Deserialize)]
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
}

#[derive(Error, Debug)]
#[error("parsing")]
pub enum ParseError {
    MissingColon,
    WrongLength,
    Int(#[from] ParseIntError),
    Float(#[from] ParseFloatError),
}

#[derive(Serialize, Deserialize)]
pub struct RichLyrics {
    channels: Vec<Channel>,
}

#[derive(Serialize, Deserialize)]
pub struct Channel {
    name: Option<String>,
    lyrics: Option<RichLine>,
}

#[derive(Serialize, Deserialize)]
pub struct RichLine {
    start: std::time::Duration,
    end: std::time::Duration,
    text: String,
}
