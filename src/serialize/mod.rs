mod block;
mod comment;
mod document;
mod expr;

use crate::error::{Error, RError};
use crate::parse::BlockKind;
use bincode;
use bincode::{Decode, Encode};
use error_stack::Report;
pub use expr::*;
use serde::{Deserialize, Serialize};
use std::any::type_name;
use std::fmt::{Debug, Write};
use std::{fmt, io};

struct IoAdapter<'a, W>(&'a mut W);

impl<'a, W: io::Write> fmt::Write for IoAdapter<'a, W> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        // Only write utf-8 bytes to the writer
        self.0.write_all(s.as_bytes()).map_err(|_| fmt::Error)
    }
}

/// Represents an object that can be serialized into a deck file format.
pub trait DeckWrite: Sized {
    fn write_to<W: Write>(&self, writer: &mut W, kind: BlockKind) -> Result<(), RError>;

    #[inline]
    fn write_to_bytes<W: io::Write>(&self, w: &mut W, kind: BlockKind) -> Result<(), RError> {
        let mut adapter = IoAdapter(w);
        self.write_to(&mut adapter, kind)
    }

    fn write_to_string(&self) -> Result<String, RError> {
        let mut s = String::new();
        self.write_to(&mut s, BlockKind::Unknown)?;
        Ok(s)
    }
}

pub trait DeckJsonCodec<'a>: Serialize + Deserialize<'a> {
    fn as_json(&self) -> Result<String, RError> {
        serde_json::to_string(self).map_err(|e| {
            Report::from(e)
                .change_context(Error::ConversionError {
                    input: type_name::<Self>().to_string(),
                    target: "JSON".to_string(),
                })
                .into()
        })
    }

    fn from_json(s: &'a str) -> Result<Self, RError> {
        serde_json::from_str(s).map_err(|e| {
            Error::ConversionError {
                input: "Json Content".to_string(),
                target: type_name::<Self>().to_string(),
            }
            .into()
        })
    }

    fn as_yaml(&self) -> Result<String, RError> {
        serde_yaml::to_string(self).map_err(|e| {
            Report::from(e)
                .change_context(Error::ConversionError {
                    input: type_name::<Self>().to_string(),
                    target: "YAML".to_string(),
                })
                .into()
        })
    }

    fn from_yaml(s: &'a str) -> Result<Self, RError>
    where
        Self: Sized,
    {
        serde_yaml::from_str(s).map_err(|e| {
            Error::ConversionError {
                input: "YAML Content".to_string(),
                target: type_name::<Self>().to_string(),
            }
            .into()
        })
    }
}

pub trait DeckBinaryCodec<'a>: Serialize + Deserialize<'a> {
    fn as_binary(&self) -> Result<Vec<u8>, RError>
    where
        Self: Encode,
    {
        bincode::encode_to_vec(self, bincode::config::standard()).map_err(|_| {
            Error::ConversionError {
                input: type_name::<Self>().to_string(),
                target: "Binary".to_string(),
            }
            .into()
        })
    }

    fn from_binary(s: &'a [u8]) -> Result<Self, RError>
    where
        Self: Sized + Decode<()>,
    {
        let (result, _) = bincode::decode_from_slice(s, bincode::config::standard())
            .map_err(|e| RError::from(Error::from(e)))?;

        Ok(result)
    }
}

pub fn write_sep<W: Write>(writer: &mut W, content: Option<&str>) -> Result<(), RError> {
    Ok(writeln!(
        writer,
        "\n* {}\n",
        content.unwrap_or("-").repeat(50)
    )?)
}
