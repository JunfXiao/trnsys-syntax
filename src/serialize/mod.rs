mod block;
mod comment;
mod document;
mod expr;

use crate::error::{Error, RError};
use bincode;
use bincode::{Decode, Encode};
use serde::{Deserialize, Serialize};
use std::any::type_name;
use std::fmt::{Debug, Write};
use std::{fmt, io};

use crate::parse::BlockKind;
pub use block::*;
pub use comment::*;
pub use document::*;
pub use expr::*;

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
    fn as_json(&self) -> Result<String, RError>
    where
        Self: Debug,
    {
        serde_json::to_string(self).map_err(|e| {
            Error::ConversionError {
                input: format!("{:?}", self),
                target: "JSON".to_string(),
            }
            .into()
        })
    }

    fn from_json(s: &'a str) -> Result<Self, RError>
    where
        Self: Sized,
    {
        serde_json::from_str(s).map_err(|e| {
            Error::ConversionError {
                input: format!("JSON Content `{}`", s),
                target: type_name::<Self>().to_string(),
            }
            .into()
        })
    }
}

pub trait DeckBinaryCodec<'a>: Serialize + Deserialize<'a> {
    fn as_binary(&self) -> Result<Vec<u8>, RError>
    where
        Self: Debug,
        Self: Encode,
    {
        bincode::encode_to_vec(self, bincode::config::standard()).map_err(|e| {
            Error::ConversionError {
                input: format!("{:?}", self),
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
