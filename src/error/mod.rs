mod content_error;
mod equation_error;
mod report;

pub use content_error::*;
pub use equation_error::*;
pub use error_stack::Result as ErrorStackResult;
pub use nom::Err as NomErr;
pub use report::*;
pub use std::error::Error as StdError;
pub use std::io::Error as IoError;
use strum_macros::Display;

// src/error.rs
use nom::error::ErrorKind;
use std::borrow::Cow;
use std::convert::Infallible;
use std::fmt::Debug;
use std::num::{ParseFloatError, ParseIntError};
use std::str::ParseBoolError;
use thiserror::Error;

/// Error that occurred during parsing
// #[derive(Debug, Error)]
// pub enum Error {
//     #[error(transparent)]
//     Syntax(#[from] SyntaxError),
//
//     #[error("IO Error: {0}")]
//     Io(#[from] std::io::Error),
// }

#[derive(Debug, Clone, Copy, Display)]
pub enum ErrorScope {
    Document,
    Block,
    Row,
    Param,
    Part,
    Expression,
}

pub type ParseResult<T, E = Error> = ErrorStackResult<T, E>;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Unknown keyword is used: \"{keyword}\"")]
    UnknownKeyword { keyword: String, scope: ErrorScope },

    #[error("Unexpected {scope}: {message}")]
    UnexpectedContent { message: String, scope: ErrorScope },

    #[error("Cannot Convert \"{}\" to type \"{target}\"",self.text_short())]
    ConversionError { input: String, target: String },

    #[error(transparent)]
    ContentError(#[from] ContentError),

    #[error("Invalid {scope}: '{kind:?}' Failed in \"{}\"", self.text_short())]
    GeneralError {
        input: String,
        kind: ErrorKind,
        scope: ErrorScope,
    },

    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),
}

impl Error {
    pub fn input(&self) -> Cow<str> {
        match self {
            Error::UnknownKeyword { keyword, .. } => keyword.into(),
            Error::ConversionError { input, .. } => input.into(),
            Error::GeneralError { input, .. } => input.into(),
            Error::ContentError(c) => c.text(),
            _ => "-".into(),
        }
    }

    pub fn text_short(&self) -> Cow<str> {
        let text = self.input().trim().to_string();
        text.split_once('\n')
            .map(|(before, _)| before.trim().to_string())
            .unwrap_or(text)
            .to_string()
            .into()
    }
}

pub type RError = ReportWrapper<Error>;

impl<'a, T> NewFromErrorKind<T> for Error
where
    T: ToString,
{
    fn new_from_error_kind(input: T, kind: ErrorKind, scope: ErrorScope) -> Self {
        Error::GeneralError {
            input: input.to_string(),
            kind,
            scope,
        }
    }
}

impl From<strum::ParseError> for Error {
    fn from(e: strum::ParseError) -> Self {
        Error::UnknownKeyword {
            keyword: e.to_string(),
            scope: ErrorScope::Block,
        }
    }
}

impl From<nom::error::Error<&str>> for Error {
    fn from(e: nom::error::Error<&str>) -> Self {
        Error::GeneralError {
            input: e.input.to_string(),
            kind: e.code,
            scope: ErrorScope::Part,
        }
    }
}

impl From<ParseIntError> for Error {
    fn from(e: ParseIntError) -> Self {
        Error::UnexpectedContent {
            message: format!("Failed to parse integer: {:?}", e.to_string()),
            scope: ErrorScope::Param,
        }
    }
}

impl From<ParseFloatError> for Error {
    fn from(e: ParseFloatError) -> Self {
        Error::UnexpectedContent {
            message: format!("Failed to parse float: {:}", e.to_string()),
            scope: ErrorScope::Param,
        }
    }
}

impl From<ParseBoolError> for Error {
    fn from(value: ParseBoolError) -> Self {
        Error::UnexpectedContent {
            message: format!("Failed to parse boolean: {:?}", value),
            scope: ErrorScope::Param,
        }
    }
}

impl From<Infallible> for Error {
    fn from(_: Infallible) -> Self {
        Error::UnexpectedContent {
            message: "Infallible error".to_string(),
            scope: ErrorScope::Document,
        }
    }
}
