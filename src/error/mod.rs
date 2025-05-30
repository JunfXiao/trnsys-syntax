
mod equation_error;
mod content_error;
mod report;

use strum_macros::Display;
pub use equation_error::*;
pub use content_error::*;
pub use report::*;
pub use std::error::Error as StdError;
pub use std::io::Error as IoError;
pub use nom::Err as NomErr;
pub use error_stack::Result as ErrorStackResult;

// src/error.rs
use thiserror::Error;
use std::num::{ParseFloatError, ParseIntError};
use nom::error::ErrorKind;
use std::fmt::Debug;
use std::convert::Infallible;
use std::borrow::Cow;
use std::str::ParseBoolError;

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
pub enum ErrorScope{
    Document,
    Block,
    Row,
    Param,
    Part,
    Expression,
}

pub type ParseResult<T, E=Error> = ErrorStackResult<T, E>;



#[derive(Debug, Error)]
pub enum Error {
    #[error("Unknown keyword is used: \"{keyword}\"")]
    UnknownKeyword{
        keyword: String,
        scope: ErrorScope,
    },
    
    #[error("Unexpected {scope}: {message}")]
    UnexpectedContent{
        message: String,
        scope: ErrorScope,
    },
    
    #[error("Cannot Convert \"{input:.20}\" to type \"{target}\"")]
    ConversionError{
        input: String,
        target: String,
    },
    
    #[error(transparent)]
    ContentError(#[from] ContentError),
    
    #[error("Invalid {scope}: '{kind:?}' Failed\n \"{input:.20}\"...")]
    GeneralError{
        input: String,
        kind: ErrorKind,
        scope: ErrorScope,
    },

    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),
    
}

impl Error {
    pub fn text(&self) -> Cow<str> {
        match self {
            Error::UnknownKeyword { keyword,.. } => keyword.into(),
            Error::ConversionError { input, .. } => input.into(),
            Error::GeneralError { input,  .. } => input.into(),
            Error::ContentError(c) => c.text(),
            _ => "-".into(),
        }
    }
}

pub type RError = ReportWrapper<Error>;


impl<'a, T> NewFromErrorKind<T> for Error
where T: ToString
{
    fn new_from_error_kind(input: T, kind: ErrorKind, scope: ErrorScope) -> Self {
        
        Error::GeneralError {
            input: input.to_string(),
            kind,
            scope
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
            message: format!("Failed to parse integer: {:?}", e.kind()),
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




