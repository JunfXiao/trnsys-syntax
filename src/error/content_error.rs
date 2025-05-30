use std::borrow::Cow;
use thiserror::Error;
use crate::error::EquationError;

#[derive(Debug, Error, Clone)]
/// Error type for mismatches or issues in the content of a block.
pub enum ContentError {

    #[error("An undefined variable '{name}' is used.")]
    UndefinedVariable{
        name: String,
    },


    #[error("'{name}' can only be defined once. It is already defined before.")]
    DuplicateDefinition{
        name: String,
    },
    
    #[error("Undefined unit with number '{unit_no}' is used.")]
    UndefinedUnit{
        unit_no: usize,
    },

    #[error("Duplicated unit number '{unit_no}'")]
    DuplicateUnit{
        unit_no: String,
    },
    
    #[error("Duplicated logical unit number '{logical_unit_no}'")]
    DuplicateLogicalUnit{
        logical_unit_no: String,
    },

    #[error("Expected {expected} parameter(s) in \"{part}\" but got {actual} instead")]
    ArgumentCount{
        expected: String,
        actual: usize,
        part: String,
    },
    
    #[error("Invalid condition in \"{part}\": expected that \"{expected}\" but \"{actual}\".")]
    InvalidCondition{
        expected: String,
        actual: String,
        part: String,
    },
    
    #[error("Invalid \"{part}\" value \"{value}\": {reason}")]
    InvalidValue{
        part: String,
        value: String,
        reason: String,
    },

    #[error(transparent)]
    Equation(#[from] EquationError),
}

impl ContentError {
    pub fn text(&self) -> Cow<str> {
        match self {
            ContentError::UndefinedVariable { name } => name.into(),
            ContentError::DuplicateDefinition { name } => name.into(),
            ContentError::UndefinedUnit { unit_no } => unit_no.to_string().into(),
            ContentError::DuplicateUnit { unit_no } => unit_no.into(),
            ContentError::DuplicateLogicalUnit { logical_unit_no } => logical_unit_no.into(),
            ContentError::Equation(e) => e.text(),
            ContentError::ArgumentCount { part, .. } => part.into(),
            ContentError::InvalidValue { part, .. } => { part.into() },
            ContentError::InvalidCondition { part, .. } => { part.into() },
            
        }
    }
}


