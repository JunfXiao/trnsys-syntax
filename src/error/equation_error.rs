use std::borrow::Cow;
use thiserror::Error;
use crate::ast::Expr;

#[derive(Debug, Error, Clone)]
pub enum EquationError {
    #[error("A value shouldn't be zero in the equation: {0}")]
    UnexpectedZero(Expr),
    
}

impl EquationError {
    pub fn text(&self) -> Cow<str> {
        match self {
            EquationError::UnexpectedZero(expr) => format!("{:#}", expr).into(),
        }
    }
}