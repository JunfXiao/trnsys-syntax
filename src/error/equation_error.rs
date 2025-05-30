use std::borrow::Cow;
use thiserror::Error;
use crate::ast::Expr;

#[derive(Debug, Error, Clone)]
pub enum EquationError {
    #[error("A value is divided by zero in the equation: {0}")]
    DividedByZero(Expr),
}

impl EquationError {
    pub fn text(&self) -> Cow<str> {
        match self {
            EquationError::DividedByZero(expr) => format!("{:#}", expr).into(),
        }
    }
}