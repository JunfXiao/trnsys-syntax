mod simulation;
mod unit;
mod listing;
mod expression;
mod trnsed;
mod equation;
mod comment;
mod context;


use std::fmt::{Display, Formatter};
pub use self::simulation::*;
pub use self::unit::*;
pub use self::listing::*;
pub use self::expression::*;
pub use self::trnsed::*;
pub use self::equation::*;
pub use self::comment::*;
pub use self::context::*;
use error_stack::Context;
use serde::{Deserialize, Serialize};
use crate::error::Error;


/// Represents a source code location
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    pub start: usize,
    pub end: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Line: {}, Column: {}, Start: {}, End: {}", self.line, self.column, self.start, self.end)
    }
}
impl Context for Span{}

/// Trait for components to validate their content
/// TODO: Implement validations for all blocks, equations and expressions
pub trait Validate {
    fn validate(&self) -> Result<(), Error>{
        self.validate_under_context(&DocContext::default())
    }

    fn validate_under_context(&self, _context: &DocContext) -> Result<(), Error> {
        Ok(())
    }
}


