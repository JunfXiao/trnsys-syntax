mod simulation;
mod unit;
mod listing;
mod expression;
mod trnsed;
mod equation;
mod comment;


use std::fmt::{Display, Formatter};
pub use self::simulation::*;
pub use self::unit::*;
pub use self::listing::*;
pub use self::expression::*;
pub use self::trnsed::*;
pub use self::equation::*;
pub use self::comment::*;
use error_stack::Context;
use serde::{Deserialize, Serialize};

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



