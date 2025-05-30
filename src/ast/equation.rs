use std::fmt::{Display, Formatter};
use derive_more::Constructor;
use serde::{Deserialize, Serialize};
use crate::ast::{CSummarize, Commented, ESummarize, Expr, Metadata};

#[derive(Debug, Clone, Constructor, Default)]
pub struct Equations<'a> {
    pub metadata: Metadata,
    pub definitions: Vec<Commented<'a, EquationDef<'a>>>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EquationDef<'a> {
    pub name: String,
    pub expr: Expr,
    pub csummarize: Option<Commented<'a, CSummarize>>,
    pub esummarize: Option<Commented<'a, ESummarize>>
}

impl Display for EquationDef<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Eq: {} = {}", self.name, self.expr)?;
        if let Some(ref csummarize) = self.csummarize {
            write!(f, "Constant Summary: {}", csummarize)?;
        }
        if let Some(ref esummarize) = self.esummarize {
            write!(f, "Equation Summary: {}", esummarize)?;
        }
        Ok(())
    }
}


/// CONSTANTS n NAME1 = value1 ... NAMEn = valuen
#[derive(Debug, Clone, Constructor, Default)]
pub struct Constants<'a> {
    pub metadata: Metadata,
    pub definitions: Vec<Commented<'a, EquationDef<'a>>>,
}