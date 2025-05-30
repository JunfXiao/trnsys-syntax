use std::fmt::{Display, Formatter};
use derive_more::Constructor;
use serde::{Deserialize, Serialize};
use crate::ast::{CSummarize, Commented, ESummarize, Expr, Metadata};

#[derive(Debug, Clone, Constructor, Default)]
pub struct Equations {
    pub metadata: Metadata,
    pub definitions: Vec<Commented<EquationDef>>
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EquationDef {
    pub name: String,
    pub expr: Expr,
    pub csummarize: Option<Commented<CSummarize>>,
    pub esummarize: Option<Commented<ESummarize>>
}

impl Display for EquationDef {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
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
pub struct Constants {
    pub metadata: Metadata,
    pub definitions: Vec<Commented<EquationDef>>,
}