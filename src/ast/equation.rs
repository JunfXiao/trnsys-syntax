use crate::ast::{Commented, Expr, Metadata};
use derive_more::Constructor;
use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Constructor, Default, Serialize, Deserialize)]
pub struct Equations {
    pub metadata: Metadata,
    pub definitions: Vec<Commented<EquationDef>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EquationDef {
    pub name: String,
    pub expr: Expr,
}

impl Display for EquationDef {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "Eq: {} = {}", self.name, self.expr)?;
        Ok(())
    }
}

/// CONSTANTS n NAME1 = value1 ... NAMEn = valuen
#[derive(Debug, Clone, Constructor, Default, Serialize, Deserialize)]
pub struct Constants {
    pub metadata: Metadata,
    pub definitions: Vec<Commented<EquationDef>>,
}
