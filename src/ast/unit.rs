// src/ast/component.rs
use super::{Expr, Assign, Designate, Commented};
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use derive_more::{Display, From};
use derive_more::with_trait::Constructor;
use serde::{Deserialize, Serialize};
use either::Either;

/// UNIT n TYPE m SOME COMMENTS
#[derive(Debug, Display, Clone, Default)]
#[display("Unit {number} of type {type_number}")]
pub struct Unit {
    pub number: u32,
    pub type_number: u32,
    pub unit_name: String,
    pub parameters: Option<Vec<Commented<Expr>>>,
    pub inputs: Option<Either<
        Vec<UnitInput<WithInitVal>>,
        Vec<UnitInput<WithLabel>>
    >>,
    pub derivatives: Option<Vec<Commented<Expr>>>,
    pub trace: Option<Commented<Trace>>,
    pub etrace: Option<Commented<Trace>>,
    pub format: Option<Commented<Format>>,
    pub metadata: Option<Commented<Metadata>>,
    pub assigns: Option<Vec<Commented<Assign>>>,
    pub designates: Option<Vec<Commented<Designate>>>,
}


pub trait InputMode:Clone+Default {
    type Field: Clone + Debug;
}

#[derive(Debug, Clone, Default, Display)]
pub struct WithInitVal;
#[derive(Debug, Clone, Default, Display)]
pub struct WithLabel;

impl InputMode for WithInitVal {
    type Field = Commented<Expr>;
}

impl InputMode for WithLabel {
    type Field = Commented<String>;
}

/// INPUTS n u1,o1 u2,o2 ... un,on v1 v2 ... vn [labels for printers/plotters]
#[derive(Debug, Clone, Constructor)]
pub struct UnitInput<IM: InputMode> {
    pub connection: Commented<Expr>,
    pub extra_data: IM::Field,
}


/// A single unit connection (unit, input/output).
///
#[derive(Debug, Clone, Hash, Serialize, Deserialize, Constructor, PartialEq, Eq)]
pub struct UnitConnection {
    pub unit: usize,
    // the index of the input/output
    pub index: usize,
}

impl Default for UnitConnection {
    fn default() -> Self {
        Self::new(0,0)
    }
}

impl Display for UnitConnection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{},{}]", self.unit, self.index)
    }
}



/// Component summary
///
/// Syntax: 
/// ```txt
/// SUMMARIZE UnitNo "descriptive text"
/// ```
#[derive(Debug, Clone)]
pub struct Summarize {
    pub description: String,
}

/// Constant summary
/// 
/// Syntax: 
/// ```txt
/// CSUMMARIZE EqnName "description"
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, Constructor, Display)]
#[display("Constant Summary: Name: {const_name}, Description: {description}")]
pub struct CSummarize {
    pub const_name: String,
    pub description: String,
}

/// Equation summary
/// 
/// Syntax: 
/// ```txt
/// ESUMMARIZE EqnName "description"
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, Constructor, Display)]
#[display("Equation Summary: Name: {eq_name}, Description: {description}")]
pub struct ESummarize {
    pub eq_name: String,
    pub description: String,

}

/// TRACE ton toff
#[derive(Debug, Clone, Constructor)]
pub struct Trace {
    pub start_time: f64,
    pub stop_time: f64,

}



/// FORMAT (format string)
#[derive(Debug, Clone, Constructor, From)]
pub struct Format {
    pub format_string: String,

}


/// Metadata from TRNSYS Studio stored in comments
#[derive(Debug, Clone, Constructor, Default)]
pub struct Metadata {
    pub unit_name: Option<String>,
    pub model: Option<String>,
    pub position: Option<(usize, usize)>,
    pub layer: Option<String>,
    pub other: HashMap<String, String>,
}


#[derive(Debug, Clone)]
pub struct End{
    
}