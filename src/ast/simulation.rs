#![allow(deprecated)]
use crate::ast::{Expr, UnitConnection};
use derive_more::{Constructor, Display, From, Into};
use derive_more::with_trait::{FromStr};
use strum::{FromRepr};
use strum_macros::EnumIter;

/// VERSION xx.x
#[derive(Debug, Clone, Constructor, From, Into, FromStr)]
pub struct Version {
    pub version: String,
}

/// SIMULATION t0 tf Δt
#[derive(Debug, Clone, Constructor)]
pub struct Simulation {
    pub start_time: Expr,
    pub stop_time: Expr,
    pub time_step: Expr,
}

/// TOLERANCES εD εA or TOLERANCES -ζD -ζA
#[derive(Debug, Clone, Constructor)]
pub struct Tolerances {
    pub integration_tolerance: f64,
    pub convergence_tolerance: f64,
}

/// LIMITS m n p
#[derive(Debug, Clone, Constructor)]
pub struct Limits {
    pub max_iterations: Expr,
    pub max_warnings: Expr,
    pub trace_limit: Option<Expr>,
}

/// NAN_CHECK n
#[derive(Debug, Clone, Constructor, From, Into)]
pub struct NanCheck {
    pub enabled: bool,
}

/// OVERWRITE_CHECK n
#[derive(Debug, Clone, Constructor, From, Into)]
pub struct OverwriteCheck {
    pub enabled: bool,
}

/// TIME_REPORT n
#[derive(Debug, Clone, Constructor, From, Into)]
pub struct TimeReport {
    pub enabled: bool,
}

/// ACCELERATE n u1,o1 u2,o2 ... un,on
#[derive(Debug, Clone, Constructor, From, Into)]
pub struct Accelerate {
    pub outputs: Vec<UnitConnection>, // (unit, output)
}

/// LOOP n REPEAT y u1 u2 ... un
#[derive(Debug, Clone, Constructor)]
pub struct Loop {
    pub units: Vec<usize>,
    pub repeat: usize,
}

/// DFQ k
#[derive(Debug, Clone, Constructor, From, Into)]
pub struct Dfq {
    pub method: DfqMethod, // 1, 2, or 3
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, FromRepr, EnumIter, Display, Default)]
#[display("{} = {:?}", *self as u8, self)]
pub enum DfqMethod {
    /// 1: Modified Euler method
    #[default]
    ModifiedEuler = 1,
    /// 2: Non-self-starting Heun method
    NonSelfStartingHeun = 2,
    /// 3: Fourth-order Adams method
    ForthOrderAdam = 3,
}

/// NOCHECK n u1,i1 u2,i2 ... un,in
#[derive(Debug, Clone, Constructor, From, Into)]
pub struct NoCheck {
    pub inputs: Vec<UnitConnection>, // (unit, input)
}

/// EQSOLVER n
#[derive(Debug, Clone, Constructor, From, Into)]
pub struct EqSolver {
    pub method: EqSolverMethod,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, FromRepr, EnumIter, Display, Default)]
#[display("{} = {:?}", *self as u8, self)]
pub enum EqSolverMethod {
    /// (Default if no value is provided) If a component output or TIME changes, update the block of
    /// equations that depend upon those values. Then update components that depend upon the first 
    /// block of equations. Continue looping until all equations have been updated appropriately. 
    /// This equation blocking method is most like the method used in TRNSYS version 15 and before.
    #[default]
    AnyChange = 0,
    /// If a component output or TIME changes by more than the value set in the TOLERANCES Statement,
    /// update the block of equations that depend upon those values. Then update components that 
    /// depend upon the first block of equations. Continue looping until all equations have been 
    /// updated appropriately.
    BeyondTolerance = 1,
    /// Treat equations as a component and update them only after updating all components.
    AsComponent = 2
}

/// SOLVER k [RFmin RFmax]
#[derive(Debug, Clone, Constructor)]
pub struct Solver {
    pub method: SolverMethod,       // 0 or 1
    /// The minimum of the relaxation factor. Only valid for [SolverMethod::Successive].
    /// 
    /// $0<=RF<=1$
    pub rf_min: Option<f64>, // Only for method 0
    /// The maximum of the relaxation factor. Only valid for [SolverMethod::Successive].
    /// 
    /// $0<=RF<=1$
    pub rf_max: Option<f64>, // Only for method 0
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, FromRepr, EnumIter, Display, Default)]
#[display("{} = {:?}", *self as u8, self)]

pub enum SolverMethod {
    #[default]
    Successive = 0,
    #[deprecated(since = "0", note = "The Powell’s method solver has been deactivated in TRNSYS 18")]
    Powell = 1,
}

/// ASSIGN filename lu
#[derive(Debug, Clone, Constructor)]
pub struct Assign {
    pub filename: String,
    pub logical_unit: usize,
}

/// DESIGNATE filename lu
#[derive(Debug, Clone, Constructor)]
pub struct Designate {
    pub filename: String,
    pub logical_unit: usize,
}

/// INCLUDE "filename"
#[derive(Debug, Clone, Constructor, From, Into)]
pub struct Include {
    pub filename: String,
}

/// Information about a TRNSED input field
#[derive(Debug, Clone, Constructor)]
pub struct TrnsedInputField {
    pub description: String,
    pub units1: String,
    pub units2: String,
    pub add: f64,
    pub mult: f64,
    pub min: f64,
    pub max: f64,
    pub help: usize,
}
