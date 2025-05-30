use super::UnitConnection;
use crate::error::{ContentError, EquationError, ParseResult};
use error_stack::{Report};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::iter;
use strum_macros::{Display, EnumString};

/// Represents a mathematical or logical expression
#[derive(Debug, Clone, Serialize, Deserialize)]
#[derive(PartialEq)]
pub enum Expr {
    Literal(f64),
    /// Represents the identifiers of equations, variables
    Identifier(String),
    /// Represents the output of a unit
    UnitOutput(UnitConnection),
    UnaryOp {
        op: UnaryOperator,
        expr: Box<Expr>,
    },
    BinaryOp {
        op: BinaryOperator,
        first: Box<Expr>,
        second: Box<Expr>,
    },
    TrinaryOp {
        op: TrinaryOperator,
        first: Box<Expr>,
        second: Box<Expr>,
        third: Box<Expr>,
    },
    /// Represents the unconnected input. Usually it's `0,0` 
    Unconnected,
}

impl Expr {
    pub fn can_be_input(&self) -> bool {
        match self {
            Expr::Literal(_) => true,
            Expr::Identifier(_) => true,
            Expr::UnitOutput(_) => true,
            Expr::Unconnected => true,
            _ => false,
        }
    }

    pub fn can_be_initial_value(&self) -> bool {
        match self {
            Expr::Literal(_) => true,
            _ => false,
        }
    }
    /// Check an expression with the given variable bindings
    /// TODO: Migrate to `DocContext` to check.
    pub fn check(&self, variables: &HashMap<String,Expr>) -> ParseResult<(), ContentError> {
        match self {
            Expr::Identifier(name) => variables
                .get(name)
                .ok_or(
                    Report::new(
                            ContentError::UndefinedVariable {
                                name: name.clone(),
                            }
                        .into(),
                    )
                    .attach_printable(self.clone()),
                )
                .and_then(|_| Ok(())),
            Expr::BinaryOp {
                op, first, second, ..
            } => {
                first.check(variables)?;
                second.check(variables)?;
                if op == &BinaryOperator::Divide {
                    if let Ok(Some(right_value)) = second.evaluate() {
                        if right_value == 0.0 {
                            return Err(Report::new(EquationError::DividedByZero(self.clone()).into()));
                        }
                    }
                }

                Ok(())
            }
            Expr::UnaryOp {  expr, .. } => {
                expr.check(variables)
            },

            Expr::TrinaryOp {
                op: _op,
                first,
                second,
                third,
            } => {
                first.check(variables)?;
                second.check(variables)?;
                third.check(variables)?;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// TODO: Evaluate the expression with the given `DocContext`.
    pub fn evaluate(&self) -> ParseResult<Option<f64>, ContentError> {
        match self {
            Expr::Literal(value) => Ok(Some(*value)),

            Expr::BinaryOp { op, first, second } => {
                let left_value = first.evaluate()?;
                let right_value = second.evaluate()?;
                if let Some(left_value) = left_value {
                    if let Some(right_value) = right_value {
                        return Some(op.evaluate((left_value, right_value))).transpose().map_err(|e|e.into());
                    }
                }
                Ok(None)
            }
            Expr::UnaryOp { op, expr } => {
                let value = expr.evaluate()?;
                if let Some(value) = value {
                    Some(op.evaluate(value)).transpose().map_err(|e|e.into())
                } else {
                    Ok(None)
                }
            }

            _ => Ok(None),
        }
    }
    
    pub fn iter_tree<'a>(&'a self) -> Box<dyn Iterator<Item=&'a Expr> + 'a> {
        match self {
            Expr::BinaryOp { first, second, .. } => {
                Box::new(
                    iter::once(self)
                        .chain(first.iter_tree())
                        .chain(second.iter_tree())
                )
            }
            Expr::UnaryOp { expr, .. } => {
                Box::new(
                    iter::once(self)
                        .chain(expr.iter_tree())
                )
            }
            Expr::TrinaryOp { first, second, third, .. } => {
                Box::new(
                    iter::once(self)
                        .chain(first.iter_tree())
                        .chain(second.iter_tree())
                        .chain(third.iter_tree())
                )
            }
            _ => {
                Box::new(iter::once(self))
            }
        }
    }
    
    /// Get all dependency identifiers of the expression
    pub fn dependencies(&self) -> (Vec<&str>, Vec<&UnitConnection>) {
        self.iter_tree()
            .fold((vec![], vec![]), 
                  |(mut ids, mut outputs), expr| {
                match expr {
                    Expr::Identifier(id) => ids.push(id),
                    Expr::UnitOutput(output) => outputs.push(output),
                    _ => {}
                }
                (ids, outputs)
            })
    }
    
    
    
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(value) => write!(f, "{}", value),
            Expr::Identifier(name) => write!(f, "{}", name),
            Expr::UnitOutput(output) => write!(f, "{}", output),
            Expr::UnaryOp { op, expr } => write!(f, "{}({})", op, expr),
            Expr::BinaryOp { op, first, second } => write!(f, "{} {} {}", first, op, second),
            Expr::TrinaryOp {
                op,
                first,
                second,
                third,
            } => write!(f, "{} {} {} {}", first, op, second, third),
            Expr::Unconnected => write!(f, "[0,0]"),
        }
    }
}

pub trait Operator<T> {
    fn evaluate(&self, value: T) -> ParseResult<f64, ContentError>;
}

/// Binary operators in expressions
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, EnumString, Display, Copy)]
pub enum BinaryOperator {
    #[strum(serialize = "+")]
    Add,
    #[strum(serialize = "-")]
    Subtract,
    #[strum(serialize = "*")]
    Multiply,
    #[strum(serialize = "/")]
    Divide,
    #[strum(serialize = "^")]
    Power,
    // Logic operators
    #[strum(serialize = "AND")]
    And,
    #[strum(serialize = "OR")]
    Or,
    #[strum(serialize = "EQ")]
    Equal,
    #[strum(serialize = "NE")]
    NotEqual,
    #[strum(serialize = "LT")]
    LessThan,
    #[strum(serialize = "LE")]
    LessThanOrEqual,
    #[strum(serialize = "GT")]
    GreaterThan,
    #[strum(serialize = "GE")]
    GreaterThanOrEqual,
}

impl Operator<(f64, f64)> for BinaryOperator {
    fn evaluate(&self, value: (f64, f64)) -> ParseResult<f64, ContentError> {
        let (left, right) = value;
        match self {
            BinaryOperator::Add => Ok(left + right),
            BinaryOperator::Subtract => Ok(left - right),
            BinaryOperator::Multiply => Ok(left * right),
            BinaryOperator::Divide => {
                if right == 0.0 {
                    Err(Report::new(EquationError::DividedByZero(
                        Expr::BinaryOp {
                            op: BinaryOperator::Divide,
                            first: Box::new(Expr::Literal(left)),
                            second: Box::new(Expr::Literal(right)),
                        }
                    ).into())
                        .attach_printable(format!("Formula: {} / {}", left, right)))
                } else {
                    Ok(left / right)
                }
            }
            BinaryOperator::Power => Ok(left.powf(right)),
            // Logic operators
            BinaryOperator::And => Ok(if left != 0.0 && right != 0.0 {
                1.0
            } else {
                0.0
            }),
            BinaryOperator::Or => Ok(if left != 0.0 || right != 0.0 {
                1.0
            } else {
                0.0
            }),
            BinaryOperator::Equal => Ok(if left == right { 1.0 } else { 0.0 }),
            BinaryOperator::NotEqual => Ok(if left != right { 1.0 } else { 0.0 }),
            BinaryOperator::LessThan => Ok(if left < right { 1.0 } else { 0.0 }),
            BinaryOperator::LessThanOrEqual => Ok(if left <= right { 1.0 } else { 0.0 }),
            BinaryOperator::GreaterThan => Ok(if left > right { 1.0 } else { 0.0 }),
            BinaryOperator::GreaterThanOrEqual => Ok(if left >= right { 1.0 } else { 0.0 }),
        }
    }
}

/// Unary operators in expressions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, EnumString, Display)]
pub enum UnaryOperator {
    #[strum(serialize = "-")]
    Negate,
    #[strum(serialize = "NOT")]
    Not,
}

impl Operator<f64> for UnaryOperator {
    fn evaluate(&self, value: f64) -> ParseResult<f64, ContentError> {
        match self {
            UnaryOperator::Negate => Ok(-value),
            UnaryOperator::Not => Ok(if value == 0.0 { 1.0 } else { 0.0 }),
        }
    }
}

/// Trinary operators in expressions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, EnumString, Display)]
pub enum TrinaryOperator {
    /// If the absolute error between the first two arguments is less than the third argument, return 1, otherwise 0.
    #[strum(serialize = "AE")]
    AE,
    /// If the first two arguments are equal, return 1, otherwise, return 0 and print an error with the given code in the 3rd argument.
    #[strum(serialize = "EQWARN")]
    EqWarn,
    /// If the first argument is greater than the second, return 1, otherwise, return 0 and print an error with the given code in the 3rd argument.
    #[strum(serialize = "GTWARN")]
    GtWarn,
    /// If the first argument is greater than or equal to the second, return 1, otherwise, return 0 and print an error with the given code in the 3rd argument.
    #[strum(serialize = "GEWARN")]
    GeWarn,
    /// If the first argument is not equal to the second, return 1, otherwise, return 0 and print an error with the given code in the 3rd argument.
    #[strum(serialize = "NEWARN")]
    NeWarn,
}

impl Operator<(f64, f64, f64)> for TrinaryOperator {
    fn evaluate(&self, value: (f64, f64, f64)) -> ParseResult<f64, ContentError> {
        let (first, second, third) = value;
        match self {
            TrinaryOperator::AE => Ok(if (first - second).abs() < third {
                1.0
            } else {
                0.0
            }),
            TrinaryOperator::EqWarn => Ok(if first == second { 1.0 } else { 0.0 }),
            TrinaryOperator::GtWarn => Ok(if first > second { 1.0 } else { 0.0 }),
            TrinaryOperator::GeWarn => Ok(if first >= second { 1.0 } else { 0.0 }),
            TrinaryOperator::NeWarn => Ok(if first != second { 1.0 } else { 0.0 }),
        }
    }
}
