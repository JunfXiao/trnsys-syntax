use crate::ast::Expr;
use crate::error::RError;
use crate::parse::BlockKind;
use crate::serialize::DeckWrite;
use std::fmt::Write;

pub const BLOCK_KIND_WITH_BRACKETS: [BlockKind; 2] = [BlockKind::Equations, BlockKind::Constants];

impl DeckWrite for Expr {
    fn write_to<W: Write>(&self, writer: &mut W, kind: BlockKind) -> Result<(), RError> {
        match self {
            Expr::Literal(l) => write!(writer, "{}", l)?,
            Expr::Identifier(i) => write!(writer, "{}", i)?,
            Expr::UnitOutput(c) => {
                if BLOCK_KIND_WITH_BRACKETS.contains(&kind) {
                    write!(writer, "[")?;
                    c.write_to(writer, kind)?;
                    write!(writer, "]")?;
                } else {
                    c.write_to(writer, kind)?;
                }
            }
            Expr::BinaryOp { op, first, second } => {
                if op.is_function_like() {
                    write!(writer, "{}(", op)?;
                    first.write_to(writer, kind)?;
                    write!(writer, ", ")?;
                    second.write_to(writer, kind)?;
                    write!(writer, ")")?;
                } else {
                    first.write_to(writer, kind)?;
                    write!(writer, " {}", op)?;
                    second.write_to(writer, kind)?;
                }
            }
            Expr::UnaryOp { op, expr } => {
                write!(writer, "{}", op)?;
                if expr.can_be_input() {
                    expr.write_to(writer, kind)?;
                } else {
                    write!(writer, "(")?;
                    expr.write_to(writer, kind)?;
                    write!(writer, ")")?;
                }
            }
            Expr::TrinaryOp {
                op,
                first,
                second,
                third,
            } => {
                write!(writer, "{}(", op)?;
                first.write_to(writer, kind)?;
                write!(writer, ", ")?;
                second.write_to(writer, kind)?;
                write!(writer, ", ")?;
                third.write_to(writer, kind)?;
                write!(writer, ")")?;
            }
            Expr::Unconnected => {
                write!(writer, "0,0")?;
            }
        }
        Ok(())
    }
}
