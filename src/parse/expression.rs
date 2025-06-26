use crate::ast::{
    BinaryOperator, Commented, EquationDef, Expr, TrinaryOperator, UnaryOperator, UnitConnection,
};
use crate::error::{Error, ErrorScope, RError};
use crate::parse::{map_report, parse_block_comment, parse_commented_row};
use nom::Parser;
use nom::bytes::complete::take_while;
use nom::bytes::tag_no_case;
use nom::character::complete::multispace0;
use nom::combinator::complete;
use nom::combinator::peek;
use nom::combinator::{all_consuming, recognize};
use nom::error::context;
use nom::multi::many_m_n;
use nom::sequence::pair;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, space0},
    combinator::{map, opt, value},
    multi::separated_list1,
    number::complete::double,
    sequence::{delimited, preceded, separated_pair, terminated},
};

type ExprResult<'a, T> = IResult<&'a str, T, RError>;

/// Parses a floating-point number literal from the input string.
///
/// Example: `1.23`, `-4.56`, `+7.89`, `-1.2e10`
pub fn parse_literal(input: &str) -> ExprResult<Expr> {
    map(double, Expr::Literal).parse(input)
}

/// Parses a unit output connection enclosed in square brackets from the input string.
///
/// Example: `[200,8]`
pub fn parse_unit_output_bracketed(input: &str) -> ExprResult<Expr> {
    let (rest, (unit, output)) = delimited(
        preceded(space0, char('[')),
        separated_pair(
            preceded(space0, nom::character::complete::u64),
            preceded(space0, char(',')),
            preceded(space0, nom::character::complete::u64),
        ),
        preceded(space0, char(']')),
    )
    .parse(input)?;

    Ok((
        rest,
        Expr::UnitOutput(UnitConnection {
            unit: unit as usize,
            index: output as usize,
        }),
    ))
}

/// Parses a unit output connection without brackets from the input string.
///
/// Example: `200,8`
pub fn parse_unit_output(input: &str) -> ExprResult<Expr> {
    let (rest, (unit, output)) = separated_pair(
        preceded(space0, nom::character::complete::u64),
        preceded(space0, char(',')),
        preceded(space0, nom::character::complete::u64),
    )
    .parse(input)?;

    Ok((
        rest,
        Expr::UnitOutput(UnitConnection {
            unit: unit as usize,
            index: output as usize,
        }),
    ))
}

/// Parses an unconnected unit output (`0,0`) from the input string.
pub fn parse_unconnected(input: &str) -> ExprResult<Expr> {
    map(tag("0,0"), |_| Expr::Unconnected).parse(input)
}

/// Parses an identifier from the input string.
/// Identifiers must start with an alphabetic character or an underscore (`_`),
/// and can be followed by any number of alphanumeric characters or underscores.
///
/// Example: `my_variable`, `_anotherIdentifier`, `Var1`
pub fn parse_identifier(input: &str) -> ExprResult<Expr> {
    fn first(c: char) -> bool {
        c.is_ascii_alphabetic()
    }
    fn rest(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    map(
        recognize(pair(take_while1(first), take_while(rest))),
        |s: &str| Expr::Identifier(s.to_owned()),
    )
    .parse(input)
}

/// Parses an expression enclosed in parentheses from the input string.
///
/// Example: `(1 + 2)`
pub fn parse_parens(input: &str) -> ExprResult<Expr> {
    delimited(
        preceded(space0, char('(')),
        parse_expr, // 递归
        preceded(space0, char(')')),
    )
    .parse(input)
}

/// Parses a primary expression from the input string.
/// A primary expression can be one of the following:
/// - A literal (e.g., `1.23`)
/// - A unit output in brackets (e.g., `[200,8]`)
/// - An unconnected unit output (e.g., `0,0`)
/// - An identifier (e.g., `my_variable`)
/// - An expression in parentheses (e.g., `(1 + 2)`)
/// - A trinary function call (e.g., `AE(a,b,c)`)
/// - A binary function call (e.g., `MAX(a,b)`)
fn parse_primary(input: &str) -> ExprResult<Expr> {
    preceded(
        space0,
        alt((
            parse_trinary_call,
            parse_binary_call,
            parse_literal,
            parse_unit_output_bracketed,
            parse_unconnected,
            parse_identifier,
            parse_parens,
        )),
    )
    .parse(input)
}
// Macro to parse a keyword followed by an opening parenthesis,
// indicating a function call.
macro_rules! keyword {
    ($op:expr) => {
        map(
            value(
                $op,
                terminated(
                    complete(tag_no_case($op.as_ref())),
                    peek(preceded(multispace0, char('('))),
                ),
            ),
            |_| $op,
        )
    };
}

/// Parses a unary operation from the input string.
/// This includes negation (`-`) and functions like `ABS()`, `SIN()`, etc.
///
/// Example: `-5`, `ABS(-10)`, `SIN(0.5)`
fn parse_unary(input: &str) -> ExprResult<Expr> {
    let mut parse_opt_op = opt(preceded(
        space0,
        alt((
            // Negation operator without parentheses
            map(tag_no_case(UnaryOperator::Negate.as_ref()), |_| {
                UnaryOperator::Negate
            }),
            // Unary operators with parentheses
            keyword!(UnaryOperator::Negate),
            keyword!(UnaryOperator::Abs),
            keyword!(UnaryOperator::ACos),
            keyword!(UnaryOperator::ASin),
            keyword!(UnaryOperator::ATan),
            keyword!(UnaryOperator::Cos),
            keyword!(UnaryOperator::Int),
            keyword!(UnaryOperator::Ln),
            keyword!(UnaryOperator::Log),
            keyword!(UnaryOperator::Not),
            keyword!(UnaryOperator::Sin),
            keyword!(UnaryOperator::Tan),
            keyword!(UnaryOperator::Exp),
        )),
    ));

    let (rest, opt_op) = parse_opt_op.parse(input)?;

    if let Some(op) = opt_op {
        let (rest, expr) = parse_unary(rest)?;
        Ok((
            rest,
            Expr::UnaryOp {
                op,
                expr: Box::new(expr),
            },
        ))
    } else {
        parse_primary(input)
    }
}

#[derive(Copy, Clone)]
struct BinInfo {
    op: BinaryOperator,
    /// precedence
    prec: u8,
    /// right associative
    right_assoc: bool,
}

/// Parses a binary operator and returns its information (operator, precedence, associativity).
/// Supported operators: `^`, `*`, `/`, `+`, `-`.
fn parse_binary_operator(input: &str) -> ExprResult<BinInfo> {
    preceded(
        space0,
        alt((
            map(tag("^"), |_| BinInfo {
                op: BinaryOperator::Power,
                prec: 4,
                right_assoc: true,
            }),
            map(tag("*"), |_| BinInfo {
                op: BinaryOperator::Multiply,
                prec: 3,
                right_assoc: false,
            }),
            map(tag("/"), |_| BinInfo {
                op: BinaryOperator::Divide,
                prec: 3,
                right_assoc: false,
            }),
            map(tag("+"), |_| BinInfo {
                op: BinaryOperator::Add,
                prec: 2,
                right_assoc: false,
            }),
            map(tag("-"), |_| BinInfo {
                op: BinaryOperator::Subtract,
                prec: 2,
                right_assoc: false,
            }),
        )),
    )
    .parse(input)
}

/// Parses an expression using the precedence-climbing algorithm (also known as Pratt parsing).
/// This function handles binary operators like `+`, `-`, `*`, `/`, and `^`,
/// respecting their precedence and associativity. It also handles implicit multiplication.
///
/// `min_prec` is the minimum precedence level for an operator to be considered.
fn _parse_expr(input: &str, min_prec: u8) -> ExprResult<Expr> {
    let (mut rest, mut lhs) = parse_unary(input)?;

    loop {
        if let Ok((after_op, op_info)) = parse_binary_operator(rest) {
            if op_info.prec < min_prec {
                break;
            }
            let next_min = if op_info.right_assoc {
                op_info.prec
            } else {
                op_info.prec + 1
            };
            let (after_rhs, rhs) = _parse_expr(after_op, next_min)?;
            lhs = Expr::BinaryOp {
                op: op_info.op,
                first: Box::new(lhs),
                second: Box::new(rhs),
            };
            rest = after_rhs;
            continue;
        }

        // Implicit multiplication
        if min_prec <= 3 {
            if let Ok((after_rhs, rhs)) = parse_unary(rest) {
                lhs = Expr::BinaryOp {
                    op: BinaryOperator::Multiply,
                    first: Box::new(lhs),
                    second: Box::new(rhs),
                };
                rest = after_rhs;
                continue;
            }
        }

        break;
    }
    Ok((rest, lhs))
}

/// Parses a trinary function call from the input string.
/// Trinary functions take three arguments.
///
/// Format: `FUNCTION_NAME(arg1, arg2, arg3)`
/// Example: `AE(var1, var2, var3)`
pub fn parse_trinary_call(input: &str) -> ExprResult<Expr> {
    let parse_op = map_report(
        preceded(
            space0,
            alt((
                keyword!(TrinaryOperator::AE),
                keyword!(TrinaryOperator::EqWarn),
                keyword!(TrinaryOperator::GtWarn),
                keyword!(TrinaryOperator::GeWarn),
                keyword!(TrinaryOperator::NeWarn),
            )),
        ),
        |e: RError| {
            e.change_context(Error::UnexpectedContent {
                message: "Cannot parse Trinary Operator.".to_string(),
                scope: ErrorScope::Expression,
            })
        },
    );

    let (rest, (op, args)) = (
        parse_op,
        delimited(
            preceded(space0, char('(')),
            separated_list1(delimited(space0, char(','), space0), parse_expr),
            preceded(space0, char(')')),
        ),
    )
        .parse(input)?;

    if args.len() != 3 {
        return Err(nom::Err::Failure(RError::new(Error::UnexpectedContent {
            message: format!("{} expects 3 arguments, got {}", op, args.len()),
            scope: ErrorScope::Expression,
        })));
    }

    let mut it = args.into_iter();
    let first = it.next().unwrap();
    let second = it.next().unwrap();
    let third = it.next().unwrap();

    Ok((
        rest,
        Expr::TrinaryOp {
            op,
            first: Box::new(first),
            second: Box::new(second),
            third: Box::new(third),
        },
    ))
}

/// Parses a binary function call from the input string.
/// Binary functions take two arguments.
///
/// Format: `FUNCTION_NAME(arg1, arg2)`
/// Example: `MAX(val1, val2)`, `AND(cond1, cond2)`
pub fn parse_binary_call(input: &str) -> ExprResult<Expr> {
    let (rest, op) = preceded(
        space0,
        alt((
            keyword!(BinaryOperator::And),
            keyword!(BinaryOperator::Or),
            keyword!(BinaryOperator::Equal),
            keyword!(BinaryOperator::NotEqual),
            keyword!(BinaryOperator::LessThan),
            keyword!(BinaryOperator::LessThanOrEqual),
            keyword!(BinaryOperator::GreaterThan),
            keyword!(BinaryOperator::GreaterThanOrEqual),
            keyword!(BinaryOperator::Max),
            keyword!(BinaryOperator::Min),
            keyword!(BinaryOperator::Modulo),
        )),
    )
    .parse(input)?;

    let (rest, args) = delimited(
        preceded(space0, char('(')),
        separated_list1(
            preceded(space0, char(',')),
            parse_expr, // 递归解析子表达式
        ),
        preceded(space0, char(')')),
    )
    .parse(rest)?;

    if args.len() != 2 {
        return Err(nom::Err::Failure(RError::new(Error::UnexpectedContent {
            message: format!("{} expects 2 arguments, got {}", op, args.len()),
            scope: ErrorScope::Expression,
        })));
    }

    let mut it = args.into_iter();
    let first = it.next().unwrap();
    let second = it.next().unwrap();

    Ok((
        rest,
        Expr::BinaryOp {
            op,
            first: Box::new(first),
            second: Box::new(second),
        },
    ))
}

/// Promotes all `nom::Err::Error` to `nom::Err::Failure` for the given parser.
/// This is useful to prevent backtracking in certain parsing scenarios.
fn promote<'a>(
    mut parser: impl Parser<&'a str, Output = Expr, Error = RError>,
) -> impl Parser<&'a str, Output = Expr, Error = RError> {
    move |input| parser.parse(input).map_err(|e| e.into())
}

/// Parses an expression from the input string using the precedence-climbing algorithm (`expr_bp`).
/// This is a wrapper around `expr_bp` that starts with a minimum precedence of 1 and
/// provides more detailed error reporting.
pub fn parse_expr(input: &str) -> IResult<&str, Expr, RError> {
    map_report(promote(|i| _parse_expr(i, 1)), |e| {
        e.change_context(Error::UnexpectedContent {
            message: format!("Cannot parse Expression: {}", input),
            scope: ErrorScope::Expression,
        })
    })
    .parse(input)
}

/// Consumes the entire input string and parses it as a single expression.
/// This function ensures that no input is left unparsed after the expression.
/// It also provides error reporting if the entire input cannot be consumed.
pub fn consume_expr(input: &str) -> ExprResult<Expr> {
    map_report(promote(all_consuming(preceded(space0, parse_expr))), |r| {
        r.attach_printable("Failed to consume entire expression.")
    })
    .parse(input)
}

/// Parses the start of an equation, specifically the identifier before the equals sign.
///
/// Example: `my_equation = ...` (would parse `my_equation`)
fn equation_start(input: &str) -> IResult<&str, Expr, RError> {
    let (input, id) = context(
        "Equation Start",
        delimited(multispace0, parse_identifier, (space0, char('='), space0)),
    )
    .parse(input)?;
    if let Expr::Identifier(_) = &id {
        Ok((input, id))
    } else {
        Err(nom::Err::Failure(RError::new(Error::UnexpectedContent {
            message: "Expected an identifier for equation name".to_string(),
            scope: ErrorScope::Expression,
        })))
    }
}

/// Parses a complete equation block, including any preceding comments,
/// the equation itself (name = expression).
fn parse_eq_block(input: &str) -> IResult<&str, Commented<EquationDef>, RError> {
    let (input, (comments_pre, eq_name, (content, comment_inline))) =
        (parse_block_comment, equation_start, parse_commented_row).parse(input)?;

    let eq_name = if let Expr::Identifier(name) = eq_name {
        name
    } else {
        return Err(nom::Err::Failure(RError::new(Error::UnexpectedContent {
            message: "Expected an identifier for equation name".to_string(),
            scope: ErrorScope::Expression,
        })));
    };

    let (_, expr) = consume_expr(content)?;

    let equation = EquationDef {
        name: eq_name,
        expr,
    };

    let mut result = Commented::from(equation);

    if let Some(comments_pre) = comments_pre {
        result.comments.comment_pre = Some(comments_pre.into_iter().map(Into::into).collect());
    }
    result.comments.comment_inline = comment_inline.map(Into::into);

    Ok((input, result))
}

/// Creates a parser that parses a specific number of equations.
///
/// `num`: The exact number of equations to parse.
pub fn parse_equations<'a>(
    num: usize,
) -> impl Parser<&'a str, Output = Vec<Commented<EquationDef>>, Error = RError> {
    many_m_n(num, num, parse_eq_block)
}

/// Parses either a literal or an identifier from the input string, consuming the entire input.
/// This is useful for parsing simple values that can be either a number or a variable name.
///
/// Example: `123`, `my_var`
pub fn parse_literal_or_identifier(input: &str) -> Result<Expr, RError> {
    let (_, result) = map_report(
        promote(all_consuming(delimited(
            space0,
            alt((parse_literal, parse_identifier)),
            space0,
        ))),
        |r| r.attach_printable("Failed to consume entire expression."),
    )
    .parse(input)
    .map_err(|e| RError::from(e))?;
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr::*;
    use crate::error::RError;

    #[test]
    fn test_parse_identifier() -> Result<(), RError> {
        let expr = "my_v2Var";
        let _ = all_consuming(parse_identifier).parse(expr)?;

        let expr = "_abc";
        let result = all_consuming(parse_identifier).parse(expr);
        assert!(
            result.is_err(),
            "Expected error for identifier starting with underscore, but got: {:?}",
            result
        );

        let expr = "123abc";
        let result = all_consuming(parse_identifier).parse(expr);
        assert!(
            result.is_err(),
            "Expected error for identifier starting with digit, but got: {:?}",
            result
        );

        for char in [
            '$', '!', '@', '#', '%', '^', '&', '*', '(', ')', '.', ',', '\'', '"',
        ] {
            let expr = format!("my_var{}", char);
            let result = all_consuming(parse_identifier).parse(&expr);
            assert!(
                result.is_err(),
                "Expected error for identifier with invalid character '{}', but got: {:?}",
                char,
                result
            );
        }

        Ok(())
    }

    #[test]
    fn test_parse_expr() -> Result<(), RError> {
        let expr = "(a + b * c)d";
        let (remaining, parsed_expr) = consume_expr(expr)?;
        assert_eq!(remaining, "");
        assert_eq!(
            parsed_expr,
            BinaryOp {
                op: BinaryOperator::Multiply,
                first: Box::new(BinaryOp {
                    op: BinaryOperator::Add,
                    first: Box::new(Identifier("a".to_string())),
                    second: Box::new(BinaryOp {
                        op: BinaryOperator::Multiply,
                        first: Box::new(Identifier("b".to_string())),
                        second: Box::new(Identifier("c".to_string())),
                    }),
                }),
                second: Box::new(Identifier("d".to_string())),
            }
        );
        Ok(())
    }

    #[test]
    fn test_parse_unary() -> Result<(), RError> {
        let expr = "-a";
        let (remaining, parsed_expr) = consume_expr(expr)?;

        assert_eq!(remaining, "");
        assert_eq!(
            parsed_expr,
            UnaryOp {
                op: UnaryOperator::Negate,
                expr: Box::new(Identifier("a".to_string())),
            }
        );
        Ok(())
    }

    #[test]
    fn test_pred() -> Result<(), RError> {
        let expr = "-a+b*c^d";
        let (remaining, parsed_expr) = consume_expr(expr)?;
        assert_eq!(remaining, "");
        assert_eq!(
            parsed_expr,
            BinaryOp {
                op: BinaryOperator::Add,
                first: Box::new(UnaryOp {
                    op: UnaryOperator::Negate,
                    expr: Box::new(Identifier("a".to_string())),
                }),
                second: Box::new(BinaryOp {
                    op: BinaryOperator::Multiply,
                    first: Box::new(Identifier("b".to_string())),
                    second: Box::new(BinaryOp {
                        op: BinaryOperator::Power,
                        first: Box::new(Identifier("c".to_string())),
                        second: Box::new(Identifier("d".to_string())),
                    }),
                }),
            }
        );
        Ok(())
    }

    #[test]
    fn test_parse_unary_complex() -> Result<(), RError> {
        let expr = "-exp(a+b)*(-c*int(d)e)";
        let (rest, result) = consume_expr(expr)?;
        assert!(
            rest.is_empty(),
            "Expected no remaining input, but got: '{}'",
            rest
        );
        assert_eq!(
            result,
            BinaryOp {
                op: BinaryOperator::Multiply,
                first: Box::new(UnaryOp {
                    op: UnaryOperator::Negate,
                    expr: Box::new(UnaryOp {
                        op: UnaryOperator::Exp,
                        expr: Box::new(BinaryOp {
                            op: BinaryOperator::Add,
                            first: Box::new(Identifier("a".to_string())),
                            second: Box::new(Identifier("b".to_string())),
                        }),
                    }),
                }),
                second: Box::new(BinaryOp {
                    op: BinaryOperator::Multiply,
                    first: Box::new(BinaryOp {
                        op: BinaryOperator::Multiply,
                        first: Box::new(UnaryOp {
                            op: UnaryOperator::Negate,
                            expr: Box::new(Identifier("c".to_string())),
                        }),
                        second: Box::new(UnaryOp {
                            op: UnaryOperator::Int,
                            expr: Box::new(Identifier("d".to_string())),
                        }),
                    }),
                    second: Box::new(Identifier("e".to_string()))
                }),
            }
        );
        Ok(())
    }

    #[test]
    fn test_parse_unit_output() -> Result<(), RError> {
        let expr = "\t 200,8";
        let result = parse_unit_output(expr);
        assert!(
            result.is_ok(),
            "Failed to parse expression: {:?}",
            result.err()
        );
        let (remaining, parsed_expr) = result.unwrap();
        assert_eq!(remaining, "");
        assert_eq!(parsed_expr, UnitOutput(UnitConnection::new(200, 8)));
        Ok(())
    }

    #[test]
    fn test_parse_unit_output_bracketed() -> Result<(), RError> {
        let expr = "[200,8]";
        let result = consume_expr(expr);
        assert!(
            result.is_ok(),
            "Failed to parse expression: {:?}",
            result.err()
        );
        let (remaining, parsed_expr) = result.unwrap();
        assert_eq!(remaining, "");
        assert_eq!(parsed_expr, UnitOutput(UnitConnection::new(200, 8)));
        Ok(())
    }

    #[test]
    fn test_parse_binary_func() -> Result<(), RError> {
        let expr = "AND(a, b)";
        let result = parse_binary_call(expr);
        assert!(
            result.is_ok(),
            "Failed to parse expression: {:?}",
            result.err()
        );
        let (remaining, parsed_expr) = result.unwrap();
        assert_eq!(remaining, "");
        assert_eq!(
            parsed_expr,
            BinaryOp {
                op: BinaryOperator::And,
                first: Box::new(Identifier("a".to_string())),
                second: Box::new(Identifier("b".to_string())),
            }
        );
        Ok(())
    }

    #[test]
    fn test_parse_trinary() -> Result<(), RError> {
        let expr = "AE(a, b, c)GR";
        // let result = parse_expr(expr);
        let result = consume_expr(expr);
        assert!(
            result.is_ok(),
            "Failed to parse expression: {:?}",
            result.err()
        );
        let (remaining, parsed_expr) = result.unwrap();
        assert_eq!(remaining, "");
        assert_eq!(
            parsed_expr,
            BinaryOp {
                op: BinaryOperator::Multiply,
                first: Box::new(TrinaryOp {
                    op: TrinaryOperator::AE,
                    first: Box::new(Identifier("a".to_string())),
                    second: Box::new(Identifier("b".to_string())),
                    third: Box::new(Identifier("c".to_string())),
                }),
                second: Box::new(Identifier("GR".to_string())),
            }
        );
        Ok(())
    }
}
