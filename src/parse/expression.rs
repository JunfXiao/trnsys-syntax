use crate::ast::{
    BinaryOperator, CSummarize, Commented, ESummarize, EquationDef, Expr, TrinaryOperator,
    UnaryOperator, UnitConnection,
};
use crate::error::{Error, ErrorScope, RError};
use crate::parse::{
    BlockKind, BlockParser, map_report, op_permutation, parse_block_comment, parse_commented_row, parse_header_of_kind,
};
use nom::Parser;
use nom::bytes::complete::take_while;
use nom::character::complete::{alphanumeric1, multispace0};
use nom::combinator::{all_consuming, complete, recognize};
use nom::combinator::peek;
use nom::error::ErrorKind;
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

/// float literal
pub fn parse_literal(input: &str) -> ExprResult<Expr> {
    map(double, Expr::Literal).parse(input)
}

/// Unit outputs like `[200,8]`
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

/// Unit outputs like `200,8` without brackets
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

pub fn parse_unconnected(input: &str) -> ExprResult<Expr> {
    map(tag("0,0"), |_| Expr::Unconnected).parse(input)
}

/// Identifiers are strings that:
/// - start with a letter or `_`
/// - followed by any number of letters, digits, or `_`
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

/// Expression within parentheses
pub fn parse_parens(input: &str) -> ExprResult<Expr> {
    delimited(
        preceded(space0, char('(')),
        parse_expr, // 递归
        preceded(space0, char(')')),
    )
    .parse(input)
}

/// Primary is ont of the followings:
/// -  literal
/// - [u,o]
/// - 0,0
/// - identifier
/// - (expr)
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

fn parse_unary(input: &str) -> ExprResult<Expr> {
    // possible comments
    // let parse_comment = opt(delimited(
    //     space0,
    //     opt(preceded(char('!'), not_line_ending)),
    //     (space0, line_ending),
    // ));

    let mut parse_opt_op = opt(preceded(
        space0,
        alt((
            map(tag("-"), |_| UnaryOperator::Negate),
            map(tag("NOT"), |_| UnaryOperator::Not),
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

/// Try to retrieve the binary operator and its precedence given the input
fn bin_op(input: &str) -> ExprResult<BinInfo> {
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

/// precedence-climbing
fn expr_bp(input: &str, min_prec: u8) -> ExprResult<Expr> {
    let (mut rest, mut lhs) = parse_unary(input)?;

    loop {
        if let Ok((after_op, op_info)) = bin_op(rest) {
            if op_info.prec < min_prec {
                break;
            }
            let next_min = if op_info.right_assoc {
                op_info.prec
            } else {
                op_info.prec + 1
            };
            let (after_rhs, rhs) = expr_bp(after_op, next_min)?;
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

macro_rules! keyword {
    ($word:literal, $op:expr) => {
        map(
            value(
                $op,
                terminated(tag($word), peek(preceded(multispace0, char('(')))),
            ),
            |_| $op,
        )
    };
}

/// Trinary
///
/// Format: TAG( first <sp> OP <sp> second <sp> third)
pub fn parse_trinary_call(input: &str) -> ExprResult<Expr> {
    let parse_op = map_report(
        preceded(
            space0,
            alt((
                keyword!("AE", TrinaryOperator::AE),
                keyword!("EQWARN", TrinaryOperator::EqWarn),
                keyword!("GTWARN", TrinaryOperator::GtWarn),
                keyword!("GEWARN", TrinaryOperator::GeWarn),
                keyword!("NEWARN", TrinaryOperator::NeWarn),
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
            separated_list1(
                delimited(space0, char(','), space0),
                parse_expr, // 递归解析子表达式
            ),
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

/// Function calls
///
/// Format: TAG( first <sp> second)
pub fn parse_binary_call(input: &str) -> ExprResult<Expr> {
    let (rest, op) = preceded(
        space0,
        alt((
            keyword!("AND", BinaryOperator::And),
            keyword!("OR", BinaryOperator::Or),
            keyword!("EQ", BinaryOperator::Equal),
            keyword!("NE", BinaryOperator::NotEqual),
            keyword!("LT", BinaryOperator::LessThan),
            keyword!("LE", BinaryOperator::LessThanOrEqual),
            keyword!("GT", BinaryOperator::GreaterThan),
            keyword!("GE", BinaryOperator::GreaterThanOrEqual),
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

fn promote<'a>(
    mut parser: impl Parser<&'a str, Output = Expr, Error = RError>,
) -> impl Parser<&'a str, Output = Expr, Error = RError> {
    move |input| {
        parser.parse(input).map_err(|e| match e {
            nom::Err::Error(ne) | nom::Err::Failure(ne) => {
                let err = Error::GeneralError {
                    input: input.to_string(),
                    kind: ErrorKind::Verify,
                    scope: ErrorScope::Expression,
                };
                nom::Err::Failure(ne.change_context(err))
            }
            nom::Err::Incomplete(needed) => nom::Err::Incomplete(needed),
        })
    }
}

/// Parse an expression and return the remaining input
pub fn parse_expr(input: &str) -> IResult<&str, Expr, RError> {
    map_report(promote(|i| expr_bp(i, 1)), |e| {
        e.change_context(Error::UnexpectedContent {
            message: format!("Cannot parse Expression: {}", input),
            scope: ErrorScope::Expression,
        })
    })
    .parse(input)
}

/// Consumes the entire input and returns the parsed expression
pub fn consume_expr(input: &str) -> ExprResult<Expr> {
    promote(all_consuming(preceded(space0, parse_expr))).parse(input)
}

//
fn equation_start(input: &str) -> IResult<&str, &str, RError> {
    delimited(
        space0,
        complete(alt((alphanumeric1, tag("_")))),
        (space0, char('='), space0),
    )
    .parse(input)
}

fn parse_eq_block(input: &str) -> IResult<&str, Commented<EquationDef>, RError> {
    let (input, (comments_pre, eq_name, (content, comment_inline))) =
        (parse_block_comment, equation_start, parse_commented_row).parse(input)?;

    let (input, (cs_header, es_header)) = op_permutation(
        parse_header_of_kind(Some(BlockKind::CSummarize), Some(2)),
        parse_header_of_kind(Some(BlockKind::ESummarize), Some(2)),
    )
    .parse(input)?;

    let c_summarize: Option<Commented<CSummarize>> = if let Some(header) = cs_header {
        Some(CSummarize::try_parse_block_without_context(header)?)
    } else {
        None
    };

    let e_summarize: Option<Commented<ESummarize>> = if let Some(header) = es_header {
        Some(ESummarize::try_parse_block_without_context(header)?)
    } else {
        None
    };

    let (_, expr) = consume_expr(content)?;

    let equation = EquationDef {
        name: eq_name.to_string(),
        expr,
        csummarize: c_summarize,
        esummarize: e_summarize,
    };

    let mut result = Commented::from(equation);

    if let Some(comments_pre) = comments_pre {
        result.comments.comment_pre = Some(comments_pre.into_iter().map(Into::into).collect());
    }
    result.comments.comment_inline = comment_inline.map(Into::into);

    Ok((input, result))
}

/// Parse an equation in the form of `name = expr`
pub fn parse_equations<'a>(
    num: usize,
) -> impl Parser<&'a str, Output = Vec<Commented<EquationDef>>, Error = RError> {
    many_m_n(num, num, parse_eq_block)
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
        let result = parse_expr(expr);
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
            UnaryOp {
                op: UnaryOperator::Negate,
                expr: Box::new(Identifier("a".to_string())),
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
