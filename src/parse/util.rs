use crate::ast::Commented;
use crate::error::{Error, ErrorScope, NewFromErrorKind, NomErr, RError, ReportWrapper};
use crate::parse::{
    parse_block_comment, parse_comment_with,
};
use error_stack::Context;
use nom::branch::alt;
use nom::bytes::complete::{is_a, tag};
use nom::bytes::is_not;
use nom::character::complete::{line_ending, multispace0, space0};
use nom::character::char;
use nom::combinator::{all_consuming, complete, cut, eof, map, opt, recognize};
use nom::error::ParseError;
use nom::multi::{many_m_n, many0};
use nom::sequence::{delimited, preceded};
use nom::{AsChar, FindToken, IResult, Input, Mode, OutputMode, PResult, Parser};
use std::borrow::Cow;
use std::marker::PhantomData;

pub fn trim<'a, P, I, O, E>(mut inner: P) -> impl Parser<I, Output = O, Error = E>
where
    P: Parser<I, Output = O, Error = E>,
    I: nom::Input,
    O: nom::Input + nom::Offset,
    <I as Input>::Item: AsChar,
    <O as Input>::Item: AsChar,
    E: nom::error::ParseError<I> + nom::error::ParseError<O>,
    <O as Input>::Iter: DoubleEndedIterator,
{
    move |input: I| {
        let (input, _) = multispace0.parse(input)?;
        let (input, inner_result) = (&mut inner).parse(input)?;
        let (inner_result, _) = multispace0.parse(inner_result)?;
        let r = inner_result.iter_elements().rev();
        let mut space_len = 0;
        let mut stopped = false;
        let mut total_len = 0;
        r.for_each(|i| {
            total_len += 1;
            if !stopped {
                if i.is_space() || i.is_newline() {
                    space_len += 1;
                } else {
                    stopped = true;
                }
            }
        });

        let inner_result = inner_result.take(total_len - space_len);
        Ok((input, inner_result))
    }
}

pub fn vec_trim<'a, P, E>(inner: P) -> impl Parser<&'a str, Output = Vec<&'a str>, Error = E>
where
    P: Parser<&'a str, Output = Vec<&'a str>, Error = E>,
    E: nom::error::ParseError<&'a str>,
{
    map(inner, |s| {
        s.iter()
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect()
    })
}

#[derive(Debug, Clone)]
pub struct AltVec<I, O, T, E = RError> {
    alternatives: Vec<T>,
    _phantom: PhantomData<(I, O, E)>,
}

impl<I, O, T, E> Parser<I> for AltVec<I, O, T, E>
where
    I: Clone + Input,
    T: Parser<I, Output = O, Error = E>,
    E: nom::error::ParseError<I>,
{
    type Output = O;
    type Error = E;

    fn process<OM: OutputMode>(
        &mut self,
        input: I,
    ) -> PResult<OM, I, Self::Output, Self::Error> {
        for alternative in &mut self.alternatives {
            match alternative.process::<OM>(input.clone()) {
                Ok((remaining, matched)) => {
                    return Ok((remaining, matched));
                }
                Err(_) => continue,
            }
        }
        Err(nom::Err::Error(OM::Error::bind(|| {
            E::from_error_kind(input, nom::error::ErrorKind::Alt)
        })))
    }
}

pub fn alt_vec<I, O, T, E>(alternatives: Vec<T>) -> AltVec<I, O, T, E> {
    AltVec {
        alternatives,
        _phantom: PhantomData,
    }
}

pub fn quoted<I, E>(input: I) -> IResult<I, I, E>
where
    E: ParseError<I>,
    I: nom::Input + nom::Offset,
    for<'a> &'a str: FindToken<<I as Input>::Item>,
    <I as Input>::Item: AsChar,
{
    delimited(char('"'), is_not("\""), char('"')).parse(input)
}

/// Parse IResult<I,O,E=SyntaxError> to IResult<I, O, Report<SyntaxError>>
pub fn to_report<I, O, E>(result: IResult<I, O, E>) -> IResult<I, O, ReportWrapper<E>>
where
    I: Input,
    E: ParseError<I> + Context + std::error::Error + Send + Sync + 'static,
{
    result.map_err(|e| e.map(|e| e.into()))
}

pub fn with_report<I, O, E, P>(
    mut parser: P,
) -> impl Parser<I, Output = O, Error = ReportWrapper<E>>
where
    I: Input,
    P: Parser<I, Output = O, Error = E>,
    E: ParseError<I> + Context + std::error::Error + Send + Sync + Clone + NewFromErrorKind<I>,
{
    move |input: I| {
        let result = parser.parse(input);
        to_report(result)
    }
}

#[track_caller]
pub fn map_report<I, O, E, P, F>(mut parser: P, mut f: F) -> impl Parser<I, Output = O, Error = E>
where
    I: Input,
    P: Parser<I, Output = O, Error = E>,
    E: ParseError<I> + std::error::Error + NewFromErrorKind<I> + Send + Sync + 'static,
    F: FnMut(E) -> E,
{
    move |input: I| {
        parser.parse(input)
            .map_err(|e| e.map(|e| f(e)))
    }
}

pub fn separated_by<'a, I, E>(
    num: Option<usize>,
    sep: &'a str,
) -> impl Parser<I, Output = Vec<I>, Error = E>
where
    I: Input + nom::Offset,
    E: ParseError<I>,
    <I as Input>::Item: AsChar,
    for<'b> &'b str: FindToken<<I as Input>::Item>,
{
    move |input: I| {
        let parse_item = complete(delimited(
            opt(is_a(sep)),
            alt((quoted, is_not(sep))),
            opt(is_a(sep)),
        ));
        let (remaining, output) = match num {
            Some(n) => many_m_n(n, n, parse_item).parse(input)?,
            None => many0(parse_item).parse(input)?,
        };

        Ok((remaining, output))
    }
}

pub fn parse_inline_values<'a, I, E>(
    num: Option<usize>,
) -> impl Parser<I, Output = Vec<I>, Error = E>
where
    I: Input + nom::Offset,
    E: ParseError<I>,
    for<'b> &'b str: FindToken<<I as Input>::Item>,
    <I as Input>::Item: AsChar,
{
    let spacings = " \t";
    separated_by(num, spacings)
}

/// Parse inline values with a strict number of parameters in the line.
/// Any violation will result in an error.
pub fn parse_inline_values_strict<'a, I>(
    num: usize,
) -> impl Parser<I, Output = Vec<I>, Error = RError>
where
    I: Input + nom::Offset + nom::Compare<&'static str> + std::fmt::Display,
    <I as Input>::Item: AsChar,
    for<'b> &'b str: FindToken<<I as Input>::Item>,
{
    map_report(
        cut((
            parse_inline_values::<_, RError>(Some(num)),
            space0,
            alt((line_ending, eof)),
        )
            .map(|(o, _, _)| o)),
        move |e| {
            e.change_context(Error::UnexpectedContent {
                message: format!("Expected exact {} parameters!", num),
                scope: ErrorScope::Param,
            })
        },
    )
}





///
/// Parse a permutation of two parsers.
/// Each parser can be present or absent with the maximum of one occurrence.
pub fn op_permutation<I, L, R, E, PA, PB>(
    mut parse_a: PA,
    mut parse_b: PB,
) -> impl FnMut(I) -> IResult<I, (Option<L>, Option<R>), E>
where
    I: Clone + Input,
    PA: Parser<I, Output = L, Error = E>,
    PB: Parser<I, Output = R, Error = E>,
    E: ParseError<I>,
{
    move |input: I| {
        let mut l_val: Option<L> = None;
        let mut r_val: Option<R> = None;
        let mut i = input.clone();

        // Try to parse PA in the first attempt, if it fails, try to parse PB
        match parse_a.parse(i.clone()) {
            Ok((i2, v)) => {
                l_val = Some(v);
                i = i2;
            }
            Err(NomErr::Error(_)) => {
                if let Ok((i2, v)) = parse_b.parse(i.clone()) {
                    r_val = Some(v);
                    i = i2;
                }
            }
            Err(e) => return Err(e), // Incomplete / Failure
        }

        // Try to parse the other one if the first one was successful
        match (&l_val, &r_val) {
            (Some(_), None) => {
                // Already have PA, try PB
                if let Ok((i2, v)) = parse_b.parse(i.clone()) {
                    r_val = Some(v);
                    i = i2;
                }
            }
            (None, Some(_)) => {
                // 已有 PB，尝试 PA
                if let Ok((i2, v)) = parse_a.parse(i.clone()) {
                    l_val = Some(v);
                    i = i2;
                }
            }
            // (None,None) or (Some(_), Some(_)) do nothing
            _ => {}
        }

        Ok((i, (l_val, r_val)))
    }
}

pub fn parse_int(input: &str) -> IResult<&str, isize, RError> {
    let (remaining, parsed) = map(recognize(is_a("0123456789")), |s: &str| {
        s.parse::<isize>().unwrap()
    })
    .parse(input)?;
    Ok((remaining, parsed))
}

/// Parse a single mixed parameter, ends with a space, newline or a comment,
/// quoted strings are supported.
pub fn parse_mixed_param<'a, I, P, PO, E>(
    mut parser: P,
    separators: Option<&'a str>,
) -> impl Parser<I, Output = Commented<PO>, Error = E>
where
    I: Input<Item = char>
        + nom::Offset
        + std::fmt::Debug
        + nom::Compare<&'static str>
        + std::fmt::Display,
    P: Parser<I, Output = PO, Error = E> + 'a,
    E: ParseError<I>,
    for<'b> &'b str: FindToken<<I as Input>::Item>,
    <I as Input>::Item: AsChar,
    PO: std::fmt::Debug,
    Cow<'a, str>: From<I>,
    <I as Input>::Iter: DoubleEndedIterator,
{
    move |input: I| {
        let separator = separators.as_deref().unwrap_or_else(|| " \t\n\r");

        let (input, (param, inline_comment, block_comment)) = preceded(
            multispace0,
            (
                complete(alt((quoted, is_not(separator)))),
                opt(parse_comment_with(tag("!"))),
                parse_block_comment,
            ),
        )
        .parse(input)?;

        let (_, output) = all_consuming(|i| parser.parse(i)).parse(param)?;
        let mut commented = Commented::from(output);
        commented.comments.comment_inline = inline_comment.map(|i|i.to_string());

        if block_comment.is_some() {
            commented.comments.comment_post =
                block_comment.map(|v| v.into_iter().map(|i|i.to_string()).collect());
        }
        Ok((input, commented))
    }
}

#[cfg(test)]
mod util_tests {
    use nom::bytes::complete::take_till;
    use super::*;
    use crate::error::RError;
    use nom::character::anychar;
    use nom::combinator::{complete, peek};
    use nom::multi::{many_till, many0, many1};

    #[test]
    fn test_trim() -> Result<(), RError> {
        let input = " \t  Hello, World!   ";
        let mut parser = trim(complete(recognize(many0(anychar))));
        let (remaining, output)  = parser.parse(input)?;

        assert_eq!(remaining, "");
        assert_eq!(output, "Hello, World!");
        Ok(())
    }

    #[test]
    fn test_alt_vec() -> Result<(), RError> {
        fn gen_alt_parser<'a>()
        -> AltVec<&'a str, &'a str, impl Parser<&'a str, Output = &'a str, Error = RError>>
        {
            let parser1 = tag("Hello");
            let parser2 = tag("World");
            let parser_vec = vec![parser1, parser2];
            alt_vec(parser_vec)
        }

        let input = "Hello";
        let mut alt_parser = gen_alt_parser();

        let result = alt_parser.parse(input)?;

        assert_eq!(result.0, "");

        let input = "World";
        let (remaining, output) = alt_parser.parse(input)?;
        assert_eq!(remaining, "");
        assert_eq!(output, "World");

        let input = "Unknown Hello World";
        let result = alt_parser.parse(input);
        assert!(result.is_err());

        let input = "WorldabcHelloWorld";
        let mut parser = delimited(
            gen_alt_parser(),
            recognize(many_till(anychar, peek(gen_alt_parser()))),
            gen_alt_parser(),
        );
        let result = parser.parse(input);
        let (remaining, output) = result?;
        assert_eq!(remaining, "World");
        assert_eq!(output, "abc");
        
        Ok(())
    }

    #[test]
    fn test_quoted_string() -> Result<(), RError> {
        let input = "\"Hello, World!\"";
        let mut parser = quoted::<_, RError>;
        let result = parser.parse(input);

        let (remaining, output) = result?;
        assert_eq!(remaining, "");
        assert_eq!(output, "Hello, World!");

        Ok(())
    }

    #[test]
    fn test_inline_params() -> Result<(), RError> {
        let input = "\tparam1 \t param2\tparam3 ";
        let mut parser = parse_inline_values::<&str, RError>(None);
        let result = parser.parse(input);
        let (remaining, output) = result?;
        assert_eq!(remaining, "");
        assert_eq!(output, vec!["param1", "param2", "param3"]);

        Ok(())
    }

    #[test]
    fn test_inline_params_n() -> Result<(), RError> {
        let input = "\tparam1 \t param2\tparam3 ";
        let mut parser = parse_inline_values::<&str, RError>(Some(2));
        let result = parser.parse(input);
        let (remaining, output) = result?;
        assert_eq!(remaining, "param3 ");
        assert_eq!(output, vec!["param1", "param2"]);

        Ok(())
    }

    #[test]
    fn test_inline_params_strict() -> Result<(), RError> {
        let input = "\tparam1 \t \"a\tparam2\"\tparam3";
        let mut parser = parse_inline_values_strict::<&str>(3);
        let result = parser.parse(input);
        let (remaining, output) = result?;
        assert_eq!(remaining, "");
        assert_eq!(output, vec!["param1", "a\tparam2", "param3"]);

        Ok(())
    }

    #[test]
    fn test_inline_params_limit_error() -> Result<(), RError> {
        let input = "\tparam1 \t param2\tparam3";
        let mut parser = parse_inline_values_strict::<&str>(2);
        let result = parser.parse(input);
        assert!(
            result.is_err(),
            "Expected an error but got: {:?}",
            result.unwrap()
        );

        Ok(())
    }



    #[test]
    fn test_op_permutation_normal() -> Result<(), RError> {
        let input = "ABA";
        let mut parser = op_permutation(tag("A"), tag("B"));
        let result = parser.parse(input);
        let (remaining, output) = result?;
        assert_eq!(remaining, "A");
        assert_eq!(output, (Some("A"), Some("B")));

        Ok(())
    }

    #[test]
    fn test_op_permutation_reverse() -> Result<(), RError> {
        let input = "BAA";
        let mut parser = op_permutation(tag("A"), tag("B"));
        let (remaining, output) = parser.parse(input)?;
        assert_eq!(remaining, "A");
        assert_eq!(output, (Some("A"), Some("B")));
        Ok(())
    }

    #[test]
    fn test_op_permutation_missing() -> Result<(), RError> {
        let input = "CD";
        let mut parser = op_permutation(tag::<&str, &str, RError>("A"), tag("B"));
        let result = parser.parse(input);
        let (remaining, output) = result?;
        assert_eq!(remaining, "CD");
        assert_eq!(output, (None, None));

        Ok(())
    }

    #[test]
    fn test_op_permutation_partial_missing_reverse() -> Result<(), RError> {
        let input = "BC";
        let mut parser = op_permutation(tag("A"), tag("B"));
        let result = parser.parse(input);
        let (remaining, output) = result?;
        assert_eq!(remaining, "C");
        assert_eq!(output, (None, Some("B")));

        Ok(())
    }

    #[test]
    fn test_op_permutation_partial_missing() -> Result<(), RError> {
        let input = "AC";
        let mut parser = op_permutation(tag("A"), tag("B"));
        let result = parser.parse(input);
        let (remaining, output) = result?;
        assert_eq!(remaining, "C");
        assert_eq!(output, (Some("A"), None));

        Ok(())
    }

    #[test]
    fn test_parse_mixed_param() -> Result<(), RError> {
        let input = "param1 \"param 2\" ! This is an inline comment\n! This is a block comment\n param3 ! Inline comment\nparam4";
        let mut parser = many1(
            preceded(
                multispace0,
                parse_mixed_param(
                    take_till(|c| c == '\0'),
                    None,
                ),
            )
        );
        let (remaining, input) = parser.parse(input)?;
        assert!(remaining.is_empty(), "Expected no remaining input: {}", remaining);

        assert_eq!(input.len(), 4, "Expected 4 parameters, got: {}", input.len());
        assert_eq!(input[0].value, "param1");
        assert_eq!(input[1].value, "param 2");
        assert_eq!(input[2].value, "param3");
        assert_eq!(input[3].value, "param4");

        Ok(())
    }
}
