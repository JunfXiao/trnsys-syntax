use super::{map_report, parse_block_comment, parse_inline_values_strict};
use crate::ast::{Commented, Comments};
use crate::error::{ContentError, Error, RError, StdError};
use crate::parse::{
    BlockKind, StrRef, TryFromStr, parse_any_block_kind, parse_commented_row, parse_inline_values,
};
use derive_more::Display;
use error_stack::{Context, Report};
use nom::character::complete::multispace0;
use nom::combinator::complete;
use nom::{AsChar, FindToken, IResult, Input, Parser};
use std::borrow::Cow;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone, Display)]
#[display("RawHeader({block_kind:?})[{items:?}]")]
/// A simple one-line block
pub struct RawHeader<'a, K = BlockKind> {
    pub block_kind: K,
    pub items: Vec<StrRef<'a>>,
}

impl<'a, K> RawHeader<'a, K>
where
    K: Display,
{
    pub fn ensure_len(&self, len: usize) -> Result<(), RError> {
        if self.items.len() != len {
            return Err(RError::new(ContentError::ArgumentCount {
                expected: len.to_string(),
                actual: self.items.len(),
                part: format!("{} Header", self.block_kind),
            }));
        }
        Ok(())
    }

    pub fn ensure_len_between(&self, min: usize, max: usize) -> Result<(), RError> {
        if self.items.len() < min || self.items.len() > max {
            return Err(RError::new(ContentError::ArgumentCount {
                expected: format!("{} to {}", min, max),
                actual: self.items.len(),
                part: format!("{} Header", self.block_kind),
            }));
        }
        Ok(())
    }

    pub fn map_into<U, F, E>(self, mut f: F) -> Result<Vec<U>, RError>
    where
        E: std::error::Error + Context,
        F: FnMut(StrRef<'a>) -> Result<U, E>,
    {
        let items = self
            .items
            .into_iter()
            .map(move |v| {
                let v_str = format!("{:?}", v);
                f(v).map_err(|e| {
                    Report::new(e).change_context(Error::ConversionError {
                        input: v_str,
                        target: std::any::type_name::<U>().to_string(),
                    })
                })
            })
            .collect::<Result<Vec<U>, _>>()?;

        Ok(items)
    }

    pub fn map<U, F, E>(&self, mut f: F) -> Result<Vec<U>, RError>
    where
        E: std::error::Error + Context,
        F: FnMut(&StrRef<'a>) -> Result<U, E>,
    {
        let items = self
            .items
            .iter()
            .map(move |v| {
                let v_str = format!("{:?}", v);
                f(v).map_err(|e| {
                    Report::new(e).change_context(Error::ConversionError {
                        input: v_str,
                        target: std::any::type_name::<U>().to_string(),
                    })
                })
            })
            .collect::<Result<Vec<U>, _>>()?;

        Ok(items)
    }

    /// Convert the items in the header to a vector of type `T`.
    pub fn to_vec<T>(&self) -> Result<Vec<T>, RError>
    where
        T: TryFromStr<'a>,
        <T as TryFromStr<'a>>::Error: std::error::Error + Context,
    {
        self.map(|v| T::try_from_str(v))
    }

    /// Transform the items in the header using a function `f`.
    pub fn transform<F, T>(&self, f: F) -> Result<Vec<T>, RError>
    where
        F: Fn(&str) -> Result<T, RError>,
    {
        self.map(|v| f(v.as_ref()))
    }
}

impl<'a, T> TryFrom<RawHeader<'a>> for Vec<T>
where
    T: TryFromStr<'a>,
    <T as TryFromStr<'a>>::Error: std::error::Error + Context,
{
    type Error = RError;

    fn try_from(value: RawHeader<'a>) -> Result<Self, Self::Error> {
        let items = value.map_into(|v| T::try_from_str(&v))?;
        Ok(items)
    }
}

/// Parse a simple one-line header WITHOUT its Keyword.
///
pub fn parse_header(input: &str) -> IResult<&str, Commented<RawHeader>, RError> {
    map_report(
        parse_header_of_kind(Some(parse_any_block_kind), None),
        |e| e.attach_printable("Invalid block header."),
    )
    .parse(input)
}

/// Parse a simple one-line header with its Keyword.
///
/// Pre Comments and Post Comments are allowed and parsed.
pub fn parse_header_of_kind<'a, I, P, K, E>(
    mut target: Option<P>,
    param_length: Option<usize>,
) -> impl Parser<I, Output = Commented<RawHeader<'a, K>>, Error = RError>
where
    I: Input + std::fmt::Display + nom::Compare<&'static str> + nom::Offset,
    <I as Input>::Item: AsChar,
    &'a str: From<I>,
    for<'b> &'b str: FindToken<<I as Input>::Item>,
    K: PartialEq<K> + Display + TryFrom<I> + std::fmt::Debug,
    <K as TryFrom<I>>::Error: StdError + Context,
    P: Parser<I, Output = K, Error = E>,
    E: nom::error::ParseError<I>,
    nom::Err<RError>: From<nom::Err<E>>,
    Cow<'a, str>: From<I>,
    <I as Input>::Iter: DoubleEndedIterator,
{
    move |input: I| {
        let (input, pre_comments) = complete(parse_block_comment).parse(input)?;

        let (input, (row_content, comment)) = complete(parse_commented_row).parse(input)?;

        let (items_str, block_kind) = if let Some(p) = &mut target {
            p.parse(row_content)?
        } else {
            parse_any_block_kind.parse(row_content)?
        };

        let (_, items) = if let Some(n) = param_length {
            parse_inline_values_strict(n).parse(items_str)
        } else {
            parse_inline_values(None).parse(items_str)
        }?;

        let converted_items = items
            .into_iter()
            .map(|i| StrRef(i.into()))
            .collect::<Vec<_>>();

        let (input, _) = multispace0.parse(input)?;

        let (input, post_comments) = parse_block_comment(input)?;

        Ok((
            input,
            Commented::new(
                RawHeader {
                    block_kind,
                    items: converted_items,
                },
                Comments {
                    comment_pre: pre_comments
                        .map(|v| v.into_iter().map(|i| i.to_string()).collect()),
                    comment_inline: comment.map(|s| s.to_string()),
                    comment_post: post_comments
                        .map(|v| v.into_iter().map(|i| i.to_string()).collect()),
                },
            ),
        ))
        // Ok((remaining, header))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::SIMPLE_BLOCK_TAGS;
    use std::str::FromStr;

    #[test]
    fn test_simple_blocks() {
        let tag_rows = SIMPLE_BLOCK_TAGS
            .iter()
            .map(|tag| (tag, format!("{} a \"b c\" c", tag)))
            .collect::<Vec<_>>();

        for (tag, row) in tag_rows {
            let result = parse_header(row.as_str());
            assert!(
                result.is_ok(),
                "Failed to parse `{}\n{:?}`:",
                row,
                result.err().unwrap()
            );
            let (remaining, block) = result.unwrap();
            assert_eq!(remaining, "");
            assert_eq!(block.block_kind, BlockKind::from_str(tag).unwrap());
            assert_eq!(block.items.len(), 3);
            assert_eq!(block.items[0], "a");
            assert_eq!(block.items[1], "b c");
            assert_eq!(block.items[2], "c");
        }
    }

    #[test]
    /// Test the int parser with a simple block
    fn test_simple_block_parse() {
        let (remaining, block) = parse_header("TIME_REPORT 1 -5 0").expect("Failed to parse");

        block
            .ensure_len(3)
            .expect("Failed to ensure length of block items");

        let values: Vec<i32> = block.to_vec().expect("Failed to convert");

        assert!(remaining.is_empty());
        assert_eq!(block.block_kind, BlockKind::TimeReport);
        assert_eq!(values.len(), 3);
        assert_eq!(values[0], 1);
        assert_eq!(values[1], -5);
        assert_eq!(values[2], 0);
    }

    #[test]
    fn test_simple_block_parse_fail() {
        let (remaining, header) = parse_header("TIME_REPORT 1? -5 0").expect("Failed to parse");

        header
            .ensure_len(3)
            .expect("Failed to ensure length of block items");

        assert!(remaining.is_empty());
        assert_eq!(header.block_kind, BlockKind::TimeReport);

        let converted = header.to_vec::<i32>();
        assert!(
            converted.is_err(),
            "Expected error, but got: {:?}",
            converted.unwrap()
        );
    }

    #[test]
    fn test_multiple_lines() -> Result<(), RError> {
        let input = r#"
        TIME_REPORT 1 -5 0
        ! This is a comment
        ! Another comment
        ANOTHER
        "#;

        let (remaining, block) = parse_header(input)?;

        assert_eq!(remaining.trim(), "ANOTHER");
        assert_eq!(block.block_kind, BlockKind::TimeReport);
        assert_eq!(block.items.len(), 3);
        assert_eq!(block.items[0], "1");
        assert_eq!(block.items[1], "-5");
        assert_eq!(block.items[2], "0");

        Ok(())
    }
}
