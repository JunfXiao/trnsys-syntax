use crate::ast::Version;
use crate::error::{ContentError, Error, ErrorScope, RError, ReportWrapper};
use crate::serialize::DeckWrite;
use nom::{AsChar, FindToken, Input, Mode, OutputMode, PResult, Parser};
use std::collections::HashSet;
use std::fmt::Debug;
use std::ops::Sub;
mod block;
mod bool_wrapper;
mod comment;
mod expression;
mod raw_header;
mod str_wrapper;
mod unit;
mod util;

pub use block::*;
pub use bool_wrapper::*;
pub use comment::*;
pub use expression::*;
pub use unit::*;

pub use raw_header::*;
pub use str_wrapper::*;
pub use util::*;

use crate::ast::*;
use block_enum_derive::BlockEnum;
use derive_more::Display;
use error_stack::{Context, Report, ResultExt};
use lazy_static::lazy_static;
use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::character::complete::{line_ending, multispace1, space0};
use nom::combinator::eof;
use nom::sequence::delimited;
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use strum::IntoEnumIterator;
use strum_macros::{AsRefStr, EnumIter, EnumString};
use uuid::Uuid;

#[derive(
    Debug,
    AsRefStr,
    EnumString,
    EnumIter,
    PartialEq,
    Copy,
    Clone,
    Display,
    Eq,
    Hash,
    BlockEnum,
    Serialize,
    Deserialize,
)] // BlockEnum
#[display("{}", self.as_ref())]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
pub enum BlockKind {
    // default unknown
    #[skip_block]
    Unknown,
    // Simulation control statements
    Version,
    Simulation,
    Tolerances,
    Limits,
    NanCheck,
    OverwriteCheck,
    TimeReport,
    Constants,
    #[strum(serialize = "EQUATIONS", serialize = "EQU")]
    Equations,
    Accelerate,
    Loop,
    Dfq,
    NoCheck,
    #[strum(serialize = "EQSOLVER")]
    EqSolver,
    Solver,
    Assign,
    Designate,
    Include,
    End,
    Unit,
    Width,
    #[strum(serialize = "NOLIST")]
    NoList,
    List,
    Map,
    #[strum(serialize = "CSUMMARIZE")]
    CSummarize,
    #[strum(serialize = "ESUMMARIZE")]
    ESummarize,
    #[skip_block]
    Trace,
    #[strum(serialize = "ETRACE")]
    #[skip_block]
    ETrace,
    #[skip_block]
    Format,
}

pub trait TypedBlock {
    fn block_kind() -> BlockKind;
}

pub trait BlockParser<'a>: Sized + Debug + TypedBlock {
    /// This function is called to check the context of the block.
    /// It is called before the block is parsed.
    fn check_context(context: &DocContext) -> Result<(), RError> {
        Ok(())
    }

    /// This function is called to register the block kind in the context.
    /// It is called after the block is parsed and before it is added to the context.
    ///
    /// By default, it registers the block kind with a new UUID, so that no duplication occurs.
    fn register(&self, context: &mut DocContext) -> Result<(), RError> {
        context.register_dep(
            GlobalId::Block(Self::block_kind(), Uuid::new_v4().to_string()),
            None,
        )?;
        Ok(())
    }

    fn try_parse_block<'b>(
        input: &'a str,
        raw_header: RawHeader<'a>,
    ) -> Result<(&'a str, Commented<Self>), RError>;

    fn try_parse_block_by_header(raw_header: RawHeader<'a>) -> Result<Commented<Self>, RError> {
        let input = "";
        let raw_header_kind = raw_header.block_kind.clone();
        let (_, result) = Self::try_parse_block(input, raw_header)
            .map_err(|e| e.attach_printable(format!("When parsing block '{}'", raw_header_kind)))?;
        Ok(result)
    }
}

lazy_static! {
    static ref BLOCK_TAGS: HashSet<String> = {
        BlockKind::iter()
            .map(|block_type| block_type.as_ref().to_string())
            .collect()
    };
    static ref MULTILINE_BLOCK_TAGS: HashSet<String> = {
        vec![BlockKind::Equations, BlockKind::Constants, BlockKind::Unit]
            .iter()
            .map(|block_type| block_type.as_ref().to_string())
            .collect()
    };
    static ref SIMPLE_BLOCK_TAGS: HashSet<String> = BLOCK_TAGS.sub(&*MULTILINE_BLOCK_TAGS);
}

pub fn parse_any_block_kind<'a, I, O>(input: I) -> IResult<I, O, RError>
where
    I: Input + std::fmt::Display + nom::Compare<&'static str>,
    O: TryFrom<I>,
    <O as TryFrom<I>>::Error: std::error::Error + Context,
    &'a str: FindToken<<I as Input>::Item>,
    <I as Input>::Item: AsChar,
{
    let mut parse_type_text = delimited(
        space0,
        is_not(" \n\t*!"),
        alt((multispace1, line_ending, eof)),
    );
    let (remaining, block_type) = parse_type_text.parse(input)?;

    let keyword = block_type.to_string();

    let block_type = O::try_from(block_type).map_err(|e| {
        ReportWrapper::from(Report::new(e).change_context(Error::UnknownKeyword {
            keyword,
            scope: ErrorScope::Param,
        }))
    })?;

    Ok((remaining, block_type))
}

impl<'a, I> Parser<I> for BlockKind
where
    I: nom::Input + std::fmt::Display + Clone,
    &'a str: FindToken<<I as nom::Input>::Item>,
    <I as nom::Input>::Item: AsChar,
{
    type Output = BlockKind;
    type Error = RError;

    fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error> {
        let mut parse_type_word = delimited(space0, is_not(" \n\t*!"), multispace1);

        let (remaining, word) = parse_type_word
            .parse(input.clone())
            .map_err(|e| nom::Err::Error(OM::Error::bind(|| RError::from(e))))?;

        let word_up = word.to_string().to_uppercase();
        let parsed_kind = BlockKind::from_str(&word_up).map_err(|e| {
            nom::Err::Error(OM::Error::bind(|| {
                RError::from(Report::new(e).change_context(Error::UnknownKeyword {
                    keyword: word_up.clone(),
                    scope: ErrorScope::Param,
                }))
            }))
        })?;

        if *self == parsed_kind {
            Ok((remaining, OM::Output::bind(|| parsed_kind)))
        } else {
            Err(nom::Err::Error(OM::Error::bind(|| {
                RError::new(ContentError::InvalidValue {
                    part: parsed_kind.to_string(),
                    value: word_up,
                    reason: "Block type does not match".into(),
                })
            })))
        }
    }
}

// impl<'a, I> Parser<I> for BlockKind
// where I: Input + std::fmt::Display,
//       &'a str: FindToken<<I as Input>::Item>,
//       <I as Input>::Item: AsChar
// {
//     type Output = BlockKind;
//     type Error = RError;
//
//     fn process<OM: OutputMode>(&mut self, input: I) -> PResult<OM, I, Self::Output, Self::Error> {
//         // Parse the block type as a string, and convert it to the enum variant
//         let mut parse_type_text = delimited(space0, is_not(" \n\t*!"), multispace1);
//
//         let transform_block_type = |input: I| {
//             let (remaining, block_type) = parse_type_text.parse(input)?;
//             let mut block_type = BlockKind::from_str(&block_type.to_string().to_uppercase()).map_err(|e| {
//                 ReportWrapper::from(
//                     Report::new(e).change_context(Error::UnknownKeyword {
//                         keyword: block_type.to_string(),
//                         scope: ErrorScope::Param,
//                     })
//                 )
//             })?;
//
//             if *self == block_type {
//                 Ok((remaining, block_type))
//             } else {
//                 Err(nom::Err::Error(ContentError::InvalidValue {
//                     part: block_type.to_string(),
//                     value: block_type.to_string(),
//                     reason: "Block type does not match".to_string(),
//                 }))
//             }
//
//
//         };
//
//
//
//
//         let (remaining, output_mode) =
//             parse_type_text
//                 .and_then(|output: I|{
//                     let block_type = BlockKind::from_str(&output.to_string().to_uppercase())
//                         .map_err(|e| {
//                         ReportWrapper::from(
//                             Report::new(e).change_context(Error::UnknownKeyword {
//                                 keyword: output.to_string(),
//                                 scope: ErrorScope::Param,
//                             })
//                         )
//                     })?;
//                     Ok(block_type)
//                 })
//                 .process::<OM>(input)?
//
//
//
//
//     }
// }
