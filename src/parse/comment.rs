use crate::error::RError;
use crate::parse::trim;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::character::complete::not_line_ending;
use nom::character::none_of;
use nom::combinator::{complete, map, opt, recognize};
use nom::multi::{many0, many1};
use nom::sequence::delimited;
use nom::{AsChar, Compare, FindToken, IResult, Input, Offset, OutputMode, PResult, Parser};
use std::fmt::{Debug, Display};


/// Parse a comment that start with the given deliminator (e.g. `*` or `!`)  and end with a newline or `eof`.
///
/// Return the comment as a trimmed string.
pub fn parse_comment_with<'a, I, E>(
    deliminator: impl Parser<I, Output = I, Error = E>,
) -> impl Parser<I, Output = I, Error = E>
where
    I: Input + Compare<&'static str> + nom::Offset,
    <I as Input>::Item: AsChar,
    E: nom::error::ParseError<I>, <I as Input>::Iter: DoubleEndedIterator
{
    trim(delimited(
        (multispace0, deliminator),
        recognize(not_line_ending),
        multispace0,
    ))
}

/// Handle the parsing of comments in a row. Return a trimmed tuple ` (Content, Comment)`.
///
/// Example Input:
/// ```txt
/// Some Value\t ! This is a comment!!
/// ```
/// Example Output:
/// ```rs
/// ("", ("Some Value", "This is a comment!!"))
/// ```
pub fn parse_commented_row<'a, I, E>(input: I) -> IResult<I, (I, Option<I>), E>
where
    I: Input + nom::Offset + nom::Compare<&'static str>,
    <I as Input>::Item: AsChar,
    E: nom::error::ParseError<I>,
    &'a str: FindToken<<I as Input>::Item>, <I as Input>::Iter: DoubleEndedIterator
{
    (
        trim(complete(recognize(many0(none_of::<I, &str, E>("!\n\r"))))),
        opt(parse_comment_with(tag("!"))),
    )
        .parse(input)
        .map_err(|e| e.map(|e| e.into()))
}

/// Parse block comments that start with the given deliminator `*` or `!`  and end with a newline.
///
/// Return a list of trimmed strings.
pub fn parse_block_comment<I, E>(input: I) -> IResult<I, Option<Vec<I>>, E>
where
    I: Input + Compare<&'static str> + Offset + Display,
    <I as Input>::Item: AsChar,
    E: nom::error::ParseError<I>,
    <I as Input>::Iter: DoubleEndedIterator
{
    parse_block_comment_with(alt((tag("*"), tag("!")))).parse(input)
}

/// Parse block comments that start with the given deliminator `*` or `!`  and end with a newline.
///
/// Return a list of trimmed strings.
pub fn parse_block_comment_with<'a, I, E>(
    deliminator: impl Parser<I, Output = I, Error = E>,
) -> impl Parser<I, Output = Option<Vec<I>>, Error = E>
where
    I: Input + Compare<&'static str> + nom::Offset,
    <I as Input>::Item: AsChar,
    E: nom::error::ParseError<I>, 
    <I as Input>::Iter: DoubleEndedIterator
{
    opt(many1(trim(parse_comment_with(deliminator))))
}

// Split a comment into two parts: the part before the splitter and the part after.
#[derive(Debug, Clone)]
pub struct CommentSplitter {
    delimiter: char,
    min_length: usize,
}

impl CommentSplitter {
    pub fn new(delimiter: char, min_length: usize) -> Self {
        Self {
            delimiter,
            min_length,
        }
    }


    /// Split a raw comment block into multiple lines based on the delimiter.
    /// The raw comment should begin with a `*` or `!` in each line.
    pub fn split_raw_str<'a>(
        &self,
        input: &'a str,
    ) -> IResult<&'a str, Option<Vec<Vec<&'a str>>>, RError> {
        map(parse_block_comment, |lines| {
            lines.map(|arr| self.split_lines(arr.as_slice()))
        })
        .parse(input)
    }

    /// Group lines based on the delimiter.
    /// Input should be a slice of parsed lines, WITHOUT the leading `*` or `!`.
    pub fn split_lines<'a, 'b>(&self, lines: &'b [&'a str]) -> Vec<Vec<&'a str>> {
        let mut out: Vec<Vec<&'a str>> = Vec::new();
        let mut current: Vec<&'a str> = Vec::new();
        let delim = self.delimiter;
        for &line in lines {
            // skip start
            let line = line.trim();
            if line.chars().all(|c| c == delim) && line.len() >= self.min_length {
                if !current.is_empty() {
                    out.push(std::mem::take(&mut current));
                }
            } else {
                current.push(line);
            }
        }
        if !current.is_empty() {
            out.push(current);
        }
        out
    }
}

impl<'a> Parser<&'a str> for CommentSplitter {
    type Output = Option<Vec<Vec<&'a str>>>;
    type Error = RError;

    fn process<OM: OutputMode>(
        &mut self,
        input: &'a str,
    ) -> PResult<OM, &'a str, Self::Output, Self::Error> {
        let mut f = |i| self.split_raw_str(i);
        f.process::<OM>(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_parse_comment_with() -> Result<(), RError> {
        let input = "* This is a comment\n";
        let (rest, comment) = parse_comment_with(tag("*")).parse(input)?;

        assert_eq!(rest, "");
        assert_eq!(comment, "This is a comment");
        Ok(())
    }

    #[test]
    fn test_parse_block_comment_with() -> Result<(), RError> {
        let input = "* This is a comment\n* Another comment\n";
        let (rest, comments) = parse_block_comment_with(tag("*")).parse(input)?;

        assert_eq!(rest, "", "Comments: {:?}", comments);
        assert_eq!(comments, Some(vec!["This is a comment", "Another comment"]));
        Ok(())
    }

    #[test]
    fn test_inline_comment_parser_1() {
        let input = "Some Value\t ! This is a comment!!";
        let result = parse_commented_row::<_, RError>(input);

        let (rest, (content, comment)) = result.unwrap();
        assert_eq!(rest, "");
        assert_eq!(content, "Some Value");
        assert_eq!(comment, Some("This is a comment!!"));
    }

    #[test]
    fn test_inline_comment_parser_2() {
        let input = "Some\t Value\t!! This is a comment\n";
        let result = parse_commented_row::<_, RError>(input);

        let (rest, (content, comment)) = result.unwrap();
        assert_eq!(rest, "");
        assert_eq!(content, "Some\t Value");
        assert_eq!(comment, Some("! This is a comment"));
    }

    #[test]
    fn test_inline_comment_parser_no_comment() {
        let input = "Some Value";
        let result = parse_commented_row::<_, RError>(input);

        let (rest, (content, comment)) = result.unwrap();
        assert_eq!(rest, "");
        assert_eq!(content, "Some Value");
        assert_eq!(comment, None);
    }

    #[test]
    fn test_block_comment_parser_normal() {
        let input = "* * This is a comment  \n \t* Another comment\t";
        let result = parse_block_comment::<&str, RError>(input);

        let (remaining, comments) = result.unwrap();
        assert_eq!(remaining, "");
        assert_eq!(
            comments,
            Some(vec!["* This is a comment", "Another comment"])
        );
    }

    #[test]
    fn test_block_comment_parser_empty_row() -> Result<(), RError> {
        let input = "*- This is a comment  \n\n* Another comment\t\n";
        let (remaining, comments) = parse_block_comment::<&str, RError>(input)?;

        assert_eq!(remaining, "");
        assert_eq!(
            comments,
            Some(vec!["- This is a comment", "Another comment"])
        );
        Ok(())
    }

    #[test]
    fn test_block_comment_splitter_raw() {
        let inputs = vec!["Line 1", "- Line 2", "- Line 3"];
        let splitter = CommentSplitter::new('-', 10);
        let success_separators = vec![
            "\t".repeat(3) + &"-".repeat(10) + &" ".repeat(5),
            "-".repeat(15),
        ];

        let failure_separators = vec![
            "-".repeat(5),
            "-".repeat(8),
            "?".to_string() + &"-".repeat(8),
        ];

        for sep in success_separators {
            let sep = format!("\n*{}\n", sep);
            let input = inputs
                .iter()
                .map(|v| "*".to_string() + v)
                .collect::<Vec<_>>()
                .join(&sep);

            let result = splitter.split_raw_str(&input);

            let err_msg = format!("Failed to parse with separator: {}", sep);

            let (remaining, comments) = result.unwrap();
            let comments = comments.expect("Failed to parse with separator");
            assert_eq!(remaining, "", "{}. Raw:\n{}", &err_msg, &input);
            assert_eq!(comments.len(), inputs.len(), "{}", &err_msg);
            for (i, comment) in comments.iter().enumerate() {
                assert_eq!(comment.len(), 1, "{}", &err_msg);
                assert_eq!(comment[0], inputs[i], "{}", &err_msg);
            }
        }

        for sep in failure_separators {
            let sep = format!("\n*{}\n", sep);
            let input = inputs
                .iter()
                .map(|v| "*".to_string() + v)
                .collect::<Vec<_>>()
                .join(&sep);
            let result = splitter.split_raw_str(&input);
            let (remaining, comments) = result.unwrap();
            let comments = comments.expect("Failed to parse with separator");
            assert_eq!(remaining, "", "Failed to parse with separator: {}", sep);
            assert_eq!(comments.len(), 1, "Failed to parse with separator: {}", sep);
            assert_eq!(
                comments[0].len(),
                inputs.len() * 2 - 1,
                "Failed to parse with separator: {}",
                sep
            );
        }
    }

    #[test]
    fn test_block_comment_parser_split_multiline() {
        let inputs = vec!["Line 1", "- Line 2", "- Line 3"];
        let splitter = CommentSplitter::new('-', 10);
        let success_separators = vec![
            "\t".repeat(3) + &"-".repeat(10) + &" ".repeat(5),
            "-".repeat(15),
        ];

        let failure_separators = vec![
            "-".repeat(5),
            "-".repeat(8),
            "?".to_string() + &"-".repeat(8),
        ];

        for sep in success_separators {
            // add sep item between every 2 input items
            let mut modified_inputs = inputs.clone();
            for i in 0..modified_inputs.len() - 1 {
                modified_inputs.insert(i * 2 + 1, &sep);
            }

            let comments = splitter.split_lines(modified_inputs.as_slice());
            for (i, comment) in comments.iter().enumerate() {
                assert_eq!(comment.len(), 1);
                assert_eq!(comment[0], inputs[i]);
            }
        }

        for sep in failure_separators {
            // add sep item between every 2 input items
            let mut inputs = inputs.clone();
            for i in 0..inputs.len() - 1 {
                inputs.insert(i * 2 + 1, &sep);
            }

            let comments = splitter.split_lines(inputs.as_slice());
            assert_eq!(comments.len(), 1);
            assert_eq!(comments[0].len(), inputs.len());
        }
    }
    
    #[test]
    fn test_block_comment_unparsable() -> Result<(),RError>{
        let input = "* This is a comment\n* Another comment\nNEXTCOMPONENT";
        let (input, result) = parse_block_comment(input)?;
        assert_eq!(input, "NEXTCOMPONENT");
        assert_eq!(result, Some(vec!["This is a comment", "Another comment"]));
        Ok(())
    }

    #[test]
    fn test_mixed_block_comment() -> Result<(), RError> {
        let input = r#"
! This is a comment
* This is another comment
*
Some Contents
! Yet another comment
        "#;
        let (input, comments) = parse_block_comment(input)?;
        assert!(input.trim().starts_with("Some Contents"), "Remaining input: '{}'", input);
        assert_eq!(
            comments,
            Some(vec![
                "This is a comment",
                "This is another comment",
                "",
            ])
        );
        Ok(())
    }
}
