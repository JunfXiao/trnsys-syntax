use crate::ast::Commented;
use crate::error::RError;
use crate::parse::BlockKind;
use crate::serialize::DeckWrite;
use std::fmt::Write;

fn write_comment_line<W: Write>(writer: &mut W, line: &str) -> Result<(), RError> {
    if line.is_empty() {
        return Ok(());
    }
    if line.starts_with(['$', '*', '!']) {
        writeln!(writer, "*{}", line.trim())?;
    } else {
        writeln!(writer, "* {}", line.trim())?;
    }
    Ok(())
}

impl<T> DeckWrite for Commented<T>
where
    T: DeckWrite + std::fmt::Debug,
{
    fn write_to<W: Write>(&self, writer: &mut W, kind: BlockKind) -> Result<(), RError> {
        // Write pre-comments if they exist
        if let Some(pre) = &self.comments.comment_pre {
            for line in pre {
                write_comment_line(writer, line)?;
            }
        }
        // Write the main value
        self.value.write_to(writer, kind)?;

        let mut has_inline = false;

        // Write inline comment if it exists
        if let Some(inline) = &self.comments.comment_inline {
            writeln!(writer, "\t! {}", inline.trim())?;
            has_inline = true;
        }
        // Write post-comments if they exist
        if let Some(post) = &self.comments.comment_post {
            if self.comments.comment_inline.is_none() {
                writeln!(writer)?;
            }
            for line in post {
                write_comment_line(writer, line)?;
            }
        } else if !has_inline {
            // Without inline and post comments, ensure the separator is written
            write!(writer, "\t")?;
        }

        Ok(())
    }
}

impl DeckWrite for String {
    fn write_to<W: Write>(&self, writer: &mut W, _kind: BlockKind) -> Result<(), RError> {
        // quote the string if it contains spaces or special characters
        if self.contains(' ') || self.contains('\t') {
            write!(writer, "\"{}\"", self.replace('"', "\\\""))?;
        } else {
            write!(writer, "{}", self)?;
        }
        Ok(())
    }
}
