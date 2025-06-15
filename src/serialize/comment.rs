use crate::ast::Commented;
use crate::error::RError;
use crate::parse::BlockKind;
use crate::serialize::DeckWrite;
use std::fmt::Write;

impl<T> DeckWrite for Commented<T>
where
    T: DeckWrite + std::fmt::Debug,
{
    fn write_to<W: Write>(&self, writer: &mut W, kind: BlockKind) -> Result<(), RError> {
        // Write pre-comments if they exist
        if let Some(pre) = &self.comments.comment_pre {
            for line in pre {
                if !line.is_empty() {
                    writeln!(writer, "! {}\n", line.trim())?;
                }
            }
        }
        // Write the main value
        self.value.write_to(writer, kind)?;
        // Write inline comment if it exists
        if let Some(inline) = &self.comments.comment_inline {
            if !inline.is_empty() {
                write!(writer, "\t! {}\n", inline.trim())?;
            }
        }
        // Write post-comments if they exist
        if let Some(post) = &self.comments.comment_post {
            for line in post {
                if !line.is_empty() {
                    writeln!(writer, "! {}\n", line.trim())?;
                }
            }
        }
        Ok(())
    }
}

impl DeckWrite for String {
    fn write_to<W: Write>(&self, writer: &mut W, kind: BlockKind) -> Result<(), RError> {
        // quote the string if it contains spaces or special characters
        if self.contains(' ') || self.contains('\t') {
            write!(writer, "\"{}\"", self.replace('"', "\\\""))?;
        } else {
            write!(writer, "{}", self)?;
        }
        Ok(())
    }
}
