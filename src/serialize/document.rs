use crate::ast::DocContext;
use crate::error::RError;
use crate::parse::BlockKind;
use crate::serialize::DeckWrite;
use std::fmt::Write;

impl DeckWrite for DocContext {
    fn write_to<W: Write>(&self, writer: &mut W, kind: BlockKind) -> Result<(), RError> {
        // write the header
        writeln!(writer, "{}", "*".repeat(20))?;
        writeln!(
            writer,
            "TrnSys Deck File Generated By trnsys-syntax Project"
        )?;
        writeln!(
            writer,
            "Repository: https://github.com/JunfXiao/trnsys-syntax"
        )?;
        writeln!(writer, "{}\n", "*".repeat(20))?;

        // write the blocks
        for block in &self.prev_blocks {
            let block = block.borrow();
            block.write_to(writer, block.kind())?;
            writeln!(writer, "\n")?;
        }
        Ok(())
    }
}
