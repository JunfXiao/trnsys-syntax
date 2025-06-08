use std::cell::RefCell;
pub mod ast;
pub mod error;
pub mod parse;

use crate::ast::DocContext;
use crate::error::{ParseResult, RError};
use crate::parse::parse_commented_block;
use error_stack::Report;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;

/// High-level representation of a TRNSYS deck file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrnsysFile {
    /// The parsed Abstract Syntax Tree
    pub ctx: DocContext,
    /// The file path if loaded from the disk
    pub path: Option<PathBuf>,
}

impl TrnsysFile {
    /// Parse a TRNSYS deck file from a string
    pub fn parse(mut input: &str) -> Result<Self, RError> {
        let mut parse_context = DocContext::new();

        while !input.trim().is_empty() {
            let context_ref = &mut parse_context;
            let (remaining, block) =
                parse_commented_block((input, context_ref)).map_err(|e| RError::from(e))?;
            context_ref.prev_blocks.push(Rc::new(RefCell::new(block)));
            input = remaining;
        }

        Ok(Self {
            ctx: parse_context,
            path: None,
        })
    }

    /// Load a TRNSYS deck file from disk
    pub fn load<P: AsRef<Path>>(path: P) -> ParseResult<Self> {
        let content = fs::read_to_string(&path).map_err(|io_err| Report::new(io_err.into()))?;
        let mut ctx = Self::parse(&content)?;
        ctx.path = Some(path.as_ref().to_path_buf());
        Ok(ctx)
    }

    /// Save the TRNSYS deck file to disk
    pub fn save<P: AsRef<Path>>(&self, path: P) -> ParseResult<()> {
        todo!();
        // let content = self.ast.to_string();
        // let path_str = path.as_ref().to_str().unwrap().to_string();
        // fs::write(&path, content)
        //     .map_err(|io_err| Report::new(io_err.into()))
        //     .attach_printable(format!("Cannot save file to '{}'", path_str))
    }

    /// Convert the deck file to a string
    pub fn to_string(&self) -> String {
        todo!()
        // self.ast.to_string()
    }

    // Other utility methods would go here
}
