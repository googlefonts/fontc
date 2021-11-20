//! Convert raw tokens into semantic events

mod context;
pub mod grammar;
mod lexer;
mod parser;
mod source;
mod token;
mod token_set;

use std::path::PathBuf;

pub use context::{HardError, ParseContext, ParseTree};
pub use parser::{Parser, SyntaxError};
pub use source::FileId;
pub use token::Kind;
pub use token_set::TokenSet;

pub(crate) use parser::{TokenComparable, TreeSink};
pub(crate) use source::{SourceList, SourceMap};
pub(crate) use token::Token;

use crate::{AstSink, GlyphMap};

/// Attempt to parse a feature file at a given path, including its imports.
pub fn parse_root_file(
    path: impl Into<PathBuf>,
    glyph_map: Option<&GlyphMap>,
    project_root: Option<PathBuf>,
) -> Result<ParseContext, HardError> {
    ParseContext::generate(path.into(), glyph_map, project_root)
}

#[cfg(test)]
pub(crate) fn make_ids<const N: usize>() -> [FileId; N] {
    let one = FileId::next();
    let mut result = [one; N];
    for i in 1..N {
        result[i] = FileId::next();
    }
    result
}
