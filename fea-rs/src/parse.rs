//! Convert raw tokens into semantic events

mod context;
pub mod grammar;
mod lexer;
mod parser;
mod source;
mod token;
mod token_set;
mod tree;

use std::path::PathBuf;

pub use context::{parse_src, HardError, IncludeStatement, ParseContext};
pub use parser::{Parser, SyntaxError};
pub use source::{FileId, Source};
pub use token::Kind;
pub use token_set::TokenSet;
pub use tree::ParseTree;

pub(crate) use parser::{TokenComparable, TreeSink};
pub(crate) use source::{SourceList, SourceMap};
pub(crate) use token::Token;

use crate::GlyphMap;

/// Attempt to parse a feature file at a given path, including its imports.
pub fn parse_root_file(
    path: impl Into<PathBuf>,
    glyph_map: Option<&GlyphMap>,
    project_root: Option<PathBuf>,
) -> Result<ParseContext, HardError> {
    ParseContext::parse_from_root(path.into(), glyph_map, project_root)
}
