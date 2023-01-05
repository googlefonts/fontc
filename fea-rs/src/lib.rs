//! Parsing and compiling the Adobe OpenType Feature File format.

#![deny(missing_docs)]

mod compile;
mod diagnostic;
mod parse;
mod token_tree;
mod types;
pub mod util;

#[cfg(test)]
mod tests;

pub use compile::{compile, validate, Compilation};
pub use diagnostic::{Diagnostic, Level};
pub use parse::{parse_root_file, parse_src, FileId, ParseTree, Source, TokenSet};
pub use token_tree::{typed, Kind, Node, NodeOrToken, Token};
pub use types::{GlyphIdent, GlyphMap, GlyphName};
