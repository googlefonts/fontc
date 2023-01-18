//! Parsing and compiling the Adobe OpenType Feature File format.

#![deny(missing_docs)]

pub mod compile;
mod diagnostic;
pub mod parse;
mod token_tree;
mod types;
pub mod util;

#[cfg(test)]
mod tests;

pub use compile::Compilation;
pub use diagnostic::{Diagnostic, Level};
pub use parse::{ParseTree, TokenSet};
pub use token_tree::{typed, Kind, Node, NodeOrToken, Token};
pub use types::{GlyphIdent, GlyphMap, GlyphName};
