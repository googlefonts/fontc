//! Parsing and compiling the Adobe OpenType Feature File format.

#![deny(missing_docs)]

mod common;
pub mod compile;
mod diagnostic;
pub mod parse;
mod token_tree;
pub mod util;

#[cfg(test)]
mod tests;

pub use common::{GlyphIdent, GlyphMap, GlyphName};
pub use compile::Compiler;
pub use diagnostic::{Diagnostic, Level};
pub use parse::{ParseTree, TokenSet};
pub use token_tree::{typed, Kind, Node, NodeOrToken, Token};
