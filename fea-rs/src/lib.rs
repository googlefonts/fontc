//! Parsing the Adobe OpenType Feature File format.

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
pub use parse::grammar::root;
pub use parse::{Kind, SyntaxError, TokenSet};
pub use token_tree::{typed, Node, NodeOrToken};
pub use types::{GlyphIdent, GlyphMap, GlyphName};
