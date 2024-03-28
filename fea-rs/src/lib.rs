//! Parsing and compiling the Adobe OpenType Feature File format.
//!
//! The main entry point for this crate is the [`Compiler`] struct, which provides
//! a builder-like interface for compiliing from source.

#![deny(missing_docs, rustdoc::broken_intra_doc_links)]

mod common;
pub mod compile;
mod diagnostic;
pub mod parse;
mod token_tree;
pub mod util;

#[cfg(test)]
mod tests;

pub use common::{GlyphIdent, GlyphMap, GlyphSet};
pub use compile::{Compiler, Opts};
pub use diagnostic::{Diagnostic, DiagnosticSet, Level};
pub use parse::{ParseTree, TokenSet};
pub use token_tree::{typed, Kind, Node, NodeOrToken, Token};
