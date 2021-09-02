//! Parsing the Adobe OpenType Feature File format.

mod ast;
mod parse;

pub use ast::{AstSink, Node, NodeOrToken};
pub use parse::grammar::root;
pub use parse::util;
pub use parse::{DebugSink, Kind, Parser, SyntaxError};
