//! Parsing the Adobe OpenType Feature File format.

mod grammar;
mod lexer;
mod parse;
mod pretty_diff;
mod token;
mod token_set;

pub use grammar::root;
pub use parse::{DebugSink, Parser};
pub use token::Kind;
