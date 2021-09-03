//! Parsing the Adobe OpenType Feature File format.

mod parse;

pub use parse::grammar::root;
pub use parse::util;
pub use parse::{DebugSink, Kind, Parser};
