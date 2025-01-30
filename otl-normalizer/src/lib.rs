//! Generating a normalized text representation for OpenType layout tables
//!
//! This currently supports a subset of GPOS (kerning and marks)

pub mod args;
mod common;
mod error;
mod gdef;
mod glyph_names;
mod gpos;
mod gsub;
mod variations;

pub use error::Error;
pub use glyph_names::NameMap;

pub use gdef::print as print_gdef;
pub use gpos::print as print_gpos;
pub use gsub::print as print_gsub;
