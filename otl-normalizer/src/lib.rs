//! Generating a normalized text representation for OpenType layout tables
//!
//! This currently supports a subset of GPOS (kerning and marks), GDEF ligature
//! carets, and the glyf and gvar tables.

pub mod args;
mod common;
mod error;
mod gdef;
mod glyf;
mod glyph_names;
mod gpos;
mod gsub;
mod variations;

pub use error::Error;
pub use glyph_names::NameMap;

pub use gdef::print as print_gdef;
pub use glyf::{print_glyf, print_gvar};
pub use gpos::print as print_gpos;
pub use gsub::print as print_gsub;
