//! Library to aid in identifying reusable shapes.
//!
//! Derived from <https://github.com/googlefonts/nanoemoji>'s implementation
//! of shape reuse.
mod error;
pub use error::Error;
pub mod reuse;
