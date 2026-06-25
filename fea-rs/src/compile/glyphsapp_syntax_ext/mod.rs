//! Handling glyphsapp syntax extensions.
//!
//! Glyphs.app sources can use `$`-prefixed tokens in their FEA that are not
//! standard FEA, and which we resolve at compile time:
//!
//! - [`number_value`]: `$name` / `${...}` numeric values, resolved to (variable)
//!   metrics.
//! - [`predicate`]: `$[...]` glyph predicates, resolved to glyph classes
//!   (<https://github.com/googlefonts/fontc/issues/92>).

pub(crate) mod number_value;
pub(crate) mod predicate;

pub(crate) use number_value::{ResolvedValue, resolve_glyphs_app_expr};
pub(crate) use predicate::Predicate;
