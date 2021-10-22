//! helpers and utilties (mostly for testing/debugging?)

pub mod debug;
mod highlighting;
#[cfg(test)]
pub mod pretty_diff;

pub use highlighting::{stringify_errors, style_for_kind};
#[cfg(test)]
pub use pretty_diff::write_line_diff;

#[doc(hidden)]
pub static SPACES: &str = "                                                                                                                                                                                    ";
