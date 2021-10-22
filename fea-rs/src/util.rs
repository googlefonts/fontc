//! helpers and utilties (mostly for testing/debugging?)

pub mod debug;
mod highlighting;
pub mod pretty_diff;

pub use highlighting::{stringify_errors, style_for_kind};
pub use pretty_diff::write_line_diff;

#[doc(hidden)]
pub static SPACES: &str = "                                                                                                                                                                                    ";
