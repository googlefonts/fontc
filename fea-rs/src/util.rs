//! helpers and utilties (mostly for testing/debugging?)

pub(crate) mod highlighting;
pub mod paths;
#[cfg(any(test, feature = "diff"))]
pub mod pretty_diff;
#[cfg(any(test, feature = "test"))]
pub mod ttx;

pub use highlighting::style_for_kind;
#[cfg(any(test, feature = "diff"))]
pub use pretty_diff::write_line_diff;

#[doc(hidden)]
pub static SPACES: &str = "                                                                                                                                                                                    ";
#[cfg(any(test, feature = "test"))]
pub(crate) static WRITE_RESULTS_VAR: &str = "FEA_WRITE_TEST_OUTPUT";
#[cfg(any(test, feature = "test"))]
pub(crate) static VERBOSE: &str = "FEA_VERBOSE";

// pass a comma separated list of words, tests which contain those words are run
#[cfg(any(test, feature = "test"))]
pub(crate) static FEA_FILTER_TESTS: &str = "FEA_FILTER_TESTS";
