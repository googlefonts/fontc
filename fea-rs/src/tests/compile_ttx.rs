//! Run the compiler against a bunch of inputs, comparing them with
//! the results of fonttools.
//!
//! set FEA_TEST_VERBOSE=1 in the environment for verbose output.

use crate::util::ttx;
static TEST_DATA1: &str = "./test-data/fonttools-tests";

#[test]
#[ignore = "disabled so we can use CI"]
fn all_compile_tests() -> Result<(), ttx::Report> {
    ttx::assert_has_ttx_executable();
    ttx::run_all_tests(TEST_DATA1, None).into_error()
}
