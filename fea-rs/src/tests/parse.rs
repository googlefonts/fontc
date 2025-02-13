//! Test parser output against expected results.
//!
//! This generates textual representations of the parse tree, which are compared
//! against saved versions.
//!
//! To regenerate the comparison files, pass FEA_WRITE_TEST_OUTPUT=1 as an
//! environment variable.

use std::{env, path::PathBuf};

use crate::{
    util::ttx::{self as test_utils, Filter, Report, TestCase, TestResult},
    GlyphIdent, GlyphMap,
};

static PARSE_GOOD: &str = "./test-data/parse-tests/good";
static PARSE_BAD: &str = "./test-data/parse-tests/bad";
static OTHER_TESTS: &[&str] = &["./test-data/include-resolution-tests/dir1/test1.fea"];
const GOOD_OUTPUT_EXTENSION: &str = "PARSE_TREE";
const BAD_OUTPUT_EXTENSION: &str = "ERR";
const GLYPH_ORDER_PATH: &str = "./test-data/simple_glyph_order.txt";

#[test]
fn parse_good() -> Result<(), Report> {
    assert!(
        std::path::Path::new(PARSE_GOOD).exists(),
        "test data is missing. Do you need to update submodules? cwd: '{:?}'",
        env::current_dir()
    );

    let glyph_map = parse_test_glyph_order();

    let results = test_utils::iter_fea_files(PARSE_GOOD, Filter::from_env())
        .chain(OTHER_TESTS.iter().map(PathBuf::from))
        .map(|path| run_good_test(path, &glyph_map))
        .collect::<Vec<_>>();
    test_utils::finalize_results(results).into_error()
}

#[test]
fn parse_bad() -> Result<(), Report> {
    test_utils::finalize_results(
        test_utils::iter_fea_files(PARSE_BAD, Filter::from_env())
            .map(run_bad_test)
            .collect(),
    )
    .into_error()
}

fn parse_test_glyph_order() -> GlyphMap {
    let raw_glyphs = std::fs::read_to_string(GLYPH_ORDER_PATH).unwrap();
    crate::compile::parse_glyph_order(&raw_glyphs)
        .unwrap()
        .iter()
        // we add one extra glyph, which could be a glyph range.
        // during parsing, when provided a glyphmap, we can disambiguate this,
        // but we can't do that otherwise
        .chain([GlyphIdent::from("two-four")])
        .collect()
}

fn run_good_test(path: PathBuf, glyphmap: &GlyphMap) -> Result<PathBuf, TestCase> {
    let verbose = std::env::var(crate::util::VERBOSE).is_ok();
    match std::panic::catch_unwind(|| match test_utils::try_parse_file(&path, Some(glyphmap)) {
        Err((_, errs)) => Err(TestCase {
            path: path.clone(),
            reason: TestResult::ParseFail(errs.to_string(false)),
        }),
        Ok(node) => {
            let output = node.root().simple_parse_tree();
            let result =
                test_utils::compare_to_expected_output(&output, &path, GOOD_OUTPUT_EXTENSION);
            if result.is_err() {
                if std::env::var(crate::util::WRITE_RESULTS_VAR).is_ok() {
                    let to_write = node.root().simple_parse_tree();
                    let to_path = path.with_extension(GOOD_OUTPUT_EXTENSION);
                    std::fs::write(to_path, to_write).expect("failed to write output");
                }
                if verbose {
                    eprintln!("{}", node.root().simple_parse_tree());
                }
            }
            result
        }
    }) {
        Err(_) => Err(TestCase {
            path,
            reason: TestResult::Panic,
        }),
        Ok(Err(e)) => Err(e),
        Ok(_) => Ok(path),
    }
}

fn run_bad_test(path: PathBuf) -> Result<PathBuf, TestCase> {
    match std::panic::catch_unwind(|| match test_utils::try_parse_file(&path, None) {
        Err((_, errs)) => {
            let msg = errs.to_string(false);
            let result = test_utils::compare_to_expected_output(&msg, &path, BAD_OUTPUT_EXTENSION);
            if result.is_err() && std::env::var(crate::util::WRITE_RESULTS_VAR).is_ok() {
                let to_path = path.with_extension(BAD_OUTPUT_EXTENSION);
                std::fs::write(to_path, &msg).expect("failed to write output");
            }
            result
        }
        Ok(_) => Err(TestCase {
            path: path.clone(),
            reason: TestResult::UnexpectedSuccess,
        }),
    }) {
        Err(_) => Err(TestCase {
            path,
            reason: TestResult::Panic,
        }),
        Ok(Err(e)) => Err(e),
        Ok(_) => Ok(path),
    }
}
