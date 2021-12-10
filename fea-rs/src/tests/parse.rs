//! Test parser output against expected results.
//!
//! This generates textual representations of the parse tree, which are compared
//! against saved versions.
//!
//! To regenerate the comparison files, pass FEA_WRITE_TEST_OUTPUT=1 as an
//! environment variable.

use std::{
    env,
    ffi::OsStr,
    path::{Path, PathBuf},
};

use crate::util::ttx::{self as test_utils, Failure, Reason, Results};
use crate::{Diagnostic, ParseTree};

static PARSE_GOOD: &str = "./test-data/parse-tests/good";
static PARSE_BAD: &str = "./test-data/parse-tests/bad";
static OTHER_TESTS: &[&str] = &["./test-data/include-resolution-tests/dir1/test1.fea"];
const WRITE_RESULTS_VAR: &str = "FEA_WRITE_TEST_OUTPUT";
const GOOD_OUTPUT_EXTENSION: &str = "PARSE_TREE";
const BAD_OUTPUT_EXTENSION: &str = "ERR";

#[test]
fn parse_good() -> Result<(), Results> {
    assert!(
        std::path::Path::new(PARSE_GOOD).exists(),
        "test data is missing. Do you need to update submodules? cwd: '{:?}'",
        env::current_dir()
    );

    let results = iter_fea_files(PARSE_GOOD)
        .chain(OTHER_TESTS.iter().map(PathBuf::from))
        .map(run_good_test)
        .collect::<Vec<_>>();
    test_utils::finalize_results(results)
}

#[test]
fn parse_bad() -> Result<(), Results> {
    test_utils::finalize_results(iter_fea_files(PARSE_BAD).map(run_bad_test).collect())
}

fn iter_fea_files(path: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
    let mut dir = path.as_ref().read_dir().unwrap();
    std::iter::from_fn(move || loop {
        let entry = dir.next()?.unwrap();
        let path = entry.path();
        if path.extension() == Some(OsStr::new("fea")) {
            return Some(path);
        }
    })
}

fn run_good_test(path: PathBuf) -> Result<PathBuf, Failure> {
    match std::panic::catch_unwind(|| match try_parse_file(&path) {
        Err((node, errs)) => Err(Failure {
            path: path.clone(),
            reason: Reason::ParseFail(test_utils::stringify_diagnostics(&node, &errs)),
        }),
        Ok(node) => {
            let output = node.root().simple_parse_tree();
            let result = compare_to_expected_output(&output, &path, GOOD_OUTPUT_EXTENSION);
            if result.is_err() && std::env::var(WRITE_RESULTS_VAR).is_ok() {
                let to_write = node.root().simple_parse_tree();
                let to_path = path.with_extension(GOOD_OUTPUT_EXTENSION);
                std::fs::write(&to_path, &to_write).expect("failed to write output");
            }
            result
        }
    }) {
        Err(_) => Err(Failure {
            path,
            reason: Reason::Panic,
        }),
        Ok(Err(e)) => Err(e),
        Ok(_) => Ok(path),
    }
}

fn run_bad_test(path: PathBuf) -> Result<PathBuf, Failure> {
    match std::panic::catch_unwind(|| match try_parse_file(&path) {
        Err((node, errs)) => {
            let msg = test_utils::stringify_diagnostics(&node, &errs);
            let result = compare_to_expected_output(&msg, &path, BAD_OUTPUT_EXTENSION);
            if result.is_err() && std::env::var(WRITE_RESULTS_VAR).is_ok() {
                let to_path = path.with_extension(BAD_OUTPUT_EXTENSION);
                std::fs::write(&to_path, &msg).expect("failed to write output");
            }
            result
        }
        Ok(_) => Err(Failure {
            path: path.clone(),
            reason: Reason::ParseFail("unexpected success".into()),
        }),
    }) {
        Err(_) => Err(Failure {
            path,
            reason: Reason::Panic,
        }),
        Ok(Err(e)) => Err(e),
        Ok(_) => Ok(path),
    }
}

/// returns the tree and any errors
fn try_parse_file(path: &Path) -> Result<ParseTree, (ParseTree, Vec<Diagnostic>)> {
    let ctx = crate::parse_root_file(path, None, None).unwrap();
    let (tree, errors) = ctx.generate_parse_tree();
    if errors.iter().any(Diagnostic::is_error) {
        Err((tree, errors))
    } else {
        Ok(tree)
    }
}

fn compare_to_expected_output(output: &str, src_path: &Path, cmp_ext: &str) -> Result<(), Failure> {
    let cmp_path = src_path.with_extension(cmp_ext);
    let expected = if cmp_path.exists() {
        std::fs::read_to_string(&cmp_path).expect("failed to read cmp_path")
    } else {
        String::new()
    };

    if expected != output {
        let diff_percent = test_utils::compute_diff_percentage(&expected, output);
        return Err(Failure {
            path: src_path.to_owned(),
            reason: Reason::CompareFail {
                expected,
                result: output.to_string(),
                diff_percent,
            },
        });
    }
    Ok(())
}
