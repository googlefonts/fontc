//! Test parser output against expected results.
//!
//! This generates textual representations of the parse tree, which are compared
//! against saved versions.
//!
//! To regenerate the comparison files, pass FEA_WRITE_PARSE_TREE=1 as an
//! environment variable.

use std::{
    env,
    ffi::OsStr,
    path::{Path, PathBuf},
};

use crate::util::ttx::{self as test_utils, Failure, Reason, Results};
use crate::{Diagnostic, ParseTree};

static PARSE_TESTS: &str = "./test-data/other-parse-tests";
static OTHER_TESTS: &[&str] = &["./test-data/include-resolution-tests/dir1/test1.fea"];
const WRITE_RESULTS_VAR: &str = "FEA_WRITE_PARSE_TREE";
const EXP_OUTPUT_EXTENSION: &str = "PARSE_TREE";

#[test]
fn all_parse_tests() -> Result<(), Results> {
    assert!(
        std::path::Path::new(PARSE_TESTS).exists(),
        "test data is missing. Do you need to update submodules? cwd: '{:?}'",
        env::current_dir()
    );

    let results = iter_fea_files(PARSE_TESTS)
        .chain(OTHER_TESTS.iter().map(PathBuf::from))
        .map(run_test)
        .collect::<Vec<_>>();
    test_utils::finalize_results(results)
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

fn run_test(path: PathBuf) -> Result<PathBuf, Failure> {
    match std::panic::catch_unwind(|| match try_parse_file(&path) {
        Err((node, errs)) => Err(Failure {
            path: path.clone(),
            reason: Reason::ParseFail(test_utils::stringify_diagnostics(&node, &errs)),
        }),
        Ok(node) => {
            let result = compare_parse_output(&node, &path);
            if result.is_err() && std::env::var(WRITE_RESULTS_VAR).is_ok() {
                let to_write = node.root().simple_parse_tree();
                let to_path = path.with_extension(EXP_OUTPUT_EXTENSION);
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

fn compare_parse_output(tree: &ParseTree, path: &Path) -> Result<(), Failure> {
    let output = tree.root().simple_parse_tree();
    let cmp_path = path.with_extension(EXP_OUTPUT_EXTENSION);
    let expected = if cmp_path.exists() {
        std::fs::read_to_string(&cmp_path).expect("failed to read cmp_path")
    } else {
        String::new()
    };

    if expected != output {
        let diff_percent = test_utils::compute_diff_percentage(&expected, &output);
        return Err(Failure {
            path: path.to_owned(),
            reason: Reason::CompareFail {
                expected,
                result: output,
                diff_percent,
            },
        });
    }
    Ok(())
}
