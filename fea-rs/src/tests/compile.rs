//! tests of the full compiler, including expected successes and failures

use std::path::{Path, PathBuf};

use crate::{
    compile::{self, Opts},
    util::ttx::{self as test_utils, Report, TestCase, TestResult},
    GlyphMap, GlyphName,
};

static ROOT_TEST_DIR: &str = "./test-data/compile-tests";
static GOOD_DIR: &str = "good";
static BAD_DIR: &str = "bad";
static GLYPH_ORDER: &str = "glyph_order.txt";
static BAD_OUTPUT_EXTENSION: &str = "ERR";

#[test]
fn should_fail() -> Result<(), Report> {
    let mut results = Vec::new();
    for (glyph_map, tests) in iter_test_groups(BAD_DIR) {
        results.extend(tests.into_iter().map(|path| run_bad_test(path, &glyph_map)));
    }
    test_utils::finalize_results(results).into_error()
}

#[test]
fn should_pass() -> Result<(), Report> {
    let mut results = Vec::new();
    for (glyph_map, tests) in iter_test_groups(GOOD_DIR) {
        results.extend(
            tests
                .into_iter()
                .map(|path| run_good_test(path, &glyph_map)),
        );
    }
    test_utils::finalize_results(results).into_error()
}

fn iter_test_groups(test_dir: &str) -> impl Iterator<Item = (GlyphMap, Vec<PathBuf>)> + '_ {
    iter_test_group_dirs(ROOT_TEST_DIR).map(move |dir| {
        let glyph_order_path = dir.join(GLYPH_ORDER);
        let glyph_order =
            std::fs::read_to_string(glyph_order_path).expect("failed to read glyph order");
        let glyph_map = glyph_order.lines().map(GlyphName::new).collect();
        let tests_dir = dir.join(test_dir);
        let tests = test_utils::iter_fea_files(tests_dir).collect::<Vec<_>>();
        (glyph_map, tests)
    })
}

fn iter_test_group_dirs(root_dir: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
    let mut dir = root_dir.as_ref().read_dir().unwrap();
    std::iter::from_fn(move || loop {
        let entry = dir.next()?.unwrap();
        let path = entry.path();
        if path.is_dir() {
            return Some(path);
        }
    })
}

fn run_bad_test(path: PathBuf, map: &GlyphMap) -> Result<PathBuf, TestCase> {
    match std::panic::catch_unwind(|| bad_test_body(&path, map)) {
        Err(_) => Err(TestCase {
            path,
            reason: TestResult::Panic,
        }),
        Ok(Err(e)) => Err(e),
        Ok(_) => Ok(path),
    }
}

fn bad_test_body(path: &Path, glyph_map: &GlyphMap) -> Result<(), TestCase> {
    match test_utils::try_parse_file(path, Some(glyph_map)) {
        Err((node, errs)) => {
            if std::env::var(super::VERBOSE).is_ok() {
                eprintln!("{}", test_utils::stringify_diagnostics(&node, &errs));
            }
            Err(TestCase {
                path: path.to_owned(),
                reason: TestResult::ParseFail(test_utils::stringify_diagnostics(&node, &errs)),
            })
        }
        Ok(node) => match compile::compile(&node, glyph_map) {
            Ok(thing) => {
                let _ = thing.build_raw(glyph_map, Default::default()).unwrap();
                Err(TestCase {
                    path: path.to_owned(),
                    reason: TestResult::UnexpectedSuccess,
                })
            }
            Err(errs) => {
                let msg = test_utils::stringify_diagnostics(&node, &errs);
                let result =
                    test_utils::compare_to_expected_output(&msg, path, BAD_OUTPUT_EXTENSION);
                if result.is_err() {
                    if std::env::var(super::VERBOSE).is_ok() {
                        eprintln!("{}", &msg);
                    }
                    if std::env::var(super::WRITE_RESULTS_VAR).is_ok() {
                        let to_path = path.with_extension(BAD_OUTPUT_EXTENSION);
                        std::fs::write(to_path, &msg).expect("failed to write output");
                    }
                }
                result
            }
        },
    }
}

fn run_good_test(path: PathBuf, map: &GlyphMap) -> Result<PathBuf, TestCase> {
    match std::panic::catch_unwind(|| good_test_body(&path, map)) {
        Err(_) => Err(TestCase {
            path,
            reason: TestResult::Panic,
        }),
        Ok(Err(e)) => Err(e),
        Ok(_) => Ok(path),
    }
}

fn good_test_body(path: &Path, glyph_map: &GlyphMap) -> Result<(), TestCase> {
    match test_utils::try_parse_file(path, Some(glyph_map)) {
        Err((node, errs)) => Err(TestCase {
            path: path.to_owned(),
            reason: TestResult::ParseFail(test_utils::stringify_diagnostics(&node, &errs)),
        }),
        Ok(node) => match compile::compile(&node, glyph_map) {
            Ok(thing) => {
                let mut x = thing
                    .build_raw(glyph_map, Opts::new().make_post_table(true))
                    .unwrap();
                x.build();
                Ok(())
            }
            Err(errs) => Err(TestCase {
                path: path.to_owned(),
                reason: TestResult::CompileFail(test_utils::stringify_diagnostics(&node, &errs)),
            }),
        },
    }
}
