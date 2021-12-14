//! tests of the full compiler, including expected successes and failures

use std::path::{Path, PathBuf};

use fonttools::font::Font;

use crate::{
    util::ttx::{self as test_utils, Failure, Reason, Results},
    GlyphMap, GlyphName,
};

static ROOT_TEST_DIR: &str = "./test-data/compile-tests";
static GOOD_DIR: &str = "good";
static BAD_DIR: &str = "bad";
static FONT_FILE: &str = "font.ttf";
static BAD_OUTPUT_EXTENSION: &str = "ERR";

#[test]
fn should_fail() -> Result<(), Results> {
    let mut results = Vec::new();
    for (font, tests) in iter_test_groups(BAD_DIR) {
        let glyph_map = make_glyph_map(&font);
        results.extend(tests.into_iter().map(|path| run_bad_test(path, &glyph_map)));
    }
    test_utils::finalize_results(results)
}

#[test]
fn should_pass() -> Result<(), Results> {
    let mut results = Vec::new();
    for (font, tests) in iter_test_groups(GOOD_DIR) {
        let glyph_map = make_glyph_map(&font);
        results.extend(
            tests
                .into_iter()
                .map(|path| run_good_test(path, &glyph_map)),
        );
    }
    test_utils::finalize_results(results)
}

fn iter_test_groups(test_dir: &str) -> impl Iterator<Item = (Font, Vec<PathBuf>)> + '_ {
    iter_test_group_dirs(ROOT_TEST_DIR).map(move |dir| {
        let font_path = dir.join(FONT_FILE);
        let font = Font::load(&font_path).unwrap();
        let tests_dir = dir.join(test_dir);
        let tests = test_utils::iter_fea_files(tests_dir).collect::<Vec<_>>();
        (font, tests)
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

fn run_bad_test(path: PathBuf, map: &GlyphMap) -> Result<PathBuf, Failure> {
    match std::panic::catch_unwind(|| bad_test_body(&path, map)) {
        Err(_) => Err(Failure {
            path,
            reason: Reason::Panic,
        }),
        Ok(Err(e)) => Err(e),
        Ok(_) => Ok(path),
    }
}

fn bad_test_body(path: &Path, glyph_map: &GlyphMap) -> Result<(), Failure> {
    match test_utils::try_parse_file(path, Some(glyph_map)) {
        Err((node, errs)) => Err(Failure {
            path: path.to_owned(),
            reason: Reason::ParseFail(test_utils::stringify_diagnostics(&node, &errs)),
        }),
        Ok(node) => match crate::compile(&node, glyph_map) {
            Ok(_) => Err(Failure {
                path: path.to_owned(),
                reason: Reason::UnexpectedSuccess,
            }),
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
                        std::fs::write(&to_path, &msg).expect("failed to write output");
                    }
                }
                result
            }
        },
    }
}

fn run_good_test(path: PathBuf, map: &GlyphMap) -> Result<PathBuf, Failure> {
    match std::panic::catch_unwind(|| good_test_body(&path, map)) {
        Err(_) => Err(Failure {
            path,
            reason: Reason::Panic,
        }),
        Ok(Err(e)) => Err(e),
        Ok(_) => Ok(path),
    }
}

fn good_test_body(path: &Path, glyph_map: &GlyphMap) -> Result<(), Failure> {
    match test_utils::try_parse_file(path, Some(glyph_map)) {
        Err((node, errs)) => Err(Failure {
            path: path.to_owned(),
            reason: Reason::ParseFail(test_utils::stringify_diagnostics(&node, &errs)),
        }),
        Ok(node) => match crate::compile(&node, glyph_map) {
            Ok(_thing) => Ok(()),
            Err(errs) => Err(Failure {
                path: path.to_owned(),
                reason: Reason::CompileFail(test_utils::stringify_diagnostics(&node, &errs)),
            }),
        },
    }
}

fn make_glyph_map(font: &Font) -> GlyphMap {
    font.tables
        .post()
        .unwrap()
        .expect("missing 'name' table")
        .glyphnames
        .as_ref()
        .map(|names| names.iter().map(GlyphName::new).collect())
        .expect("no glyph map")
}
