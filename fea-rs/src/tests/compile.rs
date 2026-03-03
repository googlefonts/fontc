//! tests of the full compiler, including expected successes and failures

use std::path::{Path, PathBuf};

use crate::{
    GlyphMap,
    compile::{Compiler, MockVariationInfo, NopFeatureProvider, Opts, error::CompilerError},
    util::ttx::{self as test_utils, Filter, Report, TestCase, TestResult},
};
use fontdrasil::types::GlyphName;

static ROOT_TEST_DIR: &str = "./test-data/compile-tests";
static GOOD_DIR: &str = "good";
static BAD_DIR: &str = "bad";
static GLYPH_ORDER: &str = "glyph_order.txt";
static BAD_OUTPUT_EXTENSION: &str = "ERR";
static IMPORT_RESOLUTION_TEST: &str = "./test-data/include-resolution-tests/dir1/test1.fea";

// tests taken directly from fonttools; these require some special handling.
#[test]
fn fonttools_tests() -> Result<(), Report> {
    let _ = env_logger::builder().is_test(true).try_init();
    test_utils::assert_has_ttx_executable();
    test_utils::run_fonttools_tests(None).into_error()
}

#[test]
fn should_fail() -> Result<(), Report> {
    let _ = env_logger::builder().is_test(true).try_init();
    let mut results = Vec::new();
    for (glyph_map, var_info, tests) in iter_test_groups(BAD_DIR) {
        results.extend(
            tests
                .into_iter()
                .map(|path| run_bad_test(path, &glyph_map, &var_info)),
        );
    }
    test_utils::finalize_results(results).into_error()
}

#[test]
fn import_resolution() {
    let _ = env_logger::builder().is_test(true).try_init();
    let glyph_map = test_utils::fonttools_test_glyph_order();
    let path = PathBuf::from(IMPORT_RESOLUTION_TEST);
    match test_utils::run_test(path, &glyph_map, &Default::default()) {
        Ok(_) => (),
        Err(e) => panic!("{:?}", e.reason),
    }
}

#[test]
fn should_pass() -> Result<(), Report> {
    let _ = env_logger::builder().is_test(true).try_init();
    let mut results = Vec::new();

    for (glyph_map, var_info, tests) in iter_test_groups(GOOD_DIR) {
        results.extend(
            tests
                .into_iter()
                .map(|path| test_utils::run_test(path, &glyph_map, &var_info)),
        );
    }
    test_utils::finalize_results(results).into_error()
}

fn iter_test_groups(
    test_dir: &str,
) -> impl Iterator<Item = (GlyphMap, MockVariationInfo, Vec<PathBuf>)> + '_ {
    iter_test_group_dirs(ROOT_TEST_DIR).map(move |dir| {
        let glyph_order_path = dir.join(GLYPH_ORDER);
        let glyph_order =
            std::fs::read_to_string(glyph_order_path).expect("failed to read glyph order");
        let glyph_map = glyph_order.lines().map(GlyphName::new).collect();
        let var_info = test_utils::make_var_info();
        let tests_dir = dir.join(test_dir);
        let tests = test_utils::iter_fea_files(tests_dir, Filter::from_env()).collect::<Vec<_>>();
        (glyph_map, var_info, tests)
    })
}

fn iter_test_group_dirs(root_dir: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
    let mut dir = root_dir.as_ref().read_dir().unwrap();
    std::iter::from_fn(move || {
        loop {
            let entry = dir.next()?.unwrap();
            let path = entry.path();
            if path.is_dir() {
                return Some(path);
            }
        }
    })
}

fn run_bad_test(
    path: PathBuf,
    map: &GlyphMap,
    var_info: &MockVariationInfo,
) -> Result<PathBuf, TestCase> {
    match std::panic::catch_unwind(|| bad_test_body(&path, map, var_info)) {
        Err(_) => Err(TestCase {
            path,
            reason: TestResult::Panic,
        }),
        Ok(Err(reason)) => Err(TestCase { path, reason }),
        Ok(_) => Ok(path),
    }
}

// Regression test for https://github.com/googlefonts/fontc/issues/1847
//
// When a variable font has no mark attachment lookups, finalize_gdef_table()
// correctly sets glyph_classes_were_inferred=true but then discards the empty
// GdefBuilder. build() later creates a new GdefBuilder for the IVS with the
// default glyph_classes_were_inferred=false, causing Compilation::gdef_classes
// to be Some({}) instead of None. This prevents fontbe from injecting
// source-derived GDEF categories.
#[test]
fn variable_font_no_marks_gdef_classes_is_none() {
    let fea = "\
languagesystem DFLT dflt;
feature kern {
    pos A <0 (wght=200:12 wght=900:22) 0 0>;
} kern;
";

    let dir = std::env::temp_dir().join("fea_rs_test_issue_1847");
    std::fs::create_dir_all(&dir).unwrap();
    let fea_path = dir.join("variable_no_marks.fea");
    std::fs::write(&fea_path, fea).unwrap();

    let glyph_order_path = Path::new(ROOT_TEST_DIR)
        .join("mini-latin")
        .join(GLYPH_ORDER);
    let glyph_order = std::fs::read_to_string(glyph_order_path).unwrap();
    let glyph_map: GlyphMap = glyph_order.lines().map(GlyphName::new).collect();
    let var_info = test_utils::make_var_info();

    let compilation =
        Compiler::<'_, NopFeatureProvider, MockVariationInfo>::new(fea_path, &glyph_map)
            .with_variable_info(&var_info)
            .compile()
            .expect("compilation should succeed");

    // The GDEF table should exist (it holds the IVS for variable GPOS values)
    assert!(
        compilation.gdef.is_some(),
        "expected GDEF table for variable font"
    );

    // But gdef_classes should be None: no explicit GlyphClassDef was declared in FEA,
    // so the caller (fontbe) should be free to inject source-derived categories.
    assert!(
        compilation.gdef_classes.is_none(),
        "expected gdef_classes to be None (no explicit classes), \
         got Some with {} entries",
        compilation.gdef_classes.as_ref().map_or(0, |m| m.len())
    );
}

fn bad_test_body(
    path: &Path,
    glyph_map: &GlyphMap,
    var_info: &MockVariationInfo,
) -> Result<(), TestResult> {
    let mut compiler: Compiler<'_, NopFeatureProvider, MockVariationInfo> =
        Compiler::new(path.to_path_buf(), glyph_map)
            .print_warnings(std::env::var(crate::util::VERBOSE).is_ok())
            .with_opts(Opts::new().make_post_table(true));
    if test_utils::is_variable(path) {
        compiler = compiler.with_variable_info(var_info);
    }

    match compiler.compile_binary() {
        Ok(_) => Err(TestResult::UnexpectedSuccess),
        // this means we have a test case that doesn't exist or something weird
        Err(CompilerError::SourceLoad(err)) => panic!("{err}"),
        Err(CompilerError::WriteFail(err)) => panic!("{err}"),
        Err(CompilerError::ParseFail(errs)) => Err(TestResult::ParseFail(errs.to_string(true))),
        Err(CompilerError::ValidationFail(errs) | CompilerError::CompilationFail(errs)) => {
            let msg = errs.to_string(false);
            let result = test_utils::compare_to_expected_output(&msg, path, BAD_OUTPUT_EXTENSION);
            if result.is_err() && std::env::var(crate::util::WRITE_RESULTS_VAR).is_ok() {
                let to_path = path.with_extension(BAD_OUTPUT_EXTENSION);
                std::fs::write(to_path, &msg).expect("failed to write output");
            }
            result.map_err(|e| e.reason)
        }
    }
}
