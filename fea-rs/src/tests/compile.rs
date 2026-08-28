//! tests of the full compiler, including expected successes and failures

use std::path::{Path, PathBuf};

use crate::{
    GlyphMap,
    compile::{
        Compilation, Compiler, MockVariationInfo, NopFeatureProvider, Opts, error::CompilerError,
    },
    util::ttx::{self as test_utils, Filter, Report, TestCase, TestResult},
};

static ROOT_TEST_DIR: &str = "./test-data/compile-tests";
static GOOD_DIR: &str = "good";
static BAD_DIR: &str = "bad";
static GLYPH_ORDER: &str = "glyph_order.txt";
static BAD_OUTPUT_EXTENSION: &str = "ERR";
static IMPORT_RESOLUTION_TEST: &str = "./test-data/include-resolution-tests/dir1/test1.fea";

// tests taken directly from fonttools; these require some special handling.
#[test]
fn fonttools_tests() -> Result<(), Report> {
    let _ = tracing_subscriber::fmt().with_test_writer().try_init();
    test_utils::assert_has_ttx_executable();
    test_utils::run_fonttools_tests(None).into_error()
}

#[test]
fn should_fail() -> Result<(), Report> {
    let _ = tracing_subscriber::fmt().with_test_writer().try_init();
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
    let _ = tracing_subscriber::fmt().with_test_writer().try_init();
    let glyph_map = test_utils::fonttools_test_glyph_order();
    let path = PathBuf::from(IMPORT_RESOLUTION_TEST);
    match test_utils::run_test(path, &glyph_map, &Default::default()) {
        Ok(_) => (),
        Err(e) => panic!("{:?}", e.reason),
    }
}

#[test]
fn should_pass() -> Result<(), Report> {
    let _ = tracing_subscriber::fmt().with_test_writer().try_init();
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
        let glyph_map = GlyphMap::new(glyph_order.lines()).unwrap();
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

/// Write `fea` to a temp file named after the test, returning its path.
fn write_temp_fea(fea: &str, test_name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("fea_rs_test_{test_name}"));
    std::fs::create_dir_all(&dir).unwrap();
    let fea_path = dir.join(format!("{test_name}.fea"));
    std::fs::write(&fea_path, fea).unwrap();
    fea_path
}

/// The mini-latin glyph order used by most compile tests.
fn mini_latin_glyph_map() -> GlyphMap {
    let glyph_order_path = Path::new(ROOT_TEST_DIR)
        .join("mini-latin")
        .join(GLYPH_ORDER);
    let glyph_order = std::fs::read_to_string(glyph_order_path).unwrap();
    GlyphMap::new(glyph_order.lines()).unwrap()
}

/// Compile a FEA string using the mini-latin glyph order.
fn compile_fea(fea: &str, test_name: &str) -> Compilation {
    let fea_path = write_temp_fea(fea, test_name);
    Compiler::<'_, NopFeatureProvider, MockVariationInfo>::new(fea_path, &mini_latin_glyph_map())
        .compile()
        .expect("compilation should succeed")
}

/// Like [`compile_fea`], but with variable font info.
fn compile_fea_variable(fea: &str, test_name: &str) -> Compilation {
    let fea_path = write_temp_fea(fea, test_name);
    let var_info = test_utils::make_var_info();
    Compiler::<'_, NopFeatureProvider, MockVariationInfo>::new(fea_path, &mini_latin_glyph_map())
        .with_variable_info(&var_info)
        .compile()
        .expect("compilation should succeed")
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
    let compilation = compile_fea_variable(
        "\
languagesystem DFLT dflt;
feature kern {
    pos A <0 (wght=200:12 wght=900:22) 0 0>;
} kern;
",
        "issue_1847",
    );

    assert!(
        compilation.gdef.is_some(),
        "expected GDEF table for variable font"
    );
    assert!(
        compilation.gdef_classes.is_none(),
        "expected gdef_classes to be None (no explicit classes), \
         got Some with {} entries",
        compilation.gdef_classes.as_ref().map_or(0, |m| m.len())
    );
}

// Verify that `# Automatic Code` comments in feature blocks produce
// insert_markers entries in the Compilation result.
#[test]
fn insert_markers_exposed_on_compilation() {
    use write_fonts::types::Tag;

    let compilation = compile_fea(
        "\
languagesystem DFLT dflt;

feature kern {
    pos A B 10;
    # Automatic Code
} kern;

feature mark {
    # Automatic Code
    pos X Y 5;
} mark;
",
        "insert_markers",
    );

    let kern_tag = Tag::new(b"kern");
    let mark_tag = Tag::new(b"mark");

    assert!(
        compilation.insert_markers.contains_key(&kern_tag),
        "expected insert marker for 'kern' feature"
    );
    assert!(
        compilation.insert_markers.contains_key(&mark_tag),
        "expected insert marker for 'mark' feature"
    );

    // kern has one lookup (pos A B 10) before the marker, so its insertion
    // point should be at lookup index 1
    let kern_marker = &compilation.insert_markers[&kern_tag];
    assert_eq!(
        kern_marker.lookup_id.to_raw(),
        1,
        "kern marker should point after the first lookup"
    );

    // mark's marker is at the start of the block, before any lookups in that
    // feature, so it should have the same lookup index as kern's (the next
    // GPOS id at that point is still 1)
    let mark_marker = &compilation.insert_markers[&mark_tag];
    assert_eq!(
        mark_marker.lookup_id.to_raw(),
        1,
        "mark marker should also be at lookup index 1"
    );

    // kern's marker appears first in source, so it should have lower priority
    assert!(
        kern_marker.priority < mark_marker.priority,
        "kern marker (priority {}) should have lower priority than mark (priority {})",
        kern_marker.priority,
        mark_marker.priority,
    );

    assert_eq!(
        compilation.insert_markers.len(),
        2,
        "expected exactly 2 insert markers, got {}",
        compilation.insert_markers.len()
    );
}

// Verify that features without `# Automatic Code` produce no insert markers.
#[test]
fn no_insert_markers_without_automatic_code() {
    let compilation = compile_fea(
        "\
languagesystem DFLT dflt;

feature kern {
    pos A B 10;
} kern;
",
        "no_insert_markers",
    );

    assert!(
        compilation.insert_markers.is_empty(),
        "expected no insert markers when no `# Automatic Code` comments present, got {}",
        compilation.insert_markers.len()
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

#[test]
fn debg_no_table_by_default() {
    let compilation = compile_fea(
        "\
feature kern {
    pos A B 10;
} kern;
",
        "no_table_by_default",
    );
    assert!(
        compilation.debg.is_none(),
        "Debg should not be emitted by default"
    );
}

// https://github.com/googlefonts/fontc/issues/1969
#[test]
fn remap_name_ids_preserves_cv_null_sentinel() {
    use write_fonts::{
        tables::{gsub::Gsub, layout::FeatureParams},
        types::{NameId, Tag},
    };

    let mut compilation = compile_fea(
        "\
feature cv02 {
    cvParameters {
        FeatUILabelNameID {
            name \"Tall W\";
        };
    };
    sub a by b;
} cv02;
",
        "remap_cv_null_sentinel",
    );

    let cv02 = Tag::new(b"cv02");
    let get_cv_name_ids = |gsub: &Gsub| -> [NameId; 4] {
        let cv_rec = gsub
            .feature_list
            .feature_records
            .iter()
            .find(|r| r.feature_tag == cv02)
            .expect("should have cv02 feature");
        let params = cv_rec
            .feature
            .feature_params
            .as_ref()
            .expect("cv02 should have feature params");
        match params {
            FeatureParams::CharacterVariant(p) => [
                p.feat_ui_label_name_id,
                p.feat_ui_tooltip_text_name_id,
                p.sample_text_name_id,
                p.first_param_ui_label_name_id,
            ],
            _ => panic!("cv02 should have CharacterVariant params"),
        }
    };

    let [_, tooltip, sample, first_param] = get_cv_name_ids(compilation.gsub.as_ref().unwrap());
    assert_eq!(tooltip, NameId::default());
    assert_eq!(sample, NameId::default());
    assert_eq!(first_param, NameId::default());

    // Simulate fontc calling remap with a non-zero offset (as happens when
    // other name IDs were already allocated before fea-rs compilation).
    compilation.remap_name_ids(512);

    let [label, tooltip, sample, first_param] = get_cv_name_ids(compilation.gsub.as_ref().unwrap());
    assert_ne!(
        label,
        NameId::default(),
        "assigned label ID should be remapped"
    );
    assert_eq!(tooltip, NameId::default(), "unset tooltip must stay 0xFFFF");
    assert_eq!(
        sample,
        NameId::default(),
        "unset sample text must stay 0xFFFF"
    );
    assert_eq!(
        first_param,
        NameId::default(),
        "unset first param must stay 0xFFFF"
    );
}

#[test]
fn mark_class_used_in_glyph_class_def() {
    // Mark classes should be accepted wherever glyph classes are expected,
    // including inside glyph class definitions.
    compile_fea(
        "\
languagesystem DFLT dflt;
markClass a <anchor 150 -10> @top;
@foo = [@top b];
",
        "mark_class_in_glyph_class",
    );
}

/// Compile a FEA string against an explicit glyph order.
fn compile_fea_with_glyphs(
    fea: &str,
    glyph_names: &[&str],
    test_name: &str,
) -> Result<Vec<u8>, CompilerError> {
    let fea_path = write_temp_fea(fea, test_name);
    let glyph_map = GlyphMap::new(glyph_names.iter().copied()).unwrap();
    Compiler::<'_, NopFeatureProvider, MockVariationInfo>::new(fea_path, &glyph_map)
        .compile_binary()
}

// Compile a predicate-bearing source that must be rejected at validation, and
// return the (message, source-range) of each error diagnostic. Panics on any
// other outcome: an out-of-scope predicate has to fail *validation*, not parse,
// build, or panic. The returned ranges let a caller assert that each diagnostic
// is attached to the offending child rather than the whole predicate.
fn predicate_validation_errors(
    fea: &str,
    test_name: &str,
) -> Vec<(String, std::ops::Range<usize>)> {
    let glyphs = ["a", "b", "a.sc"];
    let result = compile_fea_with_glyphs(fea, &glyphs, test_name);
    let Err(CompilerError::ValidationFail(errs)) = result else {
        panic!(
            "expected ValidationFail, got {:?}",
            result.map(|bytes| bytes.len())
        );
    };
    errs.diagnostics()
        .iter()
        .filter(|diag| diag.is_error())
        .map(|diag| (diag.text().to_string(), diag.span()))
        .collect()
}

// A predicate rejected at validation fails cleanly (no panic, no silent
// mis-compile) with each diagnostic attached to the offending child, not the
// whole predicate. Each case is (predicate, test name, expected (child span text, message
// fragment) pairs):
// - `category like` is doubly out of scope (Phase 2, fontc#2052): an attribute
//   error on `category` AND an operator error on `like`.
// - `NAME` is rejected because glyphsLib's object regex is case-sensitive:
//   only lowercase `name` is a supported attribute.
// - Mixing `and`/`or` attaches to the connective that breaks the chain.
// - Unquoted values must be quoted: glyphsLib types some bare values as
//   booleans or integers rather than strings -- `yes`, `123`, and even `noon`,
//   whose unanchored boolean regex match reads the leading `no`.
// - An empty quoted value is rejected (glyphsLib errors on it too), and under
//   the substring operators it would match every glyph name.
#[test]
fn glyphs_predicate_validation_diagnostics() {
    // (child span text, message fragment) pairs expected for one predicate
    type ExpectedErrors = &'static [(&'static str, &'static str)];
    let cases: &[(&str, &str, ExpectedErrors)] = &[
        (
            "$[category like \"Letter\"]",
            "glyphs_predicate_bad",
            &[("category", "2052"), ("like", "2052")],
        ),
        (
            "$[NAME == \"a\"]",
            "glyphs_predicate_upper_name",
            &[("NAME", "2052")],
        ),
        (
            "$[name == \"a\" and name == \"b\" or name == \"c\"]",
            "glyphs_predicate_mixed",
            &[("or", "2052")],
        ),
        (
            "$[name == sc]",
            "glyphs_predicate_bare_word",
            &[("sc", "must be quoted")],
        ),
        (
            "$[name == yes]",
            "glyphs_predicate_bare_bool",
            &[("yes", "must be quoted")],
        ),
        (
            "$[name == 123]",
            "glyphs_predicate_bare_number",
            &[("123", "must be quoted")],
        ),
        (
            "$[name == noon]",
            "glyphs_predicate_bare_bool_prefix",
            &[("noon", "must be quoted")],
        ),
        (
            "$[name contains \"\"]",
            "glyphs_predicate_empty_value",
            &[("\"\"", "empty value")],
        ),
    ];
    for (predicate, test_name, expected) in cases {
        let fea = format!("@x = [ {predicate} ]; feature test {{ sub @x by a; }} test;");
        let errs = predicate_validation_errors(&fea, test_name);
        assert_eq!(
            errs.len(),
            expected.len(),
            "wrong error count for `{predicate}`: {errs:?}"
        );
        for (span_text, msg_fragment) in *expected {
            let (msg, _) = errs
                .iter()
                .find(|(_, span)| &fea[span.clone()] == *span_text)
                .unwrap_or_else(|| {
                    panic!("expected an error attached to `{span_text}` for `{predicate}`, got {errs:?}")
                });
            assert!(
                msg.contains(msg_fragment),
                "error on `{span_text}` should contain {msg_fragment:?}, got: {msg}"
            );
        }
    }
}
