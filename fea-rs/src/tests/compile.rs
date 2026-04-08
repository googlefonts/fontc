//! tests of the full compiler, including expected successes and failures

use std::path::{Path, PathBuf};

use crate::{
    GlyphMap,
    compile::{
        Compilation, Compiler, MockVariationInfo, NopFeatureProvider, Opts, error::CompilerError,
    },
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

/// Compile a FEA string using the mini-latin glyph order.
fn compile_fea(fea: &str, test_name: &str) -> Compilation {
    let dir = std::env::temp_dir().join(format!("fea_rs_test_{test_name}"));
    std::fs::create_dir_all(&dir).unwrap();
    let fea_path = dir.join(format!("{test_name}.fea"));
    std::fs::write(&fea_path, fea).unwrap();

    let glyph_order_path = Path::new(ROOT_TEST_DIR)
        .join("mini-latin")
        .join(GLYPH_ORDER);
    let glyph_order = std::fs::read_to_string(glyph_order_path).unwrap();
    let glyph_map: GlyphMap = glyph_order.lines().map(GlyphName::new).collect();

    Compiler::<'_, NopFeatureProvider, MockVariationInfo>::new(fea_path, &glyph_map)
        .compile()
        .expect("compilation should succeed")
}

/// Like [`compile_fea`], but with variable font info.
fn compile_fea_variable(fea: &str, test_name: &str) -> Compilation {
    let dir = std::env::temp_dir().join(format!("fea_rs_test_{test_name}"));
    std::fs::create_dir_all(&dir).unwrap();
    let fea_path = dir.join(format!("{test_name}.fea"));
    std::fs::write(&fea_path, fea).unwrap();

    let glyph_order_path = Path::new(ROOT_TEST_DIR)
        .join("mini-latin")
        .join(GLYPH_ORDER);
    let glyph_order = std::fs::read_to_string(glyph_order_path).unwrap();
    let glyph_map: GlyphMap = glyph_order.lines().map(GlyphName::new).collect();
    let var_info = test_utils::make_var_info();

    Compiler::<'_, NopFeatureProvider, MockVariationInfo>::new(fea_path, &glyph_map)
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

/// Similar to compile_fea but with `compile_debg(true)` and return
/// the parsed Debg JSON.
fn compile_debg(fea: &str, test_name: &str) -> Option<serde_json::Value> {
    use crate::parse::SourceLoadError;
    use std::sync::Arc;

    let fea_path = format!("{test_name}.fea");

    let glyph_order_path = Path::new(ROOT_TEST_DIR)
        .join("mini-latin")
        .join(GLYPH_ORDER);
    let glyph_order = std::fs::read_to_string(glyph_order_path).unwrap();
    let glyph_map: GlyphMap = glyph_order.lines().map(GlyphName::new).collect();

    let fea = fea.to_string();
    Compiler::<NopFeatureProvider, MockVariationInfo>::new(fea_path, &glyph_map)
        .with_resolver(
            move |_path: &std::path::Path| -> Result<Arc<str>, SourceLoadError> {
                Ok(fea.as_str().into())
            },
        )
        .with_opts(Opts::new().compile_debg(true))
        .compile()
        .expect("compilation should succeed")
        .debg
        .as_ref()
        .map(|b| serde_json::from_slice(b).expect("Debg should be valid JSON"))
}

/// A named GSUB lookup referenced by a feature gets its name and source location.
#[test]
fn debg_named_gsub_lookup() {
    let json = compile_debg(
        "\
lookup smcp_default {
    sub A by B;
} smcp_default;

feature smcp {
    lookup smcp_default;
} smcp;
",
        "named_gsub",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GSUB": {
                    "0": ["named_gsub.fea:1:7", "smcp_default", null],
                }
            }
        })
    );
}

/// A named GPOS lookup referenced by a feature gets its name and source location.
#[test]
fn debg_named_gpos_lookup() {
    let json = compile_debg(
        "\
lookup kern_Default {
    pos A B -50;
} kern_Default;

feature kern {
    lookup kern_Default;
} kern;
",
        "named_gpos",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GPOS": {
                    "0": ["named_gpos.fea:1:7", "kern_Default", null]
                }
            }
        })
    );
}

/// A named GSUB lookup not referenced by any feature still appears in debug info.
#[test]
fn debg_orphan_named_gsub_lookup() {
    let json = compile_debg(
        "\
lookup sub_orphan {
    sub B by A;
} sub_orphan;
",
        "orphan_gsub",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GSUB": {
                    "0": ["orphan_gsub.fea:1:7", "sub_orphan", null],
                }
            }
        })
    );
}

/// A named GPOS lookup not referenced by any feature still appears in debug info.
#[test]
fn debg_orphan_named_gpos_lookup() {
    let json = compile_debg(
        "\
lookup pos_orphan {
    pos A B -50;
} pos_orphan;
",
        "orphan_gpos",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GPOS": {
                    "0": ["orphan_gpos.fea:1:7", "pos_orphan", null],
                }
            }
        })
    );
}

/// An anonymous GSUB rule inside a feature gets debug info with null name.
#[test]
fn debg_anonymous_gsub_rule() {
    let json = compile_debg(
        "\
feature liga {
    sub A B by B;
} liga;
",
        "anon_gsub",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GSUB": {
                    "0": ["anon_gsub.fea:2:4", null, null],
                }
            }
        })
    );
}

/// An anonymous GPOS rule inside a feature gets debug info with null name.
#[test]
fn debg_anonymous_gpos_rule() {
    let json = compile_debg(
        "\
feature dist {
    pos A -30;
} dist;
",
        "anon_gpos",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GPOS": {
                    "0": ["anon_gpos.fea:2:4", null, null]
                }
            }
        })
    );
}

/// A contextual substitution rule gets debug info.
#[test]
fn debg_contextual_substitution() {
    let json = compile_debg(
        "\
feature calt {
    sub A' by B;
} calt;
",
        "calt",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GSUB": {
                    "0": ["calt.fea:2:4", null, null],
                    "1": ["calt.fea:2:4", null, null],
                }
            }
        })
    );
}

/// A contextual positioning rule gets debug info.
#[test]
fn debg_contextual_positioning() {
    let json = compile_debg(
        "\
lookup kern_single {
    pos A -50;
} kern_single;

feature kern {
    pos A' lookup kern_single;
} kern;
",
        "ctx_pos",
    );

    let json = json.unwrap();
    let gpos = &json["com.github.fonttools.feaLib"]["GPOS"];
    assert!(!gpos.is_null(), "expected GPOS entries for contextual pos");
}

/// Re-referencing a named GSUB lookup from a second feature block must not
/// create duplicate entries.
#[test]
fn debg_reref_named_gsub_lookup() {
    let json = compile_debg(
        "\
lookup smcp_default {
    sub A by B;
} smcp_default;

feature smcp {
    lookup smcp_default;
} smcp;

feature c2sc {
    lookup smcp_default;
} c2sc;
",
        "reref_gsub",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GSUB": {
                    "0": ["reref_gsub.fea:1:7", "smcp_default", null]
                }
            }
        })
    );
}

/// Re-referencing a named GPOS lookup from a second feature block must not
/// create duplicate entries.
#[test]
fn debg_reref_named_gpos_lookup() {
    let json = compile_debg(
        "\
lookup kern_Default {
    pos A B -50;
} kern_Default;

feature kern {
    lookup kern_Default;
} kern;

feature dist {
    lookup kern_Default;
} dist;
",
        "reref_gpos",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GPOS": {
                    "0": ["reref_gpos.fea:1:7", "kern_Default", null]
                }
            }
        })
    );
}

/// Multiple anonymous GSUB rules in one feature each get valid debug info.
#[test]
fn debg_multiple_anonymous_gsub() {
    let json = compile_debg(
        "\
feature liga {
    sub A B by B;
    sub A from [A B];
} liga;
",
        "multi_gsub",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GSUB": {
                    "0": ["multi_gsub.fea:2:4", null, null],
                    "1": ["multi_gsub.fea:3:4", null, null]
                }
            }
        })
    );
}

/// Multiple anonymous GPOS rules in one feature each get valid debug info.
#[test]
fn debg_multiple_anonymous_gpos() {
    let json = compile_debg(
        "\
feature kern {
    pos A B -50;
    pos A <0 0 10 0>;
} kern;
",
        "multi_gpos",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GPOS": {
                    "0": ["multi_gpos.fea:2:4", null, null],
                    "1": ["multi_gpos.fea:3:4", null, null]
                }
            }
        })
    );
}

/// A source with both GSUB and GPOS lookups produces entries for both tables.
#[test]
fn debg_gsub_and_gpos() {
    let json = compile_debg(
        "\
lookup smcp_default {
    sub A by B;
} smcp_default;

feature smcp {
    lookup smcp_default;
} smcp;

lookup kern_Default {
    pos A B -50;
} kern_Default;

feature kern {
    lookup kern_Default;
} kern;
",
        "gsub_and_gpos",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {
                "GSUB": {
                    "0": ["gsub_and_gpos.fea:1:7", "smcp_default", null],
                },
                "GPOS": {
                    "0": ["gsub_and_gpos.fea:9:7", "kern_Default", null]
                }
            }
        })
    );
}

/// An empty feature block produces no lookup entries.
#[test]
fn debg_empty_feature_block() {
    let json = compile_debg(
        "\
feature liga {
} liga;
",
        "empty_feature_block",
    );

    assert_eq!(
        json.unwrap(),
        serde_json::json!({
            "com.github.fonttools.feaLib": {}
        })
    );
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
