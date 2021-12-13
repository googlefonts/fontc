//! ensuring that invalid inputs produce useful diagnostics

use std::path::{Path, PathBuf};

use crate::{
    util::ttx::{self as test_utils, Failure, Reason, Results},
    GlyphMap,
};

static TEST_DATA: &str = "./test-data/error-tests";
static OUTPUT_EXTENSION: &str = "ERR";

#[test]
fn expected_failures() -> Result<(), Results> {
    let glyph_map = test_utils::make_glyph_map();
    test_utils::finalize_results(
        test_utils::iter_fea_files(TEST_DATA)
            .map(|path| run_bad_test(path, &glyph_map))
            .collect(),
    )
}

fn run_bad_test(path: PathBuf, map: &GlyphMap) -> Result<PathBuf, Failure> {
    match std::panic::catch_unwind(|| try_to_compile(&path, map)) {
        Err(_) => Err(Failure {
            path,
            reason: Reason::Panic,
        }),
        Ok(Err(e)) => Err(e),
        Ok(_) => Ok(path),
    }
}

fn try_to_compile(path: &Path, glyph_map: &GlyphMap) -> Result<(), Failure> {
    match test_utils::try_parse_file(path, Some(glyph_map)) {
        Err((node, errs)) => Err(Failure {
            path: path.to_owned(),
            reason: Reason::ParseFail(test_utils::stringify_diagnostics(&node, &errs)),
        }),
        Ok(node) => match crate::compile(&node, glyph_map) {
            Ok(_) => Err(Failure {
                path: path.to_owned(),
                reason: Reason::CompileFail("unexpected success".into()),
            }),
            Err(errs) => {
                let msg = test_utils::stringify_diagnostics(&node, &errs);
                let result = test_utils::compare_to_expected_output(&msg, path, OUTPUT_EXTENSION);
                if result.is_err() && std::env::var(super::WRITE_RESULTS_VAR).is_ok() {
                    let to_path = path.with_extension(OUTPUT_EXTENSION);
                    std::fs::write(&to_path, &msg).expect("failed to write output");
                }
                result
            }
        },
    }
}
