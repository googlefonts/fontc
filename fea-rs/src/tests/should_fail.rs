//! ensuring that invalid inputs produce useful diagnostics

use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use crate::util::ttx;

static TEST_DATA: &str = "./test-data/error-tests";

//TODO: at some point it would be nice to have a fancy test harness like
// [trybuild](https://github.com/dtolnay/trybuild) that we could check our
// diagnostics against.
#[test]
fn expected_failures() {
    let glyph_map = ttx::make_glyph_map();
    let mut failures = Vec::new();
    for path in iter_compile_tests(TEST_DATA) {
        let contents = std::fs::read_to_string(&path).unwrap();
        let result = ttx::try_compile(&contents, &glyph_map);
        if result.is_ok() {
            failures.push(path);
        }
    }

    if !failures.is_empty() {
        eprintln!("failures ({}):", failures.len());
        for path in &failures {
            eprintln!("  {}", path.display());
        }
        eprintln!("");
        panic!("ahhh");
    }
}

fn iter_compile_tests(path: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> + 'static {
    let mut dir = path.as_ref().read_dir().unwrap();
    std::iter::from_fn(move || loop {
        let entry = dir.next()?.unwrap();
        let path = entry.path();
        if path.extension() == Some(OsStr::new("fea")) {
            return Some(path);
        }
    })
}
