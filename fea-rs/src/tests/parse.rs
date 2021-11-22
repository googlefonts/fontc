//! Run the parser against a bunch of inputs

use std::{
    env,
    ffi::OsStr,
    path::{Path, PathBuf},
};

use crate::Diagnostic;

static TEST_DATA1: &str = "./test-data/fonttools-tests";
static TEST_DATA2: &str = "./test-data/other-parse-tests";

static ALLOWED_PARSE_FAILURES: &[&str] = &[
    "GSUB_error.fea",     // expected failure
    "bug509.fea",         // see https://github.com/adobe-type-tools/afdko/issues/1415
    "GSUB_5_formats.fea", // ditto
    //FIXME this should be working!
    "GSUB_8.fea", // we just don't handle rsub yet?
];

/// A way to customize output when our test fails
#[derive(Default)]
struct Results {
    seen: usize,
    failures: Vec<(PathBuf, bool)>, // true if this was a panic
}

#[test]
fn all_parse_tests() -> Result<(), Results> {
    assert!(
        std::path::Path::new(TEST_DATA1).exists(),
        "{:?}",
        env::current_dir()
    );
    let mut failures = Results::default();

    for path in iter_fea_files(TEST_DATA1)
        .chain(iter_fea_files(TEST_DATA2))
        .filter(|path| {
            !ALLOWED_PARSE_FAILURES.contains(&path.file_name().unwrap().to_str().unwrap())
        })
    {
        failures.seen += 1;
        match std::panic::catch_unwind(|| try_parse_file(&path)) {
            Err(_e) => failures.push(path, true),
            Ok(Err(_)) => failures.push(path, false),
            Ok(_) => (),
        }
    }
    if failures.failures.is_empty() {
        Ok(())
    } else {
        Err(failures)
    }
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

/// returns the tree and any errors
fn try_parse_file(path: &Path) -> Result<(), Vec<Diagnostic>> {
    let ctx = crate::parse_root_file(path, None, None).unwrap();
    let (_, errors) = ctx.generate_parse_tree();
    if errors.iter().any(Diagnostic::is_error) {
        Err(errors)
    } else {
        Ok(())
    }
}

impl Results {
    fn push(&mut self, path: PathBuf, panic: bool) {
        self.failures.push((path, panic))
    }
}

impl std::fmt::Debug for Results {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(
            f,
            "failed {}/{} test cases:",
            self.failures.len(),
            self.seen
        )?;
        let widest = self
            .failures
            .iter()
            .map(|(path, _)| path.as_os_str().len())
            .max()
            .unwrap_or(0);
        for (path, panic) in &self.failures {
            let panic = if *panic { "(panic)" } else { "" };
            writeln!(f, "{:width$} {}", path.display(), panic, width = widest)?;
        }
        Ok(())
    }
}
