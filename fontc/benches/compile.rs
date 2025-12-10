use std::borrow::Cow;
use std::path::Path;
use std::{hint::black_box, path::PathBuf};

use criterion::{Criterion, criterion_group, criterion_main};
use fontc;

criterion_group!(benches, compile);
criterion_main!(benches);

fn compile(c: &mut Criterion) {
    c.benchmark_group("fontc-compile")
        .bench_function("oswald", |b| {
            let path = resolve_path("OswaldFont/sources/Oswald.glyphs");
            b.iter(move || run_compile(&path).unwrap());
        })
        // Run slow benchmarks. Criterion requires at least 10
        // samples.
        .sample_size(10)
        .bench_function("google-sans", |b| {
            let path = resolve_path("googlesans/source/GoogleSans/GoogleSans.designspace");
            b.iter(move || run_compile(&path).unwrap());
        })
        .bench_function("google-sans-flex", |b| {
            let path = resolve_path("googlesans-flex/sources/GoogleSansFlex.designspace");
            b.iter(move || run_compile(&path).unwrap());
        });
}

fn resolve_path(relative_path: impl AsRef<Path>) -> PathBuf {
    let base_path = std::env::var("FONTC_BENCH_DIR")
        .map(|x| Cow::Owned(x))
        .unwrap_or(Cow::Borrowed("../"));
    PathBuf::from_iter([Path::new(base_path.as_ref()), relative_path.as_ref()])
}

fn run_compile(path: &Path) -> Result<Vec<u8>, fontc::Error> {
    let input = fontc::Input::new(&path)
        .inspect_err(|err| match std::env::current_dir() {
            Ok(current_dir) => {
                let resolved_path = PathBuf::from_iter([&current_dir, path]);
                eprintln!(
                    r#"Failed to open {path:?} with current directory {current_dir:?}
Resolved path: {resolved_path:?}
Err: {err}"#
                );
            }
            Err(wd_err) => {
                eprintln!(
                    r#"Failed to get current directory: {wd_err}
Failed to open {path:?}
Err: {err}"#
                );
            }
        })
        .unwrap();
    let source = black_box(input.create_source().unwrap());
    let options = black_box(fontc::Options {
        ..fontc::Options::default()
    });
    fontc::generate_font(black_box(source), black_box(options))
}
