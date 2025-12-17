//! Run benchmarks with `cargo bench -p fontc --bench compile`.
//!
//! You can customize the benchmark execution using the following environment variables:
//! - `FONTC_BENCH_DIR`: Specifies the directory where the font sources are located. Defaults to `../..`.
//! - `FONTC_BENCH_SRCS`: A comma-separated list of font sources to compile.
//!
//! For example: `FONTC_BENCH_DIR=../.. FONTC_BENCH_SRCS=OswaldFont/sources/Oswald.glyphs cargo bench -p fontc --bench compile`
//!
//! See `docs/performance.md` for more details on benchmarking and profiling.

use std::borrow::Cow;
use std::path::Path;
use std::{hint::black_box, path::PathBuf};

use criterion::{Criterion, SamplingMode, criterion_group, criterion_main};

criterion_group!(benches, compile);
criterion_main!(benches);

fn compile(c: &mut Criterion) {
    env_logger::builder().is_test(true).try_init().ok();
    let mut bench_group = c.benchmark_group("fontc-compile");
    // Criterion requires at least 10 samples. The compile time of fonts is large enough that 10
    // samples is enough to get an idea of acceptable performance.
    bench_group.sample_size(50);

    let source_paths = std::env::var("FONTC_BENCH_SRCS")
        .ok()
        .map(|x| x.to_string())
        .unwrap_or_default();
    for source_path in source_paths.split(',').filter(|s| !s.is_empty()) {
        let path = &resolve_path(source_path);
        let bench_name = path.file_name().unwrap().to_string_lossy();
        bench_group.bench_function(bench_name.as_ref(), move |b| {
            b.iter(move || {
                let source = make_source(path);
                let options = black_box(fontc::Options::default());
                fontc::generate_font(black_box(source), black_box(options))
                    .unwrap_or_else(|err| panic!("Failed to run benchmark for {path:?}: {err}"))
            })
        });
    }
}

fn resolve_path(relative_path: impl AsRef<Path>) -> PathBuf {
    let base_path = std::env::var("FONTC_BENCH_DIR")
        .map(Cow::Owned)
        .unwrap_or(Cow::Borrowed("../../"));
    PathBuf::from_iter([Path::new(base_path.as_ref()), relative_path.as_ref()])
}

fn make_source(path: &Path) -> Box<dyn fontir::source::Source> {
    let input = fontc::Input::new(path).unwrap_or_else(|err| match std::env::current_dir() {
        Ok(current_dir) => {
            let resolved_path = PathBuf::from_iter([&current_dir, path]);
            panic!(
                r#"Failed to open {path:?} with current directory {current_dir:?}
Resolved path: {resolved_path:?}
Err: {err}"#
            );
        }
        Err(wd_err) => {
            panic!(
                r#"Failed to get current directory: {wd_err}
Failed to open {path:?}
Err: {err}"#
            );
        }
    });
    input
        .create_source()
        .unwrap_or_else(|err| panic!("Unable to create source for {path:?}: {err}"))
}
