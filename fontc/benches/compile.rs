use std::borrow::Cow;
use std::path::Path;
use std::{hint::black_box, path::PathBuf};

use criterion::{Criterion, criterion_group, criterion_main};
use fontc;
use log::error;

criterion_group!(benches, compile);
criterion_main!(benches);

fn compile(c: &mut Criterion) {
    env_logger::builder().is_test(true).try_init().ok();
    let mut bench_group = c.benchmark_group("fontc-compile");
    // Criterion requires at least 10 samples. The compile time of fonts is large enough that 10
    // samples is enough to get an idea of acceptable performance.
    bench_group.sample_size(10);

    let default_fonts_path =
        "OswaldFont/sources/Oswald.glyphs,Playfair/sources/Playfair.glyphspackage";
    let source_paths = std::env::var("FONTC_BENCH_SOURCES")
        .ok()
        .map(|x| x.to_string())
        .unwrap_or(default_fonts_path.to_string());
    for source_path in source_paths.split(',') {
        let path = &resolve_path(&source_path);
        let bench_name = path.file_name().unwrap().to_string_lossy();
        bench_group.bench_function(bench_name.as_ref(), move |b| {
            b.iter(move || {
                let source = make_source(path).unwrap();
                let options = black_box(fontc::Options::default());
                fontc::generate_font(black_box(source), black_box(options)).unwrap()
            })
        });
    }
}

fn resolve_path(relative_path: impl AsRef<Path>) -> PathBuf {
    let base_path = std::env::var("FONTC_BENCH_DIR")
        .map(|x| Cow::Owned(x))
        .unwrap_or(Cow::Borrowed("../"));
    PathBuf::from_iter([Path::new(base_path.as_ref()), relative_path.as_ref()])
}

fn make_source(path: &Path) -> Result<Box<dyn fontir::source::Source>, fontc::Error> {
    let input = fontc::Input::new(&path)
        .inspect_err(|err| match std::env::current_dir() {
            Ok(current_dir) => {
                let resolved_path = PathBuf::from_iter([&current_dir, path]);
                error!(
                    r#"Failed to open {path:?} with current directory {current_dir:?}
Resolved path: {resolved_path:?}
Err: {err}"#
                );
            }
            Err(wd_err) => {
                error!(
                    r#"Failed to get current directory: {wd_err}
Failed to open {path:?}
Err: {err}"#
                );
            }
        })
        .unwrap();
    input.create_source()
}
