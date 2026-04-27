//! Run benchmarks with `cargo bench -p fontc --bench compile`.
//!
//! See `docs/performance.md` for more details on benchmarking and profiling.

use std::path::Path;
use std::{hint::black_box, path::PathBuf};

use criterion::{Criterion, SamplingMode, criterion_group, criterion_main};
use rayon::prelude::*;

criterion_group!(benches, compile_benchmark);
criterion_main!(benches);

#[derive(Clone)]
struct FontSource<'a> {
    dir: &'a str,
    git_url: Option<&'a str>,
    source_path: &'a str,
}

const DEFAULT_FONT_SOURCES: &[FontSource<'static>] = &[
    // Compiles quickly.
    FontSource {
        dir: "/tmp/fontc-bench/oswald",
        git_url: Some("https://github.com/googlefonts/OswaldFont.git"),
        source_path: "sources/Oswald.glyphs",
    },
    // May take a few seconds. Based on threads.svg from --emit-timing, bottlenecked on kern-be,
    // kern-gather-be, fea, and font.
    FontSource {
        dir: "/tmp/fontc-bench/merriweather4",
        git_url: Some("https://github.com/EbenSorkin/Merriweather4.git"),
        source_path: "sources/Merriweather.glyphspackage",
    },
];

fn compile_benchmark(c: &mut Criterion) {
    env_logger::builder().is_test(true).try_init().ok();
    let mut bench_group = c.benchmark_group("fontc-compile");
    // Criterion requires at least 10 samples. The compile time of fonts is large enough that 10
    // samples is enough to get an idea of acceptable performance.
    bench_group
        .sample_size(10)
        .sampling_mode(SamplingMode::Flat);

    let env_srcs = std::env::var("FONTC_BENCH_SRCS").ok();
    let repos = env_srcs
        .as_ref()
        .map(|srcs| parse_srcs(srcs.as_str()))
        .unwrap_or(DEFAULT_FONT_SOURCES.to_vec());

    setup_srcs(&repos);

    for repo in &repos {
        let path = Path::new(repo.dir).join(repo.source_path);
        let bench_name = path.to_string_lossy();
        bench_group.bench_function(bench_name, |b| {
            b.iter(|| {
                let source = make_source(&path);
                let options = black_box(fontc::Options::default());
                fontc::generate_font(black_box(source), black_box(options))
                    .unwrap_or_else(|err| panic!("Failed to run benchmark for {path:?}: {err}"))
            })
        });
    }
}

fn setup_srcs(srcs: &[FontSource]) {
    if srcs.iter().any(|r| r.git_url.is_some())
        && let Err(e) = std::fs::create_dir_all("/tmp/fontc-bench")
    {
        eprintln!("Failed to create benchmark directory: {}", e);
    }

    srcs.into_par_iter().for_each(|src| {
        if let Some(url) = src.git_url {
            let path: &Path = Path::new(src.dir);
            if !path.exists() {
                println!("Cloning {} into {}...", url, src.dir);
                let status = std::process::Command::new("git")
                    .arg("clone")
                    .arg(url)
                    .arg(src.dir)
                    .status();

                match status {
                    Ok(status) if status.success() => println!("Successfully cloned {}", src.dir),
                    _ => {
                        eprintln!("Failed to clone {}. Skipping.", src.dir);
                        return;
                    }
                }
            }
        }

        let path = Path::new(src.dir).join(src.source_path);
        if !path.exists() {
            panic!("Source path {} does not exist.", path.display());
        }
    });
}

fn parse_srcs(srcs: &'_ str) -> Vec<FontSource<'_>> {
    srcs.split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(|s| {
            let path = Path::new(s);
            let parent = path.parent().and_then(|p| p.to_str()).unwrap_or(".");
            let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            FontSource {
                dir: if parent.is_empty() { "." } else { parent },
                git_url: None,
                source_path: file_name,
            }
        })
        .collect()
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
        .create_source(&fontc::Options::default())
        .unwrap_or_else(|err| panic!("Unable to create source for {path:?}: {err}"))
}
