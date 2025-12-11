# Performance

## Benchmarking with Criterion

fontc uses [criterion](https://crates.io/crates/criterion) for benchmarking. To run the benchmarks, use the following command:

```shell
cargo bench -p fontc --bench compile
```

This will run the benchmarks on a default set of fonts. You can customize the benchmark execution using the following environment variables:

- `FONTC_BENCH_DIR`: Specifies the directory where the font sources are
  located. This path is relative to the `fontc` crate directory. The default
  value is `../`.
- `FONTC_BENCH_SRCS`: A comma-separated list of font sources to compile. For
  example:
  `FONTC_BENCH_SRCS=OswaldFont/sources/Oswald.glyphs,Playfair/sources/Playfair.glyphspackage`

Example usage with custom fonts:

```shell
FONTC_BENCH_DIR=/path/to/fonts FONTC_BENCH_SRCS=MyFont/sources/MyFont.glyphs cargo bench -p fontc --bench compile
```

Criterion generates a report with detailed performance data, which can be found
in `target/criterion/report/index.html`. This report is useful for comparing the
performance of different branches or changes.

### Comparing branches

To compare the performance between `main` and your feature branch you can use baselines.

```shell
# On main, establish a baseline.
git checkout main
cargo bench -p fontc --bench compile -- --save-baseline main

# On your feature branch, compare to the baseline
git checkout your-feature-branch
cargo bench -p fontc --bench compile -- --baseline main
```

## Running samply

https://github.com/mstange/samply gives a nice call tree, flame graph, etc. Sample usage:

```shell
# Assuming current directory is the root of fontc
$ (cd .. && git clone https://github.com/mstange/samply)
$ (cd ../samply && cargo build --release)
$ ../samply/target/release/samply record target/release/fontc ../OswaldFont/sources/Oswald.glyphs
```

## Running flamegraph

[flamegraphs](https://www.brendangregg.com/flamegraphs.html) of fontc are very handy. They are most
easily created using `cargo flamegraph`:

```shell

# Minimize the impact of logging
$ export RUST_LOG=error
# Symbols are nice, https://github.com/flamegraph-rs/flamegraph#improving-output-when-running-with---release
$ export CARGO_PROFILE_RELEASE_DEBUG=true

# Build something and capture a flamegraph of it
$ rm -rf build/ perf.data flamegraph.svg && cargo flamegraph -p fontc -- ../OswaldFont/sources/Oswald.glyphs

# TIPS

# On macOS you might have to pass `--root` to cargo flamegraph, e.g. cargo flamegraph --root ...as above...

# If you are losing samples you might want to dial down the rayon threadcount
# You'll see a perf error similar to:
Warning:
Processed 5114 events and lost 159 chunks!

Check IO/CPU overload!

Warning:
Processed 5116 samples and lost 35.01%!

# Fix is to lower the threadcount:
$ export RAYON_NUM_THREADS=16
```

### Focused flames

https://blog.anp.lol/rust/2016/07/24/profiling-rust-perf-flamegraph/ offers examples of filtering flamegraphs. This
is very useful when you want to zoom in on a specific operation. For example, to dig into fea-rs:

```shell
# Generate a perf.data
# You can also use perf record but cargo flamegraph seems to have nice capture settings for Rust rigged
$ rm -rf build/ perf.data flamegraph.svg && cargo flamegraph -p fontc -- ../OswaldFont/sources/Oswald.glyphs

# ^ produced flamegraph.svg but it's very noisy, lets narrow our focus
# Example assumes https://github.com/brendangregg/FlameGraph is cloned in a sibling directory to fontc
$ perf script | ../FlameGraph/stackcollapse-perf.pl | grep fea_rs | ../FlameGraph/flamegraph.pl > fea-flame.svg
```
