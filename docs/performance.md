# Performance

## Benchmarking with Criterion


Only relatively large improvements or regressions are effectively detected this
way: fontc uses [criterion](https://crates.io/crates/criterion) for
benchmarking. Use the following command to run `fontc/benches/compile.rs`:

```shell
cargo bench -p fontc --bench compile
```

By default, benchmarks will run on a set of pre-defined fonts sources that will
be automatically cloned into `/tmp/fontc-bench` if they are not already present.

It takes roughly 3 minutes to run the benchmark suite.

Example output:

```
    Finished `bench` profile [optimized] target(s) in 0.14s
     Running benches/compile.rs (target/release/deps/compile-23dad87041a7d0fa)
Benchmarking fontc-compile//tmp/fontc-bench/oswald/sources/Oswald.glyphs: Warming up for 3.0000 s
fontc-compile//tmp/fontc-bench/oswald/sources/Oswald.glyphs
                        time:   [85.787 ms 86.311 ms 86.864 ms]
                        change: [+1.4202% +2.2889% +3.2090%] (p = 0.00 < 0.05)
                        Performance has regressed.
Benchmarking fontc-compile//tmp/fontc-bench/googlesans/source/GoogleSans/GoogleSans.designspace: Warming up for 3.0000 s
fontc-compile//tmp/fontc-bench/googlesans/source/GoogleSans/GoogleSans.designspace
                        time:   [4.2918 s 4.3509 s 4.3988 s]
                        change: [+0.1788% +1.8077% +3.1613%] (p = 0.03 < 0.05)
                        Change within noise threshold.
Benchmarking fontc-compile//tmp/fontc-bench/googlesans-flex/sources/GoogleSansFlex.designspace: Warming up for 3.0000 s
Warning: Unable to complete 10 samples in 5.0s. You may wish to increase target time to 120.0s.
fontc-compile//tmp/fontc-bench/googlesans-flex/sources/GoogleSansFlex.designspace
                        time:   [12.072 s 12.211 s 12.329 s]
```

To run benchmarks on your own set of fonts, you can set `FONTC_BENCH_SRCS` to a
comma-separated list of paths:

```shell
FONTC_BENCH_SRCS=OswaldFont/sources/Oswald.glyphs cargo bench -p fontc --bench compile
```

Criterion also generates a report with detailed performance data, which can be
found in `target/criterion/report/index.html`.

### Comparing branches

To compare the performance between `main` and your feature branch you can use
baselines.

```shell
# On main, establish a baseline.
git checkout main
cargo bench -p fontc --bench compile -- --save-baseline main

# On your feature branch, compare to the baseline
git checkout your-feature-branch
cargo bench -p fontc --bench compile -- --baseline main
```

This outputs readable results to the command line and
`target/criterion/report/index.html`.

## Comparing branch performance with hyperfine

Similar to Criterion comparisons, but uses the `fontc` cli tool.

```shell
To compare current branch to main when building Oswald:

$ hyperfine \
  --parameter-list branch main,$(git rev-parse --abbrev-ref HEAD) \
  --setup "git checkout {branch} && cargo build --release" \
   --warmup 5 --runs 250 \
   --prepare 'rm -rf build/' \
   "target/release/fontc --source ../OswaldFont/sources/Oswald.glyphs"

...noise...

Summary
  target/release/fontc --source ../OswaldFont/sources/Oswald.glyphs (branch = nocs) ran
    1.09 ± 0.09 times faster than target/release/fontc --source ../OswaldFont/sources/Oswald.glyphs (branch = main)

# Yay, it seems to be faster!
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
