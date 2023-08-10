# fontc
Where in we pursue oxidizing (context: https://github.com/googlefonts/oxidize) fontmake.

Converts source to IR, and then IR to font binary. Aims to be safe, incremental, and fast.

References

   * Intermediate Representation (IR)
      * [Why IR?](https://github.com/googlefonts/oxidize/blob/main/text/2022-11-14-why-ir.md)
      * [IR notes](https://github.com/googlefonts/oxidize/blob/main/text/2022-11-08-font-compiler-ir.md).
   * Editor perspective [note from Just](https://github.com/googlefonts/oxidize/issues/21)
   * [Units](resources/text/units.md)
      * Fonts have all the best units; distinguishing between them turns out to matter.

## Plan

As of 6/4/2023 we intend to:

* Get to the point where Oswald compilation matches fontmake
   * https://github.com/googlefonts/fontc/milestone/4
* Get to the point where ever more of the families for which we have source compile
  to a form that matches fontmake, or differs only in well understood ways
* Provide a Glyphs plugin to allow push-button use of the new compiler
* Once there are no known issues, switch Google Fonts to exclusively use fontc

We are discarding our prior plan to make fontmake (Python) call into Rust as it appears to be
more complex than initially anticipated and higher risk than migrating on a per-family basis.

For context see https://github.com/googlefonts/oxidize/blob/main/text/2022-07-25-PROPOSAL-build-glyphs-in-rust.md and the discussion on https://github.com/googlefonts/oxidize/pull/33.

## Sources to play with

Google Fonts has lots, you could try https://github.com/rsheeter/google_fonts_sources to get some.
Once you have them you could try building them:

```shell
cargo run --package fontc -- --source ../google_fonts_sources/sources/ofl/notosanskayahli/sources/NotoSansKayahLi.designspace
```

## Using a local copy of fontations

It is quite common to find we need changes in https://github.com/googlefonts/fontations to add a feature
or fix a bug. Prior to a release being available modify the root `Cargo.toml` to point to either a local
clone or a branch:

```toml
# Local copy
[patch.crates-io]
font-types =  { path = "../fontations/font-types" }
read-fonts =  { path = "../fontations/read-fonts" }
write-fonts = { path = "../fontations/write-fonts" }
skrifa =  { path = "../fontations/skrifa" }

# Branch
[patch.crates-io]
font-types = { git="https://github.com/googlefonts/fontations.git", branch="box" }
read-fonts = { git="https://github.com/googlefonts/fontations.git", branch="box" }
write-fonts = { git="https://github.com/googlefonts/fontations.git", branch="box" }
skrifa = { git="https://github.com/googlefonts/fontations.git", branch="box" }
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
$ rm -rf build/ && cargo flamegraph -p fontc --  --source ../OswaldFont/sources/Oswald.glyphs --emit-ir false

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

## Contributing

We have included a few git hooks that you may choose to use to ensure that
patches will pass CI; these are in `resources/githooks`.

To run the pre-push step manually:

```sh
$ ./resources/githooks/pre-push
```

If you would like to have these run automatically when you commit or push
changes, you can set this as your git hooksPath:

```sh
$ git config core.hooksPath "resources/githooks"
```


## Releasing

See https://github.com/googlefonts/fontations#releasing
