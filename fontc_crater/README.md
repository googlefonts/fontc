# `fontc_crater`

The `fontc_crater` crate (named after [rust-lang/crater]) is a tool for
performing compilation and related actions across a large number of source
fonts.



```sh
# By default font repositories and results will be written to ~/.fontc_crater_cache

# Just see if we can compile with fontc
$ cargo run --release -p=fontc_crater -- compile

# To take it for a test-spin do a limited # of fonts
$ cargo run --release -p=fontc_crater -- compile --limit 8

# Build with fontmake and fontc and compare the results with ttx_diff
$ cargo run --release -p=fontc_crater -- diff
```

This is a binary for executing font compilation (and possibly other tasks) in
bulk.

Discovery of font sources is managed by a separate tool,
[google-fonts-sources][]; this tool checks out the
[github.com/google/fonts][google/fonts] repository and looks for fonts with
known source repos. You can use the `--fonts-repo` argument to pass the path to
an existing checkout of this repository, which saves time.

Once sources are identified, they are checked out into the `FONT_CACHE`
directory, where they can be reused between runs.

## output

For detailed output, use the `-o/--out` flag to specify a path where we should
dump a json dictionary containing the outcome for each source.

## reports

You can generate a report from the saved json by passing it back to
`fontc_crater`:

```sh
$ cargo run -p fontc_crater -- report
```

## CI

This binary is also run in CI. In that case, the execution is managed by a
script in the [`fontc_crater` repo][crater-repo] and results are posted to
[github pages][crater-results].

To run in CI mode locally to play with the html output:

```shell
# clone git@github.com:googlefonts/fontc_crater.git somewhere, we'll assume at ../fontc_crater
# CI currently has it's own format for the repo list, use the file from ^

$ cargo run --release -p=fontc_crater -- ci ../fontc_crater/gf-repos-2024-08-12.json -o ../fontc_crater/results/ --html-only

# Review ../fontc_crater/results/index.html (NOT ../fontc_crater/index.html)
```

[google-fonts-sources]: https://github.com/googlefonts/google-fonts-sources
[google/fonts]: https://github.com/google/fonts
[rust-lang/crater]: https://github.com/rust-lang/crater
[crater-repo]: https://github.com/googlefonts/fontc_crater
[crater-results]: https://googlefonts.github.io/fontc_crater/


