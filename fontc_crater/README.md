# `fontc_crater`

The `fontc_crater` crate (named after [rust-lang/crater]) is a tool for
performing compilation and related actions across a large number of source
fonts.

Discovery of font sources is managed by a separate tool,
[google-fonts-sources][]; this tool checks out the
[github.com/google/fonts][google/fonts] repository and looks for fonts with
known source repos. You can use the `--fonts-repo` argument to pass the path to
an existing checkout of this repository, which saves time.

Once sources are identified, they are checked out into a cache directory, where
they can be reused between runs.

## CI

This tool is mainly used in CI. In that case, the execution is managed by a
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


