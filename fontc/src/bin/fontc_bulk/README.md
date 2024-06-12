# fontc_bulk

```sh
$ cargo run --release --bin=fontc_bulk -- compile FONT_CACHE --fonts-repo GOOGLE/FONTS  -o results.json
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

[google-fonts-sources]: https://github.com/googlefonts/google-fonts-sources
[google/fonts]: https://github.com/google/fonts
