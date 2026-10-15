# `fontc_crater`

[![Crates.io](https://img.shields.io/crates/v/fontc_crater.svg)](https://crates.io/crates/fontc_crater)
[![Docs.rs](https://docs.rs/fontc_crater/badge.svg)](https://docs.rs/fontc_crater)
[![MIT/Apache 2.0](https://img.shields.io/badge/license-MIT%2FApache-blue.svg)](#license)

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

The tool processes and generates JSON outputs for tracking successful and failed font compilations across targets, in addition to an optional HTML report.

## CI

This tool is mainly used in CI. In that case, the execution is managed by a
script in the [`fontc_crater` repo][crater-repo] and results are posted to
[github pages][crater-results].

The report also shows the progress of the current run, or how the last one
ended. If `FONTC_CRATER_PROGRESS_FILE` is set, `ci.sh` passes it to crater as
`--progress-file`, and crater rewrites that file as targets finish.
`run.sh` combines it with its own state into `status.json`, which it
force-pushes every minute to the `status` branch of the repo it publishes
results to. It passes that file's url as `FONTC_CRATER_STATUS_URL`, which
`ci.sh` passes on as `--status-url`, and the report polls it. Without a url,
the report just says that status isn't configured, so local runs don't show
the status of some other instance.

To run in CI mode locally to play with the html output:

When run in CI, we want to ensure that we have complete control over the set of
python pacakges we are comparing against; to ensure this we use `pip-compile`
(part of [`pip-tools`]) to generate a pinned `requirements.txt` from a
`requirements.in` file in the root `resources/scripts` directory.

To update the set of python packages in use, you need to update this file:

```shell
# Make sure you have pip-tools
$ python -m pip install pip-tools

# Rebuild the file
# Remove it to force a complete update rather than minimal
$ rm -f resources/scripts/requirements.txt && pip-compile resources/scripts/requirements.in

# Commit updated file
```

To run locally to rebuild the HTML:

```shell
# clone git@github.com:googlefonts/fontc_crater.git somewhere, we'll assume at ../fontc_crater
# CI currently has it's own format for the repo list, use the file from ^

$ cargo run --release -p=fontc_crater -- ci ../fontc_crater/targets.json -o ../fontc_crater/results/ --html-only

# Review ../fontc_crater/results/index.html (NOT ../fontc_crater/index.html)
```

To do a full run locally, such as to test a PR (assuming same fontc_crater
checkout as above):

```shell
# make sure the repo is up to date, so you're comparing against the latest run:
# cd ../fontc_crater && git pull

$ RUST_LOG=debug cargo run --release -p fontc_crater \
-- ci ../fontc_crater/targets.json --out ../fontc_crater/results/

```

To test a change to python dependencies without invalidating the cache of
fontmake's output, pass `--no-results-cache` and a scratch `--out` directory. To
compare against an existing run, first copy its `summary.json` and the results
file its last entry names into the scratch directory.

```shell
$ cargo run --release -p fontc_crater \
-- ci ../fontc_crater/targets.json --out scratch-results/ --no-results-cache
```

### Accepted failures

Failures that have been reviewed and accepted can be listed in `accepted.json`
in the results directory. They count as matching in the summary, and are listed
separately from other failures.

```json
[
  {
    "targets": [
      "org/repo/sources/config.yaml Font.glyphs?0123456789 (default)"
    ],
    "fails": "both",
    "reason": "the source has no variable masters",
    "link": "https://github.com/googlefonts/fontc/issues/1234",
    "stderr_contains": "no masters"
  }
]
```

- `targets` are written as they appear in the report; the sha is ignored when
  matching, so an entry keeps applying when a source repo is updated.
- `fails` is the compiler whose failure is accepted: `fontc`, `fontmake` or
  `both`.
- `link` and `stderr_contains` are optional. If `stderr_contains` is set, the
  output of a failing compiler must contain it; only the last 1000 characters
  of output are kept.

A listed target that compiles, fails some other way, or isn't in the run is
shown at the top of the report, so that the list can be kept up to date.

[google-fonts-sources]: https://github.com/googlefonts/google-fonts-sources
[google/fonts]: https://github.com/google/fonts
[rust-lang/crater]: https://github.com/rust-lang/crater
[crater-repo]: https://github.com/googlefonts/fontc_crater
[crater-results]: https://googlefonts.github.io/fontc_crater/
[`pip-tools`]: https://github.com/jazzband/pip-tools
