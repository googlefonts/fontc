#!/usr/bin/env bash

# this script is responsible for running crater. It is expected to be called
# by `fetch_and_run.sh`.

echo "this is the script that will actually run CI"
# these are set by the script that calls us
echo "input is $FONTC_CRATER_INPUT, results written to $FONTC_CRATER_RESULTS"
cargo run -p fontc_crater --release -- ci $FONTC_CRATER_INPUT --out $FONTC_CRATER_RESULTS
