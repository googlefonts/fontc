#!/usr/bin/env bash

# this script is responsible for running crater. It is expected to be called
# by `fetch_and_run.sh`.

echo "this is the script that will actually run CI"
# these are set by the script that calls us
echo "input is $FONTC_CRATER_INPUT, results written to $FONTC_CRATER_RESULTS"
# optional; if set, crater writes its progress to the file, and the report reads
# the status that the calling script publishes from the url
STATUS_ARGS="${FONTC_CRATER_PROGRESS_FILE:+--progress-file $FONTC_CRATER_PROGRESS_FILE}"
STATUS_ARGS+=" ${FONTC_CRATER_STATUS_URL:+--status-url $FONTC_CRATER_STATUS_URL}"
cargo run -p fontc_crater --release -- ci $FONTC_CRATER_INPUT --out $FONTC_CRATER_RESULTS $STATUS_ARGS
