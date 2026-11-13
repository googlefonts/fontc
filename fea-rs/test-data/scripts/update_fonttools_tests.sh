#!/usr/bin/env bash

# Sync the feaLib test data from a fonttools release.
#
# usage: run from the test-data root, passing a fonttools release tag:
#
#     ./scripts/update_fonttools_tests.sh 4.68.0
#
# Only .fea files with a sibling .ttx are run by the harness, so upstream
# files without one (include0.fea, STAT_bad.fea, ...) are copied but ignored.
#
# IMPORTANT: after syncing, also update the fonttools version pinned in
# resources/scripts/fea_ci_requirements.txt to the same tag, so that CI's ttx
# matches the expected .ttx output.

set -euo pipefail

TAG=${1:?usage: $0 <fonttools release tag>}
TEMP_DIR=$(mktemp -d "${TMPDIR:-/tmp}/fonttools.XXXXXX")
REPO=https://github.com/fonttools/fonttools.git
TEST_DATA=$TEMP_DIR/Tests/feaLib/data
DEST_DIR=./fonttools-tests

git clone --depth 1 --branch "$TAG" $REPO $TEMP_DIR
# remove the previous sync so files deleted upstream don't linger
rm -f $DEST_DIR/*.fea $DEST_DIR/*.ttx
cp $TEST_DATA/*.fea $DEST_DIR
cp $TEST_DATA/*.ttx $DEST_DIR
rm -rf $TEMP_DIR
