#!/usr/bin/env bash

# run this script from the repo root
# there are currently two files we don't want:
# - includ0.fea (part of include tests, that we don't run)
# - STAT_bad.fea (an 'expected failure', which we don't currently handle)

TEMP_DIR=./temp
REPO=https://github.com/fonttools/fonttools.git
TEST_DATA=$TEMP_DIR/Tests/feaLib/data
DEST_DIR=./fonttools-tests

git clone $REPO $TEMP_DIR
cp $TEST_DATA/*.fea $DEST_DIR
cp $TEST_DATA/*.ttx $DEST_DIR
rm -rf $TEMP_DIR
