#!/bin/bash

# This script is used to test that fontc produces repeatable builds.

if [ -z "$1" ]; then
  echo "Usage: repeatable-builds.sh FONTFILE [NUM_TIMES]"
  exit 1
fi

INPUT_SOURCE="$1"
filename=$(basename "$INPUT_SOURCE")
first="build/${filename%.*}.ttf"
# this is to ensure head.modified does not change
export SOURCE_DATE_EPOCH=$(date +%s)

build_font() {
  echo "$ fontc $1 -o $2"
  fontc "$1" -o "$2"
  if [ $? -ne 0 ]; then
    echo "Error: building $1 failed"
    exit 1
  fi
}

compare_fonts() {
  echo "$ cmp $1 $2"
  cmp "$1" "$2"
  if [ $? -ne 0 ]; then
    # tail -n +2 to skip past the first line because it emits the filename
    diff -u <(ttx -l "$1" | tail -n +2) <(ttx -l "$2" | tail -n +2)
    echo "Error: $1 and $2 are different"
    exit 1
  fi
}

echo "$ which fontc"
which fontc

build_font "$INPUT_SOURCE" "$first"

for i in $(seq 1 ${2:-1})
do
  ith="build/${filename%.*}#$i.ttf"
  build_font "$INPUT_SOURCE" "$ith"
  compare_fonts $first $ith
done

echo "Success: all fonts are identical"
