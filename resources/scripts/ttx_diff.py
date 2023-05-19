#!/usr/bin/env python3
"""Helper for comparing fontc (Rust) vs fontmake (Python) binaries.

Turns each into ttx, eliminates expected sources of difference and prints
a brief summary of the result.

fontmake should be installed in an active virtual environment.

Usage:
    # rebuild with fontmake and fontc and compare
    python resources/scripts/ttx_diff.py ../OswaldFont/sources/Oswald.glyphs
    # rebuild the fontc copy, reuse the prior fontmake copy (if present), and compare
    python resources/scripts/ttx_diff.py --rebuild fontc ../OswaldFont/sources/Oswald.glyphs
"""

from absl import app
from absl import flags
from lxml import etree
from pathlib import Path
import shutil
import subprocess
import sys


FLAGS = flags.FLAGS


flags.DEFINE_enum('rebuild', 'both', ['both', 'fontc', 'fontmake'], 'Which compilers to rebuild with if the output appears to already exist')


def run(cmd, **kwargs):
    cmd_string = " ".join(cmd)
    print(f"Running {cmd_string}")
    subprocess.run(cmd, text=True, check=True, **kwargs)


def ttx(font_file):
    ttx_file = font_file.with_suffix(".ttx")
    cmd = [
        "ttx",
        "-o",
        str(ttx_file),
        str(font_file),
    ]
    run(cmd)
    return ttx_file


def build(cmd, build_tool, ttf_find_fn, **kwargs):
    try_skip = FLAGS.rebuild not in [build_tool, "both"]
    ttfs = ttf_find_fn()
    if try_skip and len(ttfs) == 1:
        ttx_file = ttfs[0].with_suffix(".ttx")
        if ttx_file.is_file():
            print(f"skipping {build_tool}")
            return ttx_file 
    run(cmd, **kwargs)
    ttfs = ttf_find_fn()
    assert len(ttfs) == 1, ttfs
    return ttx(ttfs[0])


def build_fontc(source, build_dir):    
    cmd = [
        "cargo",
        "run",
        "-p",
        "fontc",
        "--",
        "--source",
        str(source),
        "--build-dir",
        str(build_dir),
    ]
    return build(cmd, "fontc", lambda: (build_dir / "font.ttf",))


def build_fontmake(source, build_dir):
    cmd = [
        "fontmake",
        "-o",
        "variable",
        "--no-production-names",
        "--keep-direction",
        "--filter",
        "FlattenComponentsFilter",
        "--filter",
        "DecomposeTransformedComponentsFilter",
        str(source),
    ]
    return build(cmd, "fontmake", lambda: tuple((build_dir /"variable_ttf").rglob("*.ttf")), cwd=build_dir)


def copy(old, new):
    shutil.copyfile(old, new)
    return new


def main(argv):
    if len(argv) != 2:
        sys.exit("Only one argument, a source file, is expected")

    source = Path(argv[1])
    if not source.is_file():
        sys.exit(f"No such file: {source}")

    root = Path(".").resolve()
    if root.name != "fontmake-rs":
        sys.exit("Expected to be at the root of fontmake-rs")

    build_dir = (root / "build").relative_to(root)
    build_dir.mkdir(exist_ok = True)

    if shutil.which("fontmake") is None:
        sys.exit("No fontmake")
    if shutil.which("ttx") is None:
        sys.exit("No ttx")

    fontc_ttx = copy(build_fontc(source, build_dir), build_dir / "fontc.ttx")
    fontmake_ttx = copy(build_fontmake(source.resolve(), build_dir), build_dir / "fontmake.ttx")

    print("TTX FILES")
    print("  fontc    ", fontc_ttx)
    print("  fontmake ", fontmake_ttx)

    fontc = etree.parse(fontc_ttx)
    fontmake = etree.parse(fontmake_ttx)

    print("COMPARISON")
    t1 = {e.tag for e in fontc.getroot()}
    t2 = {e.tag for e in fontmake.getroot()}
    if t1 != t2:
        if t1 -t2:
            tags = ", ".join(f"'{t}'" for t in sorted(t1 -t2))
            print(f"  Only fontc produced {tags}")

        if t2 -t1:
            tags = ", ".join(f"'{t}'" for t in sorted(t2 - t1))
            print(f"  Only fontmake produced {tags}")

    t1e = {e.tag: e for e in fontc.getroot()}
    t2e = {e.tag: e for e in fontmake.getroot()}
    for tag in sorted(t1 & t2):
        if etree.tostring(t1e[tag]) == etree.tostring(t2e[tag]):
            print(f"  Identical '{tag}'")
        else:
            print(f"  DIFF '{tag}'")


if __name__ == "__main__":
    app.run(main)
