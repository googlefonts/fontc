#!/usr/bin/env python3
"""Helper for comparing fontc (Rust) vs fontmake (Python) binaries.

Turns each into ttx, eliminates expected sources of difference and prints
a brief summary of the result.

fontmake should be installed in an active virtual environment.

Usage:
    # rebuild with fontmake and fontc and compare
    python resources/scripts/ttx_diff.py ../OswaldFont/sources/Oswald.glyphs

    # rebuild the fontc copy, reuse the prior fontmake copy (if present), and compare
    # useful if you are making changes to fontc meant to narrow the diff
    python resources/scripts/ttx_diff.py --rebuild fontc ../OswaldFont/sources/Oswald.glyphs
"""

from absl import app
from absl import flags
from lxml import etree
from pathlib import Path
import shutil
import subprocess
import sys
from typing import MutableSequence


_COMPARE_DEFAULTS = "default"
_COMPARE_GFTOOLS = "gftools"


FLAGS = flags.FLAGS


flags.DEFINE_enum(
    "compare",
    "both",
    ["both", _COMPARE_DEFAULTS, _COMPARE_GFTOOLS],
    "Compare results with default flags, with the flags gftools uses, or both. Default both. Note that as of 5/21/2023 defaults still sets flags for fontmake to match fontc behavior.",
)
flags.DEFINE_enum(
    "rebuild",
    "both",
    ["both", "fontc", "fontmake", "none"],
    "Which compilers to rebuild with if the output appears to already exist. None is handy when playing with ttx_diff.py itself.",
)
flags.DEFINE_float(
    "off_by_one_budget",
    0.1,
    "The percentage of point (glyf) or delta (gvar) values allowed to differ by one without counting as a diff",
)


def run(cmd: MutableSequence, working_dir: Path, log_file: str, **kwargs):
    cmd_string = " ".join(cmd)
    print(f"  (cd {working_dir} && {cmd_string} > {log_file} 2>&1)")
    log_file = working_dir / log_file
    with open(log_file, "w") as log_file:
        subprocess.run(
            cmd,
            text=True,
            check=True,
            cwd=working_dir,
            stdout=log_file,
            stderr=log_file,
            **kwargs,
        )


def ttx(font_file: Path):
    if font_file.suffix == ".ttx":
        return font_file
    ttx_file = font_file.with_suffix(".ttx")
    cmd = [
        "ttx",
        "-o",
        ttx_file.name,
        font_file.name,
    ]
    run(cmd, font_file.parent, "ttx.log")
    return ttx_file


# generate a simple text repr for gpos for this font
def simple_gpos_output(font_file: Path, out_path: Path):
    temppath = font_file.parent / "markkern.txt"
    cmd = [
        "cargo",
        "run",
        "-p",
        "layout-normalizer",
        "--",
        font_file.name,
        "-o",
        temppath.name,
        "--table",
        "gpos",
    ]
    run(
        cmd,
        font_file.parent,
        "kernmark.log",
    )
    copy(temppath, out_path)
    with open(out_path) as f:
        return f.read()


# run a font compiler, returning the path of the generated font
def build(
    cmd: MutableSequence, build_dir: Path, build_tool: str, ttf_find_fn, **kwargs
):
    try_skip = FLAGS.rebuild not in [build_tool, "both"]
    ttfs = ttf_find_fn()
    if try_skip and len(ttfs) == 1:
        ttx_file = ttfs[0].with_suffix(".ttx")
        if ttx_file.is_file():
            print(f"skipping {build_tool}")
            return ttx_file
    run(cmd, build_dir, build_tool + ".log", **kwargs)
    ttfs = ttf_find_fn()
    assert len(ttfs) == 1, ttfs
    return ttfs[0]


def build_fontc(source: Path, build_dir: Path, compare: str):
    cmd = [
        "cargo",
        "run",
        "-p",
        "fontc",
        "--",
        # uncomment this to compare output w/ fontmake --keep-direction
        # "--keep-direction",
        # no longer required, still useful to get human-readable glyph names in diff
        "--no-production-names",
        "--build-dir",
        ".",
        str(source),
    ]
    if compare == _COMPARE_GFTOOLS:
        cmd.append("--flatten-components")
        cmd.append("--decompose-transformed-components")
    return build(cmd, build_dir, "fontc", lambda: (build_dir / "font.ttf",))


def build_fontmake(source: Path, build_dir: Path, compare: str):
    cmd = [
        "fontmake",
        "-o",
        "variable",
        "--output-dir",
        str(source.name),
        "--drop-implied-oncurves",
        # "--keep-direction",
        # no longer required, still useful to get human-readable glyph names in diff
        "--no-production-names",
        # helpful for troubleshooting
        "--debug-feature-file",
        "debug.fea",
    ]
    if compare == _COMPARE_GFTOOLS:
        cmd += [
            "--filter",
            "FlattenComponentsFilter",
            "--filter",
            "DecomposeTransformedComponentsFilter",
        ]
    cmd.append(str(source))

    return build(
        cmd,
        build_dir,
        "fontmake",
        lambda: tuple((build_dir / source.name).rglob("*.ttf")),
    )


def copy(old, new):
    shutil.copyfile(old, new)
    return new


def name_id_to_name(ttx, xpath, attr):
    id_to_name = {
        el.attrib["nameID"]: el.text.strip()
        for el in ttx.xpath(
            "//name/namerecord[@platformID='3' and @platEncID='1' and @langID='0x409']"
        )
    }
    for el in ttx.xpath(xpath):
        if attr not in el.attrib:
            continue
        # names <= 255 have specific assigned slots, names > 255 not
        name_id = int(el.attrib[attr])
        if name_id <= 255:
            continue
        el.attrib[attr] = id_to_name[el.attrib[attr]]


def find_table(ttx, tag):
    return select_one(ttx, f"/ttFont/{tag}")


def select_one(container, xpath):
    el = container.xpath(xpath)
    if len(el) != 1:
        raise IndexError(f"Wanted 1 name element, got {len(el)}")
    return el[0]


def drop_weird_names(ttx):
    drops = list(
        ttx.xpath(
            "//name/namerecord[@platformID='1' and @platEncID='0' and @langID='0x0']"
        )
    )
    for drop in drops:
        drop.getparent().remove(drop)


def erase_modified(ttx):
    el = select_one(ttx, "//head/modified")
    del el.attrib["value"]


def erase_checksum(ttx):
    el = select_one(ttx, "//head/checkSumAdjustment")
    del el.attrib["value"]


def stat_like_fontmake(ttx):
    try:
        el = find_table(ttx, "STAT")
    except IndexError:
        return
    ver = select_one(el, "Version")
    if ver.attrib["value"] != "0x00010002":
        # nop
        return

    # fontc likes to write STAT 1.2, fontmake prefers 1.1
    # Version 1.2 adds support for the format 4 axis value table
    # So until such time as we start writing format 4 axis value tables it doesn't matter
    ver.attrib["value"] = "0x00010001"


def allow_some_off_by_ones(
    build_dir, fontc, fontmake, container, name_attr, coord_holder
):
    fontmake_num_coords = len(fontmake.xpath(f"//{container}/{coord_holder}"))
    off_by_one_budget = int(FLAGS.off_by_one_budget / 100.0 * fontmake_num_coords)
    spent = 0
    if off_by_one_budget == 0:
        return

    coord_tag = coord_holder.rpartition("/")[-1]

    for fontmake_container in fontmake.xpath(f"//{container}"):
        name = fontmake_container.attrib[name_attr]
        fontc_container = select_one(fontc, f"//{container}[@{name_attr}='{name}']")

        for (fontmake_el, fontc_el) in zip(fontmake_container.iter(), fontc_container.iter()):
            if fontmake_el.tag != fontc_el.tag:
                break
            if fontmake_el.tag != coord_tag:
                continue

            for attr in ("x", "y"):
                delta_x = abs(
                    float(fontmake_el.attrib[attr]) - float(fontc_el.attrib[attr])
                )
                if 0.0 < delta_x <= 1.0:
                    fontc_el.attrib["adjusted"] = "1"
                    fontmake_el.attrib["adjusted"] = "1"
                    fontc_el.attrib[attr] = fontmake_el.attrib[attr]
                    spent += 1
                if spent >= off_by_one_budget:
                    print(
                        f"WARN: ran out of budget ({off_by_one_budget}) to fix off-by-ones in {container}"
                    )
                    return

    if spent > 0:
        print(
            f"INFO fixed {spent} off-by-ones in {container} (budget {off_by_one_budget})"
        )


def reduce_diff_noise(build_dir, fontc, fontmake):
    for ttx in (fontc, fontmake):
        # different name ids with the same value is fine
        name_id_to_name(ttx, "//NamedInstance", "subfamilyNameID")

        # deal with https://github.com/googlefonts/fontmake/issues/1003
        drop_weird_names(ttx)

        # it's not at all helpful to see modified off by a second or two in a diff
        erase_modified(ttx)

        # for matching purposes checksum is just noise
        erase_checksum(ttx)

        stat_like_fontmake(ttx)

    allow_some_off_by_ones(
        build_dir, fontc, fontmake, "glyf/TTGlyph", "name", "/contour/pt"
    )
    allow_some_off_by_ones(
        build_dir, fontc, fontmake, "gvar/glyphVariations", "glyph", "/tuple/delta"
    )


def main(argv):
    if len(argv) != 2:
        sys.exit("Only one argument, a source file, is expected")

    source = Path(argv[1])
    if not source.is_file():
        sys.exit(f"No such file: {source}")

    root = Path(".").resolve()
    if root.name != "fontc":
        sys.exit("Expected to be at the root of fontc")

    if shutil.which("fontmake") is None:
        sys.exit("No fontmake")
    if shutil.which("ttx") is None:
        sys.exit("No ttx")

    comparisons = (FLAGS.compare,)
    if comparisons == ("both",):
        comparisons = (_COMPARE_DEFAULTS, _COMPARE_GFTOOLS)

    for compare in comparisons:
        build_dir = (root / "build" / compare).relative_to(root)
        build_dir.mkdir(parents=True, exist_ok=True)
        print(f"Compare {compare} in {build_dir}")

        fontc_ttf = build_fontc(source.resolve(), build_dir, compare)
        fontc_ttx = copy(ttx(fontc_ttf), build_dir / "fontc.ttx")
        fontmake_ttf = build_fontmake(source.resolve(), build_dir, compare)
        fontmake_ttx = copy(ttx(fontmake_ttf), build_dir / "fontmake.ttx")
        fontc_gpos = simple_gpos_output(fontc_ttf, build_dir / "fontc.markkern.txt")
        fontmake_gpos = simple_gpos_output(
            fontmake_ttf, build_dir / "fontmake.markkern.txt"
        )

        print("TTX FILES")
        print("  fontc    ", fontc_ttx)
        print("  fontmake ", fontmake_ttx)

        fontc = etree.parse(fontc_ttx)
        fontmake = etree.parse(fontmake_ttx)

        reduce_diff_noise(build_dir, fontc, fontmake)

        print("COMPARISON")
        t1 = {e.tag for e in fontc.getroot()}
        t2 = {e.tag for e in fontmake.getroot()}
        if t1 != t2:
            if t1 - t2:
                tags = ", ".join(f"'{t}'" for t in sorted(t1 - t2))
                print(f"  Only fontc produced {tags}")

            if t2 - t1:
                tags = ", ".join(f"'{t}'" for t in sorted(t2 - t1))
                print(f"  Only fontmake produced {tags}")

        t1e = {e.tag: e for e in fontc.getroot()}
        t2e = {e.tag: e for e in fontmake.getroot()}
        for tag in sorted(t1 & t2):
            t1s = etree.tostring(t1e[tag])
            t2s = etree.tostring(t2e[tag])
            p1 = build_dir / f"fontc.{tag}.ttx"
            p1.write_bytes(t1s)
            p2 = build_dir / f"fontmake.{tag}.ttx"
            p2.write_bytes(t2s)
            if t1s == t2s:
                print(f"  Identical '{tag}'")
            else:
                print(f"  DIFF '{tag}', {p1} {p2}")
                if tag == "GPOS":
                    p1 = build_dir / "fontc.markkern.txt"
                    p2 = build_dir / "fontmake.markkern.txt"
                    print(f"  (mark/kern)  {p1} {p2}")


if __name__ == "__main__":
    app.run(main)
