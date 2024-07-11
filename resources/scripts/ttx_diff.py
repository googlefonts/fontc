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

JSON:
    If the `--json` flag is passed, this tool will output JSON.

    If the script exits with a `0` status code, then `stdout` is a JSON dictionary.

    If both compilers ran successfully, this dictionary will have a single key,
    "success", which will contain a dictionary, where keys are the tags of tables
    (or another identifier) and the value is either a float representing the
    'difference ratio' (where 1.0 means identical and 0.0 means maximally
   dissimilar) or, if only one compiler produced that table, the name of that
    compiler as a string.
    For example, the output `{"success": { "GPOS": 0.99, "vmxt": "fontmake" }}`
    means that the "GPOS" table was 99% similar, and only `fontmake` produced
    the "vmtx" table (and all other tables were identical).

    If a compiler fails to exit successfully, we will return a dictionary with
    the single key, "error", where the payload is a dictionary with "source" and
    "message" fields; "source" is the name of the compiler that failed, and
    "message" is the contents of stderr, e.g.:
    `{"error": {"source": "fontmake", "message": "oh no!" }}`
"""

from absl import app
from absl import flags
from lxml import etree
from pathlib import Path
import json
import shutil
import subprocess
import sys
from difflib import SequenceMatcher
from typing import MutableSequence


_COMPARE_DEFAULTS = "default"
_COMPARE_GFTOOLS = "gftools"


FLAGS = flags.FLAGS
# used instead of a tag for the normalized mark/kern output
MARK_KERN_NAME = "(mark/kern)"


# we don't print to stdout of we're generating JSON
def maybe_print(*objects):
    if FLAGS.json:
        return
    print(*objects)


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
flags.DEFINE_bool(
    "json", False, "print results in machine-readable JSON format")


# execute a command in the provided working directory.
# The 'check' argument is passed to subprocess.run; if it is true than an
# exception will be raised if the command does not exit successfully; otherwise
# the caller can check the status via the returned `CompletedProcess` object.
def run(cmd: MutableSequence, working_dir: Path, check=False, **kwargs):
    cmd_string = " ".join(str(c) for c in cmd)
    maybe_print(f"  (cd {working_dir} && {cmd_string})")
    return subprocess.run(
        cmd,
        text=True,
        check=check,
        cwd=working_dir,
        capture_output=True,
        **kwargs,
    )


def ttx(font_file: Path, can_skip: bool):
    ttx_file = font_file.with_suffix(".ttx")
    if can_skip and ttx_file.is_file():
        return ttx_file

    cmd = [
        "ttx",
        "-o",
        ttx_file.name,
        font_file.name,
    ]
    run(cmd, font_file.parent, check=True)
    return ttx_file


# generate a simple text repr for gpos for this font
def simple_gpos_output(font_file: Path, out_path: Path, can_skip: bool):
    if not (can_skip and out_path.is_file()):
        temppath = font_file.parent / "markkern.txt"
        cmd = [
            "cargo",
            "run",
            "-p",
            "otl-normalizer",
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
            check=True,
        )
        copy(temppath, out_path)
    with open(out_path) as f:
        return f.read()


class BuildFail(Exception):
    """An exception raised if a compiler fails."""

    def __init__(self, cmd: MutableSequence, stderr: str):
        self.command = list(cmd)
        self.stderr = stderr


# run a font compiler
def build(
    cmd: MutableSequence, build_dir: Path, build_tool: str, **kwargs
):
    if can_skip(build_dir, build_tool):
        maybe_print((f"skipping {build_tool}"))
        return
    output = run(cmd, build_dir, **kwargs)
    if output.returncode != 0:
        raise BuildFail(cmd, output.stderr)


# return `true` if we can skip this build tool
def can_skip(build_dir: Path, build_tool: str) -> bool:
    try_skip = FLAGS.rebuild not in [build_tool, "both"]
    ttx_path = build_dir / (build_tool + ".ttx")
    return try_skip and ttx_path.is_file()


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
        "-o",
        "fontc.ttf",
        str(source),
    ]
    if compare == _COMPARE_GFTOOLS:
        cmd.append("--flatten-components")
        cmd.append("--decompose-transformed-components")
    return build(cmd, build_dir, "fontc")


def build_fontmake(source: Path, build_dir: Path, compare: str):
    cmd = [
        "fontmake",
        "-o",
        "variable",
        "--output-path",
        "fontmake.ttf",
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
                    maybe_print(
                        f"WARN: ran out of budget ({off_by_one_budget}) to fix off-by-ones in {container}"
                    )
                    return

    if spent > 0:
        maybe_print(
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


# returns a dictionary of {"compiler_name":  {"tag": "xml_text"}}
def generate_output(build_dir: Path, fontmake_ttf: Path, fontc_ttf: Path):
    # don't run ttx or otl-normalizer if we don't have to:
    can_skip_fontc = can_skip(build_dir, "fontc")
    can_skip_fontmake = can_skip(build_dir, "fontmake")
    fontc_ttx = ttx(fontc_ttf, can_skip_fontc)
    fontmake_ttx = ttx(fontmake_ttf, can_skip_fontmake)
    fontc_gpos = simple_gpos_output(
        fontc_ttf, build_dir / "fontc.markkern.txt", can_skip_fontc)
    fontmake_gpos = simple_gpos_output(
        fontmake_ttf, build_dir / "fontmake.markkern.txt", can_skip_fontmake
    )

    fontc = etree.parse(fontc_ttx)
    fontmake = etree.parse(fontmake_ttx)
    reduce_diff_noise(build_dir, fontc, fontmake)

    fontc = extract_comparables(fontc, build_dir, "fontc")
    fontmake = extract_comparables(fontmake, build_dir, "fontmake")
    fontc[MARK_KERN_NAME] = fontc_gpos
    fontmake[MARK_KERN_NAME] = fontmake_gpos
    return {"fontc": fontc, "fontmake": fontmake}


def print_output(build_dir: Path, output: dict[str, dict[str, str]]):
    fontc = output["fontc"]
    fontmake = output["fontmake"]
    print("COMPARISON")
    t1 = set(fontc.keys())
    t2 = set(fontmake.keys())
    if t1 != t2:
        if t1 - t2:
            tags = ", ".join(f"'{t}'" for t in sorted(t1 - t2))
            print(f"  Only fontc produced {tags}")

        if t2 - t1:
            tags = ", ".join(f"'{t}'" for t in sorted(t2 - t1))
            print(f"  Only fontmake produced {tags}")

    for tag in sorted(t1 & t2):
        t1s = fontc[tag]
        t2s = fontmake[tag]
        if t1s == t2s:
            print(f"  Identical '{tag}'")
        else:
            difference = diff_ratio(t1s, t2s)
            p1 = build_dir / path_for_output_item(tag, "fontc")
            p2 = build_dir / path_for_output_item(tag, "fontmake")
            print(f"  DIFF '{tag}', {p1} {p2} ({difference:.1%})")


def jsonify_output(output: dict[str, dict[str, str]]):
    fontc = output["fontc"]
    fontmake = output["fontmake"]
    all_tags = set(fontc.keys()) | set(fontmake.keys())
    out = dict()
    for tag in all_tags:
        if tag not in fontc:
            out[tag] = "fontmake"
        elif tag not in fontmake:
            out[tag] = "fontc"
        else:
            s1 = fontc[tag]
            s2 = fontmake[tag]
            if s1 != s2:
                ratio = diff_ratio(s1, s2)
                out[tag] = ratio
    return {"success": out}


def print_json(output):
    as_json = json.dumps(output, indent=2)
    print(as_json)


# given the ttx for a font, return a map of tags -> xml text for each root table.
# also writes the xml to individual files
def extract_comparables(font_xml, build_dir: Path, compiler: str) -> dict[str, str]:
    comparables = dict()
    tables = {e.tag: e for e in font_xml.getroot()}
    for tag in sorted(e.tag for e in font_xml.getroot()):
        table_str = etree.tostring(tables[tag])
        path = build_dir / f"{compiler}.{tag}.ttx"
        path.write_bytes(table_str)
        comparables[tag] = table_str

    return comparables


# the line-wise ratio of difference, i.e. the fraction of lines that are the same
def diff_ratio(text1: str, text2: str) -> float:
    lines1 = text1.splitlines()
    lines2 = text2.splitlines()
    m = SequenceMatcher(None, lines1, lines2)
    return m.ratio()


def path_for_output_item(tag_or_normalizer_name: str, compiler: str) -> str:
    if tag_or_normalizer_name == MARK_KERN_NAME:
        return f"{compiler}.markkern.txt"
    else:
        return f"{compiler}.{tag_or_normalizer_name}.ttx"


# log or print as json any compilation failures (and exit if there were any)
def report_errors_and_exit_if_there_were_any(errors: dict):
    if len(errors) == 0:
        return
    for error in errors.values():
        cmd = error["command"]
        stderr = error["stderr"]
        maybe_print(f"command '{cmd}' failed: '{stderr}'")

    if FLAGS.json:
        print_json({"error": errors})
    sys.exit(2)


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
        if FLAGS.json:
            sys.exit(
                "JSON output does not support multiple comparisons (try --compare default|gftools)")
        comparisons = (_COMPARE_DEFAULTS, _COMPARE_GFTOOLS)

    for compare in comparisons:
        build_dir = (root / "build" / compare).relative_to(root)
        build_dir.mkdir(parents=True, exist_ok=True)
        maybe_print(f"Compare {compare} in {build_dir}")

        failures = dict()

        try:
            build_fontc(source.resolve(), build_dir, compare)
        except BuildFail as e:
            failures["fontc"] = {"command": " ".join(
                e.command), "stderr": e.stderr}
        try:
            build_fontmake(source.resolve(), build_dir, compare)
        except BuildFail as e:
            failures["fontmake"] = {"command": " ".join(
                e.command), "stderr": e.stderr}

        report_errors_and_exit_if_there_were_any(failures)

        # if compilation completed, these exist
        fontmake_ttf = build_dir / "fontmake.ttf"
        fontc_ttf = build_dir / "fontc.ttf"
        assert fontmake_ttf.is_file(), fontmake_ttf
        assert fontc_ttf.is_file(), fontc_ttf

        output = generate_output(build_dir, fontmake_ttf, fontc_ttf)
        if not FLAGS.json:
            print_output(build_dir, output)
        else:
            output = jsonify_output(output)
            print_json(output)


if __name__ == "__main__":
    app.run(main)
