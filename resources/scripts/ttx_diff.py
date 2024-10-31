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

    If both compilers ran successfully, this dictionary will have a single key,
    "success", which will contain a dictionary, where keys are the tags of tables
    (or another identifier) and the value is either a float representing the
    'difference ratio' (where 1.0 means identical and 0.0 means maximally
   dissimilar) or, if only one compiler produced that table, the name of that
    compiler as a string.
    For example, the output `{"success": { "GPOS": 0.99, "vmxt": "fontmake" }}`
    means that the "GPOS" table was 99% similar, and only `fontmake` produced
    the "vmtx" table (and all other tables were identical).

    If one or both of the compilers fail to exit successfully, we will return a
    dictionary with the single key, "error", where the payload is a dictionary
    where keys are the name of the compiler that failed, and the body is a
    dictionary with "command" and "stderr" fields, where the "command" field
    is the command that was used to run that compiler.
"""

from absl import app
from absl import flags
from lxml import etree
from pathlib import Path
import json
import shutil
import subprocess
import sys
import os
from urllib.parse import urlparse
from cdifflib import CSequenceMatcher as SequenceMatcher
from typing import Optional, Sequence, Tuple
from glyphsLib import GSFont
from fontTools.designspaceLib import DesignSpaceDocument
import time


_COMPARE_DEFAULTS = "default"
_COMPARE_GFTOOLS = "gftools"

# environment variable used by GFTOOLS
GFTOOLS_FONTC_PATH = "GFTOOLS_FONTC_PATH"


FLAGS = flags.FLAGS
# used instead of a tag for the normalized mark/kern output
MARK_KERN_NAME = "(mark/kern)"
# maximum chars of stderr to include when reporting errors; prevents
# too much bloat when run in CI
MAX_ERR_LEN = 1000

# fontc and fontmake's builds may be off by a second or two in the
# head.created/modified; setting this makes them the same
if "SOURCE_DATE_EPOCH" not in os.environ:
   os.environ["SOURCE_DATE_EPOCH"] = str(int(time.time()))

# print to stderr
def eprint(*objects):
    print(*objects, file=sys.stderr)


flags.DEFINE_string(
    "config",
    default=None,
    help="config.yaml to be passed to gftools in gftools mode",
)
flags.DEFINE_string(
    "fontc_path",
    default=None,
    help="Optional path to precompiled fontc binary",
)
flags.DEFINE_string(
    "normalizer_path",
    default=None,
    help="Optional path to precompiled otl-normalizer binary",
)
flags.DEFINE_enum(
    "compare",
    "default",
    [_COMPARE_DEFAULTS, _COMPARE_GFTOOLS],
    "Compare results using either a default build or a build managed by gftools. Note that as of 5/21/2023 defaults still sets flags for fontmake to match fontc behavior.",
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
flags.DEFINE_bool("json", False, "print results in machine-readable JSON format")
flags.DEFINE_string("outdir", default=None, help="directory to store generated files")


# execute a command after logging it to stderr.
# All additional kwargs are passed to subprocess.run
def log_and_run(cmd: Sequence, cwd=None, **kwargs):
    cmd_string = " ".join(str(c) for c in cmd)
    if cwd is not None:
        eprint(f"  (cd {cwd} && {cmd_string})")
    else:
        eprint(f"  ({cmd_string})")
    return subprocess.run(
        cmd,
        text=True,
        cwd=cwd,
        capture_output=True,
        **kwargs,
    )


def run_ttx(font_file: Path):
    ttx_file = font_file.with_suffix(".ttx")
    # if this exists we're allowed to reuse it
    if ttx_file.is_file():
        eprint(f"reusing {ttx_file}")
        return ttx_file

    cmd = [
        "ttx",
        "-o",
        ttx_file.name,
        font_file.name,
    ]
    log_and_run(cmd, font_file.parent, check=True)
    return ttx_file


# generate a simple text repr for gpos for this font, with retry
def run_normalizer_gpos(normalizer_bin: Path, font_file: Path):
    out_path = font_file.with_suffix(".markkern.txt")
    if out_path.exists():
        eprint(f"reusing {out_path}")
    NUM_RETRIES = 5
    for i in range(NUM_RETRIES + 1):
        try:
            return try_normalizer_gpos(normalizer_bin, font_file, out_path)
        except subprocess.CalledProcessError as e:
            time.sleep(0.1)
            if i >= NUM_RETRIES:
                raise e
            eprint(f"normalizer failed with code '{e.returncode}'', retrying")


# we had a bug where this would sometimes hang in mysterious ways, so we may
# call it multiple times if it fails
def try_normalizer_gpos(normalizer_bin: Path, font_file: Path, out_path: Path):
    NORMALIZER_TIMEOUT = 60 * 10  # ten minutes
    if not out_path.is_file():
        cmd = [
            normalizer_bin.absolute(),
            font_file.name,
            "-o",
            out_path.name,
            "--table",
            "gpos",
        ]
        log_and_run(cmd, font_file.parent, check=True, timeout=NORMALIZER_TIMEOUT)
    with open(out_path) as f:
        return f.read()


class BuildFail(Exception):
    """An exception raised if a compiler fails."""

    def __init__(self, cmd: Sequence, msg: str):
        self.command = list(str(c) for c in cmd)
        self.msg = msg


# run a font compiler
def build(cmd: Sequence, build_dir: Optional[Path], **kwargs):
    output = log_and_run(cmd, build_dir, **kwargs)
    if output.returncode != 0:
        raise BuildFail(cmd, output.stderr or output.stdout)


def build_fontc(source: Path, fontc_bin: Path, build_dir: Path):
    out_file = build_dir / "fontc.ttf"
    if out_file.exists():
        eprint(f"reusing {out_file}")
        return
    cmd = [
        fontc_bin,
        # uncomment this to compare output w/ fontmake --keep-direction
        # "--keep-direction",
        # no longer required, still useful to get human-readable glyph names in diff
        "--no-production-names",
        "--build-dir",
        ".",
        "-o",
        out_file.name,
        source,
    ]
    build(cmd, build_dir)


def build_fontmake(source: Path, build_dir: Path):
    out_file = build_dir / "fontmake.ttf"
    if out_file.exists():
        eprint(f"reusing {out_file}")
        return
    variable = source_is_variable(source)
    buildtype = "variable"
    if not variable:
        buildtype = "ttf"
    cmd = [
        "fontmake",
        "-o",
        buildtype,
        "--output-path",
        out_file.name,
        "--drop-implied-oncurves",
        # "--keep-direction",
        # no longer required, still useful to get human-readable glyph names in diff
        "--no-production-names",
        # helpful for troubleshooting
        "--debug-feature-file",
        "debug.fea",
    ]
    if not variable:
        # fontmake static builds perform overlaps removal, but fontc can't do that yet.
        # Disable the filter to make the diff less noisy.
        # TODO(anthrotype): Remove if/when fontc gains the ability to remove overlaps.
        # https://github.com/googlefonts/fontc/issues/975
        cmd.append("--keep-overlaps")
    cmd.append(str(source))

    build(cmd, build_dir)


def run_gftools(
    source: Path, config: Path, build_dir: Path, fontc_bin: Optional[Path] = None
):
    tool = "fontmake" if fontc_bin is None else "fontc"
    filename = tool + ".ttf"
    out_file = build_dir / filename
    out_dir = build_dir / "gftools_temp_dir"
    cmd = [
        "gftools",
        "builder",
        config,
        "--experimental-simple-output",
        out_dir,
        "--experimental-single-source",
        source.name,
    ]
    if fontc_bin is not None:
        cmd += ["--experimental-fontc", fontc_bin]

    build(cmd, None)

    # return a concise error if gftools produces != one output
    contents = list(out_dir.iterdir()) if out_dir.exists() else list()
    if not contents:
        raise BuildFail(cmd, "gftools produced no output")
    elif len(contents) != 1:
        contents = [p.name for p in contents]
        raise BuildFail(cmd, f"gftools produced multiple outputs: {contents}")
    copy(contents[0], out_file)

    if out_dir.exists():
        shutil.rmtree(out_dir)


def source_is_variable(path: Path) -> bool:
    if path.suffix == ".ufo":
        return False
    if path.suffix == ".designspace":
        dspace = DesignSpaceDocument.fromfile(path)
        return len(dspace.sources) > 1
    if path.suffix == ".glyphs" or path.suffix == ".glyphspackage":
        font = GSFont(path)
        return len(font.masters) > 1
    # fallback to variable, the existing default, but we should never get here?
    return True


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
        if attr is None:
            if el.text is None:
                continue
            name_id = int(el.text)
            # names <= 255 have specific assigned slots, names > 255 not
            if name_id <= 255:
                continue
            el.text = id_to_name[el.text]
        else:
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


def allow_some_off_by_ones(fontc, fontmake, container, name_attr, coord_holder):
    fontmake_num_coords = len(fontmake.xpath(f"//{container}/{coord_holder}"))
    off_by_one_budget = int(FLAGS.off_by_one_budget / 100.0 * fontmake_num_coords)
    spent = 0
    if off_by_one_budget == 0:
        return

    coord_tag = coord_holder.rpartition("/")[-1]
    # put all the containers into a dict to make querying more efficient:

    fontc_items = {x.attrib[name_attr]: x for x in fontc.xpath(f"//{container}")}
    for fontmake_container in fontmake.xpath(f"//{container}"):
        name = fontmake_container.attrib[name_attr]
        fontc_container = fontc_items.get(name)
        if fontc_container is None:
            eprint(f"no item where {name_attr}='{name}' in {container}")
            continue

        for fontmake_el, fontc_el in zip(
            fontmake_container.iter(), fontc_container.iter()
        ):
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
                    eprint(
                        f"WARN: ran out of budget ({off_by_one_budget}) to fix off-by-ones in {container}"
                    )
                    return

    if spent > 0:
        eprint(
            f"INFO fixed {spent} off-by-ones in {container} (budget {off_by_one_budget})"
        )


# the order of lookups in a feature's lookuplist do not matter;
# fontc always has them in sorted order but fontmake doesn't, so sort them
def sort_fontmake_feature_lookups(ttx):
    gpos = ttx.find("GPOS")
    if gpos is None:
        return
    features = gpos.xpath("//Feature")

    for feature in features:
        # the first item is 'Feature', the second always a comment
        has_value = [el for el in feature.iter() if "value" in el.attrib]
        values = sorted(int(v.attrib["value"]) for v in has_value)
        for i, lookup_index in enumerate(has_value):
            lookup_index.attrib["value"] = str(values[i])


LOOKUPS_TO_SKIP = set([2, 4, 5, 6])  # pairpos, markbase, marklig, markmark


def remove_mark_and_kern_lookups(ttx):
    gpos = ttx.find("GPOS")
    if gpos is None:
        return
    # './/Lookup' xpath selects all the 'Lookup' elements that are descendants of
    # the current 'GPOS' node - no matter where they are under it.
    # Most importantly, this _excludes_ GSUB lookups, which shouldn't be pruned.
    for lookup in gpos.xpath(".//Lookup"):
        lookup_type_el = lookup.find("LookupType")
        lookup_type = int(lookup_type_el.attrib["value"])
        is_extension = lookup_type == 9
        if is_extension:
            # For extension lookups, take the lookup type of the wrapped subtables;
            # all extension subtables share the same lookup type so checking only
            # the first is enough.
            ext_subtable = lookup.find("ExtensionPos")
            lookup_type = int(ext_subtable.find("ExtensionLookupType").attrib["value"])
        if lookup_type not in LOOKUPS_TO_SKIP:
            continue
        # remove all the elements but the type:
        to_remove = [child for child in lookup if child.tag != "LookupType"]
        for child in to_remove:
            lookup.remove(child)
        if is_extension:
            lookup_type_el.attrib["value"] = str(lookup_type)


def reduce_diff_noise(fontc: etree.ElementTree, fontmake: etree.ElementTree):
    sort_fontmake_feature_lookups(fontmake)
    for ttx in (fontc, fontmake):
        # different name ids with the same value is fine
        name_id_to_name(ttx, "//NamedInstance", "subfamilyNameID")
        name_id_to_name(ttx, "//AxisNameID", "value")
        name_id_to_name(ttx, "//AxisNameID", None)

        # deal with https://github.com/googlefonts/fontmake/issues/1003
        drop_weird_names(ttx)

        # for matching purposes checksum is just noise
        erase_checksum(ttx)

        stat_like_fontmake(ttx)
        remove_mark_and_kern_lookups(ttx)

    allow_some_off_by_ones(fontc, fontmake, "glyf/TTGlyph", "name", "/contour/pt")
    allow_some_off_by_ones(
        fontc, fontmake, "gvar/glyphVariations", "glyph", "/tuple/delta"
    )


# returns a dictionary of {"compiler_name":  {"tag": "xml_text"}}
def generate_output(
    build_dir: Path, otl_norm_bin: Path, fontmake_ttf: Path, fontc_ttf: Path
):
    fontc_ttx = run_ttx(fontc_ttf)
    fontmake_ttx = run_ttx(fontmake_ttf)
    fontc_gpos = run_normalizer_gpos(otl_norm_bin, fontc_ttf)
    fontmake_gpos = run_normalizer_gpos(otl_norm_bin, fontmake_ttf)

    fontc = etree.parse(fontc_ttx)
    fontmake = etree.parse(fontmake_ttx)
    reduce_diff_noise(fontc, fontmake)

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
    same_lines = 0
    different_lines = 0
    for tag in all_tags:
        if tag not in fontc:
            different_lines += len(fontmake[tag])
            out[tag] = "fontmake"
        elif tag not in fontmake:
            different_lines += len(fontc[tag])
            out[tag] = "fontc"
        else:
            s1 = fontc[tag]
            s2 = fontmake[tag]
            if s1 != s2:
                ratio = diff_ratio(s1, s2)
                n_lines = max(len(s1), len(s2))
                same_lines += int(n_lines * ratio)
                different_lines += int(n_lines * (1 - ratio))
                out[tag] = ratio
            else:
                same_lines += len(s1)

    overall_diff_ratio = same_lines / (same_lines + different_lines)
    out["total"] = overall_diff_ratio
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
    return m.quick_ratio()


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
        eprint(f"command '{cmd}' failed: '{stderr}'")

    if FLAGS.json:
        print_json({"error": errors})
    sys.exit(2)


# for reproducing crater results we have a syntax that lets you specify a
# repo url as the source.
# in this scheme we pass the path to the particular source (relative the repo root)
# as a url fragment
def resolve_source(source: str) -> Path:
    if source.startswith("git@") or source.startswith("https://"):
        source_url = urlparse(source)
        repo_path = source_url.fragment
        org_name = source_url.path.split("/")[-2]
        repo_name = source_url.path.split("/")[-1]
        local_repo = (Path.home() / ".fontc_crater_cache" / org_name / repo_name).resolve()
        if not local_repo.parent.is_dir():
            local_repo.parent.mkdir()
        if not local_repo.is_dir():
            cmd = ("git", "clone", source_url._replace(fragment="").geturl())
            print("Running", " ".join(cmd), "in", local_repo.parent)
            subprocess.run(cmd, cwd=local_repo.parent, check=True)
        else:
            print(f"Reusing existing {local_repo}")
        source = local_repo / repo_path
    else:
        source = Path(source)
    if not source.exists():
        sys.exit(f"No such source: {source}")
    return source


def delete_things_we_must_rebuild(rebuild: str, fontmake_ttf: Path, fontc_ttf: Path):
    for tool, ttf_path in [("fontmake", fontmake_ttf), ("fontc", fontc_ttf)]:
        must_rebuild = rebuild in [tool, "both"]
        if must_rebuild:
            for path in [
                ttf_path,
                ttf_path.with_suffix(".ttx"),
                ttf_path.with_suffix(".markkern.txt"),
            ]:
                if path.exists():
                    os.remove(path)


# returns the path to the compiled binary
def build_crate(manifest_path: Path):
    cmd = ["cargo", "build", "--release", "--manifest-path", str(manifest_path)]
    log_and_run(cmd, cwd=None, check=True)


def get_fontc_and_normalizer_binary_paths(root_dir: Path) -> Tuple[Path, Path]:
    fontc_path = FLAGS.fontc_path
    norm_path = FLAGS.normalizer_path
    if fontc_path is None:
        fontc_manifest_path = root_dir / "fontc" / "Cargo.toml"
        fontc_path = root_dir / "target" / "release" / "fontc"
        build_crate(fontc_manifest_path)
        assert fontc_path.is_file(), "failed to build fontc?"
    else:
        fontc_path = Path(fontc_path)
        assert fontc_path.is_file(), f"fontc path '{fontc_path}' does not exist"
    if norm_path is None:
        otl_norm_manifest_path = root_dir / "otl-normalizer" / "Cargo.toml"
        norm_path = root_dir / "target" / "release" / "otl-normalizer"
        build_crate(otl_norm_manifest_path)
        assert norm_path.is_file(), "failed to build otl-normalizer?"
    else:
        norm_path = Path(norm_path)
        assert norm_path.is_file(), f"normalizer path '{norm_path}' does not exist"

    return (fontc_path, norm_path)

def get_crate_path(cli_arg: Optional[str], root_dir: Path, crate_name: str) -> Path:
    if cli_arg:
        return Path(cli_arg)

    manifest_path = root_dir / crate_name / "Cargo.toml"
    bin_path = root_dir / "target" / "release" / crate_name
    build_crate(manifest_path)
    return bin_path



def main(argv):
    if len(argv) != 2:
        sys.exit("Only one argument, a source file, is expected")

    source = resolve_source(argv[1]).resolve()

    root = Path(".").resolve()
    if root.name != "fontc":
        sys.exit("Expected to be at the root of fontc")

    fontc_bin_path = get_crate_path(FLAGS.fontc_path, root, "fontc")
    otl_bin_path = get_crate_path(FLAGS.normalizer_path, root, "otl-normalizer")

    assert fontc_bin_path.is_file(), f"fontc path '{fontc_bin_path}' does not exist"
    assert otl_bin_path.is_file(), f"normalizer path '{otl_bin_path}' does not exist"

    if shutil.which("fontmake") is None:
        sys.exit("No fontmake")
    if shutil.which("ttx") is None:
        sys.exit("No ttx")

    out_dir = root / "build"
    if FLAGS.outdir is not None:
        out_dir = Path(FLAGS.outdir).resolve()
        assert out_dir.exists(), f"output directory {out_dir} does not exist"

    diffs = False

    compare = FLAGS.compare
    build_dir = out_dir / compare
    build_dir.mkdir(parents=True, exist_ok=True)
    eprint(f"Compare {compare} in {build_dir}")

    failures = dict()

    fontmake_ttf = build_dir / "fontmake.ttf"
    fontc_ttf = build_dir / "fontc.ttf"

    # we delete all resources that we have to rebuild. The rest of the script
    # will assume it can reuse anything that still exists.
    delete_things_we_must_rebuild(FLAGS.rebuild, fontmake_ttf, fontc_ttf)

    try:
        if compare == _COMPARE_DEFAULTS:
            build_fontc(source, fontc_bin_path, build_dir)
        else:
            run_gftools(source, FLAGS.config, build_dir, fontc_bin=fontc_bin_path)
    except BuildFail as e:
        failures["fontc"] = {
            "command": " ".join(e.command),
            "stderr": e.msg[-MAX_ERR_LEN:],
        }
    try:
        if compare == _COMPARE_DEFAULTS:
            build_fontmake(source, build_dir)
        else:
            run_gftools(source, FLAGS.config, build_dir)
    except BuildFail as e:
        failures["fontmake"] = {
            "command": " ".join(e.command),
            "stderr": e.msg[-MAX_ERR_LEN:],
        }

    report_errors_and_exit_if_there_were_any(failures)

    # if compilation completed, these exist
    assert fontmake_ttf.is_file(), fontmake_ttf
    assert fontc_ttf.is_file(), fontc_ttf

    output = generate_output(build_dir, otl_bin_path, fontmake_ttf, fontc_ttf)
    if output["fontc"] == output["fontmake"]:
        eprint("output is identical")
    else:

        diffs = True
        if not FLAGS.json:
            print_output(build_dir, output)
        else:
            output = jsonify_output(output)
            print_json(output)

    sys.exit(diffs * 2)  # 0 or 2


if __name__ == "__main__":
    app.run(main)
