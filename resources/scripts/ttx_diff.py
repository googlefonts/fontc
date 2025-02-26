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

from collections import defaultdict
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
from typing import Any, Dict, Optional, Sequence, Tuple
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
LIG_CARET_NAME = "ligcaret"
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


def to_xml_string(e) -> str:
    xml = etree.tostring(e)
    # some table diffs were mismatched because of inconsistency in ending newline
    xml = xml.strip()
    return xml


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
def run_normalizer(normalizer_bin: Path, font_file: Path, table: str):
    if table == "gpos":
        out_path = font_file.with_suffix(".markkern.txt")
    elif table == "gdef":
        out_path = font_file.with_suffix(f".{LIG_CARET_NAME}.txt")
    else:
        raise ValueError(f"unknown table for normalizer: '{table}'")

    if out_path.exists():
        eprint(f"reusing {out_path}")
    NUM_RETRIES = 5
    for i in range(NUM_RETRIES + 1):
        try:
            return try_normalizer(normalizer_bin, font_file, out_path, table)
        except subprocess.CalledProcessError as e:
            time.sleep(0.1)
            if i >= NUM_RETRIES:
                raise e
            eprint(f"normalizer failed with code '{e.returncode}'', retrying")


# we had a bug where this would sometimes hang in mysterious ways, so we may
# call it multiple times if it fails
def try_normalizer(normalizer_bin: Path, font_file: Path, out_path: Path, table: str):
    NORMALIZER_TIMEOUT = 60 * 10  # ten minutes
    if not out_path.is_file():
        cmd = [
            normalizer_bin.absolute(),
            font_file.name,
            "-o",
            out_path.name,
            "--table",
            table,
        ]
        log_and_run(cmd, font_file.parent, check=True, timeout=NORMALIZER_TIMEOUT)
        # if we finished running and there's no file then there's no output:
        if not out_path.is_file():
            return ""
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
        "--emit-debug",
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
    if out_dir.exists():
        shutil.rmtree(out_dir)
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


# https://github.com/googlefonts/fontc/issues/1107
def normalize_glyf_contours(ttx):
    for glyph in ttx.xpath("//glyf/TTGlyph"):
        contours = glyph.xpath("./contour")
        if len(contours) < 2:
            continue
        normalized = sorted(contours, key=to_xml_string)
        if normalized == contours:
            continue
        # normalized contours should be inserted before any other TTGlyph's
        # subelements (e.g. instructions)
        for contour in contours:
            glyph.remove(contour)
        non_contours = list(glyph)
        for el in non_contours:
            glyph.remove(el)
        glyph.extend(normalized + non_contours)


# https://github.com/googlefonts/fontc/issues/1173
def erase_type_from_stranded_points(ttx):
    for contour in ttx.xpath("//glyf/TTGlyph/contour"):
        points = contour.xpath("./pt")
        if len(points) == 1:
            points[0].attrib["on"] = "irrelevent"


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
                    fontc_el.attrib["diff_adjusted"] = "1"
                    fontmake_el.attrib["diff_adjusted"] = "1"
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
        lookup_indices = [el for el in feature.iter() if el.tag == "LookupListIndex"]
        values = sorted(int(v.attrib["value"]) for v in lookup_indices)
        for i, lookup_index in enumerate(lookup_indices):
            lookup_index.attrib["value"] = str(values[i])


LOOKUPS_TO_SKIP = set([2, 4, 5, 6])  # pairpos, markbase, marklig, markmark


def remove_mark_and_kern_lookups(ttx):
    gpos = ttx.find("GPOS")
    if gpos is None:
        return
    # './/Lookup' xpath selects all the 'Lookup' elements that are descendants of
    # the current 'GPOS' node - no matter where they are under it.
    # Most importantly, this _excludes_ GSUB lookups, which shouldn't be pruned.
    removed_indices = set()
    for lookup in gpos.xpath(".//Lookup"):
        lookup_index = lookup.attrib["index"]
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
        removed_indices.add(lookup_index)

    # finally go and replace any of the removed indices where they are referenced
    # in features; our lookup order won't always match fonttools for mark/kern
    # because of the particulars of the feature writers, so we just ignore this
    # (these lookups are all normalized, which takes lookup order into account)
    for lookup_idx_el in gpos.xpath(".//LookupListIndex"):
        if lookup_idx_el.attrib["value"] in removed_indices:
            lookup_idx_el.attrib["value"] = "normalized"


# this all gets handled by otl-normalizer
def remove_gdef_lig_caret_and_var_store(ttx: etree.ElementTree):
    gdef = ttx.find("GDEF")
    if gdef is None:
        return

    for ident in ["LigCaretList", "VarStore"]:
        subtable = gdef.find(ident)
        if subtable is not None:
            gdef.remove(subtable)


# reassign class ids within a ClassDef, matching the fonttools behaviour.
# returns a map of new -> old ids, which can be used to reorder elements that
# used the class ids as indices
def remap_class_def_ids_like_fonttools(class_def: etree.ElementTree) -> Dict[int, int]:
    current_classes = defaultdict(list)
    for glyph in class_def.xpath(".//ClassDef"):
        cls = glyph.attrib["class"]
        current_classes[cls].append(glyph.attrib["glyph"])

    # match the sorting used in fonttools:
    # https://github.com/fonttools/fonttools/blob/8a89f4f81b0068/Lib/fontTools/otlLib/builder.py#L2689
    new_order = sorted(current_classes.values(), key=lambda s: (-len(s), s))
    new_order_map = {name: i + 1 for (i, cls) in enumerate(new_order) for name in cls}
    result = dict()
    for glyph in class_def.xpath(".//ClassDef"):
        cls = glyph.attrib["class"]
        new = new_order_map.get(glyph.attrib["glyph"])
        glyph.attrib["class"] = str(new)
        result[new] = int(cls)
    return result


def reorder_rules(lookup: etree.ElementTree, new_order: Dict[int, int], rule_name: str):
    # the rules can exist as siblings of other non-rule elements, so we can't just
    # clear all children and set them in the same order.
    # instead we remove them and then append them back in order, using 'addnext'
    # to ensure that we're inserting them at the right location.
    orig_order = [el for el in lookup.iterchildren(tag=rule_name)]
    if len(orig_order) == 0:
        return
    prev_el = orig_order[0].getprevious()
    for el in orig_order:
        lookup.remove(el)

    for ix in range(len(orig_order)):
        prev_ix = new_order.get(ix, ix)
        el = orig_order[prev_ix]
        el.set("index", str(ix))
        prev_el.addnext(el)
        prev_el = el

    # there was a funny issue where if we moved the last element elsewhere
    # in the ordering it would end up having incorrect indentation, so just
    # reindent everything to be safe.
    etree.indent(lookup, level=4)


# for each named child in container, remap the 'value' attribute using the new ordering
def remap_values(container: etree.ElementTree, new_order: Dict[int, int], child_name: str):
    # for the original use we need to map from new to old, but here we need the reverse
    rev_map = {v:k for (k,v) in new_order.items()}
    for el in container.iterchildren(child_name):
        old = int(el.attrib['value'])
        el.attrib['value'] = str(rev_map[old])


# fontmake and fontc assign glyph classes differently for class-based tables;
# fontc uses GIDs but fontmake uses glyph names, so we reorder them to be consistent.
def reorder_contextual_class_based_rules(ttx: etree.ElementTree, tag: str):
    if tag == "GSUB":
        chain_name = "ChainContextSubst"
        class_set_name = "ChainSubClassSet"
        class_rule_name = "ChainSubClassRule"

    elif tag == "GPOS":
        chain_name = "ChainContextPos"
        class_set_name = "ChainPosClassSet"
        class_rule_name = "ChainPosClassRule"
    else:
        raise ValueError("must be one of 'GPOS' or 'GSUB'")

    table = ttx.find(tag)
    if table is None:
        return
    for lookup in table.xpath(".//Lookup"):
        for chain_ctx in lookup.findall(chain_name):
            if chain_ctx is None or int(chain_ctx.attrib["Format"]) != 2:
                continue
            input_class_order = remap_class_def_ids_like_fonttools(
                chain_ctx.find("InputClassDef")
            )
            reorder_rules(chain_ctx, input_class_order, class_set_name)
            backtrack_class_order = remap_class_def_ids_like_fonttools(
                chain_ctx.find("BacktrackClassDef")
            )
            lookahead_class_order = remap_class_def_ids_like_fonttools(
                chain_ctx.find("LookAheadClassDef")
            )
            for class_set in chain_ctx.findall(class_set_name):
                for class_rule in class_set.findall(class_rule_name):
                    remap_values(class_rule, input_class_order, "Input")
                    remap_values(class_rule, backtrack_class_order, "Backtrack")
                    remap_values(class_rule, lookahead_class_order, "LookAhead")


def reduce_diff_noise(fontc: etree.ElementTree, fontmake: etree.ElementTree):
    sort_fontmake_feature_lookups(fontmake)
    reorder_contextual_class_based_rules(fontc, "GSUB")
    reorder_contextual_class_based_rules(fontc, "GPOS")
    for ttx in (fontc, fontmake):
        # different name ids with the same value is fine
        name_id_to_name(ttx, "//NamedInstance", "subfamilyNameID")
        name_id_to_name(ttx, "//AxisNameID", "value")
        name_id_to_name(ttx, "//UINameID", "value")
        name_id_to_name(ttx, "//AxisNameID", None)

        # deal with https://github.com/googlefonts/fontmake/issues/1003
        drop_weird_names(ttx)

        # for matching purposes checksum is just noise
        erase_checksum(ttx)

        stat_like_fontmake(ttx)
        remove_mark_and_kern_lookups(ttx)

        normalize_glyf_contours(ttx)

        erase_type_from_stranded_points(ttx)
        remove_gdef_lig_caret_and_var_store(ttx)

    allow_some_off_by_ones(fontc, fontmake, "glyf/TTGlyph", "name", "/contour/pt")
    allow_some_off_by_ones(
        fontc, fontmake, "gvar/glyphVariations", "glyph", "/tuple/delta"
    )


# given a font file, return a dictionary of tags -> size in bytes
def get_table_sizes(fontfile: Path) -> dict[str, int]:
    cmd = ["ttx", "-l", str(fontfile)]
    stdout = log_and_run(cmd, check=True).stdout
    result = dict()

    for line in stdout.strip().splitlines()[3:]:
        split = line.split()
        result[split[0]] = int(split[2])

    return result


# return a dict of table tag  -> size difference
# only when size difference exceeds some threshold
def check_sizes(fontmake_ttf: Path, fontc_ttf: Path):
    THRESHOLD = 1 / 10
    fontmake = get_table_sizes(fontmake_ttf)
    fontc = get_table_sizes(fontc_ttf)

    output = dict()
    shared_keys = set(fontmake.keys() & fontc.keys())

    for key in shared_keys:
        fontmake_len = fontmake[key]
        fontc_len = fontc[key]
        if fontc_len < fontmake_len:
            continue
        len_ratio = min(fontc_len, fontmake_len) / max(fontc_len, fontmake_len)
        if (1 - len_ratio) > THRESHOLD:
            rel_len = fontc_len - fontmake_len
            eprint(f"{key} {fontmake_len} {fontc_len} {len_ratio:.3} {rel_len}")
            output[key] = rel_len
    return output


# returns a dictionary of {"compiler_name":  {"tag": "xml_text"}}
def generate_output(
    build_dir: Path, otl_norm_bin: Path, fontmake_ttf: Path, fontc_ttf: Path
):
    fontc_ttx = run_ttx(fontc_ttf)
    fontmake_ttx = run_ttx(fontmake_ttf)
    fontc_gpos = run_normalizer(otl_norm_bin, fontc_ttf, "gpos")
    fontmake_gpos = run_normalizer(otl_norm_bin, fontmake_ttf, "gpos")
    fontc_gdef = run_normalizer(otl_norm_bin, fontc_ttf, "gdef")
    fontmake_gdef = run_normalizer(otl_norm_bin, fontmake_ttf, "gdef")

    fontc = etree.parse(fontc_ttx)
    fontmake = etree.parse(fontmake_ttx)
    reduce_diff_noise(fontc, fontmake)

    fontc = extract_comparables(fontc, build_dir, "fontc")
    fontmake = extract_comparables(fontmake, build_dir, "fontmake")
    size_diffs = check_sizes(fontmake_ttf, fontc_ttf)
    fontc[MARK_KERN_NAME] = fontc_gpos
    fontmake[MARK_KERN_NAME] = fontmake_gpos
    if len(fontc_gdef):
        fontc[LIG_CARET_NAME] = fontc_gdef
    if len(fontmake_gdef):
        fontmake[LIG_CARET_NAME] = fontmake_gdef
    result = {"fontc": fontc, "fontmake": fontmake}
    if len(size_diffs) > 0:
        result["sizes"] = size_diffs

    return result


def print_output(build_dir: Path, output: dict[str, dict[str, Any]]):
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
            print(f"  DIFF '{tag}', {p1} {p2} ({difference:.3%})")
    if output.get("sizes"):
        print("SIZE DIFFERENCES")
    for tag, diff in output.get("sizes", {}).items():
        print(f"SIZE DIFFERENCE: '{tag}': {diff}B")


def jsonify_output(output: dict[str, dict[str, Any]]):
    fontc = output["fontc"]
    fontmake = output["fontmake"]
    sizes = output.get("sizes", {})
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

    # then also add in size differences, if any
    for tag, size_diff in sizes.items():
        out[f"sizeof({tag})"] = size_diff
        # hacky: we don't want to be perfect if we have a size diff,
        # so let's pretend that whatever our size diff is, it corresponds
        # to some fictional table 100 lines liong
        different_lines += 100

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
        table_str = to_xml_string(tables[tag])
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
        local_repo = (
            Path.home() / ".fontc_crater_cache" / org_name / repo_name
        ).resolve()
        if not local_repo.parent.is_dir():
            local_repo.parent.mkdir(parents=True)
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
                ttf_path.with_suffix(".ligcaret.txt"),
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
