"""Update bundled data derived from glyphsLib GlyphData.xml and GlyphData_Ideographs.xml.

This script copies files out of the currently active version of glyphsLib and generates
Rust code for efficient access to the default data. Override files must be loaded separately
from XML. We only generate code for the fields we actively use.

Usage:
    python glyphs-reader/data/update.py
"""

from collections import defaultdict
import dataclasses
from dataclasses import dataclass
import glyphsLib
from importlib import resources
from io import StringIO
from lxml import etree
from pathlib import Path
from textwrap import dedent
from typing import Iterable, Mapping, Optional, Tuple


@dataclass(frozen=True)
class GlyphInfo:
    codepoint: Optional[int]
    name: str
    category: str
    subcategory: Optional[str]
    production_name: Optional[str]


def codename(name: Optional[str]) -> Optional[str]:
    if name is None:
        return None
    return name.replace(" ", "")


def read_glyph_info(file: str) -> Tuple[GlyphInfo]:
    file = resources.files(glyphsLib).joinpath("data").joinpath(file)
    with open(file) as f:
        tree = etree.parse(f)

    by_name = {}

    # Do a full pass to collect names
    for e in tree.xpath("//glyph"):
        info = GlyphInfo(
            e.attrib.get("unicode", None),
            e.attrib["name"].strip(),
            codename(e.attrib["category"]),
            codename(e.attrib.get("subCategory", None)),
            e.attrib.get("production", None),
        )
        if info.name not in by_name:
            by_name[info.name] = info
        else:
            print(f"We've already seen {info.name}!")

    # Then add alt_names where they don't overlap names
    for e in tree.xpath("//glyph[@altNames]"):
        for alt_name in e.attrib["altNames"].split(","):
            alt_name = alt_name.strip()
            if alt_name in by_name:
                print(f'Ignoring alt name "{alt_name}", already taken')
                continue
            by_name[alt_name] = dataclasses.replace(
                by_name[e.attrib["name"]],
                name=alt_name,
                codepoint=None,
                production_name=None,
            )

    return tuple(by_name.values())


def minimal_names(names_taken: set(), names: Iterable[str]) -> Mapping[str, str]:
    counts = defaultdict(int)
    for name in names:
        counts[name] += 1

    result = {}
    # most used first so it gets the shortest name
    for _, name in sorted(((v, k) for (k, v) in counts.items()), reverse=True):
        for i in range(1, len(name) + 1):
            candidate = name[:i].upper()
            if candidate not in names_taken:
                result[name] = candidate
                names_taken.add(candidate)
                break
        assert name in result, f"Unable to slot {name} {sorted(names_taken)}"
    assert len(counts) == len(result)
    return result


def write_enum_shorthands(f, enum_name, minimal_names):
    for long, short in sorted(minimal_names.items()):
        f.write(f"const {short}: {enum_name} = {enum_name}::{long};\n")


def main():
    glyph_infos = sorted(
        set(read_glyph_info("GlyphData.xml"))
        | set(read_glyph_info("GlyphData_Ideographs.xml")),
        key=lambda g: g.name,
    )
    names = {g.name for g in glyph_infos}

    # Globally unique minimized names for categories and subcategories
    shorthand_names = set()
    min_categories = minimal_names(shorthand_names, (g.category for g in glyph_infos))
    min_subcategories = minimal_names(
        shorthand_names,
        (g.subcategory for g in glyph_infos if g.subcategory is not None),
    )

    assert len(names) == len(glyph_infos), "Names aren't unique?"
    codepoints = {}
    for i, gi in enumerate(glyph_infos):
        if gi.codepoint is None:
            continue
        codepoint = int(gi.codepoint, 16)
        if codepoint not in codepoints:
            codepoints[codepoint] = i
        else:
            print(
                f"Multiple names are assigned 0x{codepoint:04x}, using the first one we saw"
            )

    production_names = []
    production_name_to_info_idx = {}
    for i, gi in enumerate(glyph_infos):
        if gi.production_name is None:
            production_names.append("")
            continue
        production_names.append(gi.production_name)
        if gi.production_name in production_name_to_info_idx:
            print(
                f"Multiple names are assigned {gi.production_name}, using the first one we saw"
            )
        else:
            production_name_to_info_idx[gi.production_name] = i

    dest_file = Path(__file__).parent.parent / "src" / "glyphslib_data.rs"

    with open(dest_file, "w") as f:
        f.write(
            f"//! Glyph data generated from glyphsLib {glyphsLib.__version__} by {Path(__file__).name}\n"
        )
        f.write("//!\n")
        f.write(f"//! {len(glyph_infos)} glyph metadata records taken from glyphsLib\n")
        f.write("\n")
        f.write(
            "use crate::glyphdata::{qr, q1, q2, q3, Category, QueryPartialResult, Subcategory};\n"
        )
        f.write("\n")

        # Write constants for enum variants to shorten giant tuple array
        write_enum_shorthands(f, "Category", min_categories)
        f.write("\n")
        write_enum_shorthands(f, "Subcategory", min_subcategories)
        f.write("\n")

        f.write("// Sorted by name, has unique names, therefore safe to bsearch\n")

        f.write("pub(crate) const GLYPH_INFO: &[(&str, QueryPartialResult)] = &[\n")
        lines = [""]
        for gi in glyph_infos:
            category = min_categories[gi.category]

            # map to shorthand
            if (None, None) == (gi.subcategory, gi.codepoint):
                entry = f"q1({category})"
            elif gi.subcategory is None:
                # codepoint must not be
                entry = f"q2({category}, 0x{gi.codepoint})"
            elif gi.codepoint is None:
                # subcategory must not be
                entry = f"q3({category}, {min_subcategories[gi.subcategory]})"
            else:
                # We must have all the things!
                entry = f"qr({category}, {min_subcategories[gi.subcategory]}, 0x{gi.codepoint})"

            codepoint = "None"
            if gi.codepoint is not None:
                codepoint = f"Some(0x{gi.codepoint})"

            fragment = f'("{gi.name}", {entry}),'
            if (len(lines[-1]) + len(fragment)) > 100:
                lines[-1] += "\n"
                lines.append("")
            lines[-1] += fragment

        for line in lines:
            f.write(line)
        f.write("\n")
        f.write("];\n")

        f.write(
            "// Sorted by codepoint, has unique codepoints, therefore safe to bsearch\n"
        )
        f.write("pub(crate) const CODEPOINT_TO_INFO_IDX: &[(u32, usize)] = &[\n")
        lines = [""]
        for codepoint, i in sorted(codepoints.items()):
            fragment = f"(0x{codepoint:04x}, {i}),"
            if (len(lines[-1]) + len(fragment)) > 100:
                lines[-1] += "\n"
                lines.append("")
            lines[-1] += fragment

        for line in lines:
            f.write(line)
        f.write("\n")
        f.write("];\n")

        f.write("// Sorted by name, has unique names, therefore safe to bsearch\n")
        f.write("pub(crate) const PRODUCTION_NAMES: &[&str] = &[\n")
        lines = [""]
        assert len(glyph_infos) == len(production_names)
        for production_name in production_names:
            fragment = f'"{production_name}",'
            if (len(lines[-1]) + len(fragment)) > 100:
                lines[-1] += "\n"
                lines.append("")
            lines[-1] += fragment

        for line in lines:
            f.write(line)
        f.write("\n")
        f.write("];\n")

        f.write(
            "// Sorted by production name, has unique production names, therefore safe to bsearch\n"
        )
        f.write("pub(crate) const PRODUCTION_NAME_TO_INFO_IDX: &[(&str, usize)] = &[\n")
        lines = [""]
        for name, i in sorted(production_name_to_info_idx.items()):
            fragment = f'("{name}", {i}),'
            if (len(lines[-1]) + len(fragment)) > 100:
                lines[-1] += "\n"
                lines.append("")
            lines[-1] += fragment

        for line in lines:
            f.write(line)
        f.write("\n")
        f.write("];\n")


if __name__ == "__main__":
    main()
