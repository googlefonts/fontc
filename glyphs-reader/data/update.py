"""Update bundled data derived from glyphsLib GlyphData.xml and GlyphData_Ideographs.xml.

This script copies files out of the currently active version of glyphsLib and generates
Rust code for efficient access to the default data. Override files must be loaded separately
from XML. We only generate code for the fields we actively use.

Usage:
    python glyphs-reader/data/update.py
"""

import dataclasses
from dataclasses import dataclass
import glyphsLib
from importlib import resources
from io import StringIO
from lxml import etree
from pathlib import Path
from textwrap import dedent
from typing import Optional, Tuple


@dataclass(frozen=True)
class GlyphInfo:
    codepoint: Optional[int]
    name: str
    category: str
    subcategory: Optional[str]


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
                by_name[e.attrib["name"]], name=alt_name, codepoint=None
            )

    return tuple(by_name.values())


def main():
    glyph_infos = sorted(
        set(read_glyph_info("GlyphData.xml"))
        | set(read_glyph_info("GlyphData_Ideographs.xml")),
        key=lambda g: g.name,
    )
    names = {g.name for g in glyph_infos}
    categories = {g.category for g in glyph_infos}
    subcategories = {g.subcategory for g in glyph_infos if g.subcategory is not None}
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

    dest_file = Path(__file__).parent.parent / "src" / "glyphslib_data.rs"

    with open(dest_file, "w") as f:
        f.write(
            f"//! Glyph data generated from glyphsLib {glyphsLib.__version__} by {Path(__file__).name}\n"
        )
        f.write("//!\n")
        f.write(f"//! {len(glyph_infos)} glyph metadata records taken from glyphsLib\n")
        f.write("\n")
        f.write("use crate::glyphdata::{qr, Category as C, QueryResult, Subcategory as S};\n")  
        f.write("\n")      
        f.write("// Sorted by name, has unique names, therefore safe to bsearch\n")

        f.write("pub(crate) const GLYPH_INFO: &[(&str, QueryResult)] = &[\n")
        for gi in glyph_infos:
            codepoint = "None"
            if gi.codepoint is not None:
                codepoint = f"Some(0x{gi.codepoint})"
            subcategory = "None"
            if gi.subcategory is not None:
                subcategory = f"Some(S::{gi.subcategory})"
            f.write(
                f'    ("{gi.name}", qr(C::{gi.category}, {subcategory}, {codepoint})),\n'
            )

        f.write("];\n")

        f.write(
            "// Sorted by codepoint, has unique codepoints, therefore safe to bsearch\n"
        )
        f.write("pub(crate) const CODEPOINT_TO_INFO_IDX: &[(u32, usize)] = &[\n")
        for codepoint, i in sorted(codepoints.items()):
            f.write(f"    (0x{codepoint:04x}, {i}), // {glyph_infos[i].name}\n")

        f.write("];\n")


if __name__ == "__main__":
    main()
