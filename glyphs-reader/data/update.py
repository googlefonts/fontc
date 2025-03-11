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
from typing import Iterable, Mapping, MutableSet, Optional, Tuple


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


def minimal_names(names_taken: MutableSet, names: Iterable[str]) -> Mapping[str, str]:
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


def write_enum_shorthands(f, enum_name, minimal_names, option):
    const_t = enum_name
    if option:
        const_t = "Option<" + enum_name + ">"
    for long, short in sorted(minimal_names.items()):
        value = f"{enum_name}::{long}"
        if option:
            value = "Some(" + value + ")"
        f.write(f"const {short}: {const_t} = {value};\n")



def write_const_array_file(value_type, plural, glyph_infos, value_fn, shorthands, preamble=None):
    dest_file = Path(__file__).parent.parent / "src" / f"glyphslib_{plural.lower()}.rs"
    with open(dest_file, "w") as f:
        f.write(
            f"//! Glyph data generated from glyphsLib {glyphsLib.__version__} by {Path(__file__).name}\n"
        )
        f.write("//!\n")
        f.write(f"//! {len(glyph_infos)} glyph metadata records taken from glyphsLib\n")
        f.write("\n")
        if value_type[0].isalpha() and value_type[0].isupper():
            f.write(
                f"use crate::glyphdata::{value_type};\n"
            )
        f.write("\n")

        values = tuple(value_fn(gi) for gi in glyph_infos)
        optional = any(v is None for v in values)

        if preamble:
            f.write(preamble)
        else:
            if optional:
                f.write(f"#[allow(non_upper_case_globals)]\nconst n: Option<{value_type}> = None;\n")
                f.write("\n")

        if shorthands:
            write_enum_shorthands(f, value_type, shorthands, optional)
            f.write("\n")

        if optional:
            const_t = f"Option<{value_type}>"
        else:
            const_t = value_type
        f.write(f"pub(crate) const {plural.upper()}: &[{const_t}] = &[\n")

        lines = [""]
        for gi in glyph_infos:
            value = value_fn(gi)
            if shorthands and value is not None:
                value = shorthands[value]

            if value is None:
                value = "n"
            fragment = value + ","
            if (len(lines[-1]) + len(fragment)) > 100:
                lines[-1] += "\n"
                lines.append("")
            lines[-1] += fragment

        for line in lines:
            f.write(line)
        f.write("\n")
        f.write("];\n")


def codepoint_literal(gi):
    if gi.codepoint is None:
        return "0"
    b10 = str(int(gi.codepoint, 16))
    b16 = "0x" + gi.codepoint
    if len(b10) < len(b16):
        return b10
    return b16


def production_name_literal(gi):
    if gi.production_name is None:
        return "n"
    if gi.codepoint is not None:
        codepoint = int(gi.codepoint, 16)
        if gi.production_name == f"uni{codepoint:04X}":
            return "U"
        if gi.production_name == f"u{codepoint:04X}":
            return "V"
    return f"C(\"{gi.production_name}\")"


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

    production_name_to_info_idx = {}
    blank = 0
    uprefix = 0
    uniprefix = 0
    interesting = 0
    for i, gi in enumerate(glyph_infos):
        if gi.production_name is None:
            blank += 1
            continue
        if gi.production_name in production_name_to_info_idx:
            print(
                f"Multiple names are assigned {gi.production_name}, using the first one we saw"
            )
        else:
            production_name_to_info_idx[gi.production_name] = i

    # We emit parallel arrays instead of a giant const array of structs because it's less bytes
    # and hopefully easier on the compiler and rust analyzer as well
    write_const_array_file("&str", "Names", glyph_infos, lambda gi: f"\"{gi.name}\"", None)
    write_const_array_file("Category", "Categories", glyph_infos, lambda gi: gi.category, min_categories)
    write_const_array_file("Subcategory", "Subcategories", glyph_infos, lambda gi: gi.subcategory, min_subcategories)
    write_const_array_file("u32", "Codepoints", glyph_infos, codepoint_literal, None)
    write_const_array_file("ProductionName", "Production_Names", glyph_infos, production_name_literal, None,
"""
#[allow(non_upper_case_globals)]
const n: ProductionName = ProductionName::None;
const U: ProductionName = ProductionName::PrefixUni;
const V: ProductionName = ProductionName::PrefixU;
const fn C(s: &'static str) -> ProductionName { 
    ProductionName::Custom(smol_str::SmolStr::new_static(s))
}

""")

    dest_file = Path(__file__).parent.parent / "src" / "glyphslib_production_name_to_idx.rs"

    with open(dest_file, "w") as f:
        f.write(
            f"//! Glyph data generated from glyphsLib {glyphsLib.__version__} by {Path(__file__).name}\n"
        )
        f.write("//!\n")
        f.write(f"//! {len(glyph_infos)} glyph metadata records taken from glyphsLib\n")

        f.write("\n")

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
