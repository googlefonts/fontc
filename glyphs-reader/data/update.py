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
from lxml import etree
from pathlib import Path
from textwrap import dedent
from typing import Iterable, Mapping, Optional, Tuple


@dataclass(frozen=True)
class GlyphInfo:
    codepoint: Optional[str]
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


def writeU24(file, value):
    file.write(value.to_bytes(3, byteorder="little"))


def write_enum_conversion(name: str, values: Tuple[str], optional: bool) -> str:
    lines = []
    typ = name
    if optional:
       typ = f"Option<{name}>" 
    lines.append(f"impl BundledEntry for {typ} {{")
    lines.append("    fn element_size() -> usize {")
    lines.append("        1")
    lines.append("    }")
    lines.append(f"    fn from_slice(raw: &[u8]) -> {typ} {{")
    lines.append("        match raw[0] {")

    if optional:
        for (i, value) in enumerate(values):
            lines.append(f"            {i} => Some({name}::{value}),")
        lines.append(f"            255 => None,")
    else:
        for (i, value) in enumerate(values):
            lines.append(f"            {i} => {name}::{value},")
    lines.append(f"            v => panic!(\"What is {{v}}\"),")
    lines.append("        }")
    lines.append("    }")
    lines.append("}")

    return "\n".join(lines) + "\n"


def main():
    glyph_infos = sorted(
        set(read_glyph_info("GlyphData.xml"))
        | set(read_glyph_info("GlyphData_Ideographs.xml")),
        key=lambda g: g.name,
    )
    names = {g.name for g in glyph_infos}
    categories = sorted({g.category for g in glyph_infos})
    subcategories = sorted({g.subcategory for g in glyph_infos if g.subcategory is not None})

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

    for gi in glyph_infos:
        if not gi.name.isascii():
            raise ValueError(f"{gi.name} invalid")

    # There was a time we wrote a bincode file. It took 10% of compile time to decode.
    # There was a time we wrote a really big const array. It was slow for rustc and RA.
    # So lets try writing a binary file we dance around using view apis, akin to how we read fonts.

    offsets_file = Path(__file__).parent.parent / "resources" / "name_offsets.dat"
    data_file = Path(__file__).parent.parent / "resources" / "names.dat"
    codepoint_to_idx_file = Path(__file__).parent.parent / "resources" / "codepoints_to_idx.dat"
    codepoint_file = Path(__file__).parent.parent / "resources" / "codepoints.dat"
    category_file = Path(__file__).parent.parent / "resources" / "categories.dat"
    subcategory_file = Path(__file__).parent.parent / "resources" / "subcategories.dat"
    enums_file = Path(__file__).parent.parent / "src" / "glyphslib_enums.rs"
    readme_file = Path(__file__).parent.parent / "resources" / "README.md"

    offset = 0
    with open(offsets_file, "wb") as f_offsets:
        with open(data_file, "wb") as f_data:            
            for gi in glyph_infos:
                data = gi.name.encode("ascii")
                writeU24(f_offsets, offset)
                f_data.write(data)
                offset += len(data)

    with open(codepoint_file, "wb") as f:
        rev_codepoints = {i:cp for (cp, i) in codepoints.items()}
        for i in range(0, len(glyph_infos)):
            codepoint = rev_codepoints.get(i, 0)
            writeU24(f, codepoint)

    with open(codepoint_to_idx_file, "wb") as f:
        for (codepoint, i) in sorted(codepoints.items()):
            writeU24(f, codepoint)
            writeU24(f, i)

    with open(category_file, "wb") as f:
        for gi in glyph_infos:
            f.write(categories.index(gi.category).to_bytes(1))

    with open(subcategory_file, "wb") as f:
        for gi in glyph_infos:
            i = 255
            if gi.subcategory is not None:
                i = subcategories.index(gi.subcategory)
            f.write(i.to_bytes(1))


    # Generate binary : enum translations
    with open(enums_file, "w") as f:
        f.write("//! Generated by glyphs-reader/data/update.py\n")
        f.write("\n")

        f.write("use crate::{glyphdata::{Category, Subcategory}, glyphdata_bundled::BundledEntry};\n")
        f.write("\n")

        f.write(write_enum_conversion("Category", categories, False))
        f.write("\n")

        f.write("/// The subcategory of a given glyph\n")
        f.write(write_enum_conversion("Subcategory", subcategories, True))
        f.write("\n")

    with open(readme_file, "w") as f:
        f.write("# Dat files\n")
        f.write("\n")
        f.write(
            f"*.dat content generated from glyphsLib {glyphsLib.__version__} by {Path(__file__).name}\n"
        )


if __name__ == "__main__":
    main()
