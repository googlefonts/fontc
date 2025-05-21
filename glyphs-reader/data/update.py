"""Update bundled data derived from glyphsLib GlyphData.xml and GlyphData_Ideographs.xml.

This script copies files out of the currently active version of glyphsLib and generates
Rust code for efficient access to the default data. Override files must be loaded separately
from XML. We only generate code for the fields we actively use.

Usage:
    python glyphs-reader/data/update.py
"""

from collections import defaultdict
from collections.abc import Callable
import dataclasses
from dataclasses import dataclass
import glyphsLib
from importlib import resources
from lxml import etree
from math import ceil, floor
from pathlib import Path
from textwrap import dedent
from typing import Iterable, Optional, Self, Tuple


@dataclass(frozen=True)
class GlyphInfo:
    codepoint: Optional[int]
    name: str
    category: str
    subcategory: Optional[str]
    production_name: Optional[str]
    script: Optional[str]

    def has_codepoint(self) -> bool:
        return self.codepoint is not None

    def production_name_or_empty(self) -> str:
        if self.production_name is None:
            return ""
        return self.production_name

    def production_name_file_bytes(self) -> bytes:
        return ProdNamePattern.for_glyph_info(self).file_bytes(self)


@dataclass(frozen=True)
class ProdNamePattern:
    display_name: str
    pred: Callable[[GlyphInfo], bool]
    file_bytes: Callable[[GlyphInfo], bytes]

    def for_glyph_info(gi: GlyphInfo) -> Self:
        for pattern in _PROD_NAME_PATTERNS:
            if pattern.pred(gi):
                return pattern
        raise ValueError(
            f"At least 1 pattern should always match, somehow {gi} defeated us"
        )


# Production names are heavily patterned, try to take advantage to compress representation
# Meant to be probed in order
_PROD_NAME_PATTERNS = (
    ProdNamePattern(
        "blank",
        lambda gi: gi.production_name is None,
        lambda _gi: b"b",
    ),
    ProdNamePattern(
        "uni-prefix",
        lambda gi: gi.has_codepoint()
        and gi.production_name == f"uni{gi.codepoint:04X}"
        and gi.codepoint <= 0xFFFF,
        lambda gi: b"u" + gi.codepoint.to_bytes(3, "little"),
    ),
    ProdNamePattern(
        "u-prefix",
        lambda gi: gi.has_codepoint()
        and gi.production_name == f"u{gi.codepoint:04X}"
        and gi.codepoint > 0xFFFF,
        lambda gi: b"v" + gi.codepoint.to_bytes(3, "little"),
    ),
    ProdNamePattern(
        "custom",
        lambda gi: True,
        lambda gi: b"s" + gi.production_name.encode("ascii"),
    ),
)


def prod_name_from_codepoint(cp: int) -> str:
    if cp <= 0xFFFF:
        return f"uni{cp:04x}"
    else:
        return f"u{cp:04x}"


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
        codepoint = e.attrib.get("unicode", None)
        if codepoint is not None:
            codepoint = int(codepoint, 16)
        info = GlyphInfo(
            codepoint,
            e.attrib["name"].strip(),
            codename(e.attrib["category"]),
            codename(e.attrib.get("subCategory", None)),
            e.attrib.get("production", None),
            e.attrib.get("script", None),
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
                production_name=e.attrib.get("production", None),
                script=e.attrib.get("script", None),
            )

    return tuple(by_name.values())


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
        for i, value in enumerate(values):
            lines.append(f"            {i} => Some({name}::{value}),")
        lines.append(f"            255 => None,")
    else:
        for i, value in enumerate(values):
            lines.append(f"            {i} => {name}::{value},")
    lines.append(f'            v => panic!("What is {{v}}"),')
    lines.append("        }")
    lines.append("    }")
    lines.append("}")

    return "\n".join(lines) + "\n"


def write_list(offset_file: Path, data_file: Path, content: Iterable[bytes]):
    offset = 0
    with open(offset_file, "wb") as f_offsets:
        with open(data_file, "wb") as f_data:
            for data in content:
                writeU24(f_offsets, offset)
                f_data.write(data)
                offset += len(data)


def camel_case(s: str) -> str:
    parts = s.split(" ")
    return "".join(p[0].upper() + p[1:] for p in parts)


def main():
    glyph_infos = sorted(
        set(read_glyph_info("GlyphData.xml"))
        | set(read_glyph_info("GlyphData_Ideographs.xml")),
        key=lambda g: g.name,
    )
    names = {g.name for g in glyph_infos}
    categories = sorted({g.category for g in glyph_infos})
    subcategories = sorted(
        {g.subcategory for g in glyph_infos if g.subcategory is not None}
    )
    scripts = sorted(
        {g.script for g in glyph_infos if g.script is not None}, key=camel_case
    )

    assert len(names) == len(glyph_infos), "Names aren't unique?"
    codepoints = {}
    for i, gi in enumerate(glyph_infos):
        if gi.codepoint is None:
            continue
        codepoint = gi.codepoint
        if codepoint not in codepoints:
            codepoints[codepoint] = i
        else:
            print(
                f"Multiple names are assigned 0x{codepoint:04x}, using the first one we saw"
            )

    for gi in glyph_infos:
        if not gi.name.isascii():
            raise ValueError(f"{gi.name} invalid")
        if gi.production_name is not None and not gi.production_name.isascii():
            raise ValueError(f"{gi.production_name} invalid")

    # There was a time we wrote a bincode file. It took 10% of compile time to decode.
    # There was a time we wrote a really big const array. It was slow for rustc and RA.
    # So lets try writing a binary file we dance around using view apis, akin to how we read fonts.

    name_offsets_file = Path(__file__).parent.parent / "resources" / "name_offsets.dat"
    name_data_file = Path(__file__).parent.parent / "resources" / "names.dat"
    codepoint_to_idx_file = (
        Path(__file__).parent.parent / "resources" / "codepoints_to_idx.dat"
    )
    codepoint_file = Path(__file__).parent.parent / "resources" / "codepoints.dat"
    category_file = Path(__file__).parent.parent / "resources" / "categories.dat"
    subcategory_file = Path(__file__).parent.parent / "resources" / "subcategories.dat"
    script_file = Path(__file__).parent.parent / "resources" / "scripts.dat"
    enums_file = Path(__file__).parent.parent / "src" / "glyphslib_enums.rs"
    prod_name_offsets_file = (
        Path(__file__).parent.parent / "resources" / "prod_name_offsets.dat"
    )
    prod_name_data_file = Path(__file__).parent.parent / "resources" / "prod_names.dat"
    prod_name_bitmap_file = (
        Path(__file__).parent.parent / "resources" / "prod_name_bitmap.dat"
    )
    readme_file = Path(__file__).parent.parent / "resources" / "README.md"

    write_list(
        name_offsets_file,
        name_data_file,
        [gi.name.encode("ascii") for gi in glyph_infos],
    )

    prod_names_by_type = defaultdict(int)
    for gi in glyph_infos:
        pattern = ProdNamePattern.for_glyph_info(gi)
        prod_names_by_type[pattern.display_name] += 1
    print("Prod name types", prod_names_by_type)

    max_codepoint = max(codepoints.keys())
    prod_name_bitmap = bytearray(ceil(max_codepoint / 8))
    num_predicted = 0
    for cp in range(0, max_codepoint + 1):
        if cp not in codepoints:
            continue
        gi = glyph_infos[codepoints[cp]]
        if gi.production_name is None:
            continue
        if prod_name_from_codepoint(cp).lower() != gi.production_name.lower():
            continue

        i = floor(cp / 8)
        prod_name_bitmap[i] |= 1 << (cp % 8)
        num_predicted += 1
    custom_prod_names = []
    for i, gi in enumerate(glyph_infos):
        if gi.production_name is None:
            continue
        if (
            gi.has_codepoint()
            and prod_name_from_codepoint(gi.codepoint).lower()
            == gi.production_name.lower()
        ):
            continue
        custom_prod_names.append((i, gi))

    print(
        f"{num_predicted}/{num_predicted + len(custom_prod_names)} non-blank production names are predictable"
    )
    with open(prod_name_bitmap_file, "wb") as f:
        f.write(prod_name_bitmap)

    prod_names = [(gi.production_name, i) for (i, gi) in custom_prod_names]
    prod_names.sort()
    write_list(
        prod_name_offsets_file,
        prod_name_data_file,
        [i.to_bytes(3, "little") + name.encode("ascii") for (name, i) in prod_names],
    )

    with open(codepoint_file, "wb") as f:
        rev_codepoints = {i: cp for (cp, i) in codepoints.items()}
        for i in range(0, len(glyph_infos)):
            codepoint = rev_codepoints.get(i, 0)
            writeU24(f, codepoint)

    with open(codepoint_to_idx_file, "wb") as f:
        for codepoint, i in sorted(codepoints.items()):
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

    with open(script_file, "wb") as f:
        for gi in glyph_infos:
            i = 255
            if gi.script is not None:
                i = scripts.index(gi.script)
            f.write(i.to_bytes(1))

    # Generate binary : enum translations
    with open(enums_file, "w") as f:
        f.write("//! Generated by glyphs-reader/data/update.py\n")
        f.write("\n")

        f.write(
            "use crate::{\n"
            "    glyphdata::{Category, Script, Subcategory},\n"
            "    glyphdata_bundled::BundledEntry,\n"
            "};"
        )
        f.write("\n")

        f.write(write_enum_conversion("Category", categories, False))
        f.write("\n")

        f.write("/// The subcategory of a given glyph\n")
        f.write(write_enum_conversion("Subcategory", subcategories, True))
        f.write("\n")

        f.write("/// The script of a given glyph\n")
        f.write(
            write_enum_conversion("Script", tuple(camel_case(s) for s in scripts), True)
        )

    with open(readme_file, "w") as f:
        f.write("# Dat files\n")
        f.write("\n")
        f.write(
            f"*.dat content generated from glyphsLib {glyphsLib.__version__} by {Path(__file__).name}\n"
        )


if __name__ == "__main__":
    main()
