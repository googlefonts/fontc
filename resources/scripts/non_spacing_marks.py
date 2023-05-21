"""glyphsLib uses GlyphData.xml to know things about glyphs.

Naively you would think we could get the same information by querying unicode
properties but it seems not, for example brevecomb-cy with no mapped codepoint
can be identified as a non-spacing mark by name. No unicode db will do that for us.

This utility generates a Rust file with key information extracted from GlyphData.xml.

Usage:
    python resources/scripts/non_spacing_marks.py
"""

from absl import app
import glyphsLib
from glyphsLib.glyphdata import GlyphData
from importlib import resources
from pathlib import Path
import sys


def glyph_data():
    files = resources.files(glyphsLib)
    with (
        files.joinpath("data/GlyphData.xml").open("rb") as f1,
        files.joinpath("data/GlyphData_Ideographs.xml").open("rb") as f2,
    ):
        return GlyphData.from_files(f1, f2)


def main(_):
    glyphs_lib = Path("../glyphsLib")
    if not glyphs_lib.is_dir():
        sys.exit(f"Missing {glyphs_lib}")
    glyphdata_file = glyphs_lib / "Lib/glyphsLib/data/GlyphData.xml"
    if not glyphdata_file.is_file():
        sys.exit(f"Missing {glyphdata_file}")

    non_spacing_marks_by_unicode = set()
    non_spacing_marks_by_name = set()
    for name, attrib in glyph_data().names.items():
        category = attrib.get("category")
        sub_category = attrib.get("subCategory")
        if category == "Mark" and sub_category == "Nonspacing":
            non_spacing_marks_by_name.add(name)
            if "production" in attrib:
                non_spacing_marks_by_name.add(attrib["production"])
            if "altNames" in attrib:
                non_spacing_marks_by_name.update(
                    attrib["altNames"].replace(" ", "").split(",")
                )
            if "unicode" in attrib:
                non_spacing_marks_by_unicode.add(attrib["unicode"])

    with open("glyphs2fontir/src/glyphdata.rs", "w") as f:
        f.write("//! Generated module with Glyphs-specific data\n")
        f.write("//!\n")
        f.write("//! Run resources/scripts/non_spacing_marks.py to update\n")

        f.write("\n")
        f.write("use std::collections::HashSet;\n")
        f.write("\n")
        f.write(
            "pub(crate) fn is_nonspacing_mark(codepoints: &HashSet<u32>, name: &str) -> bool {\n"
        )

        f.write("    // Try first by name\n")
        f.write("    match name {\n")
        for name in sorted(non_spacing_marks_by_name):
            f.write(f'        "{name}" => return true,\n')
        f.write("        _ => (),\n")
        f.write("    }\n")

        f.write("    // Failing name try by codepoint\n")
        f.write("    for cp in codepoints {\n")
        f.write("        match cp {\n")
        for cp in sorted(non_spacing_marks_by_unicode, key=lambda s: int(s, 16)):
            f.write(f"            0x{cp} => return true,\n")
        f.write("            _ => (),\n")
        f.write("        }\n")
        f.write("    }\n")
        f.write("    false\n")
        f.write("}\n")


if __name__ == "__main__":
    app.run(main)
