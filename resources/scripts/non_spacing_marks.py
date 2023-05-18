"""glyphsLib uses GlyphData.xml to know things about glyphs.

Naively you would think we could get the same information by querying unicode
properties but it seems not, for example brevecomb-cy with no mapped codepoint
can be identified as a non-spacing mark by name. No unicode db will do that for us.

This utility generates a Rust file with key information extracted from GlyphData.xml.
"""

from absl import app
from lxml import etree
from pathlib import Path
import sys


def main(_):
    glyphs_lib = Path("../glyphsLib")
    if not glyphs_lib.is_dir():
        sys.exit(f"Missing {glyphs_lib}")
    glyphdata_file = glyphs_lib / "Lib/glyphsLib/data/GlyphData.xml"
    if not glyphdata_file.is_file():
        sys.exit(f"Missing {glyphdata_file}")
    
    codepoints = set()
    names = set()
    xml = etree.parse(glyphdata_file)
    for glyph in xml.xpath("//glyph[@category='Mark' and @subCategory='Nonspacing']"):
        unicode = glyph.attrib.get("unicode")
        if unicode is not None:
            codepoints.add(unicode)
        name = glyph.attrib.get("name")
        if name:
            names.add(name)

    with open("glyphs2fontir/src/glyphdata.rs", "w") as f:
        f.write("//! Generated module with Glyphs-specific data\n")
        f.write("//!\n")
        f.write("//! Run resources/scripts/non_spacing_marks.py to update\n")
        
        f.write("\n");
        f.write("use std::collections::HashSet;\n")
        f.write("\n")
        f.write("pub(crate) fn is_nonspacing_mark(codepoints: &HashSet<u32>, name: &str) -> bool {\n")
        f.write("    match codepoints.iter().next() {\n")
        for cp in sorted(codepoints, key=lambda s: int(s, 16)):
            f.write(f"        Some(0x{cp}) => return true,\n")
        f.write("        _ => (),\n")
        f.write("    }\n")
        f.write("    match name {\n")
        for name in sorted(names):
            f.write(f"        \"{name}\" => return true,\n")
        f.write("        _ => (),\n")
        f.write("    }\n")
        f.write("    false\n")
        f.write("}")

if __name__ == "__main__":
    app.run(main)