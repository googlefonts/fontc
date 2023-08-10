"""glyphsLib uses GlyphData.xml to know things about glyphs.

Naively you would think we could get the same information by querying unicode
properties but it seems not, for example brevecomb-cy with no mapped codepoint
can be identified as a non-spacing mark by name. No unicode db will do that for us.

This utility generates a Rust file with key information extracted from GlyphData.xml.

Usage:
    python resources/scripts/non_spacing_marks.py && cargo fmt
"""

#from absl import app
import glyphsLib
from glyphsLib.glyphdata import GlyphData
from importlib import resources
import sys


def glyph_data():
    files = resources.files(glyphsLib)
    with (
        files.joinpath("data/GlyphData.xml").open("rb") as f1,
        files.joinpath("data/GlyphData_Ideographs.xml").open("rb") as f2,
    ):
        return GlyphData.from_files(f1, f2)


def first4_as_u32(p4):
    result = 0
    for (i, ch) in enumerate(p4):
        result |= ord(ch) << i * 8
    return result


def main(_):
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

        names = sorted(non_spacing_marks_by_name)

        # We're going to assume the nonspacing mark names are ascii, confirm so
        for name in names:
            for ch in name:
                assert ord(ch) <= 255, f"Cannot handle {name}"

        unused_first = set()
        for c in range(ord('a'), ord('z') + 1):
            unused_first.add(chr(c))
            unused_first.add(chr(c).upper())
        used_first = {n[0] for n in names}
        unused_first = unused_first - used_first

        used_lengths = {len(n) for n in names}
        unused_lengths = set(range(1, max(used_lengths) + 1)) - used_lengths
        print("UNUSED:", unused_first, unused_lengths)

        unique_prefix2s = {n[:2] for n in names}
        print(len(unique_prefix2s), "unique_prefix2s", sorted(unique_prefix2s))

        unique_prefix4s = {n[:4] for n in names}
        print(len(unique_prefix4s), "unique_prefix4s", sorted(unique_prefix4s))

        f.write("\n")
        f.write("#[inline(always)]\n")
        f.write(
            "pub(crate) fn might_be_a_nonspacing_mark_name(name: &str) -> bool {\n"
        )
        f.write("    // shove the first 4 chars into a u32 and see if they could possibly be a nonspacing mark\n")
        f.write("    let head = name.chars().enumerate().take(4).fold(0u32, |acc, (i, ch)| acc | (ch as u32) << (i * 8));\n")
        f.write("    matches!(head, ")
        f.write(" | ".join(str(u32) for u32 in sorted(first4_as_u32(p4) for p4 in unique_prefix4s)))        
        f.write(")\n")
        f.write("}\n")

        f.write("\n")
        f.write(
            "pub(crate) fn is_nonspacing_mark_name(name: &str) -> bool {\n"
        )

        f.write("    // fast exit: length cannot match a mark\n")
        f.write(f"    if name.len() > {max(used_lengths)}")
        if unused_lengths:
            f.write(" || matches!(name.len(), ")
            f.write(" | ".join(sorted(str(l) for l in unused_lengths)))
        f.write(") {\n")
        f.write("        return false;\n")
        f.write("    }\n")

        # flamegraph suggests this doesn't help
        # f.write("    // fast exit: first char indicates no match\n")
        # f.write("    let Some(first) = name.chars().next() else {\n")
        # f.write("        return false;\n")
        # f.write("    };\n")
        # f.write("    if matches!(first, ")
        # f.write(" | ".join(sorted(f"'{str(c)}'" for c in unused_first)))
        # f.write(") {\n")
        # f.write("        return false;\n")
        # f.write("    };\n")

        f.write("    if !might_be_a_nonspacing_mark_name(name) {\n")
        f.write("        return false;\n")
        f.write("    }\n")

        f.write("\n")

        f.write("    // slow road\n")
        f.write("    match name {\n")
        for name in names:
            f.write(f'        "{name}" => return true,\n')
        f.write("        _ => (),\n")
        f.write("    }\n")
        f.write("    false\n")
        f.write("}\n")

        f.write("\n")
        f.write(
            "fn any_nonspacing_mark_codepoint(codepoints: &HashSet<u32>) -> bool {\n"
        )
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

        f.write("\n")
        f.write(
            "pub(crate) fn is_nonspacing_mark(codepoints: &HashSet<u32>, name: &str) -> bool {\n"
        )

        f.write("    // Try first by name, then codepoint\n")
        f.write("    is_nonspacing_mark_name(name) || any_nonspacing_mark_codepoint(codepoints)\n")
        f.write("}\n")


if __name__ == "__main__":
    main(None)
