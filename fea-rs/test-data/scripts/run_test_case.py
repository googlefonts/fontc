"""
A simple script that runs one of our test cases through fonttools, generating ttf
and/or ttx outupt.

This is useful for debugging compilation differences between fea-rs and feaLib.

for usage run with --help.
"""

import os
import tempfile
from argparse import Namespace
from fontTools.ttLib import TTFont
from fontTools.feaLib.builder import (
    addOpenTypeFeaturesFromString,
)


def main():
    args = get_args()
    font = makeTTFont(args.glyph_order)
    fea = get_fea_text(args.fea)
    addOpenTypeFeaturesFromString(font, fea)
    filename = os.path.splitext(os.path.basename(args.fea))[0]
    if not args.no_ttx:
        ttx_path = filename + '.ttx'
        font.saveXML(
            ttx_path,
            tables=[
                "GDEF",
                "GSUB",
                "GPOS",
            ],
        )
    if not args.no_ttf:
        ttf_path = filename + '.ttf'
        font.save(ttf_path)


def get_fea_text(fea_path) -> str:
    with open(fea_path, "r", encoding="utf-8") as f:
        fea = f.read()
    return fea


def makeTTFont(glyph_order):
    with open(glyph_order, "r", encoding="utf-8") as f:
        glyphs = [s.strip() for s in f.readlines()]
    font = TTFont()
    font.setGlyphOrder(glyphs)
    return font


def get_args() -> Namespace:
    import argparse
    parser = argparse.ArgumentParser(
        description='build a test case with feaLib, outputing ttx')
    parser.add_argument('fea')
    parser.add_argument('glyph_order')
    parser.add_argument('--no-ttf', action='store_true',
                        help='suppress writing of ttf')
    parser.add_argument('--no-ttx', action='store_true',
                        help='suppress writing of ttx')
    return parser.parse_args()


if __name__ == "__main__":
    main()
