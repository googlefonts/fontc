"""Tests for the cross-font glyf/gvar passes over otl-normalizer's text output."""

from types import SimpleNamespace

import pytest

from ttx_diff import core
from ttx_diff.core import (
    GlyphBlocks,
    allow_some_off_by_ones,
    normalize_glyf_and_gvar,
    normalize_glyf_contours,
    normalize_gvar_contours,
    split_contours,
)

GLYF_A = """\
# glyf #
glyph A
  bbox 0 0 100 100
  contour
    50 50 on
    60 60 on
  contour
    0 0 on
    10 0 on
    10 10 on
glyph B
  bbox 0 0 5 5
  component A 1 2 flags 0x4
"""

# same contours as GLYF_A, sorted the other way round
GLYF_A_REORDERED = """\
# glyf #
glyph A
  bbox 0 0 100 100
  contour
    0 0 on
    10 0 on
    10 10 on
  contour
    50 50 on
    60 60 on
glyph B
  bbox 0 0 5 5
  component A 1 2 flags 0x4
"""

GVAR_A = """\
# gvar #
glyph A
  tuple wght=1
    1 1
    2 2
    3 3
    4 4
    5 5
    100 0
    101 0
    102 0
    103 0
  tuple wght=-1
    -1 -1
    -2 -2
    -3 -3
    -4 -4
    -5 -5
    0 0
    0 0
    0 0
    0 0
"""


@pytest.fixture
def budget(monkeypatch):
    def set_budget(percent):
        monkeypatch.setattr(core, "FLAGS", SimpleNamespace(off_by_one_budget=percent))

    return set_budget


def test_glyph_blocks_round_trip():
    blocks = GlyphBlocks(GLYF_A)
    assert list(blocks.names()) == ["A", "B"]
    assert blocks.text() == GLYF_A
    assert blocks.num_coords == 5
    assert blocks.get("C") is None
    assert blocks.lines("B") == [
        "B",
        "  bbox 0 0 5 5",
        "  component A 1 2 flags 0x4",
        "",
    ]


def test_split_contours():
    prefix, contours, suffix = split_contours(GlyphBlocks(GLYF_A).lines("A"))
    assert prefix == ["A", "  bbox 0 0 100 100"]
    assert contours == [
        "    50 50 on\n    60 60 on",
        "    0 0 on\n    10 0 on\n    10 10 on",
    ]
    assert suffix == []


def test_contours_are_sorted_when_the_sets_match():
    fontc = GlyphBlocks(GLYF_A)
    fontmake = GlyphBlocks(GLYF_A_REORDERED)
    fontc_orders, fontmake_orders = normalize_glyf_contours(fontc, fontmake)
    assert fontc.text() == fontmake.text() == GLYF_A_REORDERED
    assert fontc_orders == {"A": [2, 3, 4, 0, 1]}
    assert fontmake_orders == {}


def test_contours_are_left_alone_when_they_differ():
    fontc = GlyphBlocks(GLYF_A)
    fontmake = GlyphBlocks(GLYF_A_REORDERED.replace("60 60 on", "60 61 on"))
    fontc_orders, fontmake_orders = normalize_glyf_contours(fontc, fontmake)
    assert fontc.text() == GLYF_A
    assert fontc_orders == fontmake_orders == {}


def test_gvar_follows_the_glyf_permutation():
    blocks = GlyphBlocks(GVAR_A)
    normalize_gvar_contours(blocks, {"A": [2, 3, 4, 0, 1], "B": [0]})
    assert blocks.text() == GVAR_A.replace(
        "    1 1\n    2 2\n    3 3\n    4 4\n    5 5\n",
        "    3 3\n    4 4\n    5 5\n    1 1\n    2 2\n",
    ).replace(
        "    -1 -1\n    -2 -2\n    -3 -3\n    -4 -4\n    -5 -5\n",
        "    -3 -3\n    -4 -4\n    -5 -5\n    -1 -1\n    -2 -2\n",
    )


def test_off_by_ones_are_snapped_and_marked(budget):
    budget(100)
    fontc = GlyphBlocks(
        GLYF_A.replace("10 0 on", "11 1 on").replace("60 60 on", "60 62 on")
    )
    fontmake = GlyphBlocks(GLYF_A)
    allow_some_off_by_ones(fontc, fontmake, "glyf")
    expected = GLYF_A.replace("10 0 on", "10 0 on adjusted")
    assert fontmake.text() == expected
    # the 2-unit difference stays
    assert fontc.text() == expected.replace("60 60 on", "60 62 on")


def test_off_by_ones_respect_the_budget(budget):
    # 5 coordinates in fontmake, 20% of them is one snap
    budget(20)
    fontc = GlyphBlocks(
        GLYF_A.replace("10 0 on", "11 1 on").replace("50 50 on", "51 50 on")
    )
    fontmake = GlyphBlocks(GLYF_A)
    allow_some_off_by_ones(fontc, fontmake, "glyf")
    assert fontc.text() == GLYF_A.replace("10 0 on", "11 1 on").replace(
        "50 50 on", "50 50 on adjusted"
    )


def test_off_by_ones_skip_glyphs_with_different_point_counts(budget):
    budget(100)
    fontc = GlyphBlocks(GLYF_A.replace("    10 10 on\n", ""))
    fontmake = GlyphBlocks(GLYF_A)
    before = fontc.text()
    allow_some_off_by_ones(fontc, fontmake, "glyf")
    assert fontc.text() == before


def test_identical_texts_short_circuit(budget):
    budget(100)
    assert normalize_glyf_and_gvar(GLYF_A, GLYF_A, GVAR_A, GVAR_A) == (
        GLYF_A,
        GLYF_A,
        GVAR_A,
        GVAR_A,
    )


def test_reordered_contours_compare_equal_in_glyf_and_gvar(budget):
    budget(100)
    fontc_gvar = GVAR_A.replace(
        "    1 1\n    2 2\n    3 3\n    4 4\n    5 5\n",
        "    3 3\n    4 4\n    5 5\n    1 1\n    2 2\n",
    ).replace(
        "    -1 -1\n    -2 -2\n    -3 -3\n    -4 -4\n    -5 -5\n",
        "    -3 -3\n    -4 -4\n    -5 -5\n    -1 -1\n    -2 -2\n",
    )
    fontc_glyf, fontmake_glyf, fontc_gvar, fontmake_gvar = normalize_glyf_and_gvar(
        GLYF_A_REORDERED, GLYF_A, fontc_gvar, GVAR_A
    )
    assert fontc_glyf == fontmake_glyf
    assert fontc_gvar == fontmake_gvar
