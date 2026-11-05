"""SinglePos values must be compared after resolving their variation indices."""

import io
import os
import sys
from pathlib import Path

import pytest
from fontTools.feaLib.builder import addOpenTypeFeaturesFromString
from fontTools.fontBuilder import FontBuilder
from fontTools.otlLib.builder import buildLookup, buildSinglePos, buildValue
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib import newTable
from fontTools.ttLib.tables import otTables
from fontTools.varLib.builder import (
    buildVarData,
    buildVarDevTable,
    buildVarRegionList,
    buildVarStore,
)
from lxml import etree

from ttx_diff import core


def _variable_singlepos_font(delta, index=0, extension=False):
    builder = FontBuilder(1000, isTTF=True)
    glyphs = [".notdef", "A"]
    builder.setupGlyphOrder(glyphs)
    builder.setupCharacterMap({0x41: "A"})
    builder.setupGlyf({name: TTGlyphPen(None).glyph() for name in glyphs})
    builder.setupHorizontalMetrics({name: (500, 0) for name in glyphs})
    builder.setupHorizontalHeader(ascent=800, descent=-200)
    builder.setupNameTable({"familyName": "SinglePos", "styleName": "Regular"})
    builder.setupOS2()
    builder.setupPost()
    font = builder.font
    font.recalcTimestamp = False
    font["head"].created = font["head"].modified = 3400000000
    addOpenTypeFeaturesFromString(font, "feature dist { pos A 10; } dist;")
    value = buildValue({"XAdvance": 10, "XAdvDevice": buildVarDevTable(index)})
    font["GPOS"].table.LookupList.Lookup = [
        buildLookup(
            buildSinglePos({"A": value}, font.getReverseGlyphMap()),
            table="GPOS",
            extension=extension,
        )
    ]
    items = [[delta], [99]] if index == 0 else [[99], [delta]]
    gdef = font["GDEF"] = newTable("GDEF")
    gdef.table = otTables.GDEF()
    gdef.table.Version = 0x00010003
    gdef.table.VarStore = buildVarStore(
        buildVarRegionList([{"wght": (0, 1, 1)}], ["wght"]),
        [buildVarData([0], items)],
    )
    return font


@pytest.mark.parametrize("extension", [False, True])
@pytest.mark.parametrize("unwrap", [False, True])
def test_singlepos_xml_ignores_variation_index_allocation(cli_flags, extension, unwrap):
    cli_flags.unwrap_extensions = unwrap
    trees = []
    for index in [0, 1]:
        xml = io.StringIO()
        _variable_singlepos_font(20, index, extension).saveXML(xml)
        trees.append(etree.ElementTree(etree.fromstring(xml.getvalue().encode())))
    core.reduce_diff_noise(*trees)
    assert etree.tostring(trees[0]) == etree.tostring(trees[1])


@pytest.fixture
def normalizer_bin():
    """Opt into the Rust/Python integration with OTL_NORMALIZER_BIN."""
    binary = os.environ.get("OTL_NORMALIZER_BIN")
    if binary is None:
        pytest.skip("set OTL_NORMALIZER_BIN to run the SinglePos integration tests")
    path = Path(binary)
    assert path.is_file(), path
    return path


@pytest.mark.parametrize(
    "extension,unwrap", [(False, False), (True, False), (True, True)]
)
@pytest.mark.parametrize("delta,index,identical", [(40, 0, False), (20, 1, True)])
def test_variable_singlepos_comparison(
    tmp_path,
    cli_flags,
    monkeypatch,
    capsys,
    normalizer_bin,
    extension,
    unwrap,
    delta,
    index,
    identical,
):
    cli_flags.unwrap_extensions = unwrap
    monkeypatch.setenv(
        "PATH", str(Path(sys.executable).parent) + os.pathsep + os.environ["PATH"]
    )
    fontmake = tmp_path / "fontmake.ttf"
    fontc = tmp_path / "fontc.ttf"
    _variable_singlepos_font(20, extension=extension).save(fontmake)
    _variable_singlepos_font(delta, index, extension).save(fontc)
    output = core.generate_output(tmp_path, normalizer_bin, fontmake, fontc)
    assert (output["fontc"] == output["fontmake"]) == identical
    core.print_output(tmp_path, output)
    text = capsys.readouterr().out
    if identical:
        assert "DIFF" not in text
        assert "Identical '(mark/kern)'" in text
    else:
        assert "DIFF '(mark/kern)'" in text
