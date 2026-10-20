"""Tests for the ttx-diff CLI."""

import subprocess
import sys

import pytest
from glyphsLib import GSFont
from glyphsLib.classes import GSInstance, InstanceType

from ttx_diff.core import builds_static_export, source_is_variable


def _write_designspace(tmp_path, axes, sources):
    from fontTools.designspaceLib import (
        AxisDescriptor,
        DesignSpaceDocument,
        SourceDescriptor,
    )

    ds = DesignSpaceDocument()
    for tag, name, mn, df, mx in axes:
        a = AxisDescriptor()
        a.tag, a.name = tag, name
        a.minimum, a.default, a.maximum = mn, df, mx
        ds.addAxis(a)
    for loc in sources:
        s = SourceDescriptor()
        s.location = loc
        ds.addSource(s)
    path = tmp_path / "test.designspace"
    ds.write(path)
    return path


def _write_glyphs(tmp_path, masters, virtual_masters=None, name="test.glyphs"):
    """Create a minimal .glyphs file using glyphsLib and save to tmp_path."""
    from glyphsLib.classes import (
        GSAxis,
        GSCustomParameter,
        GSFont,
        GSFontMaster,
        GSGlyph,
        GSLayer,
    )

    font = GSFont()
    font.familyName = "Test"
    font.upm = 1000
    font.versionMajor = 1
    axis = GSAxis()
    axis.name = "Weight"
    axis.axisTag = "wght"
    font.axes = [axis]
    for val in masters:
        m = GSFontMaster()
        m.axes = [val]
        font.masters.append(m)
    if virtual_masters:
        for vm_val in virtual_masters:
            font.customParameters.append(
                GSCustomParameter(
                    "Virtual Master", [{"Axis": "Weight", "Location": vm_val}]
                )
            )
    glyph = GSGlyph("space")
    glyph.unicode = "0020"
    for m in font.masters:
        layer = GSLayer()
        layer.layerId = m.id
        layer.width = 200
        glyph.layers.append(layer)
    font.glyphs.append(glyph)
    path = tmp_path / name
    font.save(str(path))
    return path


def test_version():
    import ttx_diff

    assert hasattr(ttx_diff, "__version__")
    assert isinstance(ttx_diff.__version__, str)


def test_cli_missing_source():
    """Test CLI with non-existent source file."""
    result = subprocess.run(
        [sys.executable, "-m", "ttx_diff", "/nonexistent/file.glyphs"],
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert "No such source" in result.stderr


class TestSourceIsVariable:
    def test_ufo_is_static(self, tmp_path):
        ufo = tmp_path / "test.ufo"
        ufo.mkdir()
        (ufo / "metainfo.plist").write_text("")
        assert not source_is_variable(ufo)

    def test_ufoz_is_static(self, tmp_path):
        ufoz = tmp_path / "test.ufoz"
        ufoz.write_bytes(b"")
        assert not source_is_variable(ufoz)

    def test_designspace_multiple_sources(self, tmp_path):
        path = _write_designspace(
            tmp_path,
            axes=[("wght", "Weight", 400, 400, 700)],
            sources=[{"Weight": 400}, {"Weight": 700}],
        )
        assert source_is_variable(path)

    def test_designspace_single_source_with_axis_range(self, tmp_path):
        # 1 source but axis has min != max, like NotoSerifMakasar
        # https://github.com/googlefonts/fontc/issues/1860
        path = _write_designspace(
            tmp_path,
            axes=[("wght", "Weight", 400, 400, 700)],
            sources=[{"Weight": 400}],
        )
        assert source_is_variable(path)

    def test_designspace_point_axes_only(self, tmp_path):
        path = _write_designspace(
            tmp_path,
            axes=[("wght", "Weight", 400, 400, 400)],
            sources=[{"Weight": 400}],
        )
        assert not source_is_variable(path)

    def test_designspace_mixed_point_and_range_axes(self, tmp_path):
        # Like Mingzat: wght 400-700, wdth 100-100, XXXX 0-0
        # https://github.com/googlefonts/fontc/issues/1860
        path = _write_designspace(
            tmp_path,
            axes=[
                ("wght", "Weight", 400, 400, 700),
                ("wdth", "Width", 100, 100, 100),
                ("XXXX", "Custom", 0, 0, 0),
            ],
            sources=[{"Weight": 400, "Width": 100, "Custom": 0}],
        )
        assert source_is_variable(path)

    def test_glyphs_multiple_masters(self, tmp_path):
        path = _write_glyphs(tmp_path, masters=[400, 700])
        assert source_is_variable(path)

    def test_glyphs_single_master(self, tmp_path):
        path = _write_glyphs(tmp_path, masters=[400])
        assert not source_is_variable(path)

    def test_glyphs_virtual_masters_extend_axis_range(self, tmp_path):
        # 1 master at wght=400, virtual master at wght=700
        path = _write_glyphs(tmp_path, masters=[400], virtual_masters=["700"])
        assert source_is_variable(path)


def _instance(name, active=True, type=InstanceType.SINGLE):
    instance = GSInstance()
    instance.name = name
    instance.active = active
    instance.type = type
    return instance


@pytest.mark.parametrize(
    "instances, expected",
    [
        ([_instance("Heavy")], True),
        ([_instance("Heavy", active=False)], False),
        (
            [
                _instance("Heavy"),
                _instance("Other", active=False),
                _instance("Variable Font Settings", type=InstanceType.VARIABLE),
            ],
            True,
        ),
        ([_instance("Heavy"), _instance("Other")], False),
        ([], False),
    ],
)
def test_builds_static_export(instances, expected):
    """Only one active static instance is built with fontmake -i."""
    font = GSFont()
    font.instances = instances
    assert builds_static_export(font) == expected


def test_cli_export_requires_default_compare(tmp_path):
    path = _write_glyphs(tmp_path, masters=[400])
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "ttx_diff",
            "--compare",
            "gftools",
            "--export",
            "Test Regular",
            str(path),
        ],
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert "--export is only supported with --compare default" in result.stderr


def test_export_passed_to_both_compilers(tmp_path, monkeypatch):
    """fontc gets the name verbatim, fontmake as an escaped regex, both static."""
    from absl.testing import flagsaver

    from ttx_diff import (
        __main__,  # noqa: F401 -- defines the CLI flags
        core,
    )

    # a variable source, so the explicit export also makes fontmake build static
    path = _write_glyphs(tmp_path, masters=[400, 700])
    commands = []
    monkeypatch.setattr(core, "build", lambda cmd, build_dir: commands.append(cmd))
    name = "Test B1 Heavy (Alt)"
    with flagsaver.flagsaver(export=name, keep_overlaps=True):
        core.FLAGS.mark_as_parsed()
        core.build_fontc(path, tmp_path / "fontc", tmp_path)
        core.build_fontmake(path, tmp_path)
    fontc_cmd, fontmake_cmd = commands
    assert fontc_cmd[fontc_cmd.index("--export") + 1] == name
    assert fontmake_cmd.count("-i") == 1
    assert fontmake_cmd[fontmake_cmd.index("-i") + 1] == r"Test\ B1\ Heavy\ \(Alt\)"
    assert fontmake_cmd[fontmake_cmd.index("-o") + 1] == "ttf"
    assert "--keep-overlaps" in fontmake_cmd
