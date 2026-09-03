"""Tests for ttx_diff.core."""

import hashlib

import pytest
from lxml import etree

from ttx_diff.core import (
    FONTC_TTF_HASH_FILE,
    UNCHANGED_EXIT_CODE,
    hash_file,
    strip_fontc_version_tag,
    unwrap_extension_lookups,
    write_hash_and_maybe_exit_early,
)


def _make_tree(xml_str):
    return etree.ElementTree(etree.fromstring(xml_str))


GSUB_WITH_EXTENSION = """\
<ttFont>
  <GSUB>
    <LookupList>
      <Lookup index="0">
        <LookupType value="7"/>
        <ExtensionSubst index="0">
          <ExtensionLookupType value="1"/>
          <SingleSubst Format="2">
            <Substitution in="a" out="b"/>
          </SingleSubst>
        </ExtensionSubst>
      </Lookup>
      <Lookup index="1">
        <LookupType value="1"/>
        <SingleSubst index="0" Format="2">
          <Substitution in="c" out="d"/>
        </SingleSubst>
      </Lookup>
    </LookupList>
  </GSUB>
</ttFont>"""

GPOS_WITH_EXTENSION = """\
<ttFont>
  <GPOS>
    <LookupList>
      <Lookup index="0">
        <LookupType value="9"/>
        <ExtensionPos index="0">
          <ExtensionLookupType value="4"/>
          <MarkBasePos Format="1">
            <MarkCoverage/>
          </MarkBasePos>
        </ExtensionPos>
        <ExtensionPos index="1">
          <ExtensionLookupType value="4"/>
          <MarkBasePos Format="1">
            <MarkCoverage/>
          </MarkBasePos>
        </ExtensionPos>
      </Lookup>
    </LookupList>
  </GPOS>
</ttFont>"""


def test_gsub_extension_unwrapped():
    tree = _make_tree(GSUB_WITH_EXTENSION)
    unwrap_extension_lookups(tree)

    lookups = tree.findall(".//Lookup")
    # Extension lookup unwrapped
    assert lookups[0].find("LookupType").attrib["value"] == "1"
    assert lookups[0].find("ExtensionSubst") is None
    sub = lookups[0].find("SingleSubst")
    assert sub is not None
    assert sub.attrib["index"] == "0"

    # Non-extension lookup untouched
    assert lookups[1].find("LookupType").attrib["value"] == "1"
    assert lookups[1].find("SingleSubst") is not None


def test_gpos_extension_multiple_subtables():
    tree = _make_tree(GPOS_WITH_EXTENSION)
    unwrap_extension_lookups(tree)

    lookup = tree.find(".//Lookup")
    assert lookup.find("LookupType").attrib["value"] == "4"
    assert lookup.find("ExtensionPos") is None
    marks = lookup.findall("MarkBasePos")
    assert len(marks) == 2
    assert marks[0].attrib["index"] == "0"
    assert marks[1].attrib["index"] == "1"


GSUB_NO_EXTENSION = """\
<ttFont>
  <GSUB>
    <LookupList>
      <Lookup index="0">
        <LookupType value="1"/>
        <SingleSubst index="0" Format="2"/>
      </Lookup>
    </LookupList>
  </GSUB>
</ttFont>"""


def test_no_extensions_is_noop():
    tree = _make_tree(GSUB_NO_EXTENSION)
    before = etree.tostring(tree)
    unwrap_extension_lookups(tree)
    assert etree.tostring(tree) == before


def _name_tree(version_string):
    return _make_tree(
        f"""\
<ttFont>
  <name>
    <namerecord nameID="5" platformID="3" platEncID="1" langID="0x409">
      {version_string}
    </namerecord>
  </name>
</ttFont>"""
    )


def test_strip_fontc_version_tag_matches_fontmake():
    # After stripping, fontc's stamped version string is byte-identical to
    # fontmake's unstamped one (including TTX indentation).
    fontc = _name_tree("Version 1.000;fontc 0.6.1-dev.394+gd62ba016.dirty")
    fontmake = _name_tree("Version 1.000")
    strip_fontc_version_tag(fontc)
    strip_fontc_version_tag(fontmake)
    assert etree.tostring(fontc) == etree.tostring(fontmake)


def test_strip_fontc_version_tag_is_noop_without_tag():
    tree = _name_tree("Version 1.000")
    before = etree.tostring(tree)
    strip_fontc_version_tag(tree)
    assert etree.tostring(tree) == before


def test_strip_keeps_non_stamp_fontc_note():
    # Only the digit-led ";fontc <version>" stamp is removed; a human note that
    # merely starts a segment with "fontc " survives, so a real diff in it isn't
    # masked.
    got = _name_tree("Version 1.000;fontc is broken;fontc 0.6.1-dev.394+gd62ba016")
    want = _name_tree("Version 1.000;fontc is broken")
    strip_fontc_version_tag(got)
    assert etree.tostring(got) == etree.tostring(want)


def test_hash_file_matches_hashlib(tmp_path):
    font = tmp_path / "fontc.ttf"
    font.write_bytes(b"not really a font" * 1000)
    assert hash_file(font) == hashlib.sha256(font.read_bytes()).hexdigest()


def test_write_hash_records_the_hash(tmp_path):
    font = tmp_path / "fontc.ttf"
    font.write_bytes(b"pretend this is a font")
    write_hash_and_maybe_exit_early(font, tmp_path, None)
    assert (tmp_path / FONTC_TTF_HASH_FILE).read_text() == hash_file(font)


def test_write_hash_exits_when_the_hash_matches(tmp_path):
    font = tmp_path / "fontc.ttf"
    font.write_bytes(b"pretend this is a font")
    with pytest.raises(SystemExit) as exit:
        write_hash_and_maybe_exit_early(font, tmp_path, hash_file(font))
    assert exit.value.code == UNCHANGED_EXIT_CODE


def test_write_hash_continues_when_the_hash_differs(tmp_path):
    font = tmp_path / "fontc.ttf"
    font.write_bytes(b"pretend this is a font")
    write_hash_and_maybe_exit_early(font, tmp_path, "0" * 64)
    assert (tmp_path / FONTC_TTF_HASH_FILE).read_text() == hash_file(font)


# fontc failing to build is not a result we can cache, so there is nothing to
# record and nothing to skip
def test_write_hash_is_a_noop_without_a_font(tmp_path):
    write_hash_and_maybe_exit_early(tmp_path / "fontc.ttf", tmp_path, "0" * 64)
    assert not (tmp_path / FONTC_TTF_HASH_FILE).exists()
