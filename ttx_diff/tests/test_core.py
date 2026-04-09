"""Tests for ttx_diff.core."""

from lxml import etree

from ttx_diff.core import unwrap_extension_lookups


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
