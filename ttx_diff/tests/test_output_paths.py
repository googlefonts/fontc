"""Tests for the output file names ttx_diff reports in its DIFF lines."""

import pytest

from ttx_diff.core import (
    GLYF_NAME,
    GVAR_NAME,
    LIG_CARET_NAME,
    MARK_KERN_NAME,
    path_for_output_item,
)


@pytest.mark.parametrize(
    "name, expected",
    [
        (MARK_KERN_NAME, "fontc.markkern.txt"),
        (LIG_CARET_NAME, "fontc.ligcaret.txt"),
        (GLYF_NAME, "fontc.glyf.diff.txt"),
        (GVAR_NAME, "fontc.gvar.diff.txt"),
        ("GPOS", "fontc.GPOS.ttx"),
    ],
)
def test_path_for_output_item(name, expected):
    assert path_for_output_item(name, "fontc") == expected
