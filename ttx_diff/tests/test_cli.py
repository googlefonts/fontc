"""Tests for the ttx-diff CLI."""

import subprocess
import sys


def test_version():
    import ttx_diff

    assert hasattr(ttx_diff, "__version__")
    assert isinstance(ttx_diff.__version__, str)


def test_cli_missing_source():
    """Test CLI with non-existent source file."""
    result = subprocess.run(
        [sys.executable, "-m", "ttx_diff.cli", "/nonexistent/file.glyphs"],
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert "No such source" in result.stderr
