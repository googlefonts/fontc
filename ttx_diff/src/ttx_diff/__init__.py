"""ttx-diff: A tool for comparing font compiler outputs."""

from importlib.metadata import PackageNotFoundError, version

try:
    __version__ = version("ttx-diff")
except PackageNotFoundError:
    # Package is not installed, likely running from source
    __version__ = "0.0.0+unknown"

from ttx_diff.core import main

__all__ = ["main", "__version__"]
