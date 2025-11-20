"""Entry point wrapper for ttx-diff command-line tool.

This wrapper uses runpy to execute __main__.py as the main module,
which ensures absl-py treats its flags as main module flags and displays
them in --help output.
"""

import runpy
import sys


def main():
    """Entry point that executes ttx_diff.__main__ as the main module."""
    # Remove this script from sys.argv so the target module gets clean args
    sys.argv[0] = "ttx-diff"
    # Execute __main__.py as if it were run with python -m ttx_diff
    runpy.run_module("ttx_diff.__main__", run_name="__main__")


if __name__ == "__main__":
    main()
