"""Command-line interface for ttx-diff."""

from absl import app

from ttx_diff.core import main as core_main


def main():
    """Entry point for the ttx-diff command-line tool."""
    app.run(core_main)


if __name__ == "__main__":
    main()
