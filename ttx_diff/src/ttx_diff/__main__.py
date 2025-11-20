"""A tool for comparing font compiler outputs (fontc vs fontmake)."""

from absl import app, flags

from ttx_diff import core

_COMPARE_DEFAULTS = "default"
_COMPARE_GFTOOLS = "gftools"

# Flag definitions (moved from core.py so they appear in --help)
flags.DEFINE_boolean(
    "version", False, "Show application version and exit.", short_name="V"
)
flags.DEFINE_string(
    "config",
    default=None,
    help="config.yaml to be passed to gftools in gftools mode",
)
flags.DEFINE_string(
    "fontc_path",
    default=None,
    help="Optional path to precompiled fontc binary",
)
flags.DEFINE_string(
    "normalizer_path",
    default=None,
    help="Optional path to precompiled otl-normalizer binary",
)
flags.DEFINE_enum(
    "compare",
    "default",
    [_COMPARE_DEFAULTS, _COMPARE_GFTOOLS],
    "Compare results using either a default build or a build managed by gftools. Note that as of 5/21/2023 defaults still sets flags for fontmake to match fontc behavior.",
)
flags.DEFINE_enum(
    "rebuild",
    "both",
    ["both", "fontc", "fontmake", "none"],
    "Which compilers to rebuild with if the output appears to already exist. None is handy when playing with ttx_diff.py itself.",
)
flags.DEFINE_float(
    "off_by_one_budget",
    0.1,
    "The percentage of point (glyf) or delta (gvar) values allowed to differ by one without counting as a diff",
)
flags.DEFINE_bool("json", False, "print results in machine-readable JSON format")
flags.DEFINE_string("outdir", default=None, help="directory to store generated files")
flags.DEFINE_bool(
    "production_names",
    True,
    "rename glyphs to AGL-compliant names (uniXXXX, etc.) suitable for production. Disable to see the original glyph names.",
)

# fontmake - and so gftools' - static builds perform overlaps removal, but fontc
# can't do that yet, and so we default to disabling the filter to make the diff
# less noisy.
# TODO: Change the default if/when fontc gains the ability to remove overlaps.
# https://github.com/googlefonts/fontc/issues/975
flags.DEFINE_bool(
    "keep_overlaps",
    True,
    "Keep overlaps when building static fonts. Disable to compare with simplified outlines.",
)
flags.DEFINE_bool(
    "keep_direction", False, "Preserve contour winding direction from source."
)
flags.DEFINE_string(
    "fontc_font",
    default=None,
    help="Optional path to precompiled fontc font. Must be used with --fontmake_font.",
)
flags.DEFINE_string(
    "fontmake_font",
    default=None,
    help="Optional path to precompiled fontmake font. Must be used with --fontc_font.",
)


def main():
    """Entry point for running ttx-diff as a module."""
    app.run(core.main)


if __name__ == "__main__":
    main()
