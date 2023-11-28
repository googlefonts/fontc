# layout-normalizer

This crate converts layout to a normalized representation, suitable for comparison in a text diff.

It's purpose is to enable `resources/scripts/ttx_diff.py` to determine that layout in two binary fonts
is identical even when not binary equivalent.
