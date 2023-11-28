# glyphs-reader

This crate reads Glyphs 2 and Glyphs 3 files.

It should be referenced only by glyphs2fontir.

## Origins
Plist code copied from https://github.com/raphlinus/interp-toy/tree/main/glyphstool
at e87f62c0922ce04ea0cee83d624bd9b7d8eafbd8.

Lightly modified:

* removed code related to modifying the font to focus on reading
* fixed clippy warnings
* made Plist Hash

TODO: make from_plist return a Result