# glyphs-reader

[![Crates.io](https://img.shields.io/crates/v/glyphs-reader.svg)](https://crates.io/crates/glyphs-reader)
[![Docs.rs](https://docs.rs/glyphs-reader/badge.svg)](https://docs.rs/glyphs-reader)
[![MIT/Apache 2.0](https://img.shields.io/badge/license-MIT%2FApache-blue.svg)](#license)

This crate is a lightweight library for reading Glyphs 2 and Glyphs 3 font files, with support for writing the underlying Property List (`Plist`) format.

It is part of [`fontc`], a font compiler.

## Origins

Plist code copied from https://github.com/raphlinus/interp-toy/tree/main/glyphstool
at e87f62c0922ce04ea0cee83d624bd9b7d8eafbd8.

Lightly modified:

* removed code related to modifying the font to focus on reading
* fixed clippy warnings
* made Plist Hash

[`fontc`]: https://github.com/googlefonts/fontc
