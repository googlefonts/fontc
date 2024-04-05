# glyph data codegen

This directory contains a generated file, `generated_glyph_data.rs`, as well as
the tool that generates it: a python script `rebuild.py` and a small rust
program, `run_hf_codegen.rs`.

## data

The data comes from the `GlyphData.xml` file used by glyphs.app, which we access
through the glyphsLib python library.

Unlike unicode data, this data is focused on *glyphs*, not *characters*.
Specifically this means that glyphs are identified by their conventional names
(as used by font designers) and that entries are included for glyphs that are
not part of unicode, such as various alternatives, ligatures, fractions, etc.

## How it works

tl;dr: `$(venv) python glyphdata/rebuild.py`.

This script extracts the data from the `glyphsLib` python library and then
invokes `run_hf_codegen.rs`, which uses the [phf_codegen][] crate to generate a
static hashmap using a [perfect hash function][].

The generated file (`generated_glyph_data.rs`) is commited to source control,
and included in fontdrasil using the `include!` macro.

[phf_codegen]: https://docs.rs/phf_codegen/latest/phf_codegen/
[perfect hash function]: https://en.wikipedia.org/wiki/Perfect_hash_function

