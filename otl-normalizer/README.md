# otl-normalizer

[![Crates.io](https://img.shields.io/crates/v/otl-normalizer.svg)](https://crates.io/crates/otl-normalizer)
[![Docs.rs](https://docs.rs/otl-normalizer/badge.svg)](https://docs.rs/otl-normalizer)
[![MIT/Apache 2.0](https://img.shields.io/badge/license-MIT%2FApache-blue.svg)](#license)

This crate converts OpenType layout subtables to a normalized representation,
suitable for comparison in a text diff.

This currently supports a subset of GPOS (kerning and marks) and normalizes the GDEF ligature caret table. GSUB is stubbed out but explicitly noted as not fully supported.

It is part of the [`fontc`] project, and is used to test font compilation as
well as to compare the output of different compiler toolchains.


[`fontc`]: https://github.com/googlefonts/fontc

## License

Licensed under the Apache License, Version 2.0 ([LICENSE](../LICENSE) or http://www.apache.org/licenses/LICENSE-2.0).
