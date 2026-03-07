# fontir

[![Crates.io](https://img.shields.io/crates/v/fontir.svg)](https://crates.io/crates/fontir)
[![Docs.rs](https://docs.rs/fontir/badge.svg)](https://docs.rs/fontir)
[![MIT/Apache 2.0](https://img.shields.io/badge/license-MIT%2FApache-blue.svg)](#license)

fontir: Font Intermediate Representation

This crate defines the intermediate representation (IR) of a font used by fontc and common operations
on that IR.

It should be referenced by fontc, any frontend (_format_2fontir) crates, and the backend crate, `fontbe`, which heavily references `fontir` to read the Intermediate Representation and compile it into binary tables.
