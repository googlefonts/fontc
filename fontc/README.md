# fontc

[![Crates.io](https://img.shields.io/crates/v/fontc.svg)](https://crates.io/crates/fontc)
[![Docs.rs](https://docs.rs/fontc/badge.svg)](https://docs.rs/fontc)
[![MIT/Apache 2.0](https://img.shields.io/badge/license-MIT%2FApache-blue.svg)](#license)

fontc: Font Compiler

This crate provides the command line entrypoint into our font compiler. It also
exposes a programmatic library entrypoint (`pub fn generate_font`) for
integrating `fontc` natively as a Rust library.

Its primary task is to accept arguments from the user, create a `Workload`, and execute it using the orchestration system.

This crate can reference all other crates in the workspace.
