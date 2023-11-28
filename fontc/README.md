# fontc

[![MIT/Apache 2.0](https://img.shields.io/badge/license-MIT%2FApache-blue.svg)](#license)

fontc: Font Compiler

This crate provides the command line entrypoint into our font compiler.

It's primary task is to accept arguments from the user, create a task graph, and execute it.

This crate can reference all other crates in the workspace.