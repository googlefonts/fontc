# Repository Guidelines

## Project Structure & Module Organization

This repository is a Cargo workspace for the Rust-based `fontc` compiler. The main pipeline is split across crates: `fontc/` contains the CLI, `fontir/` the intermediate representation, `fontbe/` the binary-font backend, and `fontdrasil/` shared utilities. Source adapters are in `glyphs2fontir/`, `ufo2fontir/`, and `fontra2fontir/`; `fea-rs/` handles feature files; `otl-normalizer/` and `fontc_crater/` provide evaluation tools. Rust sources are in each crate’s `src/`, with fixtures under `resources/` or crate-specific `data/` and `testdata/`. The independent Python comparison tool is in `ttx_diff/`.

## Current Work Context

Basic COLRv1 support is implemented: `fontc` can compile the Noto Color Emoji source and generate a TTF. The output still has known bugs, so COLRv1 work should be validated against the sibling checkout at `/home/wmedrano/src/noto-emoji`, which is the primary reference/test source.

For the Noto Color Emoji reference build, use the nanoemoji TOML configuration rather than passing only the SVG directory:

- `/home/wmedrano/src/noto-emoji/colrv1/all.toml` defines the full COLRv1 build.
- `/home/wmedrano/src/noto-emoji/colrv1/noflags.toml` defines the variant without regional-indicator flags.
- The configs list the SVG inputs explicitly, primarily from `/home/wmedrano/src/noto-emoji/svg/`, with waved flag inputs under `third_party/region-flags/waved-svg/`.
- `colrv1_generate_configs.py` regenerates these configs from the SVG directories. Do not hand-edit generated input lists unless the change is intentionally to the generated config.
- The checked-in reference outputs are `/home/wmedrano/src/noto-emoji/fonts/Noto-COLRv1.ttf` and `Noto-COLRv1-noflags.ttf`.

To rebuild the reference COLRv1 fonts, follow `noto-emoji/full_rebuild.sh`; its relevant steps are `python colrv1_generate_configs.py` and `(cd colrv1 && nanoemoji *.toml)`. Compare font output against the checked-in reference fonts when making COLRv1 changes.

## Build, Test, and Development Commands

Run commands from the repository root:

- `cargo check --workspace --all-targets --all-features` checks the workspace.
- `cargo build --workspace` builds all Rust packages.
- `cargo test --workspace --all-features` runs Rust tests.
- `cargo test --locked -p fontc --all-targets --all-features` focuses on the CLI.
- `cargo fmt --all -- --check` verifies formatting; use `cargo fmt --all` to apply it.
- `cargo clippy --locked --all-features --all-targets -- -D warnings` runs CI-equivalent linting.
- `cargo run -p fontc -- path/to/source.designspace` builds a font from a source file.
- In `ttx_diff/`, install test dependencies with `pip install -e ".[test]"` and run `pytest -v`.

Keep `Cargo.lock` committed and use `--locked` for reproducible checks. The optional `./resources/githooks/pre-push` hook mirrors important CI checks.

## Coding Style & Naming Conventions

Use Rust 2024 formatting via `rustfmt.toml`. Follow idiomatic Rust naming: `snake_case` for modules, functions, and variables; `UpperCamelCase` for types and traits; and `SCREAMING_SNAKE_CASE` for constants. Prefer structured logging over `println!`/`eprintln!` (CI rejects them). Python code follows Ruff formatting and linting.

## Testing Guidelines

Rust tests generally live in `#[cfg(test)]` modules or crate `src/tests/`; add focused regression tests near the affected crate and use descriptive `snake_case` names. Use fixtures in the relevant `testdata/` or `resources/` directory. Run the narrowest package test first, then the workspace suite. `ttx_diff/tests/` uses `pytest`.

## Commit & Pull Request Guidelines

Use short, imperative, area-prefixed subjects when useful (for example, `[fea-rs] Match mark classes...`). Keep commits focused and explain compatibility or output changes. Pull requests should describe the problem, approach, affected crates, and validation commands; include comparison results for font-output changes and link related issues. Ensure formatting, lint, tests, docs, and no-`println!` checks pass before review.

## Security & Dependency Updates

Do not commit secrets or generated artifacts. Dependency changes should update manifests and `Cargo.lock` together; run workspace checks and explain the reason in the PR. Review `.cargo/audit.toml` when addressing advisories.
