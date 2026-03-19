# Agent Instructions for fontc

This file provides guidelines for AI agents working in the fontc codebase.

## Build Commands

```bash
# Build the entire workspace
cargo build

# Build a specific crate
cargo build -p fontc
cargo build -p fontbe
cargo build -p fontir

# Build for release
cargo build --release

# Build for WASM
cargo build -p fontc --target wasm32-unknown-unknown
```

## Lint and Format

```bash
# Check formatting
cargo fmt --all -- --check

# Format code
cargo fmt --all

# Run clippy lints
cargo clippy --all-features --all-targets -- -D warnings

# Run clippy with no default features
cargo clippy --all-targets --no-default-features -- -D warnings

# Check for println!/eprintln! (no debug printing allowed)
./resources/scripts/check_no_println.sh
```

## Testing

```bash
# Run all tests with all features
cargo test --all-features

# Run tests for a specific crate
cargo test -p fontbe --all-targets --all-features
cargo test -p fontc --all-targets --all-features
cargo test -p fontir --all-targets --all-features
cargo test -p fea-rs

# Run tests without default features
cargo test --no-default-features

# Run a single test
cargo test -p <crate> --test <test_name>
cargo test -p fontir test_specific_function_name
cargo test --lib -- test_function_name

# Run doc tests
cargo test --doc

# Run benchmarks
cargo bench -p fontc --bench compile
```

## Code Style

### Formatting
- Uses Rust 2024 edition (see `rustfmt.toml`)
- Run `cargo fmt --all` before committing

### Imports
- Group imports by: std → external crates → local/crates.io → workspace
- Use `use` statements for clarity, avoid deep nesting
- Prefer absolute paths for clarity: `use crate::module::Item;`

### Error Handling
- Use `thiserror` for error types with `#[derive(Error)]`
- Use `anyhow` only for error types that need `Error` trait object behavior
- Return `Result<T, Error>` for fallible operations
- Avoid `unwrap()` in production code; use `expect()` with context or `?` operator
- Use `#[allow(clippy::unwrap_used)]` only in test code

### Naming Conventions
- Types: `PascalCase` (e.g., `GlyphName`, `CoordConverter`)
- Functions/methods: `snake_case` (e.g., `from_str`, `get_axis`)
- Constants: `SCREAMING_SNAKE_CASE` for true constants
- Variables: `snake_case` (e.g., `axis_tag`, `default_value`)
- Modules: `snake_case`
- Crate names: `kebab-case`

### Documentation
- Document public APIs with doc comments (`///`)
- Include links to relevant specs (e.g., `<https://docs.microsoft.com/...>`)
- Use `//!` for module-level documentation

### Clippy Configuration
See `.clippy.toml` for lint exceptions:
- `unwrap_used` and `expect_used` are denied by default
- `indexing_slicing` is denied in fontdrasil
- Some types may ignore interior mutability checks

## Crate Structure

```
fontc/           - Main CLI compiler
fontbe/          - Backend: IR → binary font
fontir/          - Intermediate Representation types
fontdrasil/      - Shared types/utilities for FE and BE
fea-rs/          - OpenType feature file parser
glyphs-reader/   - Reads .glyphs files
glyphs2fontir/   - Converts glyphs → IR
fontra2fontir/   - Converts .fontra → IR
ufo2fontir/      - Converts UFO/designspace → IR
otl-normalizer/  - OpenType Layout normalization
fontc_crater/    - Bulk compilation testing tool
```

## Key Dependencies
- `write-fonts`: Font binary writing
- `skrifa`: Font reading and traversal
- `norad`: UFO data model
- `kurbo`: 2D geometry
- `rayon`: Parallel processing
- `thiserror`: Error derivation

## Common Patterns

### Running the Compiler
```bash
cargo run -p fontc -- resources/testdata/wght_var.designspace
cargo run -p fontc -- resources/testdata/glyphs3/WghtVar.glyphs
cargo run -p fontc -- resources/testdata/fontra/minimal.fontra

# Emit IR for debugging
cargo run -p fontc -- --emit-ir <source_file>
```

### Testing with Rstest
```rust
#[rstest]
#[case("wght", 100, 400, 900)]
#[case("wdth", 50, 100, 200)]
fn test_axis_conversion(#[case] tag: &str, #[case] min: f32, ...) {
    // ...
}
```

### Module Visibility
- Use `pub mod` for public modules, `mod` for private
- Prefer granular `pub use` exports over re-exporting entire modules

## Profiling

```bash
# Run criterion benchmarks
cargo bench -p fontc --bench compile

# Flamegraph
RUST_LOG=error cargo flamegraph -p fontc -- <source>
```

## Contributing

Before pushing, run the pre-push hook:
```bash
./resources/githooks/pre-push
```

Or set it as your git hooks path:
```bash
git config core.hooksPath "resources/githooks"
```

## No Debug Printing

The codebase does not allow `println!`, `eprintln!`, or `dbg!` in production code. See `resources/scripts/check_no_println.sh` for the allowlist.

## WASM Considerations

Some crates must support `no_std`:
- fontdrasil
- fontir
- ufo2fontir
- glyphs2fontir
- fontbe
- fontc (lib only)

Run `cargo check --no-default-features` on these crates.
