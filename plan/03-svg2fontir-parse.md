# Sub-Plan 3: Create `svg2fontir` Crate with SVG Parsing

## What

Create the `svg2fontir` workspace crate with `usvg` dependency, directory
scanning, filename→codepoint parsing, and SVG tree extraction.

## Where

**New directory**: `svg2fontir/`
**Modify**: `Cargo.toml` (workspace members), `fontc/Cargo.toml` (add dependency later)

## Files to Create

### `svg2fontir/Cargo.toml`

```toml
[package]
name = "svg2fontir"
version = "0.1.0"
edition = "2021"
license = "Apache-2.0"

[dependencies]
fontdrasil = { version = "0.1.0", path = "../fontdrasil" }
fontir = { version = "0.6.0", path = "../fontir" }
usvg = "0.44"
kurbo.workspace = true
log.workspace = true
ordered-float.workspace = true
```

### `svg2fontir/src/lib.rs`

```rust
pub mod source;
mod parse;
mod transform;
mod color;
mod toir;
```

### `svg2fontir/src/parse.rs`

Core SVG parsing module. Responsibilities:

1. **`discover_svgs(dir: &Path) -> Result<HashMap<GlyphName, PathBuf>>`**
   - Scan directory for `u{HEX}.svg` files
   - Parse hex to Unicode codepoint → glyph name
   - Skip non-matching files (no error, just warn via `log`)
   - Return map of glyph_name → file_path

2. **`parse_codepoint_from_filename(name: &str) -> Option<u32>`**
   - Strip `.svg` extension
   - Must match pattern `u` followed by 1-6 hex digits
   - Parse as u32 codepoint
   - Return None for non-matching names

3. **`parse_svg(path: &Path) -> Result<usvg::Tree>`**
   - Read file, parse with `usvg::Tree::from_str()` using default options
   - Return the parsed tree

4. **`extract_paths(tree: &usvg::Tree) -> Vec<ExtractedPath>`**
   - Walk tree recursively
   - Collect each `usvg::Path` with its fill paint, transform, and opacity
   - Group paths by their parent `usvg::Group` (for opacity/transform grouping)

```rust
pub(crate) struct ExtractedPath {
    pub segments: usvg::PathSegments,  // raw path data
    pub fill: Option<usvg::Fill>,
    pub transform: usvg::Transform,    // accumulated group transforms
    pub opacity: f32,                  // accumulated group opacity
}
```

5. **`extract_gradient_defs(tree: &usvg::Tree) -> HashMap<String, GradientInfo>`**
   - Collect all `usvg::LinearGradient` and `usvg::RadialGradient` from tree
   - Store with their stops, coordinates, spread method

### `svg2fontir/src/transform.rs` (stub for now)

```rust
// Coordinate transform module — implemented in sub-plan 4
```

### `svg2fontir/src/color.rs` (stub for now)

```rust
// Color/gradient conversion module — implemented in sub-plan 5
```

### `svg2fontir/src/toir.rs` (stub for now)

```rust
// SVG → IR conversion module — implemented in sub-plan 6
```

### `svg2fontir/src/source.rs` (stub for now)

```rust
// Source trait implementation — implemented in sub-plan 7
```

## Tests

### In `svg2fontir/src/parse.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn testdata_dir() -> PathBuf {
        Path::new("../resources/testdata/svg").to_path_buf()
    }

    #[rstest]
    #[case("u0041", Some(0x0041))]
    #[case("u1F600", Some(0x1F600))]
    #[case("u0041_u0301", None)]  // multi-codepoint not supported yet
    #[case("readme.txt", None)]
    #[case("uGGGG", None)]
    #[case("0041", None)]  // missing 'u' prefix
    #[case("U0041", None)]  // uppercase U not supported
    fn test_parse_codepoint(#[case] input: &str, #[case] expected: Option<u32>) {
        assert_eq!(parse_codepoint_from_filename(input), expected);
    }

    #[test]
    fn test_discover_svgs_simple() {
        let dir = testdata_dir().join("simple_glyphs");
        let glyphs = discover_svgs(&dir).unwrap();
        assert_eq!(glyphs.len(), 2);
        assert!(glyphs.contains_key(&GlyphName::new("A")));       // U+0041
        assert!(glyphs.contains_key(&GlyphName::new("B")));       // U+0042
    }

    #[test]
    fn test_discover_svgs_ignores_non_matching() {
        // Create temp dir with mixed files
        let dir = testdata_dir();  // contains only valid u*.svg files
        let glyphs = discover_svgs(&dir).unwrap();
        // Should find all valid SVGs, skip nothing
        for name in glyphs.keys() {
            assert!(name.as_str().len() <= 4);  // single codepoint names
        }
    }

    #[test]
    fn test_parse_simple_svg() {
        let path = testdata_dir().join("simple_glyphs/u0041.svg");
        let tree = parse_svg(&path).unwrap();
        let paths = extract_paths(&tree);
        assert!(!paths.is_empty());
        assert!(paths[0].fill.is_some());
    }

    #[test]
    fn test_parse_multi_layer_svg() {
        let path = testdata_dir().join("multi_layer/u1F600.svg");
        let tree = parse_svg(&path).unwrap();
        let paths = extract_paths(&tree);
        assert!(paths.len() >= 3);  // face, eyes, mouth
    }

    #[test]
    fn test_parse_gradient_svg() {
        let path = testdata_dir().join("gradient_glyphs/u2B50.svg");
        let tree = parse_svg(&path).unwrap();
        let gradients = extract_gradient_defs(&tree);
        assert!(!gradients.is_empty());
    }
}
```

## Workspace Changes

### Root `Cargo.toml` — add to `[workspace].members`:

```toml
"svg2fontir",
```

## How to Verify

```bash
cargo check -p svg2fontir
cargo test -p svg2fontir
cargo clippy -p svg2fontir --all-targets -- -D warnings
```

## Depends On

- Sub-plan 1 (test SVG fixtures must exist)

## Blocks

- Sub-plan 4, 5, 6, 7 (all build on the parsing foundation)
