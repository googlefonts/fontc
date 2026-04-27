# Sub-Plan 9: CLI Wiring and Flags

## What

Add SVG directory support to the fontc CLI: new Input variant, new CLI flags
for font metadata, and wiring through to SvgIrSource.

## Where

**Files**:
- `fontc/src/args.rs` — new CLI flags
- `fontc/src/lib.rs` — new Input variant + create_source
- `fontc/Cargo.toml` — add svg2fontir dependency

## Changes

### `fontc/Cargo.toml`

Add dependency:
```toml
svg2fontir = { version = "0.1.0", path = "../svg2fontir" }
```

### `fontc/src/args.rs` — New CLI Flags

Add to the `Args` struct:

```rust
/// Font family name. Required when compiling SVG directories.
#[arg(long)]
pub family_name: Option<String>,

/// Units per em. Default: 1000.
#[arg(long, default_value = "1000")]
pub upem: u16,

/// Ascender value in font units. Default: derived from SVG viewBox.
#[arg(long)]
pub ascender: Option<i16>,

/// Descender value in font units. Default: derived from SVG viewBox.
#[arg(long)]
pub descender: Option<i16>,
```

Update help text for `input_source`:
```rust
/// A designspace, ufo, glyphs, fontra file, or directory of SVGs (with .svg extension)
```

### `fontc/src/lib.rs` — Input Enum

Add variant:
```rust
pub enum Input {
    DesignSpacePath(PathBuf),
    GlyphsPath(PathBuf),
    FontraPath(PathBuf),
    GlyphsMemory(String),
    SvgDirPath(PathBuf),  // NEW
}
```

In `Input::new()`:
```rust
match ext {
    "designspace" => Ok(Input::DesignSpacePath(path.to_path_buf())),
    "ufo" => Ok(Input::DesignSpacePath(path.to_path_buf())),
    "glyphs" => Ok(Input::GlyphsPath(path.to_path_buf())),
    "glyphspackage" => Ok(Input::GlyphsPath(path.to_path_buf())),
    "fontra" => Ok(Input::FontraPath(path.to_path_buf())),
    "svg" => Ok(Input::SvgDirPath(path.to_path_buf())),  // NEW
    _ => Err(Error::UnrecognizedSource(path.to_path_buf())),
}
```

### `fontc/src/lib.rs` — Options Extension

Extend `Options` to carry SVG-specific metadata:

```rust
pub struct Options {
    // ... existing fields ...

    /// SVG font metadata (only used for SvgDirPath input)
    pub svg_metadata: Option<SvgMetadata>,
}

pub struct SvgMetadata {
    pub family_name: String,
    pub upem: u16,
    pub ascender: Option<i16>,
    pub descender: Option<i16>,
}
```

### `fontc/src/lib.rs` — create_source

```rust
pub fn create_source(&self, options: &Options) -> Result<Box<dyn Source>, Error> {
    match self {
        Input::DesignSpacePath(path) => Ok(Box::new(DesignSpaceIrSource::new(path)?)),
        Input::GlyphsPath(path) => Ok(Box::new(GlyphsIrSource::new(path)?)),
        Input::FontraPath(path) => Ok(Box::new(FontraIrSource::new(path)?)),
        Input::GlyphsMemory(source) => Ok(Box::new(GlyphsIrSource::new_from_memory(source)?)),
        Input::SvgDirPath(path) => {
            let meta = options.svg_metadata.as_ref()
                .ok_or_else(|| Error::UnrecognizedSource(path.clone()))?;
            let (ascender, descender) = derive_svg_metrics(meta, path)?;
            Ok(Box::new(SvgIrSource::new(
                path,
                meta.family_name.clone(),
                meta.upem,
                ascender,
                descender,
            )?))
        }
    }
}
```

### `derive_svg_metrics` helper

```rust
fn derive_svg_metrics(meta: &SvgMetadata, _svg_dir: &Path) -> Result<(i16, i16), Error> {
    let ascender = meta.ascender.unwrap_or(meta.upem as i16);
    let descender = meta.descender.unwrap_or(0);
    Ok((ascender, descender))
}
```

### Args → Options conversion

In `args.rs`, update `TryInto<Options>`:

```rust
impl TryFrom<Args> for Options {
    type Error = Error;
    fn try_from(args: Args) -> Result<Self, Self::Error> {
        let svg_metadata = if args.family_name.is_some() {
            Some(SvgMetadata {
                family_name: args.family_name.unwrap(),
                upem: args.upem,
                ascender: args.ascender,
                descender: args.descender,
            })
        } else {
            None
        };

        Ok(Options {
            // ... existing fields ...
            svg_metadata,
        })
    }
}
```

### Validation

In `Input::new()`, for SVG directories, validate that the path is actually a directory:

```rust
"svg" => {
    if !path.is_dir() {
        return Err(Error::ExpectedDirectory(path.to_path_buf()));
    }
    Ok(Input::SvgDirPath(path.to_path_buf()))
}
```

Also add validation that `--family-name` is provided when input is SVG.

## Usage Examples

```bash
# Basic SVG font compilation
fontc my_svgs.svg --family-name "My Icon Font" -o icons.ttf

# With custom metrics
fontc my_svgs.svg --family-name "Icons" --upem 2048 --ascender 1600 --descender -400 -o icons.ttf

# This will fail (missing required flag)
fontc my_svgs.svg
# Error: --family-name is required for SVG input
```

## Tests

### In `fontc/src/lib.rs`:

```rust
#[test]
fn test_svg_input_recognition() {
    let input = Input::new(Path::new("../resources/testdata/svg/simple_glyphs"))
        .expect("should recognize .svg directory");
    match input {
        Input::SvgDirPath(_) => {}
        _ => panic!("Expected SvgDirPath"),
    }
}

#[test]
fn test_svg_compile_simple() {
    // Integration test — see sub-plan 10
}
```

## How to Verify

```bash
cargo check -p fontc
cargo test -p fontc -- svg
cargo clippy -p fontc --all-features --all-targets -- -D warnings
cargo run -p fontc -- resources/testdata/svg/simple_glyphs --family-name "Test" -o build/test.ttf
```

## Depends On

- Sub-plan 7 (SvgIrSource must exist)

## Blocks

- Sub-plan 10 (integration tests use CLI)
