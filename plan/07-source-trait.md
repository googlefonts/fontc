# Sub-Plan 7: Source Trait Implementation

## What

Implement the `fontir::source::Source` trait for `SvgIrSource`, wiring together
all the parsing, transform, color, and IR conversion logic from sub-plans 3-6.

## Where

**File**: `svg2fontir/src/source.rs` (created as stub in sub-plan 3)

## SvgIrSource Struct

```rust
pub struct SvgIrSource {
    svg_dir: PathBuf,
    /// Parsed SVG trees indexed by glyph name
    svgs: Arc<HashMap<GlyphName, usvg::Tree>>,
    /// Outline glyph paths (layer glyphs for COLR)
    outline_glyphs: Arc<HashMap<GlyphName, kurbo::BezPath>>,
    /// Font metadata from CLI flags
    upem: u16,
    family_name: String,
    ascender: i16,
    descender: i16,
    /// The viewBox→font transform (computed once)
    font_transform: kurbo::Affine,
}
```

## Constructor

```rust
impl SvgIrSource {
    pub fn new(
        dir: &Path,
        family_name: String,
        upem: u16,
        ascender: i16,
        descender: i16,
    ) -> Result<Self, fontir::error::Error> {
        // 1. Discover SVGs
        let svg_paths = parse::discover_svgs(dir)?;

        // 2. Parse all SVGs
        let mut svgs = HashMap::new();
        for (name, path) in &svg_paths {
            let tree = parse::parse_svg(path)?;
            svgs.insert(name.clone(), tree);
        }

        // 3. Determine viewBox from first SVG (all should be consistent)
        let first_tree = svgs.values().next().unwrap();
        let view_box = first_tree.view_box();

        // 4. Compute font transform
        let font_transform = transform::viewbox_to_font_transform(&view_box, upem);

        // 5. Build paint graph (which also extracts outline paths)
        let result = toir::svg_to_paint_result(&svgs, font_transform);

        Ok(SvgIrSource {
            svg_dir: dir.to_path_buf(),
            svgs: Arc::new(svgs),
            outline_glyphs: Arc::new(result.outline_glyphs),
            upem,
            family_name,
            ascender,
            descender,
            font_transform,
        })
    }
}
```

## Source Trait Implementation

```rust
impl Source for SvgIrSource {
    fn new(root: &Path) -> Result<Self, fontir::error::Error> {
        // This is called by fontc's Input::create_source().
        // Since SVG needs CLI metadata, we use defaults here.
        // The actual values are set via Options passthrough (see sub-plan 9).
        Self::new(root, "SVG Font".to_string(), 1000, 800, -200)
    }

    fn create_static_metadata_work(&self) -> Result<Box<IrWork>, Error> {
        // Create StaticMetadata with:
        // - upem
        // - family name (NameKey::FamilyName)
        // - glyph order (from self.svgs keys)
        // - no axes (static font from SVG)
        // - no named instances
        Ok(Box::new(StaticMetadataWork { ... }))
    }

    fn create_global_metric_work(&self) -> Result<Box<IrWork>, Error> {
        // Create GlobalMetrics with:
        // - ascender, descender from CLI (or viewBox-derived)
        // - no variation model (static)
        Ok(Box::new(GlobalMetricsWork { ... }))
    }

    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, Error> {
        // One work item per outline glyph (layer glyph)
        // Each extracts contours from self.outline_glyphs
        let works: Vec<Box<IrWork>> = self.outline_glyphs
            .iter()
            .map(|(name, path)| {
                Box::new(GlyphIrWork {
                    name: name.clone(),
                    path: path.clone(),
                }) as Box<IrWork>
            })
            .collect();
        Ok(works)
    }

    fn create_color_palette_work(&self) -> Result<Box<IrWork>, Error> {
        // Collect all colors from SVGs → ColorPalettes
        Ok(Box::new(ColorPaletteWork {
            svgs: self.svgs.clone(),
        }))
    }

    fn create_color_glyphs_work(&self) -> Result<Box<IrWork>, Error> {
        // Build Paint graph from SVG hierarchy → ColorGlyphs
        Ok(Box::new(ColorGlyphsWork {
            svgs: self.svgs.clone(),
            font_transform: self.font_transform,
        }))
    }

    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error> {
        // No features for SVG fonts
        Ok(Box::new(EmptyFeaturesWork))
    }

    fn create_kerning_group_ir_work(&self) -> Result<Box<IrWork>, Error> {
        // No kerning for SVG fonts
        Ok(Box::new(EmptyKerningGroupsWork))
    }

    fn create_kerning_instance_ir_work(
        &self,
        _at: NormalizedLocation,
    ) -> Result<Box<IrWork>, Error> {
        // No kerning for SVG fonts
        Ok(Box::new(EmptyKerningInstanceWork))
    }
}
```

## Work Structs

Each `*Work` struct implements `Work<Context, WorkId, Error>`:

```rust
#[derive(Debug)]
struct StaticMetadataWork {
    upem: u16,
    family_name: String,
    glyph_names: IndexSet<GlyphName>,
}

impl Work<Context, WorkId, Error> for StaticMetadataWork {
    fn id(&self) -> WorkId { WorkId::StaticMetadata }
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // Build StaticMetadata from fields
        // Call context.static_metadata.set(...)
        // Also set context.preliminary_glyph_order
        Ok(())
    }
}
```

Similar pattern for `GlobalMetricsWork`, `GlyphIrWork`, `ColorPaletteWork`,
`ColorGlyphsWork`, `EmptyFeaturesWork`, `EmptyKerningGroupsWork`,
`EmptyKerningInstanceWork`.

## Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn testdata_dir() -> PathBuf {
        Path::new("../resources/testdata/svg").to_path_buf()
    }

    #[test]
    fn test_source_creation() {
        let source = SvgIrSource::new(
            &testdata_dir().join("simple_glyphs"),
            "Test".to_string(),
            1000,
            800,
            -200,
        ).unwrap();
        assert_eq!(source.upem, 1000);
        assert_eq!(source.svgs.len(), 2);
    }

    #[test]
    fn test_static_metadata_work() {
        let source = SvgIrSource::new(
            &testdata_dir().join("simple_glyphs"),
            "Test".to_string(),
            1000,
            800,
            -200,
        ).unwrap();
        let work = source.create_static_metadata_work().unwrap();
        assert_eq!(work.id(), WorkId::StaticMetadata);
    }

    #[test]
    fn test_glyph_ir_work_count() {
        let source = SvgIrSource::new(
            &testdata_dir().join("multi_layer"),
            "Test".to_string(),
            1000,
            800,
            -200,
        ).unwrap();
        let works = source.create_glyph_ir_work().unwrap();
        // u1F600.svg has 3 paths → 3 layer glyphs
        assert_eq!(works.len(), 3);
    }
}
```

## How to Verify

```bash
cargo test -p svg2fontir -- source
cargo check -p svg2fontir
cargo clippy -p svg2fontir --all-targets -- -D warnings
```

## Depends On

- Sub-plans 3, 4, 5, 6 (all modules must be implemented)

## Blocks

- Sub-plan 9 (CLI wiring calls SvgIrSource::new with metadata)
