# Sub-Plan 10: Integration Tests

## What

End-to-end tests that compile SVG directories through the full fontc pipeline
(SVG → IR → COLR/CPAL binary) and verify the output font.

## Where

**File**: `fontc/src/lib.rs` — existing `#[cfg(test)] mod tests` block (around line 312).

The existing `TestCompile` struct is the integration test harness. We may need
to extend it or add a parallel `compile_svg` method.

## Test Structure

### Extend TestCompile for SVG

```rust
#[cfg(test)]
impl TestCompile {
    fn compile_svg(svg_dir: &str, family_name: &str) -> Self {
        Self::compile(svg_dir, |mut opts| {
            opts.svg_metadata = Some(SvgMetadata {
                family_name: family_name.to_string(),
                upem: 1000,
                ascender: Some(800),
                descender: Some(-200),
            });
            opts
        })
    }
}
```

### Integration Tests

```rust
#[test]
fn test_compile_simple_svg_font() {
    let result = TestCompile::compile_svg(
        "svg/simple_glyphs",
        "Test Simple",
    );
    let font = result.font();

    // Verify font has expected tables
    assert!(font.table_data(tags::COLR).is_some(), "should have COLR table");
    assert!(font.table_data(tags::CPAL).is_some(), "should have CPAL table");
    assert!(font.table_data(tags::GLYF).is_some() || font.table_data(tags::CFF).is_some(),
        "should have glyph outlines");

    // Verify cmap entries
    let cmap = font.cmap().unwrap();
    // U+0041 should map to a glyph
    let gid_a = cmap.map_codepoint(0x0041).unwrap();
    assert!(gid_a.to_u16() > 0);

    // U+0042 should map to a glyph
    let gid_b = cmap.map_codepoint(0x0042).unwrap();
    assert!(gid_b.to_u16() > 0);
}

#[test]
fn test_compile_color_svg_font() {
    let result = TestCompile::compile_svg(
        "svg/multi_layer",
        "Test Color",
    );
    let font = result.font();

    // Must have COLR + CPAL
    assert!(font.table_data(tags::COLR).is_some());
    assert!(font.table_data(tags::CPAL).is_some());

    // CPAL should have colors from the SVG
    let cpal = font.cpal().unwrap();
    assert!(cpal.num_palettes() > 0);
    assert!(cpal.num_palette_entries() > 0);

    // COLR should have base glyphs
    let colr = font.colr().unwrap();
    // U+1F600 should be a color glyph
    let cmap = font.cmap().unwrap();
    let gid = cmap.map_codepoint(0x1F600).unwrap();
    assert!(colr.base_glyph(gid).is_some(), "U+1F600 should be a COLR base glyph");
}

#[test]
fn test_compile_gradient_svg_font() {
    let result = TestCompile::compile_svg(
        "svg/gradient_glyphs",
        "Test Gradient",
    );
    let font = result.font();

    assert!(font.table_data(tags::COLR).is_some());
    // Gradient paints should be in COLR v1 format
    // (v0 can't represent gradients)
}

#[test]
fn test_compile_opacity_svg_font() {
    let result = TestCompile::compile_svg(
        "svg/opacity_group",
        "Test Opacity",
    );
    let font = result.font();

    assert!(font.table_data(tags::COLR).is_some());
    // Should have composite paint in COLR
}

#[test]
fn test_svg_font_name() {
    let result = TestCompile::compile_svg(
        "svg/simple_glyphs",
        "My SVG Font",
    );
    let font = result.font();

    let name = font.name();
    // Check family name
    let family = name
        .name_record()
        .iter()
        .find(|r| r.name_id() == NameId::new(1))
        .unwrap();
    assert_eq!(
        family.string(name.offset_data()).unwrap().to_string(),
        "My SVG Font"
    );
}

#[test]
fn test_svg_font_upem() {
    let result = TestCompile::compile_svg(
        "svg/simple_glyphs",
        "Test",
    );
    let font = result.font();

    let head = font.head().unwrap();
    assert_eq!(head.units_per_em(), 1000);
}

#[test]
fn test_svg_font_glyph_order() {
    let result = TestCompile::compile_svg(
        "svg/simple_glyphs",
        "Test",
    );

    // Glyphs should be in codepoint order
    assert!(result.contains_glyph("A"));  // U+0041
    assert!(result.contains_glyph("B"));  // U+0042
}

#[test]
fn test_svg_font_has_outline_glyphs() {
    let result = TestCompile::compile_svg(
        "svg/simple_glyphs",
        "Test",
    );

    // Each SVG path should produce an outline glyph
    let glyph_a = result.read_be_glyph("A");
    // Should have contours (not empty)
    match glyph_a {
        fontbe::orchestration::context::RawGlyph::Simple(g) => {
            assert!(!g.contours.is_empty(), "A should have contours");
        }
        _ => panic!("Expected simple glyph"),
    }
}
```

## What to Verify Per Test Fixture

| Fixture | Tables | Key Assertions |
|---------|--------|----------------|
| `simple_glyphs/` | head, hhea, maxp, post, name, cmap, glyf/CFF, COLR, CPAL | 2 glyphs, solid fills, correct codepoints |
| `multi_layer/` | same + COLR v1 | Paint::Layers, multiple layer glyphs |
| `gradient_glyphs/` | same + COLR v1 | Gradient paints (linear or radial) |
| `opacity_group/` | same + COLR v1 | Composite paint present |
| `nested_groups/` | same + COLR v1 | Transform paint present |

## How to Verify

```bash
# Run all SVG integration tests
cargo test -p fontc -- svg

# Run specific test
cargo test -p fontc -- test_compile_simple_svg_font

# Run with verbose output
cargo test -p fontc -- svg --nocapture

# Full clippy check
cargo clippy --all-features --all-targets -- -D warnings
```

## Depends On

- Sub-plan 8 (backend must compile new paint types)
- Sub-plan 9 (CLI must recognize SVG input)

## Blocks

Nothing — this is the final verification step.
