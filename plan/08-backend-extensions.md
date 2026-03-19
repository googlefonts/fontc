# Sub-Plan 8: Backend — Compile New Paint Types to COLRv1

## What

Extend `fontbe/src/colr.rs` to compile the 3 new IR Paint variants
(`Composite`, `Transform`, `ColrGlyph`) to write-fonts COLRv1 format.

## Where

**File**: `fontbe/src/colr.rs` — the `to_colr_paint()` function (around line 140).

## Changes

### Add import for new write-fonts types

```rust
use write_fonts::tables::colr::{
    // existing:
    BaseGlyph, BaseGlyphList, Base, BaseGlyphPaint, Clip, ClipBox, ClipList,
    ColorLine, ColorStop, Colr, Extend, Layer, LayerList, Paint, PaintColrLayers,
    PaintGlyph, PaintLinearGradient, PaintRadialGradient, PaintSolid,
    // new:
    PaintComposite, PaintTransform, PaintColrGlyph, CompositeMode,
};
```

### Add match arms in `to_colr_paint()`

```rust
ir::Paint::Composite(c) => {
    let source = self.to_colr_paint(&c.source_paint, glyph_order, glyph_bbox)?;
    let backdrop = self.to_colr_paint(&c.backdrop_paint, glyph_order, glyph_bbox)?;
    Paint::Composite(PaintComposite {
        source_paint: Box::new(source),
        composite_mode: ir_composite_mode_to_write_fonts(c.composite_mode),
        backdrop_paint: Box::new(backdrop),
    })
}
ir::Paint::Transform(t) => {
    let inner = self.to_colr_paint(&t.paint, glyph_order, glyph_bbox)?;
    Paint::Transform(PaintTransform {
        paint: Box::new(inner),
        transform: kurbo_affine_to_colr(t.transform),
    })
}
ir::Paint::ColrGlyph(g) => {
    let gid = glyph_order.glyph_id(&g.glyph_name)
        .ok_or_else(|| ColrError::MissingGlyph(g.glyph_name.clone()))?;
    Paint::ColrGlyph(PaintColrGlyph { glyph_id: gid })
}
```

### Helper: `ir_composite_mode_to_write_fonts`

```rust
fn ir_composite_mode_to_write_fonts(mode: ir::CompositeMode) -> CompositeMode {
    match mode {
        ir::CompositeMode::SrcOver => CompositeMode::SrcOver,
    }
}
```

### Helper: `kurbo_affine_to_colr`

```rust
fn kurbo_affine_to_colr(affine: kurbo::Affine) -> write_fonts::types::Affine2x3 {
    let coeffs = affine.as_coeffs();
    write_fonts::types::Affine2x3::new(
        Fixed::from_f64(coeffs[0]),  // xx
        Fixed::from_f64(coeffs[1]),  // yx
        Fixed::from_f64(coeffs[2]),  // xy
        Fixed::from_f64(coeffs[3]),  // yy
        Fixed::from_f64(coeffs[4]),  // dx
        Fixed::from_f64(coeffs[5]),  // dy
    )
}
```

## Tests

Add tests in the existing `#[cfg(test)] mod tests` at the bottom of `colr.rs`:

```rust
#[test]
fn compile_composite_paint() {
    let ir_paint = ir::Paint::Composite(Box::new(ir::PaintComposite {
        source_paint: ir::Paint::Solid(Box::new(ir::PaintSolid {
            color: Some(ir::Color { r: 255, g: 0, b: 0, a: 128 }),
        })),
        composite_mode: ir::CompositeMode::SrcOver,
        backdrop_paint: ir::Paint::Solid(Box::new(ir::PaintSolid {
            color: Some(ir::Color { r: 0, g: 0, b: 255, a: 255 }),
        })),
    }));
    // Compile and verify the resulting write-fonts Paint is Composite
}

#[test]
fn compile_transform_paint() {
    let ir_paint = ir::Paint::Transform(Box::new(ir::PaintTransform {
        paint: ir::Paint::Solid(Box::new(ir::PaintSolid { color: None })),
        transform: kurbo::Affine::translate((100.0, 200.0)),
    }));
    // Compile and verify the resulting write-fonts Paint is Transform
}

#[test]
fn compile_colr_glyph_paint() {
    let ir_paint = ir::Paint::ColrGlyph(Box::new(ir::PaintColrGlyph {
        glyph_name: GlyphName::new("base"),
    }));
    // Compile and verify with a mock glyph order
}
```

## How to Verify

```bash
cargo test -p fontbe -- colr
cargo clippy -p fontbe --all-features --all-targets -- -D warnings
```

## Depends On

- Sub-plan 2 (IR must have new paint types)
- Sub-plan 10 integration tests will verify the full pipeline

## Blocks

- Sub-plan 10 (integration tests that compile SVG fonts)
