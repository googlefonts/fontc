# Sub-Plan 2: Extend IR with Missing COLRv1 Paint Types

## What

Add `PaintComposite`, `PaintTransform`, `PaintColrGlyph` variants and a
`CompositeMode` enum to `fontir/src/ir.rs`. All with serde derives.

## Where

**File**: `fontir/src/ir.rs` — the `Paint` enum and surrounding types (around line 2127).

## Changes

### Add CompositeMode enum

```rust
/// Porter-Duff compositing mode for COLRv1 PaintComposite.
/// Maps to write-fonts CompositeMode.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompositeMode {
    SrcOver,
    // Additional modes can be added as needed
}
```

### Add 3 new Paint variants

Add to the existing `Paint` enum (after `Layers`):

```rust
pub enum Paint {
    Glyph(Box<PaintGlyph>),
    Solid(Box<PaintSolid>),
    LinearGradient(Box<PaintLinearGradient>),
    RadialGradient(Box<PaintRadialGradient>),
    Layers(Box<Vec<Paint>>),
    Composite(Box<PaintComposite>),     // NEW
    Transform(Box<PaintTransform>),     // NEW
    ColrGlyph(Box<PaintColrGlyph>),     // NEW
}
```

### New structs

```rust
/// Porter-Duff compositing of two paints.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct PaintComposite {
    pub source_paint: Paint,
    pub composite_mode: CompositeMode,
    pub backdrop_paint: Paint,
}

/// Affine transform applied to a paint.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct PaintTransform {
    pub paint: Paint,
    pub transform: kurbo::Affine,
}

/// Reference to another base glyph's paint graph.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct PaintColrGlyph {
    pub glyph_name: GlyphName,
}
```

### Serde for kurbo::Affine

`kurbo::Affine` does not implement Serialize/Deserialize by default. Options:
- Use `#[serde(serialize_with = "...", deserialize_with = "...")]` on the `transform` field
- Or wrap in a serializable form: `[f64; 6]` (the 6 affine coefficients)

Recommended: implement via wrapper or custom serde on `PaintTransform`:
```rust
impl Serialize for PaintTransform {
    // serialize transform as [f64; 6]
}
```

## Tests

### In `fontir/src/ir.rs` (or a new test file):

```rust
#[test]
fn paint_composite_serde_roundtrip() {
    let paint = Paint::Composite(Box::new(PaintComposite {
        source_paint: Paint::Solid(Box::new(PaintSolid { color: Some(Color { r: 255, g: 0, b: 0, a: 255 }) })),
        composite_mode: CompositeMode::SrcOver,
        backdrop_paint: Paint::Solid(Box::new(PaintSolid { color: None })),
    }));
    let yaml = serde_yaml::to_string(&paint).unwrap();
    let deser: Paint = serde_yaml::from_str(&yaml).unwrap();
    assert_eq!(paint, deser);
}

#[test]
fn paint_transform_serde_roundtrip() {
    let paint = Paint::Transform(Box::new(PaintTransform {
        paint: Paint::Solid(Box::new(PaintSolid { color: None })),
        transform: kurbo::Affine::translate((10.0, 20.0)),
    }));
    let yaml = serde_yaml::to_string(&paint).unwrap();
    let deser: Paint = serde_yaml::from_str(&yaml).unwrap();
    assert_eq!(paint, deser);
}

#[test]
fn paint_colr_glyph_serde_roundtrip() {
    let paint = Paint::ColrGlyph(Box::new(PaintColrGlyph {
        glyph_name: GlyphName::new("base_glyph"),
    }));
    let yaml = serde_yaml::to_string(&paint).unwrap();
    let deser: Paint = serde_yaml::from_str(&yaml).unwrap();
    assert_eq!(paint, deser);
}
```

## How to Verify

```bash
cargo test -p fontir --all-features -- paint_composite_serde_roundtrip
cargo test -p fontir --all-features -- paint_transform_serde_roundtrip
cargo test -p fontir --all-features -- paint_colr_glyph_serde_roundtrip
cargo clippy --all-features --all-targets -- -D warnings
```

## Depends On

Nothing (pure IR type additions).

## Blocks

- Sub-plan 6 (toir conversion needs Composite/Transform paint types)
- Sub-plan 8 (backend needs to match on new variants)
