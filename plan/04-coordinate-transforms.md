# Sub-Plan 4: Coordinate Transforms — SVG ViewBox → Font Space

## What

Implement the coordinate mapping from SVG's y-down viewBox space to font's
y-up UPEM space. This is a `kurbo::Affine` applied to all extracted paths
and gradient coordinates.

## Where

**File**: `svg2fontir/src/transform.rs` (created as stub in sub-plan 3)

## Transform Chain

```
SVG space (y-down, viewBox origin at (vx, vy))
  → translate(-vx, -vy)                    // normalize viewBox origin to (0,0)
  → scale(upem / vyh)                      // scale to font UPEM
  → translate(0, upem)                     // position baseline
  → scale(1, -1)                           // flip Y axis
```

Where `vyh` = viewBox height.

Result: A point `(x, y)` in SVG space maps to `(x * upem/vyh, upem - y * upem/vyh)` in font space.

## Functions to Implement

### `viewbox_to_font_transform(view_box: &usvg::ViewBox, upem: u16) -> kurbo::Affine`

Build the full affine transform from SVG viewBox to font coordinate space.

```rust
pub(crate) fn viewbox_to_font_transform(view_box: &usvg::ViewBox, upem: u16) -> kurbo::Affine {
    let scale = upem as f64 / view_box.rect.height() as f64;
    kurbo::Affine::new([
        scale, 0.0,
        0.0, -scale,
        -view_box.rect.x() as f64 * scale,
        upem as f64 + view_box.rect.y() as f64 * scale,
    ])
}
```

### `transform_path(path: &BezPath, affine: &kurbo::Affine) -> BezPath`

Apply affine to all path elements.

```rust
pub(crate) fn transform_path(path: &BezPath, affine: &kurbo::Affine) -> BezPath {
    *affine * *path  // kurbo supports Affine * BezPath
}
```

### `transform_point(point: kurbo::Point, affine: &kurbo::Affine) -> kurbo::Point`

Apply affine to a single point (used for gradient coordinates).

```rust
pub(crate) fn transform_point(point: kurbo::Point, affine: &kurbo::Affine) -> kurbo::Point {
    *affine * point
}
```

### `advance_width(view_box: &usvg::ViewBox, upem: u16) -> u16`

Calculate proportional advance width from viewBox aspect ratio.

```rust
pub(crate) fn advance_width(view_box: &usvg::ViewBox, upem: u16) -> u16 {
    let scale = upem as f64 / view_box.rect.height() as f64;
    (view_box.rect.width() as f64 * scale).round() as u16
}
```

### `usvg_transform_to_kurbo(t: &usvg::Transform) -> kurbo::Affine`

Convert usvg's 6-float transform to kurbo's Affine.

```rust
pub(crate) fn usvg_transform_to_kurbo(t: &usvg::Transform) -> kurbo::Affine {
    kurbo::Affine::new([
        t.sx as f64, t.ky as f64,
        t.kx as f64, t.sy as f64,
        t.tx as f64, t.ty as f64,
    ])
}
```

### `compose_transforms(font_space: kurbo::Affine, local: kurbo::Affine) -> kurbo::Affine`

Compose the viewbox→font transform with a local SVG transform.

```rust
pub(crate) fn compose_transforms(font_space: kurbo::Affine, local: kurbo::Affine) -> kurbo::Affine {
    font_space * local
}
```

## Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identity_viewbox() {
        let vb = usvg::ViewBox {
            rect: usvg::Rect::new(0.0, 0.0, 100.0, 100.0).unwrap(),
            aspect: usvg::AspectRatio::default(),
        };
        let affine = viewbox_to_font_transform(&vb, 1000);

        // Origin (0,0) in SVG → (0, 1000) in font (top-left → top of font)
        let p = affine * kurbo::Point::new(0.0, 0.0);
        assert!((p.x - 0.0).abs() < 0.001);
        assert!((p.y - 1000.0).abs() < 0.001);

        // Bottom-right (100,100) in SVG → (1000, 0) in font
        let p = affine * kurbo::Point::new(100.0, 100.0);
        assert!((p.x - 1000.0).abs() < 0.001);
        assert!((p.y - 0.0).abs() < 0.001);

        // Center (50,50) → (500, 500)
        let p = affine * kurbo::Point::new(50.0, 50.0);
        assert!((p.x - 500.0).abs() < 0.001);
        assert!((p.y - 500.0).abs() < 0.001);
    }

    #[test]
    fn test_offset_viewbox() {
        let vb = usvg::ViewBox {
            rect: usvg::Rect::new(10.0, 20.0, 100.0, 100.0).unwrap(),
            aspect: usvg::AspectRatio::default(),
        };
        let affine = viewbox_to_font_transform(&vb, 1000);

        // ViewBox origin (10,20) → (0, 1000)
        let p = affine * kurbo::Point::new(10.0, 20.0);
        assert!((p.x - 0.0).abs() < 0.001);
        assert!((p.y - 1000.0).abs() < 0.001);
    }

    #[test]
    fn test_y_flip() {
        let vb = usvg::ViewBox {
            rect: usvg::Rect::new(0.0, 0.0, 100.0, 100.0).unwrap(),
            aspect: usvg::AspectRatio::default(),
        };
        let affine = viewbox_to_font_transform(&vb, 1000);

        // SVG top-center (50, 0) → font top-center (500, 1000)
        let top = affine * kurbo::Point::new(50.0, 0.0);
        assert!((top.y - 1000.0).abs() < 0.001);

        // SVG bottom-center (50, 100) → font baseline (500, 0)
        let bottom = affine * kurbo::Point::new(50.0, 100.0);
        assert!((bottom.y - 0.0).abs() < 0.001);
    }

    #[test]
    fn test_advance_width_square() {
        let vb = usvg::ViewBox {
            rect: usvg::Rect::new(0.0, 0.0, 100.0, 100.0).unwrap(),
            aspect: usvg::AspectRatio::default(),
        };
        assert_eq!(advance_width(&vb, 1000), 1000);
    }

    #[test]
    fn test_advance_width_wide() {
        let vb = usvg::ViewBox {
            rect: usvg::Rect::new(0.0, 0.0, 200.0, 100.0).unwrap(),
            aspect: usvg::AspectRatio::default(),
        };
        assert_eq!(advance_width(&vb, 1000), 2000);
    }

    #[test]
    fn test_advance_width_tall() {
        let vb = usvg::ViewBox {
            rect: usvg::Rect::new(0.0, 0.0, 50.0, 100.0).unwrap(),
            aspect: usvg::AspectRatio::default(),
        };
        assert_eq!(advance_width(&vb, 1000), 500);
    }
}
```

## How to Verify

```bash
cargo test -p svg2fontir -- transform
cargo clippy -p svg2fontir --all-targets -- -D warnings
```

## Depends On

- Sub-plan 3 (svg2fontir crate must exist)

## Blocks

- Sub-plan 5, 6 (coordinate transforms needed for color and IR conversion)
