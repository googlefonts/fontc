# Sub-Plan 5: Color and Gradient Extraction

## What

Convert SVG color and gradient data to fontc IR types (`Color`, `ColorStop`,
`ColorPalettes`, `PaintLinearGradient`, `PaintRadialGradient`).

## Where

**File**: `svg2fontir/src/color.rs` (created as stub in sub-plan 3)

## Functions to Implement

### `svg_color_to_ir(color: &usvg::Color) -> ir::Color`

```rust
pub(crate) fn svg_color_to_ir(color: &usvg::Color) -> ir::Color {
    ir::Color {
        r: (color.red * 255.0).round() as u8,
        g: (color.green * 255.0).round() as u8,
        b: (color.blue * 255.0).round() as u8,
        a: 255,
    }
}
```

### `svg_color_with_alpha(color: &usvg::Color, opacity: f32) -> ir::Color`

```rust
pub(crate) fn svg_color_with_alpha(color: &usvg::Color, opacity: f32) -> ir::Color {
    ir::Color {
        r: (color.red * 255.0).round() as u8,
        g: (color.green * 255.0).round() as u8,
        b: (color.blue * 255.0).round() as u8,
        a: (opacity * 255.0).round() as u8,
    }
}
```

### `linear_gradient_to_ir(grad: &usvg::LinearGradient, transform: kurbo::Affine) -> ir::PaintLinearGradient`

Map SVG linearGradient to `PaintLinearGradient`:
- `x1,y1,x2,y2` → `p0, p1` (transformed to font space)
- `stops` → `color_line` (Vec<ColorStop>)
- `spread_method` is ignored for now (COLR Extend defaults to Pad)

```rust
pub(crate) fn linear_gradient_to_ir(
    grad: &usvg::LinearGradient,
    font_transform: kurbo::Affine,
    fill_opacity: f32,
) -> ir::PaintLinearGradient {
    let p0 = font_transform * kurbo::Point::new(grad.x1 as f64, grad.y1 as f64);
    let p1 = font_transform * kurbo::Point::new(grad.x2 as f64, grad.y2 as f64);

    ir::PaintLinearGradient {
        color_line: stops_to_color_line(&grad.stops, fill_opacity),
        p0,
        p1,
        p2: None, // computed perpendicular by backend
    }
}
```

### `radial_gradient_to_ir(grad: &usvg::RadialGradient, transform: kurbo::Affine) -> ir::PaintRadialGradient`

Map SVG radialGradient to `PaintRadialGradient`:
- `cx,cy` → `p1` (outer center)
- `fx,fy` → `p0` (inner/focal center)
- `r` → `r1` (outer radius, transformed)
- Inner radius `r0` defaults to 0 (None)

```rust
pub(crate) fn radial_gradient_to_ir(
    grad: &usvg::RadialGradient,
    font_transform: kurbo::Affine,
    fill_opacity: f32,
) -> ir::PaintRadialGradient {
    let p0 = font_transform * kurbo::Point::new(grad.fx as f64, grad.fy as f64);
    let p1 = font_transform * kurbo::Point::new(grad.cx as f64, grad.cy as f64);

    // Transform radius by average scale factor
    let scale = (font_transform * kurbo::Vec2::new(1.0, 0.0)).hypot();
    let r1 = grad.r as f64 * scale;

    ir::PaintRadialGradient {
        color_line: stops_to_color_line(&grad.stops, fill_opacity),
        p0,
        r0: None,
        p1,
        r1: Some(ordered_float::OrderedFloat(r1 as f32)),
    }
}
```

### `stops_to_color_line(stops: &[usvg::Stop], fill_opacity: f32) -> Vec<ir::ColorStop>`

```rust
fn stops_to_color_line(stops: &[usvg::Stop], fill_opacity: f32) -> Vec<ir::ColorStop> {
    stops.iter().map(|stop| {
        ir::ColorStop {
            offset: ordered_float::OrderedFloat(stop.offset.get()),
            color: svg_color_with_alpha(&stop.color, stop.opacity * fill_opacity),
            alpha: ordered_float::OrderedFloat(stop.opacity * fill_opacity),
        }
    }).collect()
}
```

### `fill_to_paint(fill: &usvg::Fill, font_transform: kurbo::Affine) -> ir::Paint`

Convert a usvg Fill (solid color or gradient reference) to an IR Paint.

```rust
pub(crate) fn fill_to_paint(fill: &usvg::Fill, font_transform: kurbo::Affine) -> ir::Paint {
    match &fill.paint {
        usvg::Paint::Color(color) => {
            let ir_color = svg_color_with_alpha(color, fill.opacity.get());
            ir::Paint::Solid(Box::new(ir::PaintSolid {
                color: Some(ir_color),
            }))
        }
        usvg::Paint::LinearGradient(grad) => {
            ir::Paint::LinearGradient(Box::new(
                linear_gradient_to_ir(grad, font_transform, fill.opacity.get())
            ))
        }
        usvg::Paint::RadialGradient(grad) => {
            ir::Paint::RadialGradient(Box::new(
                radial_gradient_to_ir(grad, font_transform, fill.opacity.get())
            ))
        }
        _ => {
            // Pattern, other paint types — fall back to black
            ir::Paint::Solid(Box::new(ir::PaintSolid {
                color: Some(ir::Color { r: 0, g: 0, b: 0, a: 255 }),
            }))
        }
    }
}
```

### `collect_colors_from_svgs(svgs: &HashMap<GlyphName, usvg::Tree>) -> ir::ColorPalettes`

Walk all SVGs, collect unique colors, build `ColorPalettes`.

```rust
pub(crate) fn collect_colors_from_svgs(
    svgs: &HashMap<GlyphName, usvg::Tree>,
) -> ir::ColorPalettes {
    let mut colors = IndexSet::new();
    for tree in svgs.values() {
        collect_colors_recursive(tree.root(), &mut colors);
    }
    let palette: Vec<ir::Color> = colors.into_iter().collect();
    ir::ColorPalettes::new(vec![palette]).unwrap_or(None).unwrap_or_default()
}
```

## Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solid_color_to_ir() {
        let usvg_color = usvg::Color::new_rgb(255, 0, 0);
        let ir = svg_color_to_ir(&usvg_color);
        assert_eq!(ir, ir::Color { r: 255, g: 0, b: 0, a: 255 });
    }

    #[test]
    fn test_color_with_alpha() {
        let usvg_color = usvg::Color::new_rgb(100, 150, 200);
        let ir = svg_color_with_alpha(&usvg_color, 0.5);
        assert_eq!(ir, ir::Color { r: 100, g: 150, b: 200, a: 127 });
    }

    #[test]
    fn test_solid_fill_to_paint() {
        let fill = usvg::Fill {
            paint: usvg::Paint::Color(usvg::Color::new_rgb(0, 0, 255)),
            opacity: usvg::Opacity::new(1.0),
            ..Default::default()
        };
        let paint = fill_to_paint(&fill, kurbo::Affine::IDENTITY);
        match paint {
            ir::Paint::Solid(s) => {
                assert_eq!(s.color, Some(ir::Color { r: 0, g: 0, b: 255, a: 255 }));
            }
            _ => panic!("Expected PaintSolid"),
        }
    }

    #[test]
    fn test_color_palette_dedup() {
        // Two SVGs with same colors → palette should have unique entries
        // (test requires building actual SVG trees)
    }
}
```

## How to Verify

```bash
cargo test -p svg2fontir -- color
cargo clippy -p svg2fontir --all-targets -- -D warnings
```

## Depends On

- Sub-plan 3 (svg2fontir crate)
- Sub-plan 4 (coordinate transforms for gradient mapping)

## Blocks

- Sub-plan 6 (toir conversion uses these color/gradient functions)
