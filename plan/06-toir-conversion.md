# Sub-Plan 6: SVG Hierarchy → IR Paint Graph

## What

Convert the usvg tree structure (groups, paths, fills, opacity, transforms)
into fontc's IR `Paint` tree. This is the core SVG→COLR mapping logic.

## Where

**File**: `svg2fontir/src/toir.rs` (created as stub in sub-plan 3)

## SVG → Paint Mapping Rules

| SVG Element | IR Paint |
|-------------|----------|
| `<path fill="color"/>` | `Paint::Glyph(PaintSolid)` |
| `<path fill="url(#grad)"/>` | `Paint::Glyph(PaintLinearGradient/PaintRadialGradient)` |
| Multiple paths in same group | `Paint::Layers([Paint::Glyph(...), ...])` |
| `<g opacity="0.5">` | `Paint::Composite(SrcOver, child_paint, backdrop)` |
| `<g transform="...">` | `Paint::Transform(affine, child_paint)` |
| Nested groups | Recursive composition of the above |

## Functions to Implement

### `svg_to_paint_graph(svgs: &HashMap<GlyphName, usvg::Tree>, font_transform: kurbo::Affine) -> ir::ColorGlyphs`

Top-level entry: for each SVG glyph, build its Paint tree.

```rust
pub(crate) fn svg_to_paint_graph(
    svgs: &HashMap<GlyphName, usvg::Tree>,
    font_transform: kurbo::Affine,
) -> ir::ColorGlyphs {
    let mut base_glyphs = IndexMap::new();
    for (name, tree) in svgs {
        let paint = node_to_paint(tree.root(), font_transform);
        base_glyphs.insert(name.clone(), paint);
    }
    ir::ColorGlyphs { base_glyphs }
}
```

### `node_to_paint(node: &usvg::Node, parent_transform: kurbo::Affine) -> ir::Paint`

Recursive dispatcher: handles `Group` and `Path` nodes.

```rust
fn node_to_paint(node: &usvg::Node, parent_transform: kurbo::Affine) -> ir::Paint {
    match node {
        usvg::Node::Group(group) => group_to_paint(group, node, parent_transform),
        usvg::Node::Path(path) => path_to_paint(path, parent_transform),
        _ => {
            // Text, Image — skip
            ir::Paint::Solid(Box::new(ir::PaintSolid { color: None }))
        }
    }
}
```

### `group_to_paint(group: &usvg::Group, node: &usvg::Node, parent_transform: kurbo::Affine) -> ir::Paint`

Handle a `<g>` element:
1. Accumulate transform: `parent_transform * group.transform`
2. Collect children as a list of paints
3. If single child → return child directly
4. If multiple children → wrap in `Paint::Layers`
5. If group has opacity < 1.0 → wrap in `Paint::Composite(SrcOver, layers, backdrop)`

```rust
fn group_to_paint(
    group: &usvg::Group,
    node: &usvg::Node,
    parent_transform: kurbo::Affine,
) -> ir::Paint {
    let local_transform = usvg_transform_to_kurbo(&group.transform);
    let combined_transform = parent_transform * local_transform;

    let child_paints: Vec<ir::Paint> = node.children()
        .map(|child| node_to_paint(&child, combined_transform))
        .filter(|p| !is_empty_paint(p))
        .collect();

    let layers = if child_paints.len() == 1 {
        child_paints.into_iter().next().unwrap()
    } else {
        ir::Paint::Layers(Box::new(child_paints))
    };

    // Apply group opacity via SrcOver compositing
    let opacity = group.opacity.get();
    if opacity < 1.0 {
        ir::Paint::Composite(Box::new(ir::PaintComposite {
            source_paint: layers,
            composite_mode: ir::CompositeMode::SrcOver,
            backdrop_paint: ir::Paint::Solid(Box::new(ir::PaintSolid {
                color: Some(ir::Color { r: 0, g: 0, b: 0, a: (opacity * 255.0).round() as u8 }),
            })),
        }))
    } else {
        layers
    }
}
```

### `path_to_paint(path: &usvg::Path, parent_transform: kurbo::Affine) -> ir::Paint`

Convert a single `<path>` element:
1. Convert `usvg::PathSegments` to `kurbo::BezPath`
2. Apply `parent_transform` to get font-space path
3. Extract fill paint (solid or gradient)
4. Create a separate glyph for the path outline (referenced by `PaintGlyph`)
5. Return `Paint::Glyph(OutlineGlyph, FillPaint)`

```rust
fn path_to_paint(path: &usvg::Path, parent_transform: kurbo::Affine) -> ir::Paint {
    let Some(fill) = &path.fill else {
        return ir::Paint::Solid(Box::new(ir::PaintSolid { color: None }));
    };

    let bezpath = path_segments_to_bezpath(&path.data);
    let font_path = transform_path(&bezpath, &parent_transform);

    // The path outline becomes a separate glyph
    let outline_glyph_name = generate_outline_glyph_name(path);
    // ... store font_path for later Glyph IR creation ...

    let fill_paint = fill_to_paint(fill, kurbo::Affine::IDENTITY);

    ir::Paint::Glyph(Box::new(ir::PaintGlyph {
        name: outline_glyph_name,
        paint: fill_paint,
    }))
}
```

### `path_segments_to_bezpath(segments: &usvg::PathSegments) -> kurbo::BezPath`

Convert usvg path data to kurbo BezPath.

```rust
fn path_segments_to_bezpath(segments: &usvg::PathSegments) -> kurbo::BezPath {
    let mut path = kurbo::BezPath::new();
    for seg in segments.iter() {
        match seg {
            usvg::PathSegment::MoveTo { x, y } => {
                path.move_to((*x as f64, *y as f64));
            }
            usvg::PathSegment::LineTo { x, y } => {
                path.line_to((*x as f64, *y as f64));
            }
            usvg::PathSegment::CurveTo { x1, y1, x2, y2, x, y } => {
                path.curve_to((*x1 as f64, *y1 as f64), (*x2 as f64, *y2 as f64), (*x as f64, *y as f64));
            }
            usvg::PathSegment::ClosePath => {
                path.close_path();
            }
        }
    }
    path
}
```

### Outline Glyph Name Generation

Each SVG path needs a unique glyph name for its outline. Use a deterministic
scheme based on the SVG glyph name and path index:

```rust
fn generate_outline_glyph_name(svg_glyph_name: &GlyphName, path_index: usize) -> GlyphName {
    if path_index == 0 {
        svg_glyph_name.clone()
    } else {
        GlyphName::new(&format!("{}.path{}", svg_glyph_name, path_index))
    }
}
```

Wait — the outline glyph and the color base glyph share the same name. The
`PaintGlyph.name` points to the outline glyph, and the `ColorGlyphs.base_glyphs`
key is the color glyph. Actually these can be the same glyph. Let me reconsider...

In COLRv1:
- A **base glyph** (e.g., "A") has a paint graph in the COLR table
- The paint graph references **layer glyphs** (outline glyphs) by name
- The layer glyphs contain the actual contours in the `glyf`/`CFF` table

So the outline from each SVG path becomes a layer glyph. The SVG glyph itself
becomes a COLR base glyph with a paint graph referencing those layers.

Revised naming:
- Base glyph: `A` (from `u0041.svg`)
- Layer glyphs: `A.path0`, `A.path1`, etc. (one per `<path>` in the SVG)

## Glyph Collection

In addition to building the Paint graph, `toir` must also produce the outline
glyphs (contours) for the `glyf` table. These are collected alongside the paint
graph:

```rust
pub(crate) struct SvgToIrResult {
    pub paint_graph: ir::ColorGlyphs,
    pub outline_glyphs: HashMap<GlyphName, kurbo::BezPath>,  // layer glyphs with contours
}
```

## Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_path_to_paint_glyph() {
        // SVG with one <path fill="#FF0000"> → Paint::Glyph(PaintSolid)
    }

    #[test]
    fn test_multi_path_to_layers() {
        // SVG with 3 <path> → Paint::Layers([Paint::Glyph, Paint::Glyph, Paint::Glyph])
    }

    #[test]
    fn test_opacity_group_to_composite() {
        // <g opacity="0.5"> with paths → Paint::Composite(SrcOver, layers, backdrop)
    }

    #[test]
    fn test_gradient_fill_to_gradient_paint() {
        // <path fill="url(#grad)"> → Paint::Glyph(PaintLinearGradient)
    }

    #[test]
    fn test_nested_groups() {
        // <g transform="..."><g opacity="0.5"><path .../></g></g>
        // → Paint::Transform(Paint::Composite(...))
    }

    #[test]
    fn test_outline_glyph_naming() {
        // u0041.svg with 3 paths → layer glyphs: "A", "A.path1", "A.path2"
    }
}
```

## How to Verify

```bash
cargo test -p svg2fontir -- toir
cargo clippy -p svg2fontir --all-targets -- -D warnings
```

## Depends On

- Sub-plan 2 (IR must have Composite/Transform paint types)
- Sub-plan 3 (svg2fontir crate)
- Sub-plan 4 (coordinate transforms)
- Sub-plan 5 (color/gradient conversion)

## Blocks

- Sub-plan 7 (Source trait uses toir functions)
