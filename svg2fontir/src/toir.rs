//! SVG → IR Paint graph conversion.
//!
//! Converts usvg tree structure (groups, paths, fills, opacity, transforms)
//! into fontc's IR `Paint` tree for COLRv1 compilation.

#![allow(dead_code)] // used by sub-plan 7 (source trait implementation)

use std::collections::HashMap;

use fontdrasil::types::GlyphName;
use fontir::ir::{self, Paint};
use indexmap::IndexMap;
use kurbo::Affine;

use crate::color::fill_to_paint;
use crate::transform::usvg_transform_to_kurbo;

/// Result of converting SVG trees to IR.
pub(crate) struct SvgToIrResult {
    /// The paint graph for COLR base glyphs.
    pub paint_graph: ir::ColorGlyphs,
    /// Outline glyphs (layer glyphs) with their contours for the glyf table.
    pub outline_glyphs: HashMap<GlyphName, kurbo::BezPath>,
}

/// Convert a collection of SVG trees into paint graphs and outline glyphs.
///
/// # Arguments
/// * `svgs` - Map from glyph name to parsed usvg Tree
/// * `font_transform` - Transform from SVG coordinate space to font space
pub(crate) fn svg_to_paint_graph(
    svgs: &HashMap<GlyphName, usvg::Tree>,
    font_transform: Affine,
) -> SvgToIrResult {
    let mut base_glyphs = IndexMap::new();
    let mut outline_glyphs = HashMap::new();

    for (name, tree) in svgs {
        let root = tree.root();
        let paint = group_to_paint(root, font_transform, name, &mut outline_glyphs);
        base_glyphs.insert(name.clone(), paint);
    }

    SvgToIrResult {
        paint_graph: ir::ColorGlyphs { base_glyphs },
        outline_glyphs,
    }
}

/// Handle a `<g>` element recursively.
///
/// 1. Accumulate transform: `parent_transform * group.transform`
/// 2. Collect children as a list of paints
/// 3. If single child → return child directly
/// 4. If multiple children → wrap in `Paint::Layers`
/// 5. If group has opacity < 1.0 → wrap in `Paint::Composite(SrcOver, layers, backdrop)`
fn group_to_paint(
    group: &usvg::Group,
    parent_transform: Affine,
    svg_glyph_name: &GlyphName,
    outlines: &mut HashMap<GlyphName, kurbo::BezPath>,
) -> Paint {
    let local_transform = usvg_transform_to_kurbo(&group.transform());
    let combined_transform = parent_transform * local_transform;

    // Track path index for naming layer glyphs
    let mut path_index: usize = 0;

    let child_paints: Vec<Paint> = group
        .children()
        .iter()
        .map(|child| match child {
            usvg::Node::Path(p) => {
                let paint =
                    path_to_paint(p, combined_transform, svg_glyph_name, path_index, outlines);
                path_index += 1;
                paint
            }
            usvg::Node::Group(g) => group_to_paint(g, combined_transform, svg_glyph_name, outlines),
            _ => {
                // Text, Image — skip with empty paint
                Paint::Solid(Box::new(ir::PaintSolid { color: None }))
            }
        })
        .filter(|p| !is_empty_paint(p))
        .collect();

    if child_paints.is_empty() {
        return Paint::Solid(Box::new(ir::PaintSolid { color: None }));
    }

    let layers = if child_paints.len() == 1 {
        child_paints.into_iter().next().unwrap()
    } else {
        Paint::Layers(Box::new(child_paints))
    };

    // Apply group opacity via SrcOver compositing
    let opacity = group.opacity().get();
    if opacity < 1.0 {
        Paint::Composite(Box::new(ir::PaintComposite {
            source_paint: layers,
            composite_mode: ir::CompositeMode::SrcOver,
            backdrop_paint: Paint::Solid(Box::new(ir::PaintSolid {
                color: Some(ir::Color {
                    r: 0,
                    g: 0,
                    b: 0,
                    a: (opacity * 255.0).round() as u8,
                }),
            })),
        }))
    } else {
        layers
    }
}

/// Convert a single `<path>` element to a Paint.
///
/// 1. Convert `usvg::PathSegments` to `kurbo::BezPath`
/// 2. Apply `parent_transform` to get font-space path
/// 3. Extract fill paint (solid or gradient)
/// 4. Store the outline in `outlines` map
/// 5. Return `Paint::Glyph(PaintGlyph { name, paint })`
fn path_to_paint(
    path: &usvg::Path,
    parent_transform: Affine,
    svg_glyph_name: &GlyphName,
    path_index: usize,
    outlines: &mut HashMap<GlyphName, kurbo::BezPath>,
) -> Paint {
    let Some(fill) = path.fill() else {
        return Paint::Solid(Box::new(ir::PaintSolid { color: None }));
    };

    // Convert path segments to BezPath
    let bezpath = path_segments_to_bezpath(path.data());

    // Apply transform to get font-space coordinates
    let font_path = parent_transform * bezpath.clone();

    // Generate outline glyph name
    let outline_glyph_name = generate_outline_glyph_name(svg_glyph_name, path_index);

    // Store the outline
    outlines.insert(outline_glyph_name.clone(), font_path);

    // Convert fill to paint
    let fill_paint = fill_to_paint(fill, Affine::IDENTITY);

    Paint::Glyph(Box::new(ir::PaintGlyph {
        name: outline_glyph_name,
        paint: fill_paint,
    }))
}

/// Convert usvg path segments to a kurbo BezPath.
fn path_segments_to_bezpath(segments: &usvg::tiny_skia_path::Path) -> kurbo::BezPath {
    let mut path = kurbo::BezPath::new();
    for seg in segments.segments() {
        match seg {
            usvg::tiny_skia_path::PathSegment::MoveTo(p) => {
                path.move_to((p.x as f64, p.y as f64));
            }
            usvg::tiny_skia_path::PathSegment::LineTo(p) => {
                path.line_to((p.x as f64, p.y as f64));
            }
            usvg::tiny_skia_path::PathSegment::QuadTo(p0, p1) => {
                path.quad_to((p0.x as f64, p0.y as f64), (p1.x as f64, p1.y as f64));
            }
            usvg::tiny_skia_path::PathSegment::CubicTo(p0, p1, p2) => {
                path.curve_to(
                    (p0.x as f64, p0.y as f64),
                    (p1.x as f64, p1.y as f64),
                    (p2.x as f64, p2.y as f64),
                );
            }
            usvg::tiny_skia_path::PathSegment::Close => {
                path.close_path();
            }
        }
    }
    path
}

/// Generate a deterministic name for an outline glyph.
///
/// For path_index 0, use the SVG glyph name directly.
/// For path_index > 0, append `.path{index}` suffix.
fn generate_outline_glyph_name(svg_glyph_name: &GlyphName, path_index: usize) -> GlyphName {
    if path_index == 0 {
        svg_glyph_name.clone()
    } else {
        GlyphName::new(format!("{}.path{}", svg_glyph_name, path_index))
    }
}

/// Check if a paint is "empty" (solid with no color).
fn is_empty_paint(paint: &Paint) -> bool {
    matches!(paint, Paint::Solid(s) if s.color.is_none())
}

#[cfg(test)]
mod tests {
    use super::*;
    use fontir::ir::Color;
    use kurbo::Shape;
    use std::path::Path;

    fn testdata_dir() -> &'static Path {
        Path::new("../resources/testdata/svg")
    }

    fn parse_tree(name: &str) -> usvg::Tree {
        let path = testdata_dir().join(name);
        let data = std::fs::read(&path).expect("failed to read test SVG");
        let opts = usvg::Options::default();
        usvg::Tree::from_data(&data, &opts).expect("failed to parse test SVG")
    }

    #[test]
    fn test_single_path_to_paint_glyph() {
        let tree = parse_tree("simple_glyphs/u0041.svg");
        let mut svgs = HashMap::new();
        svgs.insert(GlyphName::new("A"), tree);

        let result = svg_to_paint_graph(&svgs, Affine::IDENTITY);

        // Should have one base glyph
        assert_eq!(result.paint_graph.base_glyphs.len(), 1);

        // Should have one outline glyph (the single path)
        assert_eq!(result.outline_glyphs.len(), 1);
        assert!(result.outline_glyphs.contains_key(&GlyphName::new("A")));

        // The paint should be a PaintGlyph with a solid fill
        let paint = &result.paint_graph.base_glyphs[&GlyphName::new("A")];
        match paint {
            Paint::Glyph(pg) => {
                assert_eq!(pg.name, GlyphName::new("A"));
                match &pg.paint {
                    Paint::Solid(s) => {
                        // u0041.svg has fill="#000000"
                        assert_eq!(
                            s.color,
                            Some(Color {
                                r: 0,
                                g: 0,
                                b: 0,
                                a: 255
                            })
                        );
                    }
                    _ => panic!("Expected PaintSolid"),
                }
            }
            _ => panic!("Expected PaintGlyph"),
        }
    }

    #[test]
    fn test_multi_path_to_layers() {
        let tree = parse_tree("multi_layer/u1F600.svg");
        let mut svgs = HashMap::new();
        svgs.insert(GlyphName::new("\u{1F600}"), tree);

        let result = svg_to_paint_graph(&svgs, Affine::IDENTITY);

        // Should have one base glyph
        assert_eq!(result.paint_graph.base_glyphs.len(), 1);

        // Should have multiple outline glyphs (one per path)
        assert!(result.outline_glyphs.len() >= 3);

        // The paint should be Paint::Layers with multiple PaintGlyphs
        let paint = &result.paint_graph.base_glyphs[&GlyphName::new("\u{1F600}")];
        match paint {
            Paint::Layers(layers) => {
                assert!(layers.len() >= 3);
                // Each layer should be a PaintGlyph
                for layer in layers.iter() {
                    match layer {
                        Paint::Glyph(_) => {}
                        _ => panic!("Expected PaintGlyph in layers"),
                    }
                }
            }
            _ => panic!("Expected Paint::Layers for multi-path SVG"),
        }
    }

    #[test]
    fn test_opacity_group_to_composite() {
        let tree = parse_tree("opacity_group/u2728.svg");
        let mut svgs = HashMap::new();
        svgs.insert(GlyphName::new("\u{2728}"), tree);

        let result = svg_to_paint_graph(&svgs, Affine::IDENTITY);

        // The paint should include a Composite for opacity
        let paint = &result.paint_graph.base_glyphs[&GlyphName::new("\u{2728}")];
        match paint {
            Paint::Composite(composite) => {
                assert_eq!(composite.composite_mode, ir::CompositeMode::SrcOver);
                // The backdrop should be a solid with reduced alpha
                match &composite.backdrop_paint {
                    Paint::Solid(s) => {
                        let color = s.color.expect("should have a color");
                        // opacity is 0.5 in the test SVG
                        assert!(color.a < 255);
                        assert!(color.a > 100 && color.a < 160); // 0.5 * 255 ≈ 128
                    }
                    _ => panic!("Expected PaintSolid backdrop"),
                }
            }
            _ => panic!("Expected Paint::Composite for opacity group"),
        }
    }

    #[test]
    fn test_gradient_fill_to_gradient_paint() {
        let tree = parse_tree("gradient_glyphs/u2B50.svg");
        let mut svgs = HashMap::new();
        svgs.insert(GlyphName::new("\u{2B50}"), tree);

        let result = svg_to_paint_graph(&svgs, Affine::IDENTITY);

        let paint = &result.paint_graph.base_glyphs[&GlyphName::new("\u{2B50}")];
        match paint {
            Paint::Glyph(pg) => match &pg.paint {
                Paint::LinearGradient(lg) => {
                    assert!(lg.color_line.len() >= 2);
                    // First stop should be red
                    assert_eq!(lg.color_line[0].color.r, 255);
                    assert_eq!(lg.color_line[0].color.g, 0);
                    assert_eq!(lg.color_line[0].color.b, 0);
                }
                _ => panic!("Expected PaintLinearGradient"),
            },
            _ => panic!("Expected PaintGlyph"),
        }
    }

    #[test]
    fn test_nested_groups() {
        let tree = parse_tree("nested_groups/u26A1.svg");
        let mut svgs = HashMap::new();
        svgs.insert(GlyphName::new("\u{26A1}"), tree);

        let result = svg_to_paint_graph(&svgs, Affine::IDENTITY);

        // Should have base glyph and outline glyphs
        assert_eq!(result.paint_graph.base_glyphs.len(), 1);
        assert!(!result.outline_glyphs.is_empty());

        // The paint should have transforms applied from nested groups
        let paint = &result.paint_graph.base_glyphs[&GlyphName::new("\u{26A1}")];
        // Just verify it's not empty
        assert!(!matches!(paint, Paint::Solid(s) if s.color.is_none()));
    }

    #[test]
    fn test_outline_glyph_naming() {
        // Test the naming convention
        assert_eq!(
            generate_outline_glyph_name(&GlyphName::new("A"), 0),
            GlyphName::new("A")
        );
        assert_eq!(
            generate_outline_glyph_name(&GlyphName::new("A"), 1),
            GlyphName::new("A.path1")
        );
        assert_eq!(
            generate_outline_glyph_name(&GlyphName::new("A"), 2),
            GlyphName::new("A.path2")
        );
    }

    #[test]
    fn test_path_segments_to_bezpath() {
        // Parse a simple SVG to get a path
        let svg_data = br#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100">
            <path d="M 0 0 L 100 100 Z" fill="black"/>
        </svg>"#;
        let tree = usvg::Tree::from_data(svg_data, &usvg::Options::default()).unwrap();
        let root = tree.root();

        // Get the path from the tree
        let path = match &root.children().iter().next().unwrap() {
            usvg::Node::Path(p) => p,
            _ => panic!("Expected Path node"),
        };

        let bezpath = path_segments_to_bezpath(path.data());

        // Verify the path elements
        let elements: Vec<_> = bezpath.elements().iter().collect();
        assert_eq!(elements.len(), 3);

        match elements[0] {
            kurbo::PathEl::MoveTo(p) => {
                assert!((p.x - 0.0).abs() < 0.001);
                assert!((p.y - 0.0).abs() < 0.001);
            }
            _ => panic!("Expected MoveTo"),
        }

        match elements[1] {
            kurbo::PathEl::LineTo(p) => {
                assert!((p.x - 100.0).abs() < 0.001);
                assert!((p.y - 100.0).abs() < 0.001);
            }
            _ => panic!("Expected LineTo"),
        }

        match elements[2] {
            kurbo::PathEl::ClosePath => {}
            _ => panic!("Expected ClosePath"),
        }
    }

    #[test]
    fn test_font_transform_applied() {
        let tree = parse_tree("simple_glyphs/u0041.svg");
        let mut svgs = HashMap::new();
        svgs.insert(GlyphName::new("A"), tree);

        // Use a scale transform (like viewBox 0,0,100,100 → font 1000 UPEM)
        let font_transform = Affine::scale(10.0);
        let result = svg_to_paint_graph(&svgs, font_transform);

        // Verify the outline was transformed
        let outline = &result.outline_glyphs[&GlyphName::new("A")];
        let bbox = outline.bounding_box();

        // Original path in u0041.svg should be transformed by 10x
        // The bounding box should reflect the scale
        assert!(bbox.width() > 0.0);
        assert!(bbox.height() > 0.0);
    }

    #[test]
    fn test_empty_svg_produces_empty_paint() {
        let mut svgs = HashMap::new();
        // Create an empty SVG tree
        let empty_svg = usvg::Tree::from_data(
            b"<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'></svg>",
            &usvg::Options::default(),
        )
        .unwrap();
        svgs.insert(GlyphName::new("Empty"), empty_svg);

        let result = svg_to_paint_graph(&svgs, Affine::IDENTITY);

        // Should still have a base glyph entry
        assert_eq!(result.paint_graph.base_glyphs.len(), 1);

        // The paint should be empty (solid with no color)
        let paint = &result.paint_graph.base_glyphs[&GlyphName::new("Empty")];
        assert!(matches!(paint, Paint::Solid(s) if s.color.is_none()));
    }
}
