#![allow(dead_code)] // used by sub-plan 6 (toir conversion)

use std::collections::{HashMap, HashSet};

use fontdrasil::types::GlyphName;
use fontir::ir::{self, Color, ColorPalettes, ColorStop, Paint};
use kurbo::{Affine, Point};
use ordered_float::OrderedFloat;

// Intermediate types used by parse.rs for gradient extraction.

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) struct ColorStopInfo {
    pub offset: f32,
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub alpha: f32,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) enum GradientInfo {
    Linear {
        x1: f32,
        y1: f32,
        x2: f32,
        y2: f32,
        stops: Vec<ColorStopInfo>,
    },
    Radial {
        cx: f32,
        cy: f32,
        r: f32,
        fx: f32,
        fy: f32,
        stops: Vec<ColorStopInfo>,
    },
}

// ---- usvg → IR conversion functions ----

/// Convert an usvg color (RGB, no alpha) to an IR Color with full alpha.
pub(crate) fn svg_color_to_ir(color: &usvg::Color) -> Color {
    Color {
        r: color.red,
        g: color.green,
        b: color.blue,
        a: 255,
    }
}

/// Convert an usvg color with a separate opacity to an IR Color.
pub(crate) fn svg_color_with_alpha(color: &usvg::Color, opacity: f32) -> Color {
    Color {
        r: color.red,
        g: color.green,
        b: color.blue,
        a: (opacity.clamp(0.0, 1.0) * 255.0).round() as u8,
    }
}

/// Convert usvg gradient stops to IR color line, blending stop opacity with fill opacity.
fn stops_to_color_line(stops: &[usvg::Stop], fill_opacity: f32) -> Vec<ColorStop> {
    stops
        .iter()
        .map(|stop| {
            let combined_opacity = stop.opacity().get() * fill_opacity;
            ColorStop {
                offset: OrderedFloat(stop.offset().get()),
                color: svg_color_with_alpha(&stop.color(), combined_opacity),
                alpha: OrderedFloat(combined_opacity),
            }
        })
        .collect()
}

/// Average scale factor from an affine transform.
fn affine_scale(t: &Affine) -> f64 {
    let coeffs = t.as_coeffs();
    let sx = (coeffs[0] * coeffs[0] + coeffs[1] * coeffs[1]).sqrt();
    let sy = (coeffs[2] * coeffs[2] + coeffs[3] * coeffs[3]).sqrt();
    (sx + sy) / 2.0
}

/// Convert an usvg linear gradient to an IR PaintLinearGradient.
pub(crate) fn linear_gradient_to_ir(
    grad: &usvg::LinearGradient,
    font_transform: Affine,
    fill_opacity: f32,
) -> ir::PaintLinearGradient {
    let p0 = font_transform * Point::new(grad.x1() as f64, grad.y1() as f64);
    let p1 = font_transform * Point::new(grad.x2() as f64, grad.y2() as f64);

    ir::PaintLinearGradient {
        color_line: stops_to_color_line(grad.stops(), fill_opacity),
        p0,
        p1,
        p2: None,
    }
}

/// Convert an usvg radial gradient to an IR PaintRadialGradient.
pub(crate) fn radial_gradient_to_ir(
    grad: &usvg::RadialGradient,
    font_transform: Affine,
    fill_opacity: f32,
) -> ir::PaintRadialGradient {
    let p0 = font_transform * Point::new(grad.fx() as f64, grad.fy() as f64);
    let p1 = font_transform * Point::new(grad.cx() as f64, grad.cy() as f64);

    let scale = affine_scale(&font_transform);
    let r1 = grad.r().get() as f64 * scale;

    ir::PaintRadialGradient {
        color_line: stops_to_color_line(grad.stops(), fill_opacity),
        p0,
        r0: None,
        p1,
        r1: Some(OrderedFloat(r1 as f32)),
    }
}

/// Convert a usvg Fill (solid color or gradient) to an IR Paint.
pub(crate) fn fill_to_paint(fill: &usvg::Fill, font_transform: Affine) -> Paint {
    match fill.paint() {
        usvg::Paint::Color(color) => {
            let ir_color = svg_color_with_alpha(color, fill.opacity().get());
            Paint::Solid(Box::new(ir::PaintSolid {
                color: Some(ir_color),
            }))
        }
        usvg::Paint::LinearGradient(grad) => Paint::LinearGradient(Box::new(
            linear_gradient_to_ir(grad, font_transform, fill.opacity().get()),
        )),
        usvg::Paint::RadialGradient(grad) => Paint::RadialGradient(Box::new(
            radial_gradient_to_ir(grad, font_transform, fill.opacity().get()),
        )),
        usvg::Paint::Pattern(_) => {
            // Patterns not supported in COLRv1 — fall back to opaque black
            Paint::Solid(Box::new(ir::PaintSolid {
                color: Some(Color {
                    r: 0,
                    g: 0,
                    b: 0,
                    a: 255,
                }),
            }))
        }
    }
}

/// Collect unique colors from all SVG trees and build a ColorPalettes.
pub(crate) fn collect_colors_from_svgs(
    svgs: &HashMap<GlyphName, usvg::Tree>,
) -> Option<ColorPalettes> {
    let mut colors = HashSet::new();
    for tree in svgs.values() {
        collect_colors_recursive(tree.root(), &mut colors);
    }
    let palette: Vec<Color> = colors.into_iter().collect();
    if palette.is_empty() {
        return None;
    }
    ColorPalettes::new(vec![palette]).unwrap_or(None)
}

fn collect_colors_recursive(group: &usvg::Group, colors: &mut HashSet<Color>) {
    let group_opacity = group.opacity().get();
    for node in group.children() {
        match node {
            usvg::Node::Path(p) => {
                if let Some(fill) = p.fill() {
                    match fill.paint() {
                        usvg::Paint::Color(c) => {
                            colors.insert(svg_color_with_alpha(c, fill.opacity().get()));
                        }
                        usvg::Paint::LinearGradient(grad) => {
                            for stop in grad.stops() {
                                colors.insert(svg_color_with_alpha(
                                    &stop.color(),
                                    stop.opacity().get() * fill.opacity().get(),
                                ));
                            }
                        }
                        usvg::Paint::RadialGradient(grad) => {
                            for stop in grad.stops() {
                                colors.insert(svg_color_with_alpha(
                                    &stop.color(),
                                    stop.opacity().get() * fill.opacity().get(),
                                ));
                            }
                        }
                        _ => {}
                    }
                }
            }
            usvg::Node::Group(g) => {
                collect_colors_recursive(g, colors);
            }
            _ => {}
        }
    }
    if group_opacity < 1.0 {
        colors.insert(Color {
            r: 0,
            g: 0,
            b: 0,
            a: (group_opacity * 255.0).round() as u8,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
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

    /// Extract the first fill from a parsed SVG tree.
    fn first_fill(tree: &usvg::Tree) -> Option<usvg::Fill> {
        fn walk(group: &usvg::Group) -> Option<usvg::Fill> {
            for child in group.children() {
                match child {
                    usvg::Node::Path(p) => {
                        if let Some(fill) = p.fill() {
                            return Some(fill.clone());
                        }
                    }
                    usvg::Node::Group(g) => {
                        if let Some(fill) = walk(g) {
                            return Some(fill);
                        }
                    }
                    _ => {}
                }
            }
            None
        }
        walk(tree.root())
    }

    #[test]
    fn test_solid_color_to_ir() {
        let usvg_color = usvg::Color::new_rgb(255, 0, 0);
        let ir = svg_color_to_ir(&usvg_color);
        assert_eq!(
            ir,
            Color {
                r: 255,
                g: 0,
                b: 0,
                a: 255
            }
        );
    }

    #[rstest]
    #[case(1.0, 255)]
    #[case(0.5, 128)]
    #[case(0.0, 0)]
    fn test_color_with_alpha(#[case] opacity: f32, #[case] expected_a: u8) {
        let usvg_color = usvg::Color::new_rgb(100, 150, 200);
        let ir = svg_color_with_alpha(&usvg_color, opacity);
        assert_eq!(
            ir,
            Color {
                r: 100,
                g: 150,
                b: 200,
                a: expected_a
            }
        );
    }

    #[test]
    fn test_solid_fill_to_paint() {
        let tree = parse_tree("simple_glyphs/u0041.svg");
        let fill = first_fill(&tree).expect("should have a fill");
        let paint = fill_to_paint(&fill, Affine::IDENTITY);
        match paint {
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

    #[test]
    fn test_solid_fill_from_opacity_group() {
        let tree = parse_tree("opacity_group/u2728.svg");
        let fill = first_fill(&tree).expect("should have a fill");
        let paint = fill_to_paint(&fill, Affine::IDENTITY);
        match paint {
            Paint::Solid(s) => {
                let color = s.color.expect("should have a color");
                // #FFD700 (gold) — fill opacity is 1.0, group opacity is separate
                assert_eq!(
                    color,
                    Color {
                        r: 255,
                        g: 215,
                        b: 0,
                        a: 255
                    }
                );
            }
            _ => panic!("Expected PaintSolid"),
        }
    }

    #[test]
    fn test_gradient_fill_to_paint() {
        let tree = parse_tree("gradient_glyphs/u2B50.svg");
        let fill = first_fill(&tree).expect("should have a fill");
        let paint = fill_to_paint(&fill, Affine::IDENTITY);
        match paint {
            Paint::LinearGradient(lg) => {
                assert!(
                    lg.color_line.len() >= 2,
                    "gradient should have at least 2 stops"
                );
                // First stop should be red
                assert_eq!(lg.color_line[0].color.r, 255);
                assert_eq!(lg.color_line[0].color.g, 0);
                assert_eq!(lg.color_line[0].color.b, 0);
                // Last stop should be blue
                let last = lg.color_line.last().unwrap();
                assert_eq!(last.color.r, 0);
                assert_eq!(last.color.g, 0);
                assert_eq!(last.color.b, 255);
            }
            _ => panic!("Expected PaintLinearGradient, got {:?}", paint),
        }
    }

    #[test]
    fn test_gradient_points_transformed() {
        let tree = parse_tree("gradient_glyphs/u2B50.svg");
        let fill = first_fill(&tree).expect("should have a fill");
        // Apply a transform: scale by 10 (like viewBox 0,0,100,100 → font 1000 UPEM)
        let font_transform = Affine::scale(10.0);
        let paint = fill_to_paint(&fill, font_transform);
        match paint {
            Paint::LinearGradient(lg) => {
                // x1=0→0, y1=0→0, x2=1→10, y2=1→10
                assert!((lg.p0.x - 0.0).abs() < 0.001);
                assert!((lg.p0.y - 0.0).abs() < 0.001);
                assert!((lg.p1.x - 10.0).abs() < 0.001);
                assert!((lg.p1.y - 10.0).abs() < 0.001);
            }
            _ => panic!("Expected PaintLinearGradient"),
        }
    }

    #[test]
    fn test_affine_scale_identity() {
        assert!((affine_scale(&Affine::IDENTITY) - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_affine_scale_uniform() {
        let t = Affine::scale(2.0);
        assert!((affine_scale(&t) - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_collect_colors_simple() {
        let tree = parse_tree("simple_glyphs/u0041.svg");
        let mut svgs = HashMap::new();
        svgs.insert(GlyphName::new("A"), tree);
        let palettes = collect_colors_from_svgs(&svgs);
        let palettes = palettes.expect("should have colors");
        // Black fill → 1 color
        assert_eq!(palettes.palettes[0].len(), 1);
        assert_eq!(
            palettes.palettes[0][0],
            Color {
                r: 0,
                g: 0,
                b: 0,
                a: 255
            }
        );
    }

    #[test]
    fn test_collect_colors_gradient() {
        let tree = parse_tree("gradient_glyphs/u2B50.svg");
        let mut svgs = HashMap::new();
        svgs.insert(GlyphName::new("\u{2B50}"), tree);
        let palettes = collect_colors_from_svgs(&svgs);
        let palettes = palettes.expect("should have colors");
        // Gradient with 2 stops → 2 unique colors (red, blue)
        assert_eq!(palettes.palettes[0].len(), 2);
    }

    #[test]
    fn test_collect_empty_svgs() {
        let svgs = HashMap::new();
        assert!(collect_colors_from_svgs(&svgs).is_none());
    }
}
