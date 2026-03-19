#![allow(dead_code)] // used by sub-plans 5 and 6

use kurbo::{Affine, BezPath, Point};

/// Build the affine transform from SVG y-down coordinate space to font y-up UPEM space.
///
/// The transform chain:
/// 1. scale(upem / height) — scale to font UPEM
/// 2. translate(0, upem) — position baseline
/// 3. scale(1, -1) — flip Y axis
///
/// Result: `(x, y)` in SVG → `(x * upem/height, upem - y * upem/height)` in font.
pub(crate) fn size_to_font_transform(_width: f32, height: f32, upem: u16) -> Affine {
    let scale = upem as f64 / height as f64;
    Affine::new([scale, 0.0, 0.0, -scale, 0.0, upem as f64])
}

/// Build the affine transform from SVG viewBox coordinate space to font y-up UPEM space.
///
/// The viewBox is passed as (x, y, width, height).
/// This function maps that region to the font coordinate space rectangle (0, 0, upem, upem),
/// with Y axis flipped (SVG y-down to font y-up).
///
/// The transform chain:
/// 1. Translate by (-x, -y) to move viewBox origin to (0,0)
/// 2. Scale uniformly by upem/height to map to font square (preserves aspect ratio)
/// 3. Translate by (0, upem) to position baseline
/// 4. Scale Y by -1 to flip Y axis
pub(crate) fn viewbox_to_font_transform(view_box: (f32, f32, f32, f32), upem: u16) -> Affine {
    let (x, y, _width, height) = view_box;
    let scale = upem as f64 / height as f64;
    Affine::new([
        scale,
        0.0,
        0.0,
        -scale,
        -x as f64 * scale,
        upem as f64 + y as f64 * scale,
    ])
}

/// Apply an affine transform to a Bezier path.
pub(crate) fn transform_path(path: &BezPath, affine: &Affine) -> BezPath {
    *affine * path.clone()
}

/// Apply an affine transform to a single point.
pub(crate) fn transform_point(point: Point, affine: &Affine) -> Point {
    *affine * point
}

/// Calculate proportional advance width from SVG aspect ratio.
///
/// For a square SVG, advance width = upem.
/// For a wide SVG, advance width scales proportionally.
pub(crate) fn advance_width(width: f32, height: f32, upem: u16) -> u16 {
    let scale = upem as f64 / height as f64;
    (width as f64 * scale).round() as u16
}

/// Convert usvg's transform to kurbo's Affine.
///
/// usvg Transform layout: `[sx kx tx; ky sy ty; 0 0 1]`
/// kurbo Affine layout: `[a c e; b d f]` = `[sx ky tx; kx sy ty]`
pub(crate) fn usvg_transform_to_kurbo(t: &usvg::tiny_skia_path::Transform) -> Affine {
    Affine::new([
        t.sx as f64,
        t.ky as f64,
        t.kx as f64,
        t.sy as f64,
        t.tx as f64,
        t.ty as f64,
    ])
}

/// Compose the viewbox-to-font transform with a local SVG transform.
///
/// Applies `font_space` after `local`, so a point transformed by the result
/// is first transformed by `local`, then by `font_space`.
pub(crate) fn compose_transforms(font_space: Affine, local: Affine) -> Affine {
    font_space * local
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[test]
    fn test_identity_size() {
        let affine = size_to_font_transform(100.0, 100.0, 1000);

        // Origin (0,0) in SVG → (0, 1000) in font (top-left → top of font)
        let p = affine * Point::new(0.0, 0.0);
        assert!((p.x - 0.0).abs() < 0.001);
        assert!((p.y - 1000.0).abs() < 0.001);

        // Bottom-right (100,100) in SVG → (1000, 0) in font
        let p = affine * Point::new(100.0, 100.0);
        assert!((p.x - 1000.0).abs() < 0.001);
        assert!((p.y - 0.0).abs() < 0.001);

        // Center (50,50) → (500, 500)
        let p = affine * Point::new(50.0, 50.0);
        assert!((p.x - 500.0).abs() < 0.001);
        assert!((p.y - 500.0).abs() < 0.001);
    }

    #[test]
    fn test_y_flip() {
        let affine = size_to_font_transform(100.0, 100.0, 1000);

        // SVG top-center (50, 0) → font top-center (500, 1000)
        let top = affine * Point::new(50.0, 0.0);
        assert!((top.y - 1000.0).abs() < 0.001);

        // SVG bottom-center (50, 100) → font baseline (500, 0)
        let bottom = affine * Point::new(50.0, 100.0);
        assert!((bottom.y - 0.0).abs() < 0.001);
    }

    #[rstest]
    #[case(100.0, 100.0, 1000, 1000)] // square
    #[case(200.0, 100.0, 1000, 2000)] // wide
    #[case(50.0, 100.0, 1000, 500)] // tall
    fn test_advance_width(
        #[case] w: f32,
        #[case] h: f32,
        #[case] upem: u16,
        #[case] expected: u16,
    ) {
        assert_eq!(advance_width(w, h, upem), expected);
    }

    #[test]
    fn test_usvg_transform_identity() {
        let usvg_t = usvg::tiny_skia_path::Transform::default();
        let kurbo_a = usvg_transform_to_kurbo(&usvg_t);
        let p = kurbo_a * Point::new(10.0, 20.0);
        assert!((p.x - 10.0).abs() < 0.001);
        assert!((p.y - 20.0).abs() < 0.001);
    }

    #[test]
    fn test_usvg_transform_translate() {
        let usvg_t = usvg::tiny_skia_path::Transform::from_translate(5.0, 10.0);
        let kurbo_a = usvg_transform_to_kurbo(&usvg_t);
        let p = kurbo_a * Point::new(0.0, 0.0);
        assert!((p.x - 5.0).abs() < 0.001);
        assert!((p.y - 10.0).abs() < 0.001);
    }

    #[test]
    fn test_usvg_transform_scale() {
        let usvg_t = usvg::tiny_skia_path::Transform::from_scale(2.0, 3.0);
        let kurbo_a = usvg_transform_to_kurbo(&usvg_t);
        let p = kurbo_a * Point::new(10.0, 20.0);
        assert!((p.x - 20.0).abs() < 0.001);
        assert!((p.y - 60.0).abs() < 0.001);
    }

    #[test]
    fn test_compose_transforms() {
        let font_space = size_to_font_transform(100.0, 100.0, 1000);
        let local =
            usvg_transform_to_kurbo(&usvg::tiny_skia_path::Transform::from_translate(10.0, 0.0));
        let composed = compose_transforms(font_space, local);

        // Point (0,0) → local (10,0) → font (100, 1000)
        let p = composed * Point::new(0.0, 0.0);
        assert!((p.x - 100.0).abs() < 0.001);
        assert!((p.y - 1000.0).abs() < 0.001);
    }

    #[test]
    fn test_transform_path() {
        let affine = size_to_font_transform(100.0, 100.0, 1000);
        let mut path = BezPath::new();
        path.move_to((0.0, 0.0));
        path.line_to((100.0, 100.0));
        let transformed = transform_path(&path, &affine);

        let mut iter = transformed.iter();
        match iter.next().unwrap() {
            kurbo::PathEl::MoveTo(p) => {
                assert!((p.x - 0.0).abs() < 0.001);
                assert!((p.y - 1000.0).abs() < 0.001);
            }
            _ => panic!("expected MoveTo"),
        }
        match iter.next().unwrap() {
            kurbo::PathEl::LineTo(p) => {
                assert!((p.x - 1000.0).abs() < 0.001);
                assert!((p.y - 0.0).abs() < 0.001);
            }
            _ => panic!("expected LineTo"),
        }
    }
}
