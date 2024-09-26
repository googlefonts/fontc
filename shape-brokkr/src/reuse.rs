//! Functions to help find opportunities to reuse shapes.
//!
//! We assume the provided path is valid, in particular if it contains any
//! path elements it starts with a move.

use kurbo::{Affine, BezPath, PathEl, Point, Vec2};
use log::warn;

const _ALMOST_EQUAL_TOLERANCE: f64 = 1e-9;
const _SIGNIFICANCE_FACTOR: f64 = 5.0; // Must be at least N x tolerance to be significant
const X_BASIS: Vec2 = Vec2::new(1.0, 0.0);

/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/geometric_types.py#L23>
trait AlmostEqual {
    fn almost_equals(self, other: Self, tolerance: f64) -> bool;
}

impl AlmostEqual for f64 {
    #[inline]
    fn almost_equals(self, other: Self, tolerance: f64) -> bool {
        (self - other).abs() <= tolerance
    }
}

impl AlmostEqual for Vec2 {
    #[inline]
    fn almost_equals(self, other: Self, tolerance: f64) -> bool {
        self.x.almost_equals(other.x, tolerance) && self.y.almost_equals(other.y, tolerance)
    }
}

impl AlmostEqual for Point {
    #[inline]
    fn almost_equals(self, other: Self, tolerance: f64) -> bool {
        self.x.almost_equals(other.x, tolerance) && self.y.almost_equals(other.y, tolerance)
    }
}

impl AlmostEqual for &PathEl {
    fn almost_equals(self, other: Self, tolerance: f64) -> bool {
        match (self, other) {
            (PathEl::MoveTo(sp0), PathEl::MoveTo(op0)) => sp0.almost_equals(*op0, tolerance),
            (PathEl::LineTo(sp0), PathEl::LineTo(op0)) => sp0.almost_equals(*op0, tolerance),
            (PathEl::QuadTo(sp0, sp1), PathEl::QuadTo(op0, op1)) => {
                sp0.almost_equals(*op0, tolerance) && sp1.almost_equals(*op1, tolerance)
            }
            (PathEl::CurveTo(sp0, sp1, sp2), PathEl::CurveTo(op0, op1, op2)) => {
                sp0.almost_equals(*op0, tolerance)
                    && sp1.almost_equals(*op1, tolerance)
                    && sp2.almost_equals(*op2, tolerance)
            }
            (PathEl::ClosePath, PathEl::ClosePath) => true,
            _ => {
                debug_assert!(
                    std::mem::discriminant(self) != std::mem::discriminant(other),
                    "Missing case for {self:?}, {other:?}?"
                );
                false
            }
        }
    }
}

impl AlmostEqual for &BezPath {
    fn almost_equals(self, other: Self, tolerance: f64) -> bool {
        let self_el = self.elements();
        let other_el = other.elements();
        if self_el.len() != other_el.len() {
            return false;
        }
        self_el
            .iter()
            .zip(other_el)
            .all(|(e1, e2)| e1.almost_equals(e2, tolerance))
    }
}

fn round_scalar(v: f64, ndigits: u32) -> f64 {
    let mul = 10i32.pow(ndigits) as f64;
    let result = (v * mul).round() / mul;
    // -0.0 is rarely useful
    if result == -0.0 {
        0.0
    } else {
        result
    }
}

fn round_pt(pt: Point, ndigits: u32) -> Point {
    Point::new(round_scalar(pt.x, ndigits), round_scalar(pt.y, ndigits))
}

fn round_el(el: PathEl, ndigits: u32) -> PathEl {
    match el {
        PathEl::MoveTo(p) => PathEl::MoveTo(round_pt(p, ndigits)),
        PathEl::LineTo(p) => PathEl::LineTo(round_pt(p, ndigits)),
        PathEl::QuadTo(p0, p1) => PathEl::QuadTo(round_pt(p0, ndigits), round_pt(p1, ndigits)),
        PathEl::CurveTo(p0, p1, p2) => PathEl::CurveTo(
            round_pt(p0, ndigits),
            round_pt(p1, ndigits),
            round_pt(p2, ndigits),
        ),
        PathEl::ClosePath => PathEl::ClosePath,
    }
}

fn round_path(path: BezPath, ndigits: u32) -> BezPath {
    path.into_iter().map(|el| round_el(el, ndigits)).collect()
}

fn round_affine(affine: Affine, ndigits: u32) -> Affine {
    let mut coeffs = affine.as_coeffs();
    for v in coeffs.iter_mut() {
        *v = round_scalar(*v, ndigits);
    }
    Affine::new(coeffs)
}

// Shift the path such that it begins at 0,0
fn move_to_origin(path: &BezPath) -> BezPath {
    let Some(PathEl::MoveTo(first_move)) = path.elements().first() else {
        return path.clone();
    };
    let shift = Vec2::new(-first_move.x, -first_move.y);
    let transform = Affine::translate(shift);
    let mut path = path.clone();
    path.apply_affine(transform);
    path
}

/// If we thought of the path as a series of vectors to the endpoints of each successive
/// drawing command what would it look like?
///
/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/svg_reuse.py#L90>
struct VecsIter<'a> {
    els: &'a [PathEl],
    idx: usize,
    last_move: Point,
    curr_pos: Point,
}

impl<'a> VecsIter<'a> {
    fn new(path: &'a BezPath) -> Self {
        Self {
            els: path.elements(),
            idx: 0,
            last_move: Point::ZERO,
            curr_pos: Point::ZERO,
        }
    }
}

impl<'a> Iterator for VecsIter<'a> {
    type Item = Vec2;

    fn next(&mut self) -> Option<Self::Item> {
        self.els.get(self.idx).map(|v| {
            self.idx += 1;
            match v {
                PathEl::MoveTo(p) => {
                    let result = *p - self.curr_pos;
                    self.last_move = *p;
                    self.curr_pos = *p;
                    result
                }
                PathEl::LineTo(p) | PathEl::QuadTo(_, p) | PathEl::CurveTo(_, _, p) => {
                    let result = *p - self.curr_pos;
                    self.curr_pos = *p;
                    result
                }
                // Python treats z as 0,0, we compute the actual vec
                PathEl::ClosePath => {
                    let result = self.last_move - self.curr_pos;
                    self.curr_pos = self.last_move;
                    result
                }
            }
        })
    }
}

/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/svg_reuse.py#L164>
fn first_significant(
    vecs: VecsIter,
    val_fn: impl Fn(Vec2) -> f64,
    tolerance: f64,
) -> Option<(usize, Vec2)> {
    let tolerance = tolerance * _SIGNIFICANCE_FACTOR;
    vecs.enumerate()
        .skip(1) // skip initial move
        .find(|(_, vec)| val_fn(*vec).abs() > tolerance)
        .map(|(idx, vec)| (idx, vec.clone()))
}

/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/svg_reuse.py#L176-L185>
fn first_significant_for_both(
    s1_vecs: VecsIter,
    s2_vecs: VecsIter,
    val_fn: impl Fn(Vec2) -> f64,
    tolerance: f64,
) -> Option<(usize, Vec2, Vec2)> {
    let tolerance = _SIGNIFICANCE_FACTOR * tolerance;
    s1_vecs
        .zip(s2_vecs)
        .enumerate()
        .skip(1) // skip initial move
        .find_map(|(idx, (vec1, vec2))| {
            if val_fn(vec1).abs() > tolerance && val_fn(vec2).abs() > tolerance {
                Some((idx, vec1, vec2))
            } else {
                None
            }
        })
}

/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/svg_reuse.py#L141>
fn angle(v: Vec2) -> f64 {
    v.y.atan2(v.x)
}

/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/svg_reuse.py#L146>
fn affine_vec_to_vec(from: Vec2, to: Vec2) -> Affine {
    // rotate initial to have the same angle as target (may have different magnitude)
    let angle = angle(to) - angle(from);
    let affine = Affine::rotate(angle);
    let vec = (affine * from.to_point()).to_vec2();

    // scale to target magnitude
    let scale = if !vec.hypot().almost_equals(0.0, _ALMOST_EQUAL_TOLERANCE) {
        to.hypot() / vec.hypot()
    } else {
        0.0
    };
    affine * Affine::scale(scale)
}

/// Build a version of shape that will compare == to other shapes even if offset, scaled, rotated, etc.
///
/// Intended use is to normalize multiple shapes to identify opportunity for reuse.
/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/svg_reuse.py#L240>
///
/// At time of writing does *not* produce the same result for equivalent shapes with different point order
/// or drawing commands.
pub fn normalize(path: &BezPath, tolerance: f64) -> BezPath {
    // Always start at 0,0
    let mut path = move_to_origin(path);

    // Normalize first activity to [1 0]; eliminates rotation and uniform scaling
    if let Some((_, vec_first)) = first_significant(VecsIter::new(&path), Vec2::hypot, tolerance) {
        if !vec_first.almost_equals(X_BASIS, _ALMOST_EQUAL_TOLERANCE) {
            path.apply_affine(affine_vec_to_vec(vec_first, X_BASIS));
        }
    }

    // Normalize first y activity to 1.0; eliminates mirroring and non-uniform scaling
    if let Some((_, vecy)) = first_significant(VecsIter::new(&path), |vec| vec.y, tolerance) {
        if !vecy.y.almost_equals(1.0, _ALMOST_EQUAL_TOLERANCE) {
            path.apply_affine(Affine::scale_non_uniform(1.0, 1.0 / vecy.y));
        }
    }

    path
}

/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/svg_reuse.py#L32-L36>s
fn first_move(path: &BezPath) -> Option<Point> {
    path.elements().first().and_then(|el| {
        if let PathEl::MoveTo(p) = el {
            Some(p.to_owned())
        } else {
            None
        }
    })
}

/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/svg_reuse.py#L137-L138>
fn nth_vector(path: &BezPath, n: usize) -> Option<Vec2> {
    VecsIter::new(path).skip(n).next()
}

fn try_affine_between(affine: &Affine, s1: &BezPath, s2: &BezPath, tolerance: f64) -> bool {
    // TODO we could just walk along comparing elements rather than cloning the whole path
    let mut s1_prime = s1.clone();
    s1_prime.apply_affine(*affine);
    eprintln!(
        "try_affine_between\n  {affine:?}\n  works? {}\n  diff {:?}",
        s1_prime.almost_equals(s2, tolerance),
        path_diff(&s1_prime, s2),
    );
    s1_prime.almost_equals(s2, tolerance)
}

/// Combines affines in logical order, e.g. "do [0] then [1] then [2]".
///
/// Using * you end up writing everything backwards, e.g. to move then rotate
/// you would rotate * move (rotate *of* move)
///
/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/svg_transform.py#L187-L193>
fn combine_ltr(affines: &[Affine]) -> Affine {
    affines
        .iter()
        .rev()
        .copied()
        .reduce(|acc, e| acc * e)
        .unwrap_or(Affine::IDENTITY)
}

/// Returns an affine that turns s1 into s2 or None if no solution was found.
///
/// Intended use is to call this only when the normalized versions of the shapes
/// are the same, in which case finding a solution is typical.
///
/// <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/src/picosvg/svg_reuse.py#L296-L383>
pub fn affine_between(s1: &BezPath, s2: &BezPath, tolerance: f64) -> Option<Affine> {
    // Easy mode?
    if s1.elements().len() != s2.elements().len() {
        return None;
    }
    if s1.almost_equals(s2, tolerance) {
        return Some(Affine::IDENTITY);
    }

    // Just move to the same start point?
    let (Some(s1_move), Some(s2_move)) = (first_move(s1), first_move(s2)) else {
        warn!("At least one input does not start with a move");
        return None
    };
    let affine = Affine::translate(s2_move - s1_move);
    if try_affine_between(&affine, s1, s2, tolerance) {
        return Some(affine); // TODO: round
    }

    // Align the first edge with a significant x part.
    // Fixes rotation, x-scale, and uniform scaling.
    let Some((s2_vec1x_idx, s2_vec1x)) = first_significant(VecsIter::new(s2), |v| v.x, tolerance) else {
        // bail out if we find no first edge with significant x part
        // https://github.com/googlefonts/picosvg/issues/246
        eprintln!("no first x-part");
        return None
    };
    let Some(s1_vec1) = nth_vector(s1, s2_vec1x_idx) else {
        eprintln!("s1_vec1 at [{s2_vec1x_idx}]");
        return None;
    };

    let s1_to_origin = Affine::translate(-s1_move.to_vec2());
    let s2_to_origin = Affine::translate(-s2_move.to_vec2());
    let s1_vec1_to_s2_vec1x = affine_vec_to_vec(s1_vec1, s2_vec1x);

    // Move to s2 start
    let origin_to_s2 = Affine::translate(s2_move.to_vec2());

    let affine = combine_ltr(&[s1_to_origin, s1_vec1_to_s2_vec1x, origin_to_s2]);
    if try_affine_between(&affine, s1, s2, tolerance) {
        return Some(affine); // TODO: round
    }

    // Could be non-uniform scaling and/or mirroring
    // Make the aligned edge the x axis then align the first edge with a significant y part.

    // Rotate first edge to lie on x axis
    let s2_vec1_angle = angle(s2_vec1x);
    let rotate_s2vec1_onto_x = Affine::rotate(-s2_vec1_angle);
    let rotate_s2vec1_off_x = Affine::rotate(s2_vec1_angle);

    let affine = s1_to_origin * s1_vec1_to_s2_vec1x * rotate_s2vec1_onto_x;
    let mut s1_prime = s1.clone();
    s1_prime.apply_affine(affine);

    let affine = s2_to_origin * rotate_s2vec1_onto_x;
    let mut s2_prime = s2.clone();
    s2_prime.apply_affine(affine);

    // The first vector we aligned now lies on the x axis
    // Find and align the first vector that heads off into y for both
    if let Some((idx, s1_vecy, s2_vecy)) = first_significant_for_both(
        VecsIter::new(&s1_prime),
        VecsIter::new(&s2_prime),
        |v| v.y,
        tolerance,
    ) {
        let affine = combine_ltr(&[
            s1_to_origin,
            s1_vec1_to_s2_vec1x,
            rotate_s2vec1_onto_x, // lie vec1 along x axis
            Affine::scale_non_uniform(1.0, s2_vecy.y / s1_vecy.y), // scale first y-vectors to match; x-parts should already match
            rotate_s2vec1_off_x, // restore the rotation we removed
            origin_to_s2,        // drop into final position
        ]);
        if try_affine_between(&affine, s1, s2, tolerance) {
            return Some(affine); // TODO: round
        }
    }

    // If we still aren't the same give up
    return None;
}

fn path_diff(path1: &BezPath, path2: &BezPath) -> BezPath {
    assert_eq!(path1.elements().len(), path2.elements().len());
    let mut path = BezPath::new();
    for (el1, el2) in path1.elements().iter().zip(path2.elements()) {
        match (el1, el2) {
            (PathEl::MoveTo(path1_p1), PathEl::MoveTo(path2_p1)) => {
                path.move_to((*path2_p1 - *path1_p1).to_point())
            }
            (PathEl::LineTo(path1_p1), PathEl::LineTo(path2_p1)) => {
                path.line_to((*path2_p1 - *path1_p1).to_point())
            }
            (PathEl::QuadTo(path1_p1, path1_p2), PathEl::QuadTo(path2_p1, path2_p2)) => path
                .quad_to(
                    (*path2_p1 - *path1_p1).to_point(),
                    (*path2_p2 - *path1_p2).to_point(),
                ),
            (
                PathEl::CurveTo(path1_p1, path1_p2, path1_p3),
                PathEl::CurveTo(path2_p1, path2_p2, path2_p3),
            ) => path.curve_to(
                (*path2_p1 - *path1_p1).to_point(),
                (*path2_p2 - *path1_p2).to_point(),
                (*path2_p3 - *path1_p3).to_point(),
            ),
            (PathEl::ClosePath, PathEl::ClosePath) => path.close_path(),
            _ => panic!("path elements of inconsistent type: {el1:?}, {el2:?}"),
        };
    }
    path
}

#[cfg(test)]
mod tests {
    use kurbo::{Affine, BezPath, PathEl, Point, Vec2};

    use crate::reuse::{affine_between, first_significant, round_affine, round_path, VecsIter, path_diff};

    use super::{move_to_origin, normalize};

    fn sample_triangle() -> BezPath {
        BezPath::from_svg("M5,5 L10,0 L10,10 Z").unwrap()
    }

    fn sample_rect(at: impl Into<Point>, width: f64, height: f64) -> BezPath {
        let mut path = BezPath::new();
        let at = at.into();
        path.move_to(at);
        path.line_to(at + (width, 0.0));
        path.line_to(at + (0.0, height));
        path.line_to(at + (-width, 0.0));
        path.close_path();
        path
    }

    #[test]
    fn simple_move_to_origin() {
        let original = sample_triangle();
        assert_eq!("M0,0 L5,-5 L5,5 Z", move_to_origin(&original).to_svg());
    }

    #[test]
    fn vecs_ing_triangle() {
        assert_eq!(
            vec![
                Vec2::new(5.0, 5.0),
                Vec2::new(5.0, -5.0),
                Vec2::new(0.0, 10.0),
                Vec2::new(-5.0, -5.0),
            ],
            VecsIter::new(&sample_triangle()).collect::<Vec<_>>()
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L46>
    #[test]
    fn vecs_ing_box() {
        assert_eq!(
            vec![
                Vec2::new(10.0, 10.0),
                Vec2::new(10.0, 0.0),
                Vec2::new(0.0, 10.0),
                Vec2::new(-10.0, 0.0),
                Vec2::new(0.0, -10.0),
            ],
            VecsIter::new(&BezPath::from_svg("M10,10 h10 v10 h-10 z").unwrap()).collect::<Vec<_>>()
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L57-L73>
    #[test]
    fn vecs_ing_arc_x() {
        // arc from 0,0 to 2,0. Apex and farthest point at 1,0.5
        // vectors formed by start => farthest, farthest => end
        assert_eq!(
            vec![
                Vec2::new(0.0, 0.0),
                Vec2::new(2.0, 0.0),
                Vec2::new(0.0, 0.5),
            ],
            VecsIter::new(&BezPath::from_svg("M0,0 a 1 0.5 0 1 1 2,0").unwrap())
                .collect::<Vec<_>>()
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L74-L85>
    #[test]
    fn vecs_ing_arc_y() {
        // arc from 0,0 to 0,2. Apex and farthest point at 0,0.5
        // vectors formed by start => farthest, farthest => end
        assert_eq!(
            vec![
                Vec2::new(0.0, 0.0),
                Vec2::new(0.5, 0.0),
                Vec2::new(0.0, 2.0),
            ],
            VecsIter::new(&BezPath::from_svg("M0,0 a 0.5 1 0 1 1 0,2").unwrap())
                .collect::<Vec<_>>()
        );
    }

    // Arc from Noto Emoji that was resulting in sqrt of a very small negative
    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L86-L98>
    #[test]
    fn vecs_ing_arc_small_value() {
        assert_eq!(
            vec![
                Vec2::new(0.0, 0.0),
                Vec2::new(-3.5, 0.0),
                Vec2::new(0.0, 1.73),
                Vec2::new(3.5, 0.0),
                Vec2::new(0.0, 1.73),
                Vec2::new(0.0, 0.0),
            ],
            VecsIter::new(
                &BezPath::from_svg("M0,0 a1.75 1.73 0 1 1 -3.5,0 a1.75 1.73 0 1 1 3.5,0 z")
                    .unwrap()
            )
            .collect::<Vec<_>>()
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L99-L110>
    #[test]
    fn vecs_ing_arc_from_openmoji() {
        assert_eq!(
            vec![
                Vec2::new(11.011, 11.0),
                Vec2::new(49.989000000000004, 0.0),
                Vec2::new(0.0, 0.0),
                Vec2::new(0.0, 0.0),
                Vec2::new(0.0, 49.767),
                Vec2::new(0.0, 0.0),
            ],
            VecsIter::new(
                &BezPath::from_svg("M11.011,11 L61,11 A0 0 0 0 1 61,11 L61,60.767 Z").unwrap()
            )
            .collect::<Vec<_>>()
        );
    }

    // Our expected differs from Python as it uses absolute rather than relative.
    //
    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L22>
    #[test]
    fn normalize_triangle() {
        let input = BezPath::from_svg("M-1,-1 L 0,1 L 1, -1 z").unwrap();
        let expected_norm = "M0,0 L1,-0 L0.4,1 Z";
        let actual_norm = round_path(normalize(&input, 0.1), 4);
        assert_eq!(expected_norm, actual_norm.to_svg());
    }

    // Example from Noto Emoji that caused problems in Python
    // Expected differs from python as we do the math slightly differently; shapes are very similar.
    //
    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L22>
    #[test]
    fn normalize_noto_emoji_eyes() {
        let input = BezPath::from_svg("M44.67,45.94 L44.67,45.94 C40.48,45.94 36.67,49.48 36.67,55.36 C36.67,61.24 40.48,64.77 44.67,64.77 L44.67,64.77 C48.86,64.77 52.67,61.23 52.67,55.36 C52.67,49.49 48.86,45.94 44.67,45.94 Z").unwrap();
        let expected_norm = "M0,0 L0,0 C0.2195,-0.262 0.6374,-0.3123 1,-0 C1.3626,0.3123 1.3808,0.738 1.1613,1 L1.1613,1 C0.9419,1.262 0.524,1.3123 0.162,1.0005 C-0.2001,0.6888 -0.2195,0.262 0,0 Z";
        let actual_norm = round_path(normalize(&input, 0.1), 4);
        assert_eq!(expected_norm, actual_norm.to_svg());
    }

    #[test]
    fn first_significant_skips_move() {
        let path = sample_rect((20.0, 20.0), 100.0, 20.0);
        assert_eq!(
            Some((1, Vec2::new(100.0, 0.0))),
            first_significant(VecsIter::new(&path), |e| e.hypot(), 0.01)
        );
    }

    fn assert_no_affine_between(s1: &BezPath, s2: &BezPath, tolerance: f64) {
        assert_ne!(
            normalize(s1, tolerance).to_svg(),
            normalize(s2, tolerance).to_svg()
        );
        assert_eq!(None, affine_between(s1, s2, tolerance));
    }

    fn assert_affine_between(s1: &BezPath, s2: &BezPath, expected_affine: Affine, tolerance: f64) {
        // should normalize the same if we can affine between
        let norm1 = normalize(s1, tolerance);
        let norm2 = normalize(s2, tolerance);
        let norm_diff = path_diff(&norm2, &norm1);
        assert_eq!(
            round_path(norm1, 2).to_svg(),
            round_path(norm2, 2).to_svg(),
            "element-wise diff: {}",
            round_path(norm_diff, 2).to_svg()
        );

        assert_eq!(
            Some(expected_affine),
            affine_between(s1, s2, tolerance).map(|a| round_affine(a, 4)),
            "wrong affine between:\ns1: {s1:?}\ns2: {s2:?}"
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L123-L124>
    #[test]
    fn no_affine_between_rect_and_circle() {
        let rect = sample_rect((1.0, 1.0), 1.0, 1.0);
        // circle from SVGCircle(r=1).as_path().arcs_to_cubics().round_floats(2).d
        let circle = BezPath::from_svg("M1,0 C1,0.55 0.55,1 0,1 C-0.55,1 -1,0.55 -1,0 C-1,-0.55 -0.55,-1 0,-1 C0.55,-1 1,-0.55 1,0 Z").unwrap();
        assert_no_affine_between(&rect, &circle, 32.0);
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L125-L131>
    #[test]
    fn affine_between_rect_and_itself() {
        let rect = sample_rect((1.0, 1.0), 1.0, 1.0);
        assert_affine_between(&rect, &rect, Affine::IDENTITY, 0.01);
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L139-L145>
    #[test]
    fn affine_between_offset_rect() {
        let rect1 = sample_rect((0.0, 1.0), 1.0, 1.0);
        let rect2 = sample_rect((1.0, 0.0), 1.0, 1.0);
        assert_affine_between(&rect1, &rect2, Affine::translate((1.0, -1.0)), 0.01);
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L146-L152>
    #[test]
    fn affine_between_rects1() {
        let rect1 = sample_rect((20.0, 20.0), 100.0, 20.0);
        let rect2 = sample_rect((40.0, 30.0), 60.0, 20.0);
        assert_affine_between(
            &rect1,
            &rect2,
            Affine::new([0.6, 0.0, 0.0, 1.0, 28.0, 10.0]),
            0.01,
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L153-L159>
    #[test]
    fn affine_between_clock_circles() {
        todo!()
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L160-L171>
    #[test]
    fn affine_between_real_example() {
        let path1 =
            BezPath::from_svg("M18 12H2 c-1.104 0-2 .896-2 2h20c0-1.104-.896-2-2-2z").unwrap();
        let path2 =
            BezPath::from_svg("M34 12H18c-1.104 0-2 .896-2 2h20c0-1.104-.896-2-2-2z").unwrap();
        assert_affine_between(&path1, &path2, Affine::translate((16.0, 0.0)), 0.01);
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L172-L178>
    #[test]
    fn affine_between_mirrored_triangles() {
        let path1 = BezPath::from_svg("m60,64 -50,-32 0,30 z").unwrap();
        let path2 = BezPath::from_svg("m68,64 50,-32 0,30 z").unwrap();
        assert_affine_between(
            &path1,
            &path2,
            Affine::new([-1.0, 0.0, 0.0, 1.0, 128.0, -0.0]),
            0.01,
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L179-L185>
    #[test]
    fn affine_between_rotated_scaled_triangles() {
        let path1 = BezPath::from_svg("m50,100 -48,-75 81,0 z").unwrap();
        let path2 = BezPath::from_svg("m70,64 50,-32 0,54 z").unwrap();
        assert_affine_between(
            &path1,
            &path2,
            Affine::new([0.0, 0.6667, -0.6667, 0.0, 136.6667, 30.6667]),
            0.01,
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L187-L193>
    #[test]
    fn affine_between_square_and_rect() {
        let rect1 = sample_rect((10.0, 10.0), 50.0, 50.0);
        let rect2 = sample_rect((70.0, 20.0), 20.0, 100.0);
        assert_affine_between(
            &rect1,
            &rect2,
            Affine::new([0.4, 0.0, 0.0, 2.0, 66.0, 0.0]),
            0.01,
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L194-L200>
    #[test]
    fn affine_between_mirrored_squares() {
        let path1 = BezPath::from_svg("M10,10 10,60 60,60 60,10 z").unwrap();
        let path2 = BezPath::from_svg("M70,120 90,120 90,20 70,20 z").unwrap();
        assert_affine_between(
            &path1,
            &path2,
            Affine::new([0.0, -2.0, 0.4, 0.0, 66.0, 140.0]),
            0.01,
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L201-L212>
    #[test]
    fn affine_between_noto_emoji_shapes() {
        let path1 = BezPath::from_svg("M98.267,28.379 L115.157,21.769 Q116.007,21.437 116.843,21.802 Q117.678,22.168 118.011,23.017 Q118.343,23.867 117.978,24.703 Q117.612,25.538 116.763,25.871 L99.873,32.481 Q99.023,32.813 98.187,32.448 Q97.352,32.082 97.019,31.233 Q96.687,30.383 97.052,29.547 Q97.418,28.712 98.267,28.379 Z").unwrap();
        let path2 = BezPath::from_svg("M81.097,20.35 L79.627,4.2 Q79.544,3.291 80.128,2.59 Q80.712,1.889 81.62,1.807 Q82.529,1.724 83.23,2.308 Q83.931,2.892 84.013,3.8 L85.483,19.95 Q85.566,20.859 84.982,21.56 Q84.398,22.261 83.49,22.343 Q82.581,22.426 81.88,21.842 Q81.179,21.258 81.097,20.35 Z").unwrap();
        assert_affine_between(
            &path1,
            &path2,
            Affine::new([0.249, -0.859, 0.859, 0.249, 32.255, 97.667]),
            1.0,
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L213-L223>
    #[test]
    fn affine_between_noto_emoji_eyes() {
        let path1 = BezPath::from_svg("M44.67,45.94L44.67,45.94 c-4.19,0-8,3.54-8,9.42 s3.81,9.41,8,9.41l0,0 c4.19,0,8-3.54,8-9.41 S48.86,45.94,44.67,45.94z").unwrap();
        let path2 = BezPath::from_svg("M83,45.94   L83,45.94    c-4.19,0-8,3.54-8,9.42 s3.81,9.41,8,9.41l0,0 c4.19,0,8-3.54,8-9.41 S87.21,45.94,83,45.94z").unwrap();
        assert_affine_between(&path1, &path2, Affine::translate((38.33, 0.0)), 0.1);
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L224-L230>
    #[test]
    fn affine_between_circles() {
        todo!()
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L231-L237>
    #[test]
    fn affine_between_rects2() {
        let rect1 = BezPath::from_svg("M4,4 L8,4 L8,8 L4,8 L4,4 Z").unwrap();
        let rect2 = BezPath::from_svg("M2,2 L8,2 L8,4 L2,4 L2,2 Z").unwrap();
        assert_affine_between(
            &rect1,
            &rect2,
            Affine::new([1.5, 0.0, 0.0, 0.5, -4.0, 0.0]),
            0.01,
        );
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L238-L245>
    #[test]
    fn affine_between_arcs_in_same_dimension() {
        todo!()
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L246-L257>
    #[test]
    fn affine_between_arcs_in_same_dimension2() {
        todo!()
    }

    // <https://github.com/googlefonts/picosvg/blob/69cbfec486eca35a46187405abc39f608d3b2963/tests/svg_reuse_test.py#L258-L269>
    #[test]
    fn affine_between_arcs_in_same_dimension3() {
        todo!()
    }
}
