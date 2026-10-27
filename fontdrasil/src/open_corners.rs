//! Detecting 'open corners' in outlines
//!
//! An open corner is where, instead of putting a node at the intended corner
//! between two segments, the designer continues the first segment slightly
//! past it and adds a short line segment back to the start of the second.
//!
//! See [glyphsLib's eraseOpenCorners] for the original.
//!
//! [glyphsLib's eraseOpenCorners]: https://github.com/googlefonts/glyphsLib/blob/74c63244fdb/Lib/glyphsLib/filters/eraseOpenCorners.py

use std::ops::Range;

use kurbo::{Line, ParamCurve, PathSeg, Point, Shape};
use ordered_float::OrderedFloat;

/// Find the open corner formed by `one` and `two`, if there is one.
///
/// The segments are joined by a line from the end of `one` to the start of
/// `two`. If that line is an open corner that should be erased, this returns
/// where the two segments cross, which is the actual corner.
pub fn open_corner(one: PathSeg, two: PathSeg) -> Option<Intersection> {
    let line = Line::new(one.end(), two.start());
    log::trace!("considering ({}..{})", line.p0, line.p1);
    // both points either side of the line must be on its right
    // (see discussion at <https://github.com/googlefonts/glyphsLib/pull/663>)
    // <https://github.com/googlefonts/glyphsLib/blob/74c63244fdb/Lib/glyphsLib/filters/eraseOpenCorners.py#L66-L71>
    let before = match one {
        PathSeg::Line(line) => line.p0,
        PathSeg::Quad(quad) => quad.p1,
        PathSeg::Cubic(cube) => cube.p2,
    };
    let after = match two {
        PathSeg::Line(line) => line.p1,
        PathSeg::Quad(quad) => quad.p1,
        PathSeg::Cubic(cube) => cube.p1,
    };
    if point_is_left_of_line(line, before) || point_is_left_of_line(line, after) {
        log::trace!("crossing points {before} and {after} not on same side of line");
        return None;
    }

    let Some(intersection) = intersection(one, two) else {
        log::trace!("no intersections");
        return None;
    };

    let Intersection { t0, t1 } = intersection;
    // invert value of t0 so for both values '0' means at the open corner
    // <https://github.com/googlefonts/glyphsLib/blob/74c63244fdbef1da5/Lib/glyphsLib/filters/eraseOpenCorners.py#L105>
    let t0_inv = 1.0 - t0;
    log::trace!("found intersections at {t0} and {t1}");
    // from glyphsapp: https://github.com/googlefonts/fontc/issues/1600#issuecomment-3190896627
    (((t0_inv < 0.5 && t1 < 0.5) || (t0_inv < 0.3 && t1 < 0.99) || (t0_inv < 0.99 && t1 < 0.3))
        && t0_inv > 0.001
        && t1 > 0.001)
        .then_some(intersection)
}

/// Find the intersection of the two segments, if one exists.
fn intersection(one: PathSeg, two: PathSeg) -> Option<Intersection> {
    let candidate = seg_seg_intersection(one, two)?;

    // there is a bug in kurbo that can cause it to report spurious intersections
    // (see https://github.com/linebender/kurbo/issues/411).
    // as a temporary workaround here we check that the point of intersection
    // on each segment are relatively close to one another, and discard
    // if not.
    let p1 = one.eval(candidate.t0);
    let p2 = two.eval(candidate.t1);
    let dist = p1.distance(p2);
    // the value of 0.2 was chosen experimentally (it lets our tests pass,
    // but doesn't seem to hurt any fonts in crater)
    if dist < 0.2 { Some(candidate) } else { None }
}

//https://github.com/googlefonts/glyphsLib/blob/74c63244fdbe/Lib/glyphsLib/filters/eraseOpenCorners.py#L14
// 'left' from the perspective of an observer standing on line.p0 and lookign at line.p1?
fn point_is_left_of_line(line: Line, point: Point) -> bool {
    let Line { p0: a, p1: b } = line;
    (b.x - a.x) * (point.y - a.y) - (b.y - a.y) * (point.x - a.x) >= 0.0
}

/// Where two segments cross.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Intersection {
    /// Location of the hit on the first segment, in range 0..=1
    pub t0: f64,
    /// Location on the second segment
    pub t1: f64,
}

impl Intersection {
    // used to avoid duplicate equivalent intersections:
    // https://github.com/fonttools/fonttools/blob/7b50bde2ee/Lib/fontTools/misc/bezierTools.py#L1366
    fn unique_key(&self) -> (u64, u64) {
        (
            (self.t0 / PY_ACCURACY) as u64,
            (self.t1 / PY_ACCURACY) as u64,
        )
    }
}

/// Find an intersection of two segments, if any exist
///
/// It is possible for segments to intersect multiple times; in this case we
/// will return the segment nearest to the start of `seg1``
fn seg_seg_intersection(seg1: PathSeg, seg2: PathSeg) -> Option<Intersection> {
    let hit = match (seg1, seg2) {
        (PathSeg::Line(line), seg) => seg
            .intersect_line(line)
            .iter()
            .min_by_key(|hit| OrderedFloat(hit.line_t))
            .map(|hit| Intersection {
                t0: hit.line_t,
                t1: hit.segment_t,
            }),
        (seg, PathSeg::Line(line)) => seg
            .intersect_line(line)
            .iter()
            .min_by_key(|hit| OrderedFloat(hit.segment_t))
            .map(|hit| Intersection {
                t0: hit.segment_t,
                t1: hit.line_t,
            }),
        (bez0, bez1) => return curve_curve_intersection_py(bez0, bez1),
    }?;
    if let (PathSeg::Line(l1), PathSeg::Line(l2)) = (seg1, seg2) {
        let pt = l1.eval(hit.t0);
        // the bezierTools code for line intersections has a bunch of special
        // cases that were causing us to deviate, so we try to cover those here
        // as they come up:
        //
        // special check for close x coords:
        // https://github.com/fonttools/fonttools/blob/a6f59a4f87a011/Lib/fontTools/misc/bezierTools.py#L1193-L1212

        if py_isclose(l1.end().x, l1.start().x) || py_isclose(l2.start().x, l2.end().x) {
            return Some(hit);
        }
        // final guard statement
        // https://github.com/fonttools/fonttools/blob/a6f59a4f87a/Lib/fontTools/misc/bezierTools.py#L1221-L1223
        if !(l1.p0.points_are_on_same_side(pt, l1.p1) && l2.p1.points_are_on_same_side(pt, l2.p0)) {
            return None;
        }
    }
    Some(hit)
}

// https://docs.python.org/3.13/library/math.html#math.isclose
fn py_isclose(a: f64, b: f64) -> bool {
    const TOLERANCE: f64 = 1e-09;
    (a - b).abs() <= (TOLERANCE * a.abs().max(b.abs()))
}

/// A helper for testing the position of a pair of points in reference to an origin
///
/// This is intended to reproduce the behaviour of the
/// [`_both_points_are_on_same_side_of_origin`][pyref] function in python.
///
/// [pyref]: https://github.com/fonttools/fonttools/blob/a6f59a4f87a01110/Lib/fontTools/misc/bezierTools.py#L1148
trait SameSide {
    /// Test whether both points
    fn points_are_on_same_side(&self, a: Point, b: Point) -> bool;
}

impl SameSide for Point {
    fn points_are_on_same_side(&self, a: Point, b: Point) -> bool {
        let x_diff = (a.x - self.x) * (b.x - self.x);
        let y_diff = (a.y - self.y) * (b.y - self.y);
        x_diff > 0.0 || y_diff > 0.0
    }
}

// https://github.com/fonttools/fonttools/blob/cb159dea72/Lib/fontTools/misc/bezierTools.py#L1307
const PY_ACCURACY: f64 = 1e-3;

// based on impl in fonttools/bezierTools; we split it in two,
// with the recursive bit below, and this as a little wrapper.
//https://github.com/fonttools/fonttools/blob/cb159dea72703/Lib/fontTools/misc/bezierTools.py#L1306
fn curve_curve_intersection_py(seg1: PathSeg, seg2: PathSeg) -> Option<Intersection> {
    let mut result = Vec::new();
    curve_curve_py_impl(seg1, seg2, &(0.0..1.0), &(0.0..1.0), &mut result);
    result.first().copied()
}

fn curve_curve_py_impl(
    seg1: PathSeg,
    seg2: PathSeg,
    range1: &Range<f64>,
    range2: &Range<f64>,
    buf: &mut Vec<Intersection>,
) {
    fn midpoint(range: &Range<f64>) -> f64 {
        0.5 * (range.start + range.end)
    }

    let bounds1 = seg1.bounding_box();
    let bounds2 = seg2.bounding_box();
    if !bounds1.overlaps(bounds2) {
        return;
    }
    // if bounds intersect but they're tiny, approximate
    if bounds1.area() < PY_ACCURACY && bounds2.area() < PY_ACCURACY {
        let hit = Intersection {
            t0: midpoint(range1),
            t1: midpoint(range2),
        };
        let key = hit.unique_key();
        // python dedupes after, using a set; the number of hits is bounded
        // and it's probably just cheaper to be quadratic
        if !buf.iter().any(|x| x.unique_key() == key) {
            buf.push(hit);
        }
        return;
    }

    // otherwise split the segments in half and try again on subsegments.
    let (seg1_1, seg1_2) = seg1.subdivide();
    let seg1_1_range = range1.start..midpoint(range1);
    let seg1_2_range = midpoint(range1)..range1.end;
    let (seg2_1, seg2_2) = seg2.subdivide();
    let seg2_1_range = range2.start..midpoint(range2);
    let seg2_2_range = midpoint(range2)..range2.end;
    curve_curve_py_impl(seg1_1, seg2_1, &seg1_1_range, &seg2_1_range, buf);
    curve_curve_py_impl(seg1_2, seg2_1, &seg1_2_range, &seg2_1_range, buf);
    curve_curve_py_impl(seg1_1, seg2_2, &seg1_1_range, &seg2_2_range, buf);
    curve_curve_py_impl(seg1_2, seg2_2, &seg1_2_range, &seg2_2_range, buf);
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)] // test code
    use kurbo::{CubicBez, Vec2};

    use super::*;

    // ensure that we match fonttools when intersection produces more than one
    // hit (in this case fonttools returns the hit with the lowest `t0`)
    #[test]
    fn seg_seg_intersect_order() {
        let _ = tracing_subscriber::fmt().with_test_writer().try_init();
        let seg1 = PathSeg::Cubic(CubicBez::new(
            (21.0, 34.0),
            (21.0, 33.0),
            (21.0, 33.0),
            (22.0, 33.0),
        ));

        let seg2 = Line::new((22.0, 32.0), (21.0, 34.0));

        let raw_intersections = seg1.intersect_line(seg2);
        assert_eq!(raw_intersections.len(), 2);
        let one_intersection = seg_seg_intersection(seg1, seg2.into()).unwrap();
        assert_eq!(
            one_intersection.t0,
            raw_intersections
                .iter()
                .min_by_key(|hit| OrderedFloat(hit.segment_t))
                .unwrap()
                .segment_t
        );

        let reverse_intersection = seg_seg_intersection(seg2.into(), seg1).unwrap();
        assert_eq!(
            reverse_intersection.t0,
            raw_intersections
                .iter()
                .min_by_key(|hit| OrderedFloat(hit.line_t))
                .unwrap()
                .line_t
        )
    }

    // ensure that we match fonttools when intersection produces more than one
    // hit (in which case fonttools uses the first hit, which is based on the
    // operation order of the divide/conquer calls in curve_curve_intersection_py
    #[test]
    fn curve_curve_intersect_order() {
        let _ = tracing_subscriber::fmt().with_test_writer().try_init();
        let seg1 = CubicBez::new(
            (336.0, 150.0),
            (340.0, 151.0),
            (341.0, 151.0),
            (339.0, 152.0),
        )
        .into();
        let seg2 = CubicBez::new(
            (340.0, 152.0),
            (340.0, 151.0),
            (338.0, 149.0),
            (335.0, 148.0),
        )
        .into();

        let hit = curve_curve_intersection_py(seg1, seg2).unwrap();
        // matches bezierTools as of 7b50bde2e
        assert_eq!(hit.t1, 0.29296875);
    }

    #[test]
    fn same_sidedness() {
        let origin = Point { x: 1.0, y: 1.0 };
        // both right
        assert!(
            origin.points_are_on_same_side(
                origin + Vec2::new(1.0, 1.0),
                origin + Vec2::new(1.0, -1.0)
            )
        );
        // both left
        assert!(origin.points_are_on_same_side(
            origin + Vec2::new(-1.0, 1.0),
            origin + Vec2::new(-1.0, -1.0)
        ));
        //// both above
        assert!(
            origin.points_are_on_same_side(
                origin + Vec2::new(-1.0, 1.0),
                origin + Vec2::new(-1.0, 1.0)
            )
        );

        // both below
        assert!(origin.points_are_on_same_side(
            origin + Vec2::new(-1.0, -1.0),
            origin + Vec2::new(1.0, -1.0)
        ));

        // one up-left and one down-right (fail)
        assert!(
            !origin.points_are_on_same_side(
                origin + Vec2::new(-1.0, 1.0),
                origin + Vec2::new(1.0, -1.0)
            )
        );

        // zero doesn't count
        assert!(
            !origin.points_are_on_same_side(
                origin + Vec2::new(0.0, -1.0),
                origin + Vec2::new(0.0, 1.0)
            )
        );
        assert!(
            !origin.points_are_on_same_side(
                origin + Vec2::new(-1.0, 0.0),
                origin + Vec2::new(1.0, 0.0)
            )
        );
    }

    // https://github.com/linebender/kurbo/issues/411
    #[test]
    fn kurbo_411() {
        let one = CubicBez::new(
            (452.0, 240.0),
            (462.667, 78.667),
            (480.667, -146.333),
            (506.0, -435.0),
        )
        .into();
        let two = Line::new((385.0, 146.0), (438., 243.)).into();
        assert!(intersection(one, two).is_none());
    }
}
