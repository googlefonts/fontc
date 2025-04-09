//! removing 'open corners' from glyphs
//!
//! see <https://github.com/googlefonts/glyphsLib/blob/74c63244fdb/Lib/glyphsLib/filters/eraseOpenCorners.py>

use std::{borrow::Cow, ops::Range};

use kurbo::{BezPath, CubicBez, Line, ParamCurve, PathEl, PathSeg, Point, QuadBez, Shape};
use ordered_float::OrderedFloat;

/// Removes 'outside open corners'.
///
/// This is a convention in some outlines where instead of adding a point
/// exactly at the intended corner between two segments, the designer
/// instead continues the segment slightly past the intended corner, and
/// then adds an additional short line segment to 'change direction' before
/// beginning the next segment.
///
/// The rationale for this is that sometimes the UX for manipulating a bezier
/// can make it challanging to draw certain kinds of transitions, and adding
/// the extra segment makes it easier to end up with the desired shape.
///
/// This method removes these extra segments, and determines the correct
/// actual corner point.
///
/// ```text
///   __
///   \/ before
///   /\       /\
///  /  \     /  \
/// /____\   /____\ after
/// ```
///
/// See:
/// - <https://github.com/googlefonts/fontmake/issues/288>
/// - <https://github.com/googlefonts/glyphsLib/issues/255#issuecomment-339591136>
/// - <https://github.com/googlefonts/glyphsLib/blob/74c63244fdb/Lib/glyphsLib/filters/eraseOpenCorners.py#L42>
pub fn erase_open_corners(path: &BezPath) -> Option<BezPath> {
    CornerErasureCtx::new(path).erase_corners()
}

/// Manages state during corner erasure.
///
/// When erasing corners, we need to modify the path after each erasure, So
/// that each operation incorporates the changes from previous operations.
///
/// To handle this, we use a `Cow` of path elements; in the general case (where
/// there are no open corners) this is borrowed, but if we need to erase anything
/// then we use [`Cow::to_mut`] to create a new copy, and modify it in place.
struct CornerErasureCtx<'a> {
    els: Cow<'a, [PathEl]>,
}

impl<'a> CornerErasureCtx<'a> {
    fn new(path: &'a BezPath) -> Self {
        CornerErasureCtx {
            els: path.elements().into(),
        }
    }

    fn is_closed(&self) -> bool {
        matches!(self.els.last(), Some(PathEl::ClosePath))
    }

    /// the number of possible open corners; this is a function of the number
    /// of path segments.
    fn n_possible_candidates(&self) -> usize {
        // if closed, there is no candidate for the close node or the node before
        // (which ends at the start, which was already handled)
        if self.els.last() == Some(&PathEl::ClosePath) {
            self.els.len() - 2
        } else {
            // if open, the three last elements aren't candidates (because
            // they do not have enough following elements)
            self.els.len().saturating_sub(3)
        }
    }

    /// loop through possible open corners, erasing any we find.
    fn erase_corners(mut self) -> Option<BezPath> {
        // we need at least three line or curve elements to have an open corner
        if (self.is_closed() && self.els.len() < 5) || self.els.len() < 4 {
            return None;
        }
        let mut made_changes = false;
        let mut ix = 0;
        // we don't use `for _ in 0..self.n_possible_candidates()` because
        // if we erase a corner the number of candidates changes
        while ix <= self.n_possible_candidates() {
            made_changes |= self.erase_open_corner_if_present(ix);
            ix += 1;
        }

        if !made_changes {
            return None;
        }

        Some(BezPath::from_vec(self.els.into_owned()))
    }

    /// Check for an open corner at a segment, and remove it if found.
    fn erase_open_corner_if_present(&mut self, seg_ix: usize) -> bool {
        let Some(maybe_corner) = self.possible_corner(seg_ix) else {
            return false;
        };
        log::trace!("considering segment starting at {}", maybe_corner.one.end());
        if !maybe_corner.points_are_right_of_line() {
            log::trace!(
                "crossing points {} and {} not on same side of line",
                maybe_corner.point_before_line(),
                maybe_corner.point_after_line()
            );
            return false;
        }

        let Some(intersection) = maybe_corner.intersection() else {
            log::trace!("no intersections");
            return false;
        };

        let Intersection { t0, t1, .. } = intersection;
        // invert value of t0 so for both values '0' means at the open corner
        // <https://github.com/googlefonts/glyphsLib/blob/74c63244fdbef1da5/Lib/glyphsLib/filters/eraseOpenCorners.py#L105>
        let t0_inv = 1.0 - t0;
        log::trace!("found intersections at {t0_inv} and {t1}");
        if ((t0_inv < 0.5 && t1 < 0.5) || t0_inv < 0.3 || t1 < 0.3)
            && t1 > 0.0001
            && t0_inv > 0.0001
        {
            log::debug!("found an open corner");
            // this looks like an open corner, so now do the deletion
            let new_prev = maybe_corner.one.subsegment(0.0..intersection.t0);
            let first_ix = maybe_corner.first_el_idx;
            self.overwrite_el(first_ix, new_prev);

            let new_next = maybe_corner.two.subsegment(intersection.t1..1.0);

            let next_seg_el_ix = self.wrapping_el_idx(first_ix + 2);
            // for the 'next' segment, the start point is the end point of 'prev', so we're good
            self.overwrite_el(next_seg_el_ix, new_next);

            // this is the open corner line, which we remove
            let line_seg_el_ix = self.wrapping_el_idx(first_ix + 1);
            self.els.to_mut().remove(line_seg_el_ix);

            // finally always make sure that start/end line up in a closed curve:
            if self.is_closed() {
                let second_last = self.els.len() - 2;
                let last_pt = self.els[second_last].end_point().unwrap();
                *self.els.to_mut().get_mut(0).unwrap() = PathEl::MoveTo(last_pt);
            }
            return true;
        }
        false
    }
    fn overwrite_el(&mut self, el_ix: usize, new_seg: PathSeg) {
        match (new_seg, self.els.to_mut().get_mut(el_ix).unwrap()) {
            (PathSeg::Line(new), PathEl::LineTo(end)) => {
                *end = new.p1;
            }
            (PathSeg::Quad(new), PathEl::QuadTo(p1, p2)) => {
                *p1 = new.p1;
                *p2 = new.p2;
            }
            (PathSeg::Cubic(new), PathEl::CurveTo(p1, p2, p3)) => {
                *p1 = new.p1;
                *p2 = new.p2;
                *p3 = new.p3;
            }
            // should not be reachable
            _ => panic!("el/seg mismatch"),
        }
    }

    /// Return the previous & next segments if the segment at `i` is a line.
    ///
    /// A corner is possible anytime a line segment occurs between two other
    /// path segments. This handles the annoying indexing logic of converting
    /// from the raw `PathEl`s to the appropriate `PathSeg`s.
    ///
    /// The conversion between elements and segments is not super intuitive.
    /// As a rule, the segment at `i` is the segment which begins at the end point
    /// of element i; for instance segment 0 is the segment that starts at
    /// the first (moveto) point in the curve.
    fn possible_corner(&self, i: usize) -> Option<PossibleCorner> {
        // if we're a closed path, the second-last element (the element before 'close')
        // brings us back to the start, which we've already handled.
        // (in an open path there's no wrapping so this can also never be a candidate)
        if i >= self.els.len().saturating_sub(2) {
            return None;
        }
        // the index of the element containing the start point of the prevoius segment.
        // this is two elements before element[i], not counting moveto/close
        let prev_seg_start = match i {
            // open path can't start with a corner
            0 if !self.is_closed() => return None,
            0 => self.els.len() - 3,
            _ => i - 1,
        };
        // stash this to use later if we delete, so we don't need to duplicate
        // this logic
        let first_el_idx = self.wrapping_el_idx(prev_seg_start + 1);

        let start = self.els.get(prev_seg_start).and_then(PathEl::end_point)?;
        let one = self
            .wrapping_get_el(prev_seg_start + 1)
            .and_then(|el| make_seg(el, start))?;
        if let PathEl::LineTo(line_to) = self.wrapping_get_el(prev_seg_start + 2)? {
            let two = self
                .wrapping_get_el(prev_seg_start + 3)
                .and_then(|el| make_seg(el, line_to))?;
            return Some(PossibleCorner {
                one,
                two,
                first_el_idx,
            });
        }
        None
    }

    fn wrapping_get_el(&self, i: usize) -> Option<PathEl> {
        self.els.get(self.wrapping_el_idx(i)).copied()
    }

    /// Determine the correct element index for a 'naive' index, wrapping if necessary.
    ///
    /// Specifically, if the path is closed and `i` is >= the last index, wrap
    /// around, skipping the `Close` and `MoveTo` elements.
    fn wrapping_el_idx(&self, i: usize) -> usize {
        // open path means no wrapping, just return an element
        if !self.is_closed() {
            return i;
        }

        if i == 0 {
            // we don't care about the moveto in a closed path, since it is
            // always the same point as the end of the last element, so we return that
            return self.els.len().saturating_sub(2);
        }

        // otherwise, wrapping.
        // when we wrap, we need to skip the first el (moveto) and the last (close)
        if i >= self.els.len() - 1 {
            // modulo length - 1 (for the close element) + 1 to skip move
            i % (self.els.len() - 1) + 1
        } else {
            i
        }
    }
}

/// Two segments that are connected by a line.
///
/// The line is not encoded explicitly, as it always starts at the last point
/// of `one` and ends at the first point of `two`.
#[derive(Clone, Debug)]
struct PossibleCorner {
    /// The actual index of the element describing `one`.
    ///
    /// This is the first element we need to modify in closing a corner; we
    /// can compute the other elements from it.
    first_el_idx: usize,
    /// The segment leading into the corner.
    one: PathSeg,
    /// The segment leading out of the corner.
    two: PathSeg,
}

impl PossibleCorner {
    /// Return the point (on-or-off-curve) immediately before the start of the line
    fn point_before_line(&self) -> Point {
        match self.one {
            PathSeg::Line(line) => line.p0,
            PathSeg::Quad(quad) => quad.p1,
            PathSeg::Cubic(cube) => cube.p2,
        }
    }

    /// Return the point (on-or-off-curve) immediately following the line
    fn point_after_line(&self) -> Point {
        match self.two {
            PathSeg::Line(line) => line.p1,
            PathSeg::Quad(quad) => quad.p1,
            PathSeg::Cubic(cube) => cube.p1,
        }
    }

    // the line part of the corner; this is what will be erased
    fn line(&self) -> Line {
        Line::new(self.one.end(), self.two.start())
    }

    /// Are the incoming point from the previous segment and the outgoing point
    /// from the next segment both on the right side of the line?
    ///
    /// (see discussion at <https://github.com/googlefonts/glyphsLib/pull/663>)
    /// <https://github.com/googlefonts/glyphsLib/blob/74c63244fdb/Lib/glyphsLib/filters/eraseOpenCorners.py#L66-L71>
    fn points_are_right_of_line(&self) -> bool {
        let prev_point = self.point_before_line();
        let next_point = self.point_after_line();
        let line = self.line();

        !(point_is_left_of_line(line, prev_point) || point_is_left_of_line(line, next_point))
    }

    /// Find the intersection of the two segments, if one exists.
    ///
    /// If this is an open corner, the intersection point will be the new corner.
    fn intersection(&self) -> Option<Intersection> {
        let candidate = seg_seg_intersection(self.one, self.two)?;

        // there is a bug in kurbo that can cause it to report spurious intersections
        // (see https://github.com/linebender/kurbo/issues/411).
        // as a temporary workaround here we check that the point of intersection
        // on each segment are relatively close to one another, and discard
        // if not.
        let p1 = self.one.eval(candidate.t0);
        let p2 = self.two.eval(candidate.t1);
        let dist = p1.distance(p2);
        // the value of 0.2 was chosen experimentally (it lets our tests pass,
        // but doesn't seem to hurt any fonts in crater)
        if dist < 0.2 {
            Some(candidate)
        } else {
            None
        }
    }
}

fn make_seg(element: PathEl, p0: Point) -> Option<PathSeg> {
    match element {
        PathEl::LineTo(p1) => Some(Line::new(p0, p1).into()),
        PathEl::QuadTo(p1, p2) => Some(QuadBez::new(p0, p1, p2).into()),
        PathEl::CurveTo(p1, p2, p3) => Some(CubicBez::new(p0, p1, p2, p3).into()),
        _ => None,
    }
}

//https://github.com/googlefonts/glyphsLib/blob/74c63244fdbe/Lib/glyphsLib/filters/eraseOpenCorners.py#L14
// 'left' from the perspective of an observer standing on line.p0 and lookign at line.p1?
fn point_is_left_of_line(line: Line, point: Point) -> bool {
    let Line { p0: a, p1: b } = line;
    (b.x - a.x) * (point.y - a.y) - (b.y - a.y) * (point.x - a.x) >= 0.0
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Intersection {
    // location of hit on first segment, in range 0..=1
    t0: f64,
    // location on second segment
    t1: f64,
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
    result.sort_by_key(|hit| (OrderedFloat(hit.t0), OrderedFloat(hit.t1)));
    result.dedup_by_key(|hit| ((hit.t0 / PY_ACCURACY) as i64, (hit.t1 / PY_ACCURACY) as i64));
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
        buf.push(Intersection {
            t0: midpoint(range1),
            t1: midpoint(range2),
        });
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

    use kurbo::Vec2;
    use write_fonts::OtRound;

    use super::*;

    //https://github.com/googlefonts/glyphsLib/blob/74c63244fdbef1da540d/tests/eraseOpenCorners_test.py#L7
    #[allow(non_snake_case)]
    mod python_test_glyphs {
        pub(super) fn space() -> kurbo::BezPath {
            kurbo::BezPath::new()
        }

        pub(super) fn hasCornerGlyph() -> kurbo::BezPath {
            let mut path = kurbo::BezPath::new();
            path.move_to((20.0, 0.0));
            path.line_to((179.0, 0.0));
            path.line_to((60.0, 353.0));
            path.line_to((198.0, 360.0));
            path.line_to((20.0, 0.0));
            path.close_path();
            path
        }
        pub(super) fn curvyCornerGlyph() -> kurbo::BezPath {
            let mut path = kurbo::BezPath::new();
            path.move_to((400.0, 0.0));
            path.curve_to((400.0, 100.0), (450.0, 300.0), (300.0, 300.0));
            path.line_to((200.0, 100.0));
            path.curve_to((250.0, 100.0), (450.0, 150.0), (450.0, 50.0));
            path.line_to((400.0, 0.0));
            path.close_path();
            path
        }
        pub(super) fn doubleCornerGlyph() -> kurbo::BezPath {
            let mut path = kurbo::BezPath::new();
            path.move_to((100.0, 0.0));
            path.line_to((400.0, 0.0));
            path.line_to((400.0, 500.0));
            path.line_to((500.0, 400.0));
            path.line_to((0.0, 400.0));
            path.line_to((100.0, 500.0));
            path.line_to((100.0, 0.0));
            path.close_path();
            path
        }
        pub(super) fn doubleCornerGlyphTrickyBitInMiddle() -> kurbo::BezPath {
            let mut path = kurbo::BezPath::new();
            path.move_to((100.0, 500.0));
            path.line_to((100.0, 0.0));
            path.line_to((400.0, 0.0));
            path.line_to((400.0, 500.0));
            path.line_to((500.0, 400.0));
            path.line_to((0.0, 400.0));
            path.line_to((100.0, 500.0));
            path.close_path();
            path
        }
        pub(super) fn curveCorner() -> kurbo::BezPath {
            let mut path = kurbo::BezPath::new();
            path.move_to((316.0, 437.0));
            path.curve_to(
                (388.67761, 437.0),
                (446.1305580343, 401.4757887467),
                (475.0, 344.0),
            );
            path.line_to((588.0, 407.0));
            path.line_to((567.0, 260.0));
            path.curve_to((567.0, 414.0), (464.0, 510.0), (316.0, 510.0));
            path.line_to((316.0, 437.0));
            path.close_path();
            path
        }

        pub(super) fn dotabove_ar() -> kurbo::BezPath {
            let mut path = kurbo::BezPath::new();
            path.move_to((58.0, -58.0));
            path.curve_to((90.0, -58.0), (116.0, -32.0), (116.0, 0.0));
            path.curve_to((116.0, 32.0), (90.0, 58.0), (58.0, 58.0));
            path.curve_to((26.0, 58.0), (0.0, 32.0), (0.0, 0.0));
            path.curve_to((0.0, -32.0), (26.0, -58.0), (58.0, -58.0));
            path.close_path();
            path
        }
        pub(super) fn sofiaSans() -> kurbo::BezPath {
            let mut path = kurbo::BezPath::new();
            path.move_to((190.0, 327.0));
            path.line_to((199.0, 497.0));
            path.line_to((199.0, 488.0));
            path.line_to((32.0, 503.0));
            path.line_to((190.0, 327.0));
            path.close_path();
            path
        }
        pub(super) fn largeCrossing() -> kurbo::BezPath {
            let mut path = kurbo::BezPath::new();
            path.move_to((378.0, 615.0));
            path.line_to((344.0, 706.0));
            path.line_to((444.0, 660.0));
            path.line_to((281.0, 699.0));
            path.line_to((378.0, 615.0));
            path.close_path();
            path
        }
    }

    macro_rules! assert_approx {
        ($left:expr, $right:expr) => {
            assert!(($left - $right).abs() < 1e-4, "{} !~= {}", $left, $right)
        };
    }

    #[test]
    fn ctx_test_open() {
        let mut path = kurbo::BezPath::new();
        path.move_to((10., 10.));
        path.line_to((20., 20.));
        path.line_to((30., 30.));
        path.line_to((40., 40.));
        let ctx = CornerErasureCtx::new(&path);
        assert!(ctx.possible_corner(0).is_none());
        let first = ctx.possible_corner(1).unwrap();
        assert_eq!(first.one, Line::new((10., 10.), (20., 20.)).into());
        assert_eq!(first.two, Line::new((30., 30.), (40., 40.,)).into());
        // not a closed path so we don't wrap around; only a single segment exists
        assert!(ctx.possible_corner(2).is_none());
    }

    #[test]
    fn ctx_test_closed() {
        let mut path = kurbo::BezPath::new();
        path.move_to((10., 10.));
        path.line_to((20., 20.));
        path.line_to((30., 30.));
        path.quad_to((50., 50.), (100., 100.));
        path.line_to((10., 10.));
        path.close_path();

        let ctx = CornerErasureCtx::new(&path);
        assert_eq!(
            ctx.wrapping_get_el(1).unwrap(),
            PathEl::LineTo((20., 20.).into())
        );
        assert_eq!(
            ctx.wrapping_get_el(2).unwrap(),
            PathEl::LineTo((30., 30.).into())
        );
        assert_eq!(
            ctx.wrapping_get_el(3).unwrap(),
            PathEl::QuadTo((50., 50.).into(), (100., 100.).into())
        );

        assert_eq!(
            ctx.wrapping_get_el(4).unwrap(),
            PathEl::LineTo((10., 10.).into())
        );

        assert_eq!(
            ctx.wrapping_get_el(5).unwrap(),
            PathEl::LineTo((20., 20.).into())
        );
        assert_eq!(
            ctx.wrapping_get_el(6).unwrap(),
            PathEl::LineTo((30., 30.).into())
        );

        let candi = ctx.possible_corner(0).unwrap();
        assert_eq!(candi.line(), Line::new((10., 10.), (20., 20.)));
        assert_eq!(candi.one.start(), (100., 100.).into());
        assert_eq!(candi.two.end(), (30., 30.).into());

        let candi = ctx.possible_corner(1).unwrap();
        assert_eq!(candi.line(), Line::new((20., 20.), (30., 30.)));
        assert_eq!(candi.one.start(), (10., 10.).into());
        assert_eq!(candi.two.end(), (100., 100.).into());

        // no candidate, the middle seg is a quad not a line
        assert!(ctx.possible_corner(2).is_none());

        let candi = ctx.possible_corner(3).unwrap();
        assert_eq!(candi.line(), Line::new((100., 100.), (10., 10.)));
        assert_eq!(candi.one.start(), (30., 30.).into());
        assert_eq!(candi.two.end(), (20., 20.).into());

        assert!(ctx.possible_corner(4).is_none());
    }

    #[test]
    fn test_empty_glyph() {
        let glyph = python_test_glyphs::space();
        let reveresed = glyph.reverse_subpaths();
        for g in [glyph, reveresed] {
            assert!(erase_open_corners(&g).is_none());
        }
    }

    #[test]
    fn test_corner_glyph() {
        let glyph = python_test_glyphs::hasCornerGlyph();
        let after = erase_open_corners(&glyph).unwrap();
        let new_pt = after.segments().nth(1).unwrap().end();

        assert_approx!(new_pt.x, 114.5417);
        assert_approx!(new_pt.y, 191.2080);
    }

    #[test]
    fn test_curve_curve_glyph() {
        let glyph = python_test_glyphs::curvyCornerGlyph();
        let after = erase_open_corners(&glyph);

        let new_pt = after.unwrap().segments().next().unwrap().start();
        // teaching to the test: because our segment splitting impl is different
        // from fonttools', we get slightly different values here; we might get
        // a few off-by-ones but that's okay?
        assert_approx!(new_pt.x, 406.4859);
        assert_approx!(new_pt.y, 104.5666);
    }

    #[test]
    fn test_double_corner_glyph() {
        let glyph = python_test_glyphs::doubleCornerGlyph();
        let after = erase_open_corners(&glyph).unwrap();
        let &[PathEl::MoveTo(one), PathEl::LineTo(two), PathEl::LineTo(tre), PathEl::LineTo(four), PathEl::LineTo(end), PathEl::ClosePath] =
            after.elements()
        else {
            panic!("wrong path elements: {:?}", after.elements())
        };
        assert_eq!(
            [one, two, tre, four],
            [
                Point::new(100., 0.,),
                Point::new(400., 0.,),
                Point::new(400., 400.,),
                Point::new(100., 400.,)
            ]
        );

        // sanity check
        assert_eq!(one, end);
        assert!(erase_open_corners(&glyph.reverse_subpaths()).is_none());
    }

    #[test]
    fn test_double_corner_glyph_wrap() {
        let glyph = python_test_glyphs::doubleCornerGlyphTrickyBitInMiddle();
        let after = erase_open_corners(&glyph).unwrap();
        let &[PathEl::MoveTo(one), PathEl::LineTo(two), PathEl::LineTo(tre), PathEl::LineTo(four), PathEl::LineTo(end), PathEl::ClosePath] =
            after.elements()
        else {
            panic!("wrong path elements: {:?}", after.elements())
        };
        assert_eq!(
            [one, two, tre, four],
            [
                Point::new(100., 400.,),
                Point::new(100., 0.,),
                Point::new(400., 0.,),
                Point::new(400., 400.,),
            ]
        );
        // sanity check
        assert_eq!(one, end);
    }

    #[test]
    fn test_curve_corner() {
        let glyph = python_test_glyphs::curveCorner();
        let after = erase_open_corners(&glyph).unwrap();
        let PathEl::CurveTo(pt, _, _) = after.elements()[3] else {
            panic!("nope")
        };
        assert_approx!(pt.x, 501.81019);
        assert_approx!(pt.y, 462.5782);

        assert!(erase_open_corners(&glyph.reverse_subpaths()).is_none());
    }

    #[test]
    fn test_circle_no_overlap() {
        let glyph = python_test_glyphs::dotabove_ar();
        assert!(erase_open_corners(&glyph).is_none())
    }

    #[test]
    fn test_self_loop() {
        let glyph = python_test_glyphs::sofiaSans();
        let after = erase_open_corners(&glyph).unwrap();
        assert!(after.elements().len() == glyph.elements().len() - 1);
    }

    #[test]
    fn large_crossing() {
        let glyph = python_test_glyphs::largeCrossing();
        let after = erase_open_corners(&glyph).unwrap();
        assert!(after.elements().len() == glyph.elements().len() - 1);
    }

    // https://github.com/linebender/kurbo/issues/411
    #[test]
    fn kurbo_411() {
        let candidate = PossibleCorner {
            first_el_idx: 0,
            one: CubicBez::new(
                (452.0, 240.0),
                (462.667, 78.667),
                (480.667, -146.333),
                (506.0, -435.0),
            )
            .into(),
            two: Line::new((385.0, 146.0), (438., 243.)).into(),
        };
        assert!(candidate.intersection().is_none());
    }

    /// a closed triangle with an open corner at each vertex
    #[test]
    fn joan_four_sc_ss10() {
        let mut path = BezPath::new();
        path.move_to((332.0, 155.0));
        path.line_to((317.0, 184.0));
        path.line_to((509.0, 184.0)); // real line
        path.line_to((493.0, 168.0));
        path.line_to((493.0, 412.0)); // real line
        path.line_to((514.0, 405.0));
        path.line_to((332.0, 155.0)); // real line
        path.close_path();

        let after = erase_open_corners(&path).unwrap();
        let pts = after
            .segments()
            .map(|seg| seg.start().ot_round())
            .collect::<Vec<_>>();

        assert_eq!(pts, [(353, 184), (493, 184), (493, 376)]);
    }

    #[test]
    fn closed_curved_triangle() {
        let mut path = BezPath::new();
        path.move_to((332.0, 155.0));
        path.line_to((317.0, 184.0));
        path.curve_to((317., 202.), (509., 206.), (509.0, 184.0)); // real line
        path.line_to((493.0, 168.0));
        path.curve_to((470., 168.), (501., 402.), (493.0, 412.0)); // real line
        path.line_to((514.0, 405.0));
        path.curve_to((505., 405.), (332., 238.), (332.0, 155.0)); // real line
        path.close_path();
        let after = erase_open_corners(&path).unwrap();
        let pts = after
            .segments()
            .map(|seg| seg.start().ot_round())
            .collect::<Vec<_>>();

        assert_eq!(pts, [(341, 194), (485, 195), (494, 389)]);
    }

    #[test]
    fn only_two_segments() {
        let mut path = BezPath::new();
        path.move_to((10., 10.));
        path.line_to((13., 10.));
        path.curve_to((11., 8.), (11., 8.), (10., 10.));
        path.close_path();

        assert!(erase_open_corners(&path).is_none());
    }

    // ensure that we match fonttools when intersection produces more than one
    // hit (in this case fonttools returns the hit with the lowest `t0`)
    #[test]
    fn seg_seg_intersect_order() {
        let _ = env_logger::builder().is_test(true).try_init();
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

    #[test]
    fn corner_with_t() {
        let _ = env_logger::builder().is_test(true).try_init();
        let mut path = BezPath::new();
        path.move_to((11.0, 4.0));
        path.line_to((17.0, 34.0));
        path.line_to((8.0, 29.0));
        path.line_to((7.0, 27.0));
        path.curve_to((7.0, 27.0), (6.0, 25.0), (6.0, 24.0));
        path.line_to((9.0, 25.0));
        path.line_to((7.0, 24.0));
        path.line_to((1.0, 24.0));
        path.line_to((11.0, 4.0));
        path.close_path();

        assert!(erase_open_corners(&path).is_none());
    }

    #[test]
    fn corner_with_t_2() {
        let _ = env_logger::builder().is_test(true).try_init();
        let mut path = BezPath::new();
        path.move_to((3.0, 155.0));
        path.line_to((14.0, 155.0));
        path.line_to((14.0, 0.0));
        path.line_to((80.0, 111.0));
        path.line_to((14.0, 111.0));
        path.line_to((3.0, 155.0));
        path.close_path();

        assert!(erase_open_corners(&path).is_some());
    }

    #[test]
    fn same_sidedness() {
        let origin = Point { x: 1.0, y: 1.0 };
        // both right
        assert!(origin
            .points_are_on_same_side(origin + Vec2::new(1.0, 1.0), origin + Vec2::new(1.0, -1.0)));
        // both left
        assert!(origin.points_are_on_same_side(
            origin + Vec2::new(-1.0, 1.0),
            origin + Vec2::new(-1.0, -1.0)
        ));
        //// both above
        assert!(origin
            .points_are_on_same_side(origin + Vec2::new(-1.0, 1.0), origin + Vec2::new(-1.0, 1.0)));

        // both below
        assert!(origin.points_are_on_same_side(
            origin + Vec2::new(-1.0, -1.0),
            origin + Vec2::new(1.0, -1.0)
        ));

        // one up-left and one down-right (fail)
        assert!(!origin
            .points_are_on_same_side(origin + Vec2::new(-1.0, 1.0), origin + Vec2::new(1.0, -1.0)));

        // zero doesn't count
        assert!(!origin
            .points_are_on_same_side(origin + Vec2::new(0.0, -1.0), origin + Vec2::new(0.0, 1.0)));
        assert!(!origin
            .points_are_on_same_side(origin + Vec2::new(-1.0, 0.0), origin + Vec2::new(1.0, 0.0)));
    }
}
