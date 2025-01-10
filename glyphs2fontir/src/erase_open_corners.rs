//! removing 'open corners' from glyphs
//!
//! see <https://github.com/googlefonts/glyphsLib/blob/74c63244fdb/Lib/glyphsLib/filters/eraseOpenCorners.py>

use std::ops::Range;

use kurbo::{BezPath, Line, ParamCurve, PathEl, PathSeg, Point, Shape};
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
    // first: this is generally a nop, so let's not actually mutate anything
    // unless we have to; instead we will accumulate mutations and apply
    // them at the end.
    let mut todo = Vec::new();

    let is_closed = matches!(path.elements().last(), Some(PathEl::ClosePath));
    // we could write this using iterators but I think it's cleaner this way
    for candidate in iter_erasure_candidates(path.elements()) {
        if !candidate.points_are_right_of_line() {
            continue;
        }
        let Some(Intersection { t0, t1, .. }) = candidate.intersection() else {
            continue;
        };
        // invert value of t0 so for both values '0' means at the open corner
        // <https://github.com/googlefonts/glyphsLib/blob/74c63244fdbef1da5/Lib/glyphsLib/filters/eraseOpenCorners.py#L105>
        let t0_inv = 1.0 - t0;
        if ((t0_inv < 0.5 && t1 < 0.5) || t0_inv < 0.3 || t1 < 0.3)
            && t1 > 0.0001
            && t0_inv > 0.0001
        {
            let ix = candidate.ix;
            todo.extend([
                (ix, SegmentOp::Replace(candidate.prev.subsegment(0.0..t0))),
                (ix + 1, SegmentOp::Delete),
                (
                    ix + 2,
                    SegmentOp::Replace(candidate.next.subsegment(t1..1.0)),
                ),
            ]);
        }
    }
    if todo.is_empty() {
        return None;
    }

    // it will be simpler to operate on segments:
    let segments = kurbo::segments(path.elements().iter().copied()).collect::<Vec<_>>();
    // and then we will make an equal number of ops as segments, also to simplify:
    let mut ops = vec![SegmentOp::Retain; segments.len()];
    for (ix, op) in todo {
        // ix isn't always right because we didn't know the number of segments
        // before, and we handle the first two points at the end
        ops[ix % segments.len()] = op;
    }

    let segs = segments
        .iter()
        .zip(ops)
        .filter_map(|(seg, op)| op.apply(*seg))
        .collect::<Vec<_>>();

    let mut els = Vec::with_capacity(path.elements().len());

    // now handle the move_to; for a closed path this is the last point,
    // for an open path it was the start point of the first segment (which is
    // ignored when converting segment->element)
    let first_pt = if is_closed {
        // unwrap is fine, we must have at least two segments if we had an open corner
        segs.last().unwrap().end()
    } else {
        segs.first().unwrap().start()
    };

    els.push(PathEl::MoveTo(first_pt));
    els.extend(segs.iter().map(PathSeg::as_path_el));
    if is_closed {
        els.push(PathEl::ClosePath);
    }
    Some(BezPath::from_vec(els))
}

#[derive(Clone, Debug)]
enum SegmentOp {
    Retain,
    Delete,
    Replace(PathSeg),
}

impl SegmentOp {
    fn apply(self, seg: PathSeg) -> Option<PathSeg> {
        match self {
            SegmentOp::Retain => Some(seg),
            SegmentOp::Delete => None,
            SegmentOp::Replace(seg) => Some(seg),
        }
    }
}

/// A potentially open corner.
///
/// This is any line segment, along with the preceding and following path segments.
struct ErasureCandidate {
    // index of the *first* participating segment, i.e 'prev'.
    ix: usize,
    seg: Line,
    prev: PathSeg,
    next: PathSeg,
}

impl ErasureCandidate {
    // Are the incoming point from the previous segment and the outgoing point
    // from the next segment both on the right side of the line?
    // (see discussion at https://github.com/googlefonts/glyphsLib/pull/663)
    // https://github.com/googlefonts/glyphsLib/blob/74c63244fdb/Lib/glyphsLib/filters/eraseOpenCorners.py#L66-L71
    fn points_are_right_of_line(&self) -> bool {
        // return the last point in this segment not shared with the next segment
        // (i.e., ignoring the final point)
        fn last_non_shared_point(seg: PathSeg) -> Point {
            match seg {
                PathSeg::Line(line) => line.p0,
                PathSeg::Quad(quad_bez) => quad_bez.p1,
                PathSeg::Cubic(cubic_bez) => cubic_bez.p2,
            }
        }

        // return the first point in this segment not shared with the previous
        // segment (i.e, ignoring the first point)
        fn first_non_shared_point(seg: PathSeg) -> Point {
            match seg {
                PathSeg::Line(line) => line.p1,
                PathSeg::Quad(quad_bez) => quad_bez.p1,
                PathSeg::Cubic(cubic_bez) => cubic_bez.p1,
            }
        }
        let prev_point = last_non_shared_point(self.prev);
        let next_point = first_non_shared_point(self.next);
        !(point_is_left_of_line(self.seg, prev_point)
            || point_is_left_of_line(self.seg, next_point))
    }

    /// The intersection of the two path segments
    ///
    /// If this is an open corner, the intersection point will be the new corner.
    fn intersection(&self) -> Option<Intersection> {
        let candidate = seg_seg_intersection(self.prev, self.next)?;
        // there is a bug in kurbo that can cause it to report spurious intersections
        // (see https://github.com/linebender/kurbo/issues/411).
        // as a temporary workaround here we check that the point of intersection
        // on each segment are relatively close to one another, and discard
        // if not.
        let p1 = self.prev.eval(candidate.t0);
        let p2 = self.next.eval(candidate.t1);
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

//https://github.com/googlefonts/glyphsLib/blob/74c63244fdbe/Lib/glyphsLib/filters/eraseOpenCorners.py#L14
// 'left' from the perspective of an observer standing on line.p0 and lookign at line.p1?
fn point_is_left_of_line(line: Line, point: Point) -> bool {
    let Line { p0: a, p1: b } = line;
    (b.x - a.x) * (point.y - a.y) - (b.y - a.y) * (point.x - a.x) >= 0.0
}

/// given a slice of path elements, determine which *segments* might be open corners.
fn iter_erasure_candidates(els: &[PathEl]) -> impl Iterator<Item = ErasureCandidate> + use<'_> {
    iter_segment_triplets(els)
        .enumerate()
        .filter_map(|(ix, (prev, seg, next))| {
            if let PathSeg::Line(seg) = seg {
                Some(ErasureCandidate {
                    ix,
                    seg,
                    prev,
                    next,
                })
            } else {
                None
            }
        })
}

/// Iterate over all the length-three windows of segments.
///
/// This will loop once, so that the last segment begins the last window, e.g.
///
/// For the segments `s0, s1, s2, s3` this will produce
/// `(s0, s1, s2), (s1, s2, s3), (s2, s3, s0), and (s3, s0, s1)`
fn iter_segment_triplets(
    els: &[PathEl],
) -> impl Iterator<Item = (PathSeg, PathSeg, PathSeg)> + use<'_> {
    const EMPTY_SEG: PathSeg = PathSeg::Line(Line {
        p0: Point::ZERO,
        p1: Point::ZERO,
    });

    let is_closed = matches!(els.last(), Some(PathEl::ClosePath));

    // if the path is closed, we want to loop around to consider the first two segments
    // following the last.
    let n_loop_around = if is_closed { 2 } else { 0 };
    let mut iter = kurbo::segments(els.iter().copied())
        .chain(kurbo::segments(els.iter().copied()).take(n_loop_around))
        .peekable();

    // if this is none we will end iteration on the first call to `next` anyway,
    // and the code is simpler without this being an option
    let mut prev = iter.next().unwrap_or(EMPTY_SEG);
    std::iter::from_fn(move || {
        let seg = iter.next()?;
        let next = iter.peek()?;
        let result = (prev, seg, *next);
        prev = seg;
        Some(result)
    })
}

#[derive(Clone, Debug)]
struct Intersection {
    // location of hit on first segment, in range 0..=1
    t0: f64,
    // location on second segment
    t1: f64,
    point: Point,
}

/// Find an intersection of two segments, if any exist
///
/// It is possible for segments to intersect multiple times; in this case we
/// will return the segment nearest to the end of `seg1``
fn seg_seg_intersection(seg1: PathSeg, seg2: PathSeg) -> Option<Intersection> {
    match (seg1, seg2) {
        (PathSeg::Line(line), seg) => seg
            .intersect_line(line)
            .iter()
            .min_by_key(|hit| OrderedFloat(hit.segment_t))
            .map(|hit| Intersection {
                t0: hit.line_t,
                t1: hit.segment_t,
                point: line.eval(hit.line_t),
            }),
        (seg, PathSeg::Line(line)) => seg
            .intersect_line(line)
            .iter()
            .min_by_key(|hit| OrderedFloat(hit.line_t))
            .map(|hit| Intersection {
                t0: hit.segment_t,
                t1: hit.line_t,
                point: line.eval(hit.line_t),
            }),
        (bez0, bez1) => curve_curve_intersection_py(bez0, bez1),
    }
}

// https://github.com/fonttools/fonttools/blob/cb159dea72/Lib/fontTools/misc/bezierTools.py#L1307
const PY_ACCURACY: f64 = 1e-3;

// based on impl after impl in fonttools/bezierTools; we split it in two,
// with the recursive bit below, and this as a little wrapper.
//https://github.com/fonttools/fonttools/blob/cb159dea72703/Lib/fontTools/misc/bezierTools.py#L1306
fn curve_curve_intersection_py(seg1: PathSeg, seg2: PathSeg) -> Option<Intersection> {
    let mut result = Vec::new();
    curve_curve_py_impl(seg1, seg2, &(0.0..1.0), &(0.0..1.0), &mut result);
    result.sort_by_key(|hit| (OrderedFloat(hit.t0), OrderedFloat(hit.t1)));
    result.dedup_by_key(|hit| ((hit.t0 / PY_ACCURACY) as i64, (hit.t1 / PY_ACCURACY) as i64));
    result.first().cloned().map(|mut hit| {
        // set the point now, by eval'ing the whole input segment.
        hit.point = seg1.eval(hit.t0);
        hit
    })
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
            // we use a dummy point now, we will set the correct point at base
            // when we have the original segment
            point: Point::ZERO,
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

    use kurbo::CubicBez;

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
        let candidate = ErasureCandidate {
            ix: 0,
            seg: Line::new((385.0, 146.0), (438.0, 243.0)),
            prev: CubicBez::new(
                (452.0, 240.0),
                (462.667, 78.667),
                (480.667, -146.333),
                (506.0, -435.0),
            )
            .into(),
            next: Line::new((385.0, 146.0), (438.0, 243.0)).into(),
        };
        assert!(candidate.intersection().is_none());
    }
}
