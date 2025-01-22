//! common utility for building a glyph, shared between backends

use kurbo::{BezPath, PathEl, Point};

use crate::error::PathConversionError;

/// Helps convert points-of-type to a bezier path.
///
/// Source formats tend to use streams of point-of-type. Curve manipulation is
/// often easier on bezier path, so provide a mechanism to convert.
/// While kurbo::BezPath can contain multiple subpaths, and this builder could be
/// used to convert multiple contours (i.e. list of points) into a single BezPath,
/// our GlyphInstance.contours is defined as a `Vec<BezPath>`, so frontends should
/// convert one contour at a time.
#[derive(Debug)]
pub struct GlyphPathBuilder {
    offcurve: Vec<Point>,
    leading_offcurve: Vec<Point>,
    path: Vec<PathEl>,
    first_oncurve: Option<OnCurve>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum OnCurve {
    Move(Point),
    Line(Point),
    Quad(Point),
    Cubic(Point),
}

impl OnCurve {
    fn point(&self) -> &Point {
        match self {
            OnCurve::Move(p) => p,
            OnCurve::Line(p) => p,
            OnCurve::Quad(p) => p,
            OnCurve::Cubic(p) => p,
        }
    }

    fn is_move(&self) -> bool {
        matches!(self, OnCurve::Move(_))
    }
}

impl GlyphPathBuilder {
    pub fn new(estimated_num_elements: usize) -> GlyphPathBuilder {
        let mut capacity = estimated_num_elements.next_power_of_two();
        if capacity == estimated_num_elements {
            capacity += 4; // close path often adds a few
        }
        GlyphPathBuilder {
            offcurve: Vec::with_capacity(2),
            leading_offcurve: Vec::new(),
            path: Vec::with_capacity(capacity),
            first_oncurve: None,
        }
    }

    fn check_num_offcurve(
        &self,
        expected: impl Fn(usize) -> bool,
    ) -> Result<(), PathConversionError> {
        if !expected(self.offcurve.len()) {
            return Err(PathConversionError::TooManyOffcurvePoints {
                num_offcurve: self.offcurve.len(),
                points: self.offcurve.clone(),
            });
        }
        Ok(())
    }

    fn is_empty(&self) -> bool {
        self.first_oncurve.is_none() && self.leading_offcurve.is_empty()
    }

    fn begin_path(&mut self, oncurve: OnCurve) -> Result<(), PathConversionError> {
        assert!(self.first_oncurve.is_none());
        self.path.push(PathEl::MoveTo(*oncurve.point()));
        self.first_oncurve = Some(oncurve);
        Ok(())
    }

    /// Lifts the "pen" to Point `p` and marks the beginning of an open contour.
    ///
    /// A point of this type can only be the first point in a contour.
    /// Cf. "move" in <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn move_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        if !self.is_empty() {
            return Err(PathConversionError::MoveAfterFirstPoint { point: p.into() });
        }
        self.begin_path(OnCurve::Move(p.into()))
    }

    /// Draws a line from the previous point to Point `p`.
    ///
    /// The previous point cannot be an off-curve point.
    /// If this is the first point in a contour, the contour is assumed to be closed,
    /// i.e. a cyclic list of points with no predominant start point.
    /// Cf. "line" in <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn line_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.check_num_offcurve(|v| v == 0)?;
        if self.first_oncurve.is_none() {
            self.begin_path(OnCurve::Line(p.into()))?;
        } else {
            self.path.push(PathEl::LineTo(p.into()));
        }
        Ok(())
    }

    /// Draws a quadratic curve/spline from the last non-offcurve point to Point `p`.
    ///
    /// This uses the TrueType "implied on-curve point" principle.
    /// The number of preceding off-curve points can be n >= 0. When n=0, a straight line is
    /// implied. If n=1, a single quadratic Bezier curve is drawn. If n>=2, a sequence of
    /// quadratic Bezier curves is drawn, with the implied on-curve points at the midpoints
    /// between pairs of successive off-curve points.
    /// If this is the first point in a contour, the contour is assumed to be closed.
    /// Cf. "qcurve" in <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn qcurve_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        // https://github.com/googlefonts/fontmake-rs/issues/110
        // Guard clauses: degenerate cases
        if self.first_oncurve.is_none() {
            return self.begin_path(OnCurve::Quad(p.into()));
        }
        if self.offcurve.is_empty() {
            return self.line_to(p);
        }

        // Insert an implied oncurve between every pair of offcurve points
        for window in self.offcurve.windows(2) {
            let curr = window[0];
            let next = window[1];
            // current offcurve to halfway to the next one
            let implied = Point::new((curr.x + next.x) / 2.0, (curr.y + next.y) / 2.0);
            self.path.push(PathEl::QuadTo(curr, implied));
        }
        // last but not least, the last offcurve to the provided point
        self.path
            .push(PathEl::QuadTo(*self.offcurve.last().unwrap(), p.into()));
        self.offcurve.clear();

        Ok(())
    }

    /// Draws a cubic curve from the previous non-offcurve point to Point `p`.
    ///
    /// Type of curve depends on the number of accumulated off-curves: 0 (straight line),
    /// 1 (quadratic Bezier) or 2 (cubic Bezier).
    /// If this is the first point in a contour, the contour is assumed to be closed.
    /// Cf. "curve" in <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn curve_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        if self.first_oncurve.is_some() {
            match self.offcurve.len() {
                0 => self.path.push(PathEl::LineTo(p.into())),
                1 => self.path.push(PathEl::QuadTo(self.offcurve[0], p.into())),
                2 => self.path.push(PathEl::CurveTo(
                    self.offcurve[0],
                    self.offcurve[1],
                    p.into(),
                )),
                _ => self.check_num_offcurve(|v| v < 3)?,
            }
            self.offcurve.clear();
        } else {
            self.begin_path(OnCurve::Cubic(p.into()))?;
        }
        Ok(())
    }

    /// Append off-curve point `p` to the following curve segment.
    ///
    /// The type of curve is defined by following on-curve point, which can be either a
    /// (cubic) "curve" or (quadratic) "qcurve".
    /// If offcurve is the first point in a contour, the contour is assumed to be closed.
    /// Cf. "offcurve" in <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn offcurve(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        if self.first_oncurve.is_some() {
            self.offcurve.push(p.into());
        } else {
            self.leading_offcurve.push(p.into());
        }
        Ok(())
    }

    /// Ends the current sub-path.
    ///
    /// It's called automatically by `build()` thus can be
    /// omitted when building one BezPath per contour, but can be called manually in
    /// order to build multiple contours into a single BezPath.
    fn end_path(&mut self) -> Result<(), PathConversionError> {
        // a contour that does *not* start with a move is assumed to be closed
        // https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types
        if !self.first_oncurve.is_some_and(|on| on.is_move()) {
            self.close_path()?;
        }

        self.check_num_offcurve(|v| v == 0)?;
        self.first_oncurve = None;
        Ok(())
    }

    fn close_path(&mut self) -> Result<(), PathConversionError> {
        // Flush any leading off-curves to the end. This matches fontTools' PointToSegmentPen
        // always starting/ending a closed contour on the first on-curve point:
        // https://github.com/fonttools/fonttools/blob/57fb47/Lib/fontTools/pens/pointPen.py#L147-L155
        if !self.leading_offcurve.is_empty() {
            self.offcurve.append(&mut self.leading_offcurve);
        }
        // Take dangling off-curves to imply a curve back to sub-path start.
        // For closed paths we explicitly output the implied closing line
        // equivalent to fontTools' PointToSegmentPen(outputImpliedClosingLine=True)
        if let Some(first_oncurve) = self.first_oncurve {
            match first_oncurve {
                OnCurve::Line(pt) => self.line_to(pt)?,
                OnCurve::Quad(pt) => self.qcurve_to(pt)?,
                OnCurve::Cubic(pt) => self.curve_to(pt)?,
                _ => unreachable!(),
            }
            self.path.push(PathEl::ClosePath);
        } else if !self.offcurve.is_empty() {
            // special TrueType oncurve-less quadratic contour, we assume the path
            // starts at midpoint between the first and last offcurves
            let first_offcurve = self.offcurve[0];
            let last_offcurve = *self.offcurve.last().unwrap();
            let implied_oncurve = first_offcurve.midpoint(last_offcurve);
            self.begin_path(OnCurve::Quad(implied_oncurve))?;
            self.close_path()?;
        }
        Ok(())
    }

    /// Builds the kurbo::BezPath from the accumulated points.
    pub fn build(mut self) -> Result<BezPath, PathConversionError> {
        self.end_path()?;
        Ok(BezPath::from_vec(self.path))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_qcurve_with_no_offcurve_is_a_line_open_contour() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.move_to((2.0, 2.0)).unwrap(); // open contour
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_qcurve_with_no_offcurve_is_a_line_closed_contour() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.qcurve_to((2.0, 2.0)).unwrap(); // closed, ie not starting with 'move'
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_curve_with_no_offcurve_is_a_line_open_contour() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.move_to((2.0, 2.0)).unwrap(); // open contour
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_curve_with_no_offcurve_is_a_line_closed_contour() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.curve_to((2.0, 2.0)).unwrap(); // closed
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_curve_with_one_offcurve_is_a_single_quad_open_contour() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.move_to((2.0, 2.0)).unwrap(); // open
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_curve_with_one_offcurve_is_a_single_quad_closed_contour() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.curve_to((2.0, 2.0)).unwrap(); // closed
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_qcurve_with_one_offcurve_is_a_single_quad_to_open_contour() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_qcurve_with_one_offcurve_is_a_single_quad_to_closed_contour() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.qcurve_to((2.0, 2.0)).unwrap(); // closed
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_qcurve_with_two_offcurve_is_two_quad_to_open_contour() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((5.0, 4.0)).unwrap();
        builder.qcurve_to((6.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2 Q5,4 6,2", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_qcurve_with_two_offcurve_is_two_quad_to_closed_contour() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.qcurve_to((2.0, 2.0)).unwrap(); // closed
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((5.0, 4.0)).unwrap();
        builder.qcurve_to((6.0, 2.0)).unwrap();
        assert_eq!(
            "M2,2 Q3,0 4,2 Q5,4 6,2 L2,2 Z",
            builder.build().unwrap().to_svg()
        );
    }

    #[test]
    fn last_line_always_emits_implied_closing_line() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.line_to((2.0, 2.0)).unwrap();
        builder.line_to((4.0, 2.0)).unwrap();
        // a closing line is implied by Z, but emit it nonetheless
        assert_eq!("M2,2 L4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn last_line_emits_nop_implied_closing_line() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.line_to((2.0, 2.0)).unwrap();
        builder.line_to((4.0, 2.0)).unwrap();
        // duplicate last point, not to be confused with the closing line implied by Z
        builder.line_to((2.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2 L2,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn last_quad_equals_move_no_closing_line() {
        // if last curve point is equal to move, there's no need to disambiguate it from
        // the implicit closing line, so we don't emit one
        let mut builder = GlyphPathBuilder::new(0);
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((2.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn last_cubic_equals_move_no_closing_line() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((0.0, 3.0)).unwrap();
        builder.curve_to((2.0, 2.0)).unwrap();
        assert_eq!("M2,2 C3,0 0,3 2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn last_quad_not_equal_move_do_emit_closing_line() {
        // if last point is different from move, then emit the implied closing line
        let mut builder = GlyphPathBuilder::new(0);
        builder.line_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn last_cubic_not_equal_move_do_emit_closing_line() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.line_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((0.0, 3.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!(
            "M2,2 C3,0 0,3 4,2 L2,2 Z",
            builder.build().unwrap().to_svg()
        );
    }

    #[test]
    fn start_on_first_oncurve_irrespective_of_offcurves() {
        // the following three closed contours are all equivalent and get normalized
        // to the same path, which begins/ends on the first on-curve point i.e. (2,2).
        let expected = "M2,2 C6,0 0,6 4,2 C3,0 0,3 2,2 Z";

        let mut builder = GlyphPathBuilder::new(0);
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((0.0, 3.0)).unwrap();
        builder.curve_to((2.0, 2.0)).unwrap();
        builder.offcurve((6.0, 0.0)).unwrap();
        builder.offcurve((0.0, 6.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!(expected, builder.build().unwrap().to_svg());

        let mut builder = GlyphPathBuilder::new(0);
        builder.offcurve((0.0, 3.0)).unwrap();
        builder.curve_to((2.0, 2.0)).unwrap();
        builder.offcurve((6.0, 0.0)).unwrap();
        builder.offcurve((0.0, 6.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        assert_eq!(expected, builder.build().unwrap().to_svg());

        let mut builder = GlyphPathBuilder::new(0);
        builder.curve_to((2.0, 2.0)).unwrap();
        builder.offcurve((6.0, 0.0)).unwrap();
        builder.offcurve((0.0, 6.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((0.0, 3.0)).unwrap();
        assert_eq!(expected, builder.build().unwrap().to_svg());
    }

    #[test]
    fn closed_quadratic_contour_without_oncurve_points() {
        let mut builder = GlyphPathBuilder::new(0);
        // builder.qcurve_to((0.0, 1.0)).unwrap();  // implied
        builder.offcurve((1.0, 1.0)).unwrap();
        builder.offcurve((1.0, -1.0)).unwrap();
        builder.offcurve((-1.0, -1.0)).unwrap();
        builder.offcurve((-1.0, 1.0)).unwrap();
        assert_eq!(
            "M0,1 Q1,1 1,0 Q1,-1 0,-1 Q-1,-1 -1,0 Q-1,1 0,1 Z",
            builder.build().unwrap().to_svg()
        );
    }

    #[test]
    fn invalid_move_after_first_point() {
        // A point of type 'move' must be the first point in an (open) contour.
        let mut builder = GlyphPathBuilder::new(0);
        builder.move_to((2.0, 2.0)).unwrap();
        builder.end_path().unwrap();
        // move_to after ending the current subpath is OK
        builder.move_to((3.0, 3.0)).unwrap();
        // but it's an error if we try to do move_to again
        let result = builder.move_to((4.0, 4.0));

        assert_eq!(
            result,
            Err(PathConversionError::MoveAfterFirstPoint {
                point: (4.0, 4.0).into()
            })
        );

        builder.end_path().unwrap();
        builder.line_to((5.0, 5.0)).unwrap();
        // can't move_to in the middle of a closed (not starting with move_to) subpath
        let result = builder.move_to((6.0, 6.0));

        assert_eq!(
            result,
            Err(PathConversionError::MoveAfterFirstPoint {
                point: (6.0, 6.0).into()
            })
        );

        builder.end_path().unwrap();
        builder.offcurve((7.0, 7.0)).unwrap();
        // can't move_to after an offcurve point
        let result = builder.move_to((8.0, 8.0));

        assert_eq!(
            result,
            Err(PathConversionError::MoveAfterFirstPoint {
                point: (8.0, 8.0).into()
            })
        );
    }

    #[test]
    fn closed_path_with_trailing_cubic_offcurves() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.curve_to((10.0, 0.0)).unwrap();
        builder.line_to((0.0, 10.0)).unwrap();
        builder.offcurve((5.0, 10.0)).unwrap();
        builder.offcurve((10.0, 5.0)).unwrap();

        let path = builder.build().unwrap();

        assert_eq!("M10,0 L0,10 C5,10 10,5 10,0 Z", path.to_svg());
    }

    #[test]
    fn closed_path_with_trailing_quadratic_offcurves() {
        let mut builder = GlyphPathBuilder::new(0);
        builder.qcurve_to((10.0, 0.0)).unwrap();
        builder.line_to((0.0, 10.0)).unwrap();
        builder.offcurve((5.0, 10.0)).unwrap();
        builder.offcurve((10.0, 5.0)).unwrap();

        let path = builder.build().unwrap();

        assert_eq!("M10,0 L0,10 Q5,10 7.5,7.5 Q10,5 10,0 Z", path.to_svg());
    }
}
