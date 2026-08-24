use kurbo::BezPath;
use linesweeper::{BinaryOp, FillRule};

/// Combines one or more [`BezPath`]s into a single one by concatenating their elements.
pub fn combine_paths<'a>(bez_paths: impl IntoIterator<Item = &'a BezPath>) -> BezPath {
    bez_paths.into_iter().flat_map(|path| path.iter()).collect()
}

/// Removes overlaps from a single [`BezPath`] using the given [`FillRule`].
///
/// If you have multiple beziers, you will need to [combine them](combine_paths) first.
pub fn remove_overlaps(
    bez_path: &BezPath,
    fill_rule: FillRule,
) -> Result<Vec<BezPath>, linesweeper::Error> {
    // TODO: if linesweeper gave us an owned iterator, we could just have this method return a
    //       Contour iterator instead of a Vec<BezPath>, and then re-use it in has_overlaps without
    //       incurring allocation overhead
    let beziers = linesweeper::binary_op(bez_path, &BezPath::new(), fill_rule, BinaryOp::Union)?
        .contours()
        .cloned()
        .map(|contour| contour.path)
        .collect::<Vec<_>>();
    Ok(beziers)
}

/// Checks if the given [`BezPath`] overlaps itself.
///
/// This is done by removing overlaps with [non-zero](FillRule::NonZero) and
/// [even-odd](FillRule::EvenOdd) fill rules and seeing if the results are different, as these two
/// algorithms handle overlapping areas differently.
///
/// If you have multiple beziers, you will need to [combine them](combine_paths) first.
pub fn has_overlaps(bez_path: &BezPath) -> Result<bool, linesweeper::Error> {
    let non_zero_beziers = linesweeper::binary_op(
        bez_path,
        &BezPath::new(),
        FillRule::NonZero,
        BinaryOp::Union,
    )?;
    let even_odd_beziers = linesweeper::binary_op(
        bez_path,
        &BezPath::new(),
        FillRule::EvenOdd,
        BinaryOp::Union,
    )?;

    // TODO: Could Contours implement PartialEq?
    //       Ok(non_zero_beziers != even_odd_beziers)
    let has_overlaps = Iterator::ne(
        non_zero_beziers.contours().map(|nzc| &nzc.path),
        even_odd_beziers.contours().map(|eoc| &eoc.path),
    );
    Ok(has_overlaps)
}

#[cfg(test)]
mod tests {
    #![expect(clippy::indexing_slicing, clippy::expect_used)]
    use super::*;
    use kurbo::{PathEl, Point};

    fn square(x0: f64, y0: f64, x1: f64, y1: f64) -> BezPath {
        BezPath::from_vec(vec![
            PathEl::MoveTo(Point::new(x0, y0)),
            PathEl::LineTo(Point::new(x1, y0)),
            PathEl::LineTo(Point::new(x1, y1)),
            PathEl::LineTo(Point::new(x0, y1)),
            PathEl::LineTo(Point::new(x0, y0)),
            PathEl::ClosePath,
        ])
    }

    fn overlapping_squares() -> (BezPath, BezPath) {
        // Artist's rendition:
        //      ┌────────┐
        //      │        │
        //  ┌───┼────┐   │
        //  │ A │    │ B │
        //  │   └────┼───┘
        //  │        │
        //  └────────┘
        (square(0.0, 0.0, 10.0, 10.0), square(5.0, 5.0, 15.0, 15.0))
    }

    #[test]
    fn combine_combines() {
        let full_path = vec![
            PathEl::MoveTo(Point::new(5., 5.)),
            PathEl::LineTo(Point::new(15.0, 15.0)),
            PathEl::MoveTo(Point::new(10., 10.)),
            PathEl::LineTo(Point::new(15.0, 15.0)),
        ];
        let a = BezPath::from_iter(full_path[..2].iter().copied());
        let b = BezPath::from_iter(full_path[2..].iter().copied());

        let ab = combine_paths(&[a, b]);
        assert_eq!(ab, BezPath::from_vec(full_path));
    }

    #[test]
    fn merges_paths() {
        let (square_a, square_b) = overlapping_squares();
        let combined = combine_paths([&square_a, &square_b]);

        let beziers = remove_overlaps(&combined, FillRule::NonZero)
            .expect("linesweeper should remove overlaps");
        let [bezier] = beziers.as_slice() else {
            panic!(
                "removing overlaps from two squares with non-zero fill should produce a single BezPath"
            );
        };

        // Artist's rendition:
        //      ┌────────┐
        //      │        │
        //  ┌───┘        │
        //  │   A <3 B   │
        //  │        ┌───┘
        //  │        │
        //  └────────┘
        let expected = BezPath::from_vec(vec![
            PathEl::MoveTo(Point::new(0.0, 10.0)),
            PathEl::LineTo(Point::new(0.0, 0.0)),
            PathEl::LineTo(Point::new(10.0, 0.0)),
            PathEl::LineTo(Point::new(10.0, 5.0)),
            PathEl::LineTo(Point::new(15.0, 5.0)),
            PathEl::LineTo(Point::new(15.0, 15.0)),
            PathEl::LineTo(Point::new(5.0, 15.0)),
            PathEl::LineTo(Point::new(5.0, 10.0)),
            PathEl::LineTo(Point::new(0.0, 10.0)),
            PathEl::ClosePath,
        ]);

        assert_eq!(bezier, &expected);
    }

    #[test]
    fn detects_overlaps() {
        let (square_a, square_b) = overlapping_squares();
        let combined = combine_paths([&square_a, &square_b]);

        let res = has_overlaps(&combined).expect("linesweeper should detect overlaps");
        assert!(res, "overlaps should have been found");
    }

    #[test]
    fn no_overlaps_in_disjoint_squares() {
        //  ┌────────┐          ┌────────┐
        //  │   A    │          │   B    │
        //  └────────┘          └────────┘
        let square_a = square(0.0, 0.0, 10.0, 10.0);
        let far_square = square(20.0, 20.0, 30.0, 30.0);
        let combined = combine_paths([&square_a, &far_square]);

        let res = has_overlaps(&combined).expect("linesweeper should not error");
        assert!(!res, "disjoint contours are not overlapping");
    }

    #[test]
    fn no_overlaps_in_square_with_counter() {
        // 'O'-style glyph: outer contour with an opposite-winding inner counter
        //  ┌──────────────┐
        //  │    outer     │
        //  │  ┌────────┐  │
        //  │  │counter │  │
        //  │  └────────┘  │
        //  └──────────────┘
        let outer = square(0.0, 0.0, 10.0, 10.0);
        let counter = square(2.0, 2.0, 8.0, 8.0).reverse_subpaths();
        let combined = combine_paths([&outer, &counter]);

        let res = has_overlaps(&combined).expect("linesweeper should not error");
        assert!(!res, "a counter is not an overlap");
    }

    #[test]
    fn detects_duplicate_contours() {
        // identical contour twice: even-odd cancels them out, non-zero keeps one,
        // so the two results differ in length
        let square_a = square(0.0, 0.0, 10.0, 10.0);
        let combined = combine_paths([&square_a, &square_a]);

        let res = has_overlaps(&combined).expect("linesweeper should not error");
        assert!(res, "duplicate contours are overlapping");
    }
}
