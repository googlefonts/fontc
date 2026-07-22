use kurbo::BezPath;
use linesweeper::{BinaryOp, FillRule};

pub fn combine_paths<'a>(bez_paths: impl IntoIterator<Item = &'a BezPath>) -> BezPath {
    bez_paths.into_iter().flat_map(|path| path.iter()).collect()
}

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
