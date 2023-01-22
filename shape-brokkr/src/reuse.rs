//! Functions to help find opportunities to reuse shapes.
//!
//! We assume the provided path is valid, in particular if it contains any
//! path elements it starts with a move.

use kurbo::{Affine, BezPath, PathEl, Point, Vec2};

use crate::Error;

// Shift the path such that it begins at 0,0
fn move_to_origin(path: &BezPath) -> Result<BezPath, Error> {
    if path.elements().is_empty() {
        return Ok(path.clone());
    }

    let Some(PathEl::MoveTo(first_move)) = path.elements().first() else {
        return Err(Error::InvalidPath(path.clone(), "Does not start with MoveTo".to_string()));
    };
    let shift = Vec2::new(-first_move.x, -first_move.y);
    let transform = Affine::translate(shift);
    let mut path = path.clone();
    path.apply_affine(transform);
    Ok(path)
}

/// If we thought of the path as a series of vectors to the endpoints of each successive
/// drawing command what would it look like?
fn vectors(path: &BezPath) -> Vec<Vec2> {
    // BezPath deals in absolutes and may not start at 0

    let mut vecs = Vec::new();
    let mut last_move = Point::ZERO; // path must start with a move
    let mut curr_pos = Point::ZERO;

    for el in path.elements().iter() {
        match el {
            PathEl::MoveTo(p) => {
                last_move = *p;
                curr_pos = *p;
            }
            PathEl::LineTo(p) | PathEl::QuadTo(_, p) | PathEl::CurveTo(_, _, p) => {
                vecs.push(*p - curr_pos);
                curr_pos = *p;
            }
            PathEl::ClosePath => {
                vecs.push(last_move - curr_pos);
                curr_pos = last_move;
            }
        }
    }

    vecs
}

#[cfg(test)]
mod tests {
    use kurbo::{BezPath, Vec2};

    use crate::reuse::vectors;

    use super::move_to_origin;

    fn sample_triangle() -> BezPath {
        BezPath::from_svg("M5,5 L10,0 L10,10 Z").unwrap()
    }

    #[test]
    fn simple_move_to_origin() {
        let original = sample_triangle();
        assert_eq!(
            "M0 0L5 -5L5 5Z",
            move_to_origin(&original).unwrap().to_svg()
        );
    }

    #[test]
    fn vecs_ing_triangle() {
        assert_eq!(
            vec![
                Vec2::new(5.0, -5.0),
                Vec2::new(0.0, 10.0),
                Vec2::new(-5.0, -5.0),
            ],
            vectors(&sample_triangle())
        );
    }
}
