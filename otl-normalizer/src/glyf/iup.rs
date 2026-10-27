//! Interpolation of untouched points, a port of fontTools' `varLib.iup.iup_delta`

pub(crate) type Coord = (i32, i32);
pub(crate) type Delta = (f64, f64);

/// Fill in the deltas omitted from a tuple by interpolating between the
/// explicit ones, contour by contour.
///
/// `ends` are the inclusive end indices of each contour; the four phantom
/// points following the last contour are each treated as their own contour.
pub(crate) fn iup_delta(deltas: &[Option<Coord>], coords: &[Coord], ends: &[usize]) -> Vec<Delta> {
    let n = coords.len();
    let mut out = Vec::with_capacity(n);
    let mut start = 0;
    for end in ends.iter().copied().chain(n - 4..n) {
        let end = end + 1;
        iup_contour(&deltas[start..end], &coords[start..end], &mut out);
        start = end;
    }
    out
}

fn iup_contour(deltas: &[Option<Coord>], coords: &[Coord], out: &mut Vec<Delta>) {
    let n = deltas.len();
    let indices: Vec<usize> = (0..n).filter(|i| deltas[*i].is_some()).collect();
    if indices.len() == n {
        out.extend(deltas.iter().map(|d| to_delta(d.unwrap())));
        return;
    }
    let (Some(&first), Some(&last)) = (indices.first(), indices.last()) else {
        out.extend(std::iter::repeat_n((0.0, 0.0), n));
        return;
    };
    let explicit = |i: usize| deltas[i].unwrap();
    if first != 0 {
        iup_segment(
            &coords[..first],
            coords[first],
            explicit(first),
            coords[last],
            explicit(last),
            out,
        );
    }
    out.push(to_delta(explicit(first)));
    let mut start = first;
    for &end in &indices[1..] {
        if end - start > 1 {
            iup_segment(
                &coords[start + 1..end],
                coords[start],
                explicit(start),
                coords[end],
                explicit(end),
                out,
            );
        }
        out.push(to_delta(explicit(end)));
        start = end;
    }
    if start != n - 1 {
        iup_segment(
            &coords[start + 1..n],
            coords[start],
            explicit(start),
            coords[first],
            explicit(first),
            out,
        );
    }
}

fn iup_segment(
    coords: &[Coord],
    rc1: Coord,
    rd1: Coord,
    rc2: Coord,
    rd2: Coord,
    out: &mut Vec<Delta>,
) {
    let xs = iup_axis(coords.iter().map(|c| c.0), rc1.0, rd1.0, rc2.0, rd2.0);
    let ys = iup_axis(coords.iter().map(|c| c.1), rc1.1, rd1.1, rc2.1, rd2.1);
    out.extend(xs.into_iter().zip(ys));
}

fn iup_axis(coords: impl Iterator<Item = i32>, x1: i32, d1: i32, x2: i32, d2: i32) -> Vec<f64> {
    if x1 == x2 {
        let value = if d1 == d2 { d1 as f64 } else { 0.0 };
        return coords.map(|_| value).collect();
    }
    let (x1, x2, d1, d2) = if x1 > x2 {
        (x2, x1, d2, d1)
    } else {
        (x1, x2, d1, d2)
    };
    let scale = (d2 - d1) as f64 / (x2 - x1) as f64;
    coords
        .map(|x| {
            if x <= x1 {
                d1 as f64
            } else if x >= x2 {
                d2 as f64
            } else {
                d1 as f64 + (x - x1) as f64 * scale
            }
        })
        .collect()
}

fn to_delta(coord: Coord) -> Delta {
    (coord.0 as f64, coord.1 as f64)
}

/// fontTools' `otRound`
pub(crate) fn ot_round(value: f64) -> i32 {
    (value + 0.5).floor() as i32
}

#[cfg(test)]
mod tests {
    use super::*;

    fn round(deltas: &[Delta]) -> Vec<Coord> {
        deltas
            .iter()
            .map(|(x, y)| (ot_round(*x), ot_round(*y)))
            .collect()
    }

    #[test]
    fn dense_is_unchanged() {
        let deltas = [
            Some((1, 2)),
            Some((3, 4)),
            Some((0, 0)),
            Some((0, 0)),
            Some((0, 0)),
            Some((0, 0)),
        ];
        let coords = [(0, 0), (10, 10), (0, 0), (0, 0), (0, 0), (0, 0)];
        let out = iup_delta(&deltas, &coords, &[1]);
        assert_eq!(
            round(&out),
            [(1, 2), (3, 4), (0, 0), (0, 0), (0, 0), (0, 0)]
        );
    }

    #[test]
    fn missing_phantoms_are_zero() {
        let deltas = [Some((1, 2)), None, None, None, None];
        let coords = [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0)];
        let out = iup_delta(&deltas, &coords, &[0]);
        assert_eq!(round(&out), [(1, 2), (0, 0), (0, 0), (0, 0), (0, 0)]);
    }

    #[test]
    fn interpolates_between_references() {
        let coords = [
            (0, 0),
            (100, 0),
            (100, 100),
            (0, 100),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
        ];
        let deltas = [
            Some((0, 0)),
            None,
            Some((10, 20)),
            None,
            None,
            None,
            None,
            None,
        ];
        let out = iup_delta(&deltas, &coords, &[3]);
        assert_eq!(
            round(&out),
            [
                (0, 0),
                (10, 0),
                (10, 20),
                (0, 20),
                (0, 0),
                (0, 0),
                (0, 0),
                (0, 0)
            ]
        );
    }

    #[test]
    fn rounds_like_fonttools() {
        let coords = [(0, 0), (50, 50), (100, 100), (0, 0), (0, 0), (0, 0), (0, 0)];
        let deltas = [Some((0, 0)), None, Some((3, -3)), None, None, None, None];
        let out = iup_delta(&deltas, &coords, &[2]);
        assert_eq!(round(&out)[1], (2, -1));
        assert_eq!(ot_round(-1.5), -1);
        assert_eq!(ot_round(1.5), 2);
        assert_eq!(ot_round(-2.5), -2);
    }

    #[test]
    fn wraps_around_contour() {
        let coords = [
            (0, 0),
            (10, 0),
            (20, 0),
            (30, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
        ];
        let deltas = [
            None,
            Some((10, 0)),
            None,
            Some((40, 0)),
            None,
            None,
            None,
            None,
        ];
        let out = iup_delta(&deltas, &coords, &[3]);
        assert_eq!(round(&out)[..4], [(10, 0), (10, 0), (25, 0), (40, 0)]);
    }
}
