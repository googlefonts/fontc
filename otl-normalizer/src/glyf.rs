//! Normalizing the glyf and gvar tables

use std::{fmt::Write as _, io};

use fontdrasil::types::GlyphName;
use write_fonts::read::{
    FontRef, ReadError, TableProvider,
    tables::{
        glyf::{
            Anchor, CompositeGlyph, CompositeGlyphFlags, Glyf, Glyph, SimpleGlyph, SimpleGlyphFlags,
        },
        loca::{Loca, LocaGlyph},
    },
    types::{F2Dot14, GlyphId, GlyphId16, Tag},
};

use crate::{Error, NameMap};

mod iup;

use iup::Coord;

/// Print the outline of every glyph in glyph name order
///
/// Each glyph's block has its bounding box, then its contours (one point per
/// line) or its components, then any instructions as hex. For example:
///
/// ```text
/// glyph exclamdown
///   bbox 170 -388 385 1001
///   component exclam 555 987 scale -1 flags 0x4
/// glyph underscore
///   bbox -8 -286 1028 -163
///   contour
///     -2 -286 on
///     -8 -259 off
///     ...
/// ```
pub fn print_glyf(f: &mut dyn io::Write, font: &FontRef, names: &NameMap) -> Result<(), Error> {
    let glyf = font.glyf()?;
    let loca = font.loca(None)?;
    writeln!(f, "# glyf #")?;
    for (gid, name) in sorted_glyphs(names) {
        writeln!(f, "glyph {name}")?;
        let Some(outline) = Outline::read(gid, &glyf, &loca, names)? else {
            continue;
        };
        let [x_min, y_min, x_max, y_max] = outline.bbox;
        writeln!(f, "  bbox {x_min} {y_min} {x_max} {y_max}")?;
        for contour in outline.contours() {
            writeln!(f, "  contour")?;
            for point in contour {
                writeln!(f, "    {point}")?;
            }
        }
        for component in &outline.components {
            writeln!(f, "  {component}")?;
        }
        if !outline.instructions.is_empty() {
            writeln!(f, "  instructions {}", hex(outline.instructions))?;
        }
    }
    Ok(())
}

/// Print every glyph's variation tuples, with a delta for every point
///
/// Each tuple line has the tuple's region on each axis it uses, as the peak
/// alone or as start:peak:end when the region isn't implied by the peak. The
/// deltas follow, one per point and then one per phantom point, with any the
/// font omits filled in by IUP. For example:
///
/// ```text
/// glyph underscore
///   tuple wght=1
///     0 -68
///     -7 -37
///     ...
///     0 0
///     110 0
///     0 0
///     0 0
///   tuple wght=1 GRAD=-1
///     0 1
///     ...
/// ```
pub fn print_gvar(f: &mut dyn io::Write, font: &FontRef, names: &NameMap) -> Result<(), Error> {
    let gvar = font.gvar()?;
    let glyf = font.glyf()?;
    let loca = font.loca(None)?;
    let axes: Vec<Tag> = font
        .fvar()?
        .axes()?
        .iter()
        .map(|axis| axis.axis_tag())
        .collect();
    writeln!(f, "# gvar #")?;
    for (gid, name) in sorted_glyphs(names) {
        let Some(variations) = gvar.glyph_variation_data(gid.into())? else {
            continue;
        };
        let mut tuples = variations.tuples().peekable();
        if tuples.peek().is_none() {
            continue;
        }
        writeln!(f, "glyph {name}")?;
        let (coords, ends) = match Outline::read(gid, &glyf, &loca, names)? {
            Some(outline) => outline.into_coords_and_ends(),
            None => (Vec::new(), Vec::new()),
        };
        let n = coords.len() + 4;
        let coords: Vec<Coord> = coords
            .into_iter()
            .chain(std::iter::repeat_n((0, 0), 4))
            .collect();
        for tuple in tuples {
            let mut line = String::from("  tuple");
            let peak = tuple.peak();
            let intermediate = tuple.intermediate_start().zip(tuple.intermediate_end());
            for (i, tag) in axes.iter().enumerate() {
                let peak = peak.get(i).unwrap_or_default();
                let (start, end) = intermediate
                    .as_ref()
                    .map(|(start, end)| {
                        (
                            start.get(i).unwrap_or_default(),
                            end.get(i).unwrap_or_default(),
                        )
                    })
                    .unwrap_or_else(|| (peak.min(F2Dot14::ZERO), peak.max(F2Dot14::ZERO)));
                if peak == F2Dot14::ZERO && start == F2Dot14::ZERO && end == F2Dot14::ZERO {
                    continue;
                }
                write!(line, " {tag}=").unwrap();
                if start == peak.min(F2Dot14::ZERO) && end == peak.max(F2Dot14::ZERO) {
                    write!(line, "{peak}").unwrap();
                } else {
                    write!(line, "{start}:{peak}:{end}").unwrap();
                }
            }
            writeln!(f, "{line}")?;
            let mut sparse = vec![None; n];
            for delta in tuple.deltas() {
                if let Some(slot) = sparse.get_mut(delta.position as usize) {
                    *slot = Some((delta.x_delta, delta.y_delta));
                }
            }
            for (x, y) in iup::iup_delta(&sparse, &coords, &ends) {
                writeln!(f, "    {} {}", iup::ot_round(x), iup::ot_round(y))?;
            }
        }
    }
    Ok(())
}

fn sorted_glyphs(names: &NameMap) -> Vec<(GlyphId16, &GlyphName)> {
    let mut glyphs: Vec<_> = names.0.iter().map(|(gid, name)| (*gid, name)).collect();
    glyphs.sort_by(|a, b| a.1.as_str().cmp(b.1.as_str()));
    glyphs
}

struct Outline<'a> {
    bbox: [i16; 4],
    points: Vec<Point>,
    ends: Vec<usize>,
    components: Vec<Component>,
    instructions: &'a [u8],
}

impl<'a> Outline<'a> {
    fn read(
        gid: GlyphId16,
        glyf: &Glyf<'a>,
        loca: &Loca<'a>,
        names: &NameMap,
    ) -> Result<Option<Self>, ReadError> {
        let glyph = match loca.get(GlyphId::from(gid), glyf) {
            Some(LocaGlyph::Glyph(glyph)) => glyph,
            Some(LocaGlyph::Empty) | None => return Ok(None),
        };
        let bbox = [glyph.x_min(), glyph.y_min(), glyph.x_max(), glyph.y_max()];
        Ok(Some(match glyph {
            Glyph::Simple(glyph) => {
                let (points, ends) = read_simple(&glyph)?;
                Outline {
                    bbox,
                    points,
                    ends,
                    components: Vec::new(),
                    instructions: glyph.instructions(),
                }
            }
            Glyph::Composite(glyph) => Outline {
                bbox,
                points: Vec::new(),
                ends: Vec::new(),
                components: read_components(&glyph, names),
                instructions: glyph.instructions().unwrap_or_default(),
            },
        }))
    }

    fn contours(&self) -> impl Iterator<Item = Vec<Point>> + '_ {
        let mut start = 0;
        self.ends.iter().map(move |end| {
            let contour = normalize_contour(&self.points[start..*end + 1]);
            start = *end + 1;
            contour
        })
    }

    fn into_coords_and_ends(self) -> (Vec<Coord>, Vec<usize>) {
        if self.components.is_empty() {
            let coords = self
                .points
                .iter()
                .map(|p| (p.x as i32, p.y as i32))
                .collect();
            (coords, self.ends)
        } else {
            let coords: Vec<_> = self.components.iter().map(|c| c.offset()).collect();
            let ends = (0..coords.len()).collect();
            (coords, ends)
        }
    }
}

fn read_simple(glyph: &SimpleGlyph) -> Result<(Vec<Point>, Vec<usize>), ReadError> {
    let ends: Vec<usize> = glyph
        .end_pts_of_contours()
        .iter()
        .map(|end| end.get() as usize)
        .collect();
    let n = ends.last().map(|end| end + 1).unwrap_or(0);
    let mut flags = Vec::with_capacity(n);
    let mut data = glyph.glyph_data().iter();
    while flags.len() < n {
        let flag =
            SimpleGlyphFlags::from_bits_truncate(*data.next().ok_or(ReadError::OutOfBounds)?);
        let repeats = if flag.contains(SimpleGlyphFlags::REPEAT_FLAG) {
            *data.next().ok_or(ReadError::OutOfBounds)? as usize + 1
        } else {
            1
        };
        flags.extend(std::iter::repeat_n(flag, repeats.min(n - flags.len())));
    }
    let points: Vec<_> = glyph
        .points()
        .zip(flags)
        .map(|(point, flags)| Point {
            x: point.x,
            y: point.y,
            kind: if point.on_curve {
                PointKind::On
            } else {
                PointKind::Off
            },
            overlap: flags.contains(SimpleGlyphFlags::OVERLAP_SIMPLE),
            cubic: flags.contains(SimpleGlyphFlags::CUBIC),
        })
        .collect();
    if points.len() != n {
        return Err(ReadError::OutOfBounds);
    }
    Ok((points, ends))
}

fn read_components(glyph: &CompositeGlyph, names: &NameMap) -> Vec<Component> {
    glyph
        .components()
        .map(|component| Component {
            name: names.get(component.glyph).clone(),
            flags: component.flags,
            anchor: component.anchor,
            transform: component
                .flags
                .intersects(
                    CompositeGlyphFlags::WE_HAVE_A_SCALE
                        | CompositeGlyphFlags::WE_HAVE_AN_X_AND_Y_SCALE
                        | CompositeGlyphFlags::WE_HAVE_A_TWO_BY_TWO,
                )
                .then(|| {
                    let t = component.transform;
                    [t.xx, t.yx, t.xy, t.yy]
                }),
        })
        .collect()
}

// https://github.com/googlefonts/fontc/issues/1653 and /issues/1173
fn normalize_contour(points: &[Point]) -> Vec<Point> {
    if let [point] = points {
        return vec![Point {
            kind: PointKind::Stranded,
            ..*point
        }];
    }
    if points.iter().any(|p| p.kind == PointKind::On) {
        return points.to_vec();
    }
    let dist = |p: &Point| (p.x as i64).pow(2) + (p.y as i64).pow(2);
    let (closest, _) = points
        .iter()
        .enumerate()
        .fold(None, |best: Option<(usize, i64)>, (i, p)| match best {
            Some((_, d)) if d <= dist(p) => best,
            _ => Some((i, dist(p))),
        })
        .unwrap();
    let mut rotated = points[closest..].to_vec();
    rotated.extend_from_slice(&points[..closest]);
    rotated
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PointKind {
    On,
    Off,
    /// The only point in its contour.
    ///
    /// A lone point has no curve, so whether it is on or off the curve doesn't
    /// matter, and fontc and fontmake don't always agree on it ([#1173]).
    ///
    /// [#1173]: https://github.com/googlefonts/fontc/issues/1173
    Stranded,
}

#[derive(Clone, Copy, Debug)]
struct Point {
    x: i16,
    y: i16,
    kind: PointKind,
    overlap: bool,
    cubic: bool,
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.x, self.y)?;
        match self.kind {
            PointKind::On => f.write_str(" on")?,
            PointKind::Off => f.write_str(" off")?,
            PointKind::Stranded => (),
        }
        if self.overlap {
            f.write_str(" overlap")?;
        }
        if self.cubic {
            f.write_str(" cubic")?;
        }
        Ok(())
    }
}

const PRINTED_COMPONENT_FLAGS: u16 = 0x1e14;

struct Component {
    name: GlyphName,
    flags: CompositeGlyphFlags,
    anchor: Anchor,
    transform: Option<[F2Dot14; 4]>,
}

impl Component {
    fn offset(&self) -> Coord {
        match self.anchor {
            Anchor::Offset { x, y } => (x as i32, y as i32),
            Anchor::Point { .. } => (0, 0),
        }
    }
}

impl std::fmt::Display for Component {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "component {}", self.name)?;
        match self.anchor {
            Anchor::Offset { x, y } => write!(f, " {x} {y}")?,
            Anchor::Point { base, component } => write!(f, " firstPt {base} secondPt {component}")?,
        }
        if let Some([xx, yx, xy, yy]) = self.transform {
            if yx != F2Dot14::ZERO || xy != F2Dot14::ZERO {
                write!(f, " scalex {xx} scale01 {yx} scale10 {xy} scaley {yy}")?;
            } else if xx != yy {
                write!(f, " scalex {xx} scaley {yy}")?;
            } else {
                write!(f, " scale {xx}")?;
            }
        }
        write!(
            f,
            " flags {:#x}",
            self.flags.bits() & PRINTED_COMPONENT_FLAGS
        )
    }
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().fold(String::new(), |mut s, b| {
        write!(s, "{b:02x}").unwrap();
        s
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn point(x: i16, y: i16, on: bool) -> Point {
        Point {
            x,
            y,
            kind: if on { PointKind::On } else { PointKind::Off },
            overlap: false,
            cubic: false,
        }
    }

    fn text(points: &[Point]) -> Vec<String> {
        points.iter().map(|p| p.to_string()).collect()
    }

    #[test]
    fn all_off_curve_contours_start_closest_to_origin() {
        let points = [
            point(100, 100, false),
            point(-10, 5, false),
            point(0, 50, false),
        ];
        assert_eq!(
            text(&normalize_contour(&points)),
            ["-10 5 off", "0 50 off", "100 100 off"]
        );
    }

    #[test]
    fn equidistant_points_keep_the_earliest() {
        let points = [
            point(30, 40, false),
            point(-50, 0, false),
            point(0, 50, false),
        ];
        assert_eq!(
            text(&normalize_contour(&points)),
            ["30 40 off", "-50 0 off", "0 50 off"]
        );
    }

    #[test]
    fn contours_with_on_curve_points_are_untouched() {
        let points = [point(100, 100, false), point(0, 0, true)];
        assert_eq!(text(&normalize_contour(&points)), ["100 100 off", "0 0 on"]);
    }

    #[test]
    fn stranded_points_lose_their_type() {
        assert_eq!(text(&normalize_contour(&[point(3, 4, true)])), ["3 4"]);
        assert_eq!(text(&normalize_contour(&[point(3, 4, false)])), ["3 4"]);
    }

    #[test]
    fn point_flags_are_printed() {
        let mut p = point(1, 2, true);
        p.overlap = true;
        p.cubic = true;
        assert_eq!(p.to_string(), "1 2 on overlap cubic");
    }

    #[test]
    fn components_print_like_fonttools() {
        let mut component = Component {
            name: "A".into(),
            flags: CompositeGlyphFlags::ARGS_ARE_XY_VALUES
                | CompositeGlyphFlags::ARG_1_AND_2_ARE_WORDS
                | CompositeGlyphFlags::ROUND_XY_TO_GRID
                | CompositeGlyphFlags::USE_MY_METRICS
                | CompositeGlyphFlags::WE_HAVE_A_SCALE,
            anchor: Anchor::Offset { x: 300, y: -5 },
            transform: Some([
                F2Dot14::from_f32(0.5),
                F2Dot14::ZERO,
                F2Dot14::ZERO,
                F2Dot14::from_f32(0.5),
            ]),
        };
        assert_eq!(
            component.to_string(),
            "component A 300 -5 scale 0.5 flags 0x204"
        );
        component.transform = Some([
            F2Dot14::from_f32(0.5),
            F2Dot14::ZERO,
            F2Dot14::ZERO,
            F2Dot14::ONE,
        ]);
        component.anchor = Anchor::Point {
            base: 3,
            component: 7,
        };
        assert_eq!(
            component.to_string(),
            "component A firstPt 3 secondPt 7 scalex 0.5 scaley 1 flags 0x204"
        );
        component.transform = None;
        assert_eq!(
            component.to_string(),
            "component A firstPt 3 secondPt 7 flags 0x204"
        );
        component.flags |= CompositeGlyphFlags::WE_HAVE_INSTRUCTIONS
            | CompositeGlyphFlags::UNSCALED_COMPONENT_OFFSET;
        assert_eq!(
            component.to_string(),
            "component A firstPt 3 secondPt 7 flags 0x1204"
        );
    }
}
