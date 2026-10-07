//! Corner components support
//!
//! Implements corner component insertion for Glyphs fonts.
//! Based on: <https://github.com/googlefonts/glyphsLib/blob/main/Lib/glyphsLib/filters/cornerComponents.py>

use std::collections::BTreeMap;

use kurbo::{Affine, Line, ParamCurve, ParamCurveArclen, ParamCurveNearest, PathSeg, Point};
use smol_str::SmolStr;
use thiserror::Error;
use write_fonts::OtRound;

use crate::font::{Alignment, Glyph, Hint, HintType, Layer, Node, NodeType, Path, Shape};

impl OtRound<Node> for Node {
    fn ot_round(self) -> Node {
        let (x, y) = self.pt.ot_round();
        Node {
            pt: Point::new(x as _, y as _),
            node_type: self.node_type,
        }
    }
}

#[derive(Debug, Error, Clone)]
#[error("component '{component}' failed: {reason}")]
pub struct BadCornerComponent {
    component: SmolStr,
    reason: BadCornerComponentReason,
}

#[derive(Debug, Error, Clone)]
pub enum BadCornerComponentReason {
    #[error("corner glyph contains no layer '{0}'")]
    MissingLayer(SmolStr),
    #[error("glyph contains no paths")]
    NoPaths,
    #[error("no path at shape index '{0}'")]
    BadShapeIndex(usize),
    #[error("path contains too few points")]
    PathTooShort,
}

impl BadCornerComponentReason {
    // convenience to turn this into the actual error type we return
    fn add_name(self, component: SmolStr) -> BadCornerComponent {
        BadCornerComponent {
            component,
            reason: self,
        }
    }
}

/// Find the t parameter on a segment at a given distance along it
///
/// <https://github.com/googlefonts/glyphsLib/blob/f90e4060/Lib/glyphsLib/filters/cornerComponents.py#L151>
fn point_on_seg_at_distance(seg: PathSeg, distance: f64) -> f64 {
    seg.inv_arclen(distance, 1e-6)
}

/// Insert all corner components for a layer
pub(crate) fn insert_corner_components_for_layer(
    layer: &mut Layer,
    glyphs: &BTreeMap<SmolStr, Glyph>,
) -> Result<(), BadCornerComponent> {
    let mut corner_hints: Vec<Hint> = layer
        .hints
        .iter()
        .filter(|h| h.type_ == HintType::Corner)
        .cloned()
        .collect();

    // make sure we do earlier shapes first and earlier nodes first
    corner_hints.sort_by_key(|hint| (hint.shape_index, hint.node_index));

    if corner_hints.is_empty() {
        return Ok(());
    }

    // if we insert points for one corner, it will change the index of
    // a subsequent corner, so we track how many points we've inserted.
    let mut inserted_pts = 0;
    let mut current_shape = 0;

    for hint in corner_hints {
        if hint.shape_index != current_shape {
            current_shape = hint.shape_index;
            inserted_pts = 0;
        }

        let Some(corner_glyph) = glyphs.get(&hint.name) else {
            log::warn!("corner component '{}' not found", hint.name);
            continue;
        };

        let component = corner_glyph
            .layers
            .iter()
            .find(|l| l.layer_id == layer.master_id())
            .ok_or_else(|| BadCornerComponentReason::MissingLayer(layer.master_id().into()))
            .and_then(CornerComponent::new)
            .map_err(|e| e.add_name(hint.name.clone()))?;

        let n_points = component.corner_path.nodes.len() - 1;
        layer
            .insert_corner_component(component, &hint, inserted_pts)
            .map_err(|e| e.add_name(hint.name.clone()))?;
        inserted_pts += n_points;
    }

    // Clear hints after applying
    layer.hints.retain(|h| h.type_ != HintType::Corner);

    Ok(())
}

impl Layer {
    // approximately follows the logic at https://github.com/googlefonts/glyphsLib/blob/f90e4060b/Lib/glyphsLib/filters/cornerComponents.py#L230
    fn insert_corner_component(
        &mut self,
        mut component: CornerComponent,
        hint: &Hint,
        delta_pt_index: usize,
    ) -> Result<(), BadCornerComponentReason> {
        let path = match self.shapes.get_mut(hint.shape_index) {
            Some(Shape::Path(p)) => p,
            _ => return Err(BadCornerComponentReason::BadShapeIndex(hint.shape_index)),
        };

        let point_idx = (hint.node_index + delta_pt_index) % path.nodes.len();
        let scale = Affine::scale_non_uniform(hint.scale.x.0, hint.scale.y.0);
        // first scale the component as required by the hint
        component.apply_transform(scale);

        let AlignmentState {
            mut instroke_pt,
            outstroke_pt: _,
            correction,
            // this mutates the component, aligning it with the target segment
        } = component.align_to_main_path(path, hint, point_idx);

        let original_outstroke = path.get_next_segment(point_idx).unwrap();
        if hint.alignment != Alignment::InStroke && correction {
            instroke_pt = component
                .recompute_instroke_intersection_point(path, point_idx)
                .unwrap_or(instroke_pt);

            if !matches!(
                component.corner_path.get_next_segment(0).unwrap(),
                PathSeg::Line(_)
            ) {
                component.stretch_first_seg_to_fit(instroke_pt);
            }
        }
        // adjust the instroke (the stroke leading into the point where we're
        // adding the new corner)
        path.split_instroke(point_idx, instroke_pt);
        // now insert the corner into the path
        let insert_pt = path.next_idx(point_idx);
        path.nodes.splice(
            insert_pt..insert_pt,
            component
                .corner_path
                .nodes
                .iter()
                .cloned()
                .skip(1)
                .map(|node| node.ot_round()),
        );

        let added_points = component.corner_path.nodes.len() - 1;
        // 'prev' because the last point we inserted is the new outstroke start
        let new_outstroke_idx = path.prev_idx(insert_pt + added_points);

        // then adjust the outstroke
        if let Some(outstroke_intersection_point) =
            component.recompute_outstroke_intersection_point(original_outstroke, hint)
        {
            path.fixup_outstroke(
                original_outstroke,
                outstroke_intersection_point,
                new_outstroke_idx,
            );
        }

        // and finally, add any new extra paths
        for mut path in component.other_paths.into_iter() {
            path.nodes
                .iter_mut()
                .for_each(|node| *node = node.ot_round());
            self.shapes.push(Shape::Path(path));
        }

        Ok(())
    }
}

impl Hint {
    fn is_flipped(&self) -> bool {
        self.scale.x.0 * self.scale.y.0 < 0.0
    }
}

impl Path {
    fn set_point(&mut self, idx: usize, point: Point) {
        self.nodes[idx].pt = point;
        self.nodes[idx] = self.nodes[idx].ot_round();
    }

    // should maybe be called "truncate instroke?"
    //https://github.com/googlefonts/glyphsLib/blob/f90e4060ba/Lib/glyphsLib/filters/cornerComponents.py#L414
    fn split_instroke(&mut self, point_idx: usize, intersection: Point) {
        let instroke = self.get_previous_segment(point_idx).unwrap();
        let nearest_t = instroke.nearest(intersection, 1e-6).t;
        let split = instroke.subsegment(0.0..nearest_t);
        match split {
            PathSeg::Line(line) => self.set_point(point_idx, line.p1),
            PathSeg::Quad(quad) => {
                self.set_point(point_idx, quad.p2);
                let idx = self.prev_idx(point_idx);
                self.set_point(idx, quad.p1);
            }
            PathSeg::Cubic(cubic) => {
                self.set_point(point_idx, cubic.p3);
                let idx = self.prev_idx(point_idx);
                self.set_point(idx, cubic.p2);
                let idx = self.prev_idx(idx);
                self.set_point(idx, cubic.p1);
            }
        };
    }

    fn fixup_outstroke(&mut self, original: PathSeg, intersection: Point, point_idx: usize) {
        let nearest_t = original.nearest(intersection, 1e-6).t;
        let split = original.subsegment(nearest_t..1.0);
        match split {
            PathSeg::Line(line) => self.set_point(point_idx, line.p0),
            PathSeg::Quad(quad) => {
                self.set_point(point_idx, quad.p0);
                let idx = self.next_idx(point_idx);
                self.set_point(idx, quad.p1);
            }
            PathSeg::Cubic(cubic) => {
                self.set_point(point_idx, cubic.p0);
                let idx = self.next_idx(point_idx);
                self.set_point(idx, cubic.p1);
                let idx = self.next_idx(idx);
                self.set_point(idx, cubic.p2);
            }
        }
    }
}

struct CornerComponent {
    corner_path: Path,
    other_paths: Vec<Path>,
    // the 'left' anchor of the component, (0,0) by default
    #[expect(dead_code)] // used for alignment, not handled yet
    left: Point,
    // the 'right' anchor of the component, (0,0) by default
    #[expect(dead_code)] // used for alignment, not handled yet
    right: Point,
}

impl CornerComponent {
    fn new(corner_layer: &Layer) -> Result<Self, BadCornerComponentReason> {
        let origin = corner_layer.get_anchor_pt("origin").unwrap_or_default();
        let left = corner_layer.get_anchor_pt("left").unwrap_or_default();
        let right = corner_layer.get_anchor_pt("right").unwrap_or_default();

        // Extract the main path and other paths
        let mut path_iter = corner_layer
            .shapes
            .iter()
            .filter_map(Shape::as_path)
            .cloned()
            // apply the origin here
            .map(|mut path| {
                path.nodes
                    .iter_mut()
                    .for_each(|node| node.pt -= origin.to_vec2());
                path
            });

        let corner_path = path_iter.next().ok_or(BadCornerComponentReason::NoPaths)?;
        if corner_path.nodes.len() < 2 {
            return Err(BadCornerComponentReason::PathTooShort);
        }
        let other_paths = path_iter.collect::<Vec<_>>();

        Ok(Self {
            corner_path,
            other_paths,
            left,
            right,
        })
    }

    fn apply_transform(&mut self, transform: Affine) {
        for node in self.corner_path.nodes.iter_mut().chain(
            self.other_paths
                .iter_mut()
                .flat_map(|path| path.nodes.iter_mut()),
        ) {
            node.pt = transform * node.pt;
        }
    }

    fn last_point(&self) -> Point {
        // by construction we are not empty
        self.corner_path.nodes.last().unwrap().pt
    }

    fn reverse_corner_path(&mut self) {
        self.corner_path.reverse();
        // fixup the node types; a simple cubic bezier corner has types,
        // 'line, offcurve, offcurve, curveto' and when reversed we end up
        // with a lineto at the end, which we later think is an error:
        let [.., p0, pn] = self.corner_path.nodes.as_mut_slice() else {
            return;
        };
        if p0.node_type == NodeType::OffCurve {
            pn.node_type = match pn.node_type {
                NodeType::Line => NodeType::Curve,
                NodeType::LineSmooth => NodeType::CurveSmooth,
                other => other,
            };
        }
    }

    //https://github.com/googlefonts/glyphsLib/blob/f90e4060/Lib/glyphsLib/filters/cornerComponents.py#L340
    fn align_to_main_path(&mut self, path: &Path, hint: &Hint, point_idx: usize) -> AlignmentState {
        let mut angle = (-self.last_point().y).atan2(self.last_point().x);
        if hint.is_flipped() {
            angle += std::f64::consts::FRAC_PI_2;
            self.reverse_corner_path();
        }

        let instroke = path.get_previous_segment(point_idx).unwrap();
        let outstroke = path.get_next_segment(point_idx).unwrap();
        let target_pt = path.nodes.get(point_idx).unwrap().pt;

        // calculate outstroke angle
        let distance = if hint.is_flipped() {
            self.last_point().y
        } else {
            self.last_point().x
        };

        let outstroke_t = point_on_seg_at_distance(outstroke, distance.abs());
        let outstroke_pt = outstroke.eval(outstroke_t);
        let outstroke_angle = (outstroke_pt - target_pt).angle();

        // calculate instroke angle
        let distance = if hint.is_flipped() {
            -self.corner_path.nodes.first().unwrap().pt.x
        } else {
            self.corner_path.nodes.first().unwrap().pt.y
        };

        let instroke_t = point_on_seg_at_distance(instroke, distance.abs());
        let instroke_pt = instroke.reverse().eval(instroke_t);
        let instroke_angle = (target_pt - instroke_pt).angle() + std::f64::consts::FRAC_PI_2;

        let correction = !(py_is_close(instroke_t, 0.0) || py_is_close(instroke_t, 1.0));

        let angle = angle
            + match hint.alignment {
                Alignment::OutStroke => outstroke_angle,
                Alignment::InStroke => instroke_angle,
                Alignment::Middle => (instroke_angle + outstroke_angle) / 2.0,
                _ => 0.0,
            };

        // rotate the paths around the origin and align them
        // so that the origin of the corner starts on the target node
        //https://github.com/googlefonts/glyphsLib/blob/f90e4060/Lib/glyphsLib/filters/cornerComponents.py#L384
        let xform = Affine::translate(target_pt.to_vec2()).pre_rotate(angle);
        self.apply_transform(xform);

        AlignmentState {
            instroke_pt,
            outstroke_pt,
            correction,
        }
    }

    //https://github.com/googlefonts/glyphsLib/blob/f90e4060/Lib/glyphsLib/filters/cornerComponents.py#L396
    fn recompute_instroke_intersection_point(
        &self,
        path: &Path,
        target_node_ix: usize,
    ) -> Option<Point> {
        // see ref above, this just treats it as a line
        let first_seg_as_line = &self.corner_path.nodes.as_slice()[..2];
        let first_seg_as_line = Line::new(first_seg_as_line[0].pt, first_seg_as_line[1].pt);
        let instroke = path.get_previous_segment(target_node_ix).unwrap();
        unbounded_seg_seg_intersection(first_seg_as_line.into(), instroke)
    }

    //https://github.com/googlefonts/glyphsLib/blob/f90e4060b/Lib/glyphsLib/filters/cornerComponents.py#L401
    fn recompute_outstroke_intersection_point(
        &self,
        original_outstroke: PathSeg,
        hint: &Hint,
    ) -> Option<Point> {
        if hint.is_flipped() {
            unbounded_seg_seg_intersection(
                self.corner_path
                    .get_previous_segment(self.corner_path.nodes.len() - 1)
                    .unwrap(),
                original_outstroke,
            )
        } else {
            // the python all uses custom geometry fns, which i would like to avoid..
            let nearest = original_outstroke.nearest(self.last_point(), 1e-6);
            Some(original_outstroke.eval(nearest.t))
        }
    }

    fn stretch_first_seg_to_fit(&mut self, intersection_pt: Point) {
        let delta = intersection_pt - self.corner_path.nodes[0].pt;
        self.corner_path.nodes[1].pt += delta;
    }
}

/// Find the intersection of two unbounded segments
///
/// <https://github.com/googlefonts/glyphsLib/blob/f90e4060b/Lib/glyphsLib/filters/cornerComponents.py#L127>
fn unbounded_seg_seg_intersection(seg1: PathSeg, seg2: PathSeg) -> Option<Point> {
    // Line-line intersection
    match (seg1, seg2) {
        (PathSeg::Line(one), PathSeg::Line(two)) => one.crossing_point(two),
        (seg, PathSeg::Line(line)) | (PathSeg::Line(line), seg) => {
            // a value by which we extend our line, to find the crossing point.
            // should be enough for anybody!
            const LITERALLY_UNBOUNDED: f64 = 1e9;

            // Extend the line by 1000 units in both directions to simulate unbounded line
            let direction = (line.p1 - line.p0).normalize();
            let extended_line = Line::new(
                line.p0 - direction * LITERALLY_UNBOUNDED,
                line.p1 + direction * LITERALLY_UNBOUNDED,
            );
            seg.intersect_line(extended_line)
                .first()
                .map(|hit| seg.eval(hit.segment_t))
        }
        _ => None,
    }
}

// https://docs.python.org/3.14/library/math.html#math.isclose
fn py_is_close(a: f64, b: f64) -> bool {
    // abs(a-b) <= max(rel_tol * max(abs(a), abs(b)), abs_tol).
    const REL_TOL: f64 = 1e-09;
    (a - b).abs() <= REL_TOL * a.abs().max(b.abs())
}

struct AlignmentState {
    instroke_pt: Point,
    #[expect(dead_code, reason = "python does it")]
    outstroke_pt: Point,
    correction: bool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::Font;
    use std::path::{Path as FilePath, PathBuf};

    fn testdata_dir() -> PathBuf {
        let mut dir = FilePath::new("../resources/testdata");
        if !dir.is_dir() {
            dir = FilePath::new("./resources/testdata");
        }
        dir.to_path_buf()
    }

    fn glyphs3_dir() -> PathBuf {
        testdata_dir().join("glyphs3")
    }

    // Each case in CornerComponents.glyphs has a <case>.expectation glyph with
    // Glyphs 3.5's own decomposition of its corners, made by glyphsLib's
    // tests/tools/corner_components_expectations.py.
    //
    // Cases where we don't yet match Glyphs:
    const MISMATCHES: &[&str] = &[
        "ad_curved_instroke",
        "ak_right_slanted",
        "al_unaligned",
        "align_instroke_concave",
        "align_instroke_flipx_concave",
        "align_instroke_flipxy_acute",
        "align_instroke_squashed",
        "align_middle_concave",
        "align_middle_flipx_acute",
        "align_middle_flipx_concave",
        "align_middle_flipy_acute",
        "align_middle_squashed",
        "align_outstroke_flipx_acute",
        "align_outstroke_flipx_concave",
        "align_outstroke_flipy_acute",
        "align_unaligned_concave",
        "align_unaligned_flipx_acute",
        "align_unaligned_flipx_concave",
        "align_unaligned_flipxy_acute",
        "align_unaligned_flipy_acute",
        "anchor_left_instroke",
        "anchor_left_instroke_flipx",
        "anchor_left_instroke_square",
        "anchor_left_middle",
        "anchor_left_middle_flipx",
        "anchor_left_on_path_instroke",
        "anchor_left_on_path_instroke_flipx",
        "anchor_left_on_path_middle",
        "anchor_left_on_path_middle_flipx",
        "anchor_left_on_path_outstroke_flipx",
        "anchor_left_on_path_unaligned",
        "anchor_left_on_path_unaligned_flipx",
        "anchor_left_outstroke_flipx",
        "anchor_left_right_instroke",
        "anchor_left_right_instroke_flipx",
        "anchor_left_right_middle",
        "anchor_left_right_middle_flipx",
        "anchor_left_right_outstroke",
        "anchor_left_right_outstroke_flipx",
        "anchor_left_right_unaligned",
        "anchor_left_right_unaligned_flipx",
        "anchor_left_unaligned_flipx",
        "anchor_origin_instroke",
        "anchor_origin_left",
        "anchor_origin_left_flipx",
        "anchor_origin_outstroke_flipx",
        "anchor_right_instroke",
        "anchor_right_instroke_flipx",
        "anchor_right_instroke_square",
        "anchor_right_middle",
        "anchor_right_middle_flipx",
        "anchor_right_outstroke",
        "anchor_right_outstroke_flipx",
        "anchor_right_outstroke_square",
        "anchor_right_unaligned",
        "anchor_right_unaligned_flipx",
        "angle_concave",
        "angle_counter",
        "ap_twoofthem",
        "au_left_anchoronpath",
        "av_left_anchoroffpath",
        "curve_bracketed_instroke_curvedboth",
        "curve_bracketed_instroke_curvedin",
        "curve_bracketed_instroke_curvedout",
        "curve_bracketed_instroke_tight",
        "curve_bracketed_outstroke_curvedboth",
        "curve_bracketed_outstroke_curvedin",
        "curve_bracketed_outstroke_curvedout",
        "curve_bracketed_outstroke_tight",
        "curve_cupped_instroke_curvedboth",
        "curve_cupped_instroke_curvedin",
        "curve_cupped_instroke_curvedout",
        "curve_cupped_instroke_tight",
        "curve_cupped_outstroke_tight",
        "curve_flare_instroke_curvedboth",
        "curve_flare_instroke_curvedin",
        "curve_flare_instroke_flipx_curvedin",
        "curve_flare_instroke_tight",
        "curve_flare_outstroke_curvedboth",
        "curve_flare_outstroke_curvedin",
        "curve_flare_outstroke_curvedout",
        "curve_flare_outstroke_flipx_curvedin",
        "curve_flare_outstroke_tight",
        "curve_flare_turned_instroke_curvedboth",
        "curve_flare_turned_instroke_curvedin",
        "curve_flare_turned_instroke_curvedout",
        "curve_flare_turned_instroke_square",
        "curve_flare_turned_instroke_tight",
        "curve_flare_turned_outstroke_curvedboth",
        "curve_flare_turned_outstroke_curvedin",
        "curve_flare_turned_outstroke_curvedout",
        "curve_flare_turned_outstroke_square",
        "curve_flare_turned_outstroke_tight",
        "multi_concave",
        "multi_flipx",
        "multi_instroke_acute",
        "orient_mirrored_acute",
        "orient_mirrored_square",
        "orient_mirrored_turned_acute",
        "orient_mirrored_turned_concave",
        "orient_mirrored_turned_square",
        "orient_reversed_acute",
        "orient_reversed_concave",
        "orient_reversed_square",
        "orient_tilted_acute",
        "orient_tilted_concave",
        "orient_turned_acute",
        "orient_turned_back_acute",
        "orient_turned_back_concave",
        "orient_turned_back_square",
        "orient_turned_concave",
        "orient_turned_square",
        "orient_upside_down_concave",
        "real_alkatra_l",
        "real_aoboshi_g",
        "real_aoboshi_l",
        "real_aoboshi_x",
        "real_bellota_p",
        "real_bellota_sha",
        "real_hina_uroko",
        "real_hina_yoko",
        "real_iansui_rhook",
        "real_iansui_sturn",
        "real_inconsolata_d",
        "real_montagu_k_arm",
        "real_montagu_k_leg",
        "real_playfair_de",
        "real_playfair_descender",
        "real_plexkr_mil",
        "where_duplicate",
        "where_short_instroke",
        "where_short_outstroke",
        "where_straight_node",
    ];

    fn master_layer<'a>(font: &'a Font, glyph_name: &str) -> Option<&'a Layer> {
        font.glyphs
            .get(glyph_name)?
            .layers
            .iter()
            .find(|layer| layer.layer_id == font.masters[0].id)
    }

    fn on_curves(layer: &Layer) -> Vec<Vec<Point>> {
        layer
            .shapes
            .iter()
            .filter_map(Shape::as_path)
            .map(|path| {
                path.nodes
                    .iter()
                    .filter(|node| node.node_type != NodeType::OffCurve)
                    .map(|node| node.pt)
                    .collect()
            })
            .collect()
    }

    fn node_distance(nodes: &[Point], others: impl Iterator<Item = Point>) -> f64 {
        nodes
            .iter()
            .zip(others)
            .map(|(a, b)| (a.x - b.x).abs().max((a.y - b.y).abs()))
            .fold(0.0, f64::max)
    }

    /// The furthest any of our nodes is from Glyphs'.
    ///
    /// Each of our contours is matched with whichever of Glyphs' contours and
    /// start points fits it best.
    fn furthest_node(ours: &[Vec<Point>], glyphs: &[Vec<Point>]) -> f64 {
        let lengths = |contours: &[Vec<Point>]| {
            let mut lengths: Vec<_> = contours.iter().map(Vec::len).collect();
            lengths.sort();
            lengths
        };
        if lengths(ours) != lengths(glyphs) {
            return f64::INFINITY;
        }
        let mut glyphs = glyphs.to_vec();
        let mut furthest = 0.0f64;
        for contour in ours {
            let (distance, i) = glyphs
                .iter()
                .enumerate()
                .filter(|(_, other)| other.len() == contour.len())
                .flat_map(|(i, other)| {
                    (0..other.len().max(1)).map(move |k| {
                        let rotated = other.iter().cycle().skip(k).copied();
                        (node_distance(contour, rotated), i)
                    })
                })
                .min_by(|a, b| a.0.total_cmp(&b.0))
                .unwrap();
            furthest = furthest.max(distance);
            glyphs.remove(i);
        }
        furthest
    }

    /// The outline as straight lines, with curves flattened.
    fn outline(layer: &Layer) -> Vec<Line> {
        let mut lines = Vec::new();
        for path in layer.shapes.iter().filter_map(Shape::as_path) {
            let on_curves: Vec<_> = (0..path.nodes.len())
                .filter(|i| path.nodes[*i].node_type != NodeType::OffCurve)
                .collect();
            let n_segments = if path.closed {
                on_curves.len()
            } else {
                on_curves.len().saturating_sub(1)
            };
            for &i in &on_curves[..n_segments] {
                match path.get_next_segment(i).unwrap() {
                    PathSeg::Line(line) => lines.push(line),
                    curve => {
                        let points: Vec<_> =
                            (0..=24).map(|j| curve.eval(j as f64 / 24.0)).collect();
                        lines.extend(points.windows(2).map(|pair| Line::new(pair[0], pair[1])));
                    }
                }
            }
        }
        lines
    }

    fn distance_to_line(pt: Point, line: Line) -> f64 {
        let d = line.p1 - line.p0;
        let t = match d.hypot2() {
            0.0 => 0.0,
            length2 => ((pt - line.p0).dot(d) / length2).clamp(0.0, 1.0),
        };
        pt.distance(line.eval(t))
    }

    /// The furthest any point on `lines` is from `others`, checking every few units.
    fn furthest_point(lines: &[Line], others: &[Line]) -> f64 {
        const STEP: f64 = 3.0;
        let mut furthest = 0.0f64;
        for line in lines {
            let n = (line.length() / STEP).ceil().max(1.0) as usize;
            for i in 0..n {
                let pt = line.eval(i as f64 / n as f64);
                let nearest = others
                    .iter()
                    .map(|other| distance_to_line(pt, *other))
                    .fold(f64::INFINITY, f64::min);
                furthest = furthest.max(nearest);
            }
        }
        furthest
    }

    /// Decompose the corners in `case` and compare the result with Glyphs'.
    ///
    /// We and Glyphs round to whole units at different points, so we allow a
    /// unit's difference. Handles are only compared through the outline: one a
    /// few units off can move the curve by less than one.
    fn check_case(font: &Font, case: &str) -> Result<(), String> {
        let mut ours = master_layer(font, case).unwrap().clone();
        let glyphs = master_layer(font, &format!("{case}.expectation")).ok_or("no expectation")?;
        insert_corner_components_for_layer(&mut ours, &font.glyphs).map_err(|e| e.to_string())?;
        let node = furthest_node(&on_curves(&ours), &on_curves(glyphs));
        if node > 1.0 {
            return Err(format!("a node is {node} units from Glyphs'"));
        }
        let (ours, glyphs) = (outline(&ours), outline(glyphs));
        let outline = furthest_point(&ours, &glyphs).max(furthest_point(&glyphs, &ours));
        if outline > 1.0 {
            return Err(format!("the outline is {outline:.1} units from Glyphs'"));
        }
        Ok(())
    }

    #[test]
    fn corner_components_match_glyphs() {
        let font = Font::load_raw(glyphs3_dir().join("CornerComponents.glyphs")).unwrap();
        let cases: Vec<_> = font
            .glyphs
            .keys()
            .filter(|name| {
                master_layer(&font, name)
                    .is_some_and(|layer| layer.hints.iter().any(|h| h.type_ == HintType::Corner))
            })
            .collect();
        for name in MISMATCHES {
            assert!(
                cases.iter().any(|case| case == name),
                "no such case '{name}'"
            );
        }
        let unexpected: Vec<_> = cases
            .iter()
            .filter_map(|case| {
                match (check_case(&font, case), MISMATCHES.contains(&case.as_str())) {
                    (Ok(()), true) => {
                        Some(format!("{case}: matches Glyphs, remove it from MISMATCHES"))
                    }
                    (Err(e), false) => Some(format!("{case}: {e}")),
                    _ => None,
                }
            })
            .collect();
        assert!(unexpected.is_empty(), "{}", unexpected.join("\n"));
    }
}
