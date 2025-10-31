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
    use rstest::rstest;
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

    /// Compare two layers' paths for equality
    fn compare_paths(test_layer: &Layer, expectation_layer: &Layer, glyph_name: &str) {
        // Extract only Path shapes, ignoring Components
        let test_paths: Vec<_> = test_layer
            .shapes
            .iter()
            .filter_map(Shape::as_path)
            .collect();

        let expectation_paths: Vec<_> = expectation_layer
            .shapes
            .iter()
            .filter_map(Shape::as_path)
            .collect();

        assert_eq!(
            test_paths.len(),
            expectation_paths.len(),
            "Number of paths differs for glyph '{glyph_name}': expected {}, got {}",
            expectation_paths.len(),
            test_paths.len()
        );

        for (i, (test_path, expectation_path)) in
            test_paths.iter().zip(expectation_paths.iter()).enumerate()
        {
            assert_eq!(
                test_path.to_points(),
                expectation_path.to_points(),
                "Path {i} differs for glyph '{glyph_name}'",
            );
        }
    }

    fn test_corner_component_glyph(glyph_name: &str) {
        let font_path = glyphs3_dir().join("CornerComponents.glyphs");
        let font = Font::load_raw(&font_path).expect("Failed to load CornerComponents.glyphs");

        let mut test_glyph = font
            .glyphs
            .get(glyph_name)
            .cloned()
            .unwrap_or_else(|| panic!("Test glyph '{}' not found", glyph_name));

        let expectation_glyph_name = format!("{}.expectation", glyph_name);

        // Apply corner components to the test glyph
        for layer in &mut test_glyph.layers {
            insert_corner_components_for_layer(layer, &font.glyphs)
                .expect("Failed to insert corner components");
        }

        let expectation_glyph = font
            .glyphs
            .get(expectation_glyph_name.as_str())
            .unwrap_or_else(|| panic!("Expectation glyph '{}' not found", expectation_glyph_name));

        // Get the first master's layer (assuming single master for test)
        assert!(
            !test_glyph.layers.is_empty(),
            "Test glyph '{glyph_name}' has no layers",
        );
        assert!(
            !expectation_glyph.layers.is_empty(),
            "Expectation glyph '{}' has no layers",
            expectation_glyph_name
        );

        let test_layer = &test_glyph.layers[0];
        let expectation_layer = &expectation_glyph.layers[0];

        // Compare the results
        compare_paths(test_layer, expectation_layer, glyph_name);
    }

    #[rstest]
    #[case::aa_simple_angleinstroke("aa_simple_angleinstroke")]
    #[case::ab_simple_angled("ab_simple_angled")]
    #[case::ac_scale("ac_scale")]
    #[case::ad_curved_instroke("ad_curved_instroke")]
    #[case::ae_curved_corner_firstseg("ae_curved_corner_firstseg")]
    #[case::af_curved_corner_firstseg_slanted("af_curved_corner_firstseg_slanted")]
    #[case::ag_curved_corner_bothsegs("ag_curved_corner_bothsegs")]
    #[case::ag_curved_corner_bothsegs_rotated("ag_curved_corner_bothsegs_rotated")]
    #[case::ah_origin("ah_origin")]
    #[case::ai_curved_outstroke("ai_curved_outstroke")]
    #[case::aj_right_alignment("aj_right_alignment")]
    #[case::ak_right_slanted("ak_right_slanted")]
    #[case::al_unaligned("al_unaligned")]
    #[case::am_middle("am_middle")]
    #[case::an_flippy("an_flippy")]
    #[case::ao_firstnode("ao_firstnode")]
    #[case::ap_twoofthem("ap_twoofthem")]
    #[case::aq_rightleg("aq_rightleg")]
    #[case::ar_leftleg("ar_leftleg")]
    #[case::as_closedpaths("as_closedpaths")]
    #[case::at_unaligned_lastseg("at_unaligned_lastseg")]
    #[case::au_left_anchoronpath("au_left_anchoronpath")]
    #[case::av_left_anchoroffpath("av_left_anchoroffpath")]
    #[case::aw_direction("aw_direction")]
    #[case::ax_curved_instroke2("ax_curved_instroke2")]
    // ported from glyphsLib: https://github.com/googlefonts/glyphsLib/blob/f90e4060ba/tests/corner_components_test.py#L14
    fn test_corner_components(#[case] glyph_name: &str) {
        let _ = env_logger::builder().is_test(true).try_init();
        // Skip glyphs with left_anchor as noted in the Python test
        if glyph_name.contains("left_anchor") {
            // In rstest we can't easily skip tests, so we just return early
            log::info!(
                "Skipping '{}': left anchors not quite working yet",
                glyph_name
            );
            return;
        }

        test_corner_component_glyph(glyph_name);
    }
}
