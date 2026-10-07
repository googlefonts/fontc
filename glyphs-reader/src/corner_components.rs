//! Corner components support
//!
//! Implements corner component insertion for Glyphs fonts.
//! Based on: <https://github.com/googlefonts/glyphsLib/blob/main/Lib/glyphsLib/filters/cornerComponents.py>

use std::collections::BTreeMap;

use fontdrasil::open_corners::open_corner;
use kurbo::{Affine, CubicBez, Line, ParamCurve, PathSeg, Point, Vec2};
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
    #[error("quadratic curves are not supported")]
    Quadratic,
    #[error("node {0} is an off-curve point")]
    OffCurveNode(usize),
}

impl BadCornerComponentReason {
    /// Whether the problem is with this one corner, which we can leave out.
    fn skips_corner(&self) -> bool {
        matches!(
            self,
            Self::BadShapeIndex(_) | Self::Quadratic | Self::OffCurveNode(_)
        )
    }

    // convenience to turn this into the actual error type we return
    fn add_name(self, component: SmolStr) -> BadCornerComponent {
        BadCornerComponent {
            component,
            reason: self,
        }
    }
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
        .filter(|h| {
            let found = glyphs.contains_key(&h.name);
            if !found {
                log::warn!("corner component '{}' not found", h.name);
            }
            found
        })
        .cloned()
        .collect();
    layer.hints.retain(|h| h.type_ != HintType::Corner);

    // make sure we do earlier shapes first and earlier nodes first
    corner_hints.sort_by_key(|hint| (hint.shape_index, hint.node_index));

    if corner_hints.is_empty() {
        return Ok(());
    }
    let corner_hints = erase_open_corners(layer, corner_hints);

    // if we insert points for one corner, it will change the index of
    // a subsequent corner, so we track how many points we've inserted.
    let mut inserted_pts = 0;
    let mut current_shape = 0;
    let mut last_node = None;

    for hint in corner_hints {
        if hint.shape_index != current_shape {
            current_shape = hint.shape_index;
            inserted_pts = 0;
        }

        let Some(corner_glyph) = glyphs.get(&hint.name) else {
            continue;
        };

        // a corner replaces its node, so like Glyphs we only apply the first
        let node = (hint.shape_index, hint.node_index);
        if last_node.replace(node) == Some(node) {
            log::warn!(
                "ignoring corner component '{}': there is already a corner on that node",
                hint.name
            );
            continue;
        }

        let component = corner_glyph
            .layers
            .iter()
            .find(|l| l.layer_id == layer.master_id())
            .ok_or_else(|| BadCornerComponentReason::MissingLayer(layer.master_id().into()))
            .and_then(CornerComponent::new)
            .map_err(|e| e.add_name(hint.name.clone()))?;

        match layer.insert_corner_component(component, &hint, inserted_pts) {
            Ok(shift) => inserted_pts += shift,
            Err(e) if e.skips_corner() => {
                log::warn!("skipping corner component '{}': {e}", hint.name);
            }
            Err(e) => return Err(e.add_name(hint.name.clone())),
        }
    }

    Ok(())
}

/// Erase the open corners in a layer that has corners to apply.
///
/// Glyphs erases the open corners in all of a glyph's paths before it applies
/// corners, so a corner on the start of an open corner's line goes on the
/// erased corner, and one on the line's end is lost along with its node.
///
/// Returns the hints that are left, with their node indices updated.
fn erase_open_corners(layer: &mut Layer, hints: Vec<Hint>) -> Vec<Hint> {
    // for each path, its original length and the original index of each of
    // its remaining nodes
    let mut originals: BTreeMap<usize, (usize, Vec<usize>)> = BTreeMap::new();
    for (shape_index, shape) in layer.shapes.iter_mut().enumerate() {
        let Shape::Path(path) = shape else {
            continue;
        };
        let original_len = path.nodes.len().max(1);
        let mut kept: Vec<usize> = (0..path.nodes.len()).collect();
        // like a UFO contour, start from the last node
        'erase: loop {
            let n = path.nodes.len();
            for idx in (0..n).map(|i| (i + n - 1) % n) {
                if let Some(removed) = path.erase_open_corner_at(idx) {
                    kept.remove(removed);
                    continue 'erase;
                }
            }
            break;
        }
        originals.insert(shape_index, (original_len, kept));
    }
    hints
        .into_iter()
        .filter_map(|mut hint| {
            let Some((original_len, kept)) = originals.get(&hint.shape_index) else {
                return Some(hint);
            };
            let original = hint.node_index % original_len;
            let Some(idx) = kept.iter().position(|&o| o == original) else {
                log::warn!("corner component '{}' lost with its open corner", hint.name);
                return None;
            };
            hint.node_index = idx;
            Some(hint)
        })
        .collect()
}

impl Layer {
    // follows glyphsLib's CornerComponentApplier.apply
    fn insert_corner_component(
        &mut self,
        mut component: CornerComponent,
        hint: &Hint,
        delta_pt_index: isize,
    ) -> Result<isize, BadCornerComponentReason> {
        let path = match self.shapes.get_mut(hint.shape_index) {
            Some(Shape::Path(p)) => p,
            _ => return Err(BadCornerComponentReason::BadShapeIndex(hint.shape_index)),
        };

        if path.nodes.len() < 2 {
            return Err(BadCornerComponentReason::PathTooShort);
        }
        let point_idx = (hint.node_index as isize + delta_pt_index)
            .rem_euclid(path.nodes.len() as isize) as usize;
        if path.nodes[point_idx].node_type == NodeType::OffCurve {
            return Err(BadCornerComponentReason::OffCurveNode(hint.node_index));
        }
        let instroke = line_or_cubic(path.get_previous_segment(point_idx))?;
        let outstroke = line_or_cubic(path.get_next_segment(point_idx))?;
        let node = path.nodes[point_idx].pt;

        // A negative scale applies the corner backwards, swapping its left and
        // right anchors. Glyphs reverses the other paths too, so that they
        // keep their direction.
        component.apply_transform(Affine::scale_non_uniform(hint.scale.x.0, hint.scale.y.0));
        if hint.is_flipped() {
            component.reverse_paths();
            std::mem::swap(&mut component.left, &mut component.right);
        }

        // Glyphs points each end of the corner at the point as far along its
        // stroke as the end is from the origin, measuring both strokes from
        // the target node. Where the corner has a left anchor, that's what it
        // points along the instroke, and likewise a right anchor along the
        // outstroke.
        let (first, last) = component.ends();
        let instroke_dir = aim_along(instroke.reverse(), first.to_vec2().hypot());
        let outstroke_dir = aim_along(outstroke, last.to_vec2().hypot());

        // If the corner turns the other way from the host path, Glyphs
        // mirrors it to fit. Unaligned, it isn't mirrored, but its ends turn
        // the other way instead. A host that runs straight on counts as an
        // inside corner, and a corner with an end on the origin as an outside
        // one.
        let host_turn = match instroke_dir.cross(outstroke_dir) {
            0.0 if instroke_dir.dot(outstroke_dir) < 0.0 => 1.0,
            turn => turn,
        };
        let (in_end, out_end) = component.pointing_ends();
        let zero_end = in_end == Vec2::ZERO || out_end == Vec2::ZERO;
        let corner_turn = if zero_end {
            -1.0
        } else {
            in_end.cross(out_end)
        };
        let mut turns_other_way = host_turn * corner_turn < 0.0;
        if turns_other_way && hint.alignment != Alignment::Unaligned {
            component.mirror();
            turns_other_way = false;
        }
        let (in_end, out_end) = component.pointing_ends();
        let fitting = Fitting {
            alignment: hint.alignment,
            ends: [in_end, out_end],
            sign: if turns_other_way { -1.0 } else { 1.0 },
        };

        // Glyphs then aims each end again, the instroke's first, as far along
        // its stroke as fitting the corner would leave that end node from the
        // origin. That matters where an end is sheared to fit a curved stroke.
        let strokes = [instroke.reverse(), outstroke];
        let mut directions = [instroke_dir, outstroke_dir];
        let mut reach = [0.0; 2];
        let (first, last) = component.ends();
        for (i, (end, pt)) in [(End::First, first), (End::Last, last)]
            .into_iter()
            .enumerate()
        {
            let fit = component
                .end_fit(end, &fitting, directions, true)
                .unwrap_or(Affine::IDENTITY);
            reach[i] = (fit * pt).to_vec2().hypot();
            directions[i] = aim_along(strokes[i], reach[i]);
        }

        // the corner as a whole turns to fit the stroke it's aligned to, and
        // its ends do the rest of the turning
        let rotation = component.rotation(&fitting, directions);
        for end in [End::First, End::Last] {
            if let Some(fit) = component.end_fit(end, &fitting, directions, false) {
                component.apply_end_fit(end, fit);
            }
        }
        let (first, last) = component.ends();
        let (instroke_cut, outstroke_cut) = (first.to_vec2().hypot(), last.to_vec2().hypot());
        let [instroke_dir, outstroke_dir] = directions;

        // Where Glyphs aimed past the end of a curve, it takes the stroke to
        // be a line: it straightens the instroke, which then ends at the
        // corner's first node like any line, and leaves the outstroke uncut.
        // It also straightens an instroke it would cut past the end of.
        let past_end = |stroke: PathSeg, distance: f64| match stroke {
            PathSeg::Cubic(cubic) => distance >= glyphs_length(cubic),
            _ => false,
        };
        let straight_instroke = past_end(instroke, reach[0].max(instroke_cut));
        let uncut_outstroke = past_end(outstroke, reach[1]);

        // and an end on the origin doesn't slide it
        let slide_dir = |end: Vec2, dir: Vec2| match end {
            Vec2::ZERO => Vec2::ZERO,
            _ => dir,
        };
        component.place(
            hint.alignment,
            node,
            rotation,
            slide_dir(in_end, instroke_dir),
            slide_dir(out_end, outstroke_dir),
        );

        // The corner's first node takes the place of the target node, and its
        // last node starts the outstroke. Curved strokes are cut as far along
        // them as the fitted ends were from the origin before the corner slid
        // into place, and the instroke's cut takes the first node's place.
        let first = component.ends().0;
        let target_idx = point_idx;
        let mut point_idx = point_idx;
        match instroke {
            PathSeg::Cubic(_) if straight_instroke => {
                point_idx = path.straighten_instroke(point_idx, first);
            }
            PathSeg::Cubic(cubic) => {
                let t = t_at_distance(instroke.reverse(), instroke_cut);
                path.replace_instroke(point_idx, cubic.subsegment(0.0..1.0 - t));
            }
            _ => path.set_point(point_idx, first),
        }
        let corner_nodes = &mut component.corner_path.nodes;
        for node in corner_nodes.iter_mut().skip(1) {
            *node = node.ot_round();
        }
        let insert_pt = path.next_idx(point_idx);
        path.nodes
            .splice(insert_pt..insert_pt, corner_nodes.iter().skip(1).cloned());
        let added_points = corner_nodes.len() - 1;
        // 'prev' because the last point we inserted is the new outstroke start
        let new_outstroke_idx = path.prev_idx(insert_pt + added_points);
        let last = component.ends().1;
        match outstroke {
            PathSeg::Cubic(_) if uncut_outstroke => (),
            PathSeg::Cubic(cubic) => {
                let t = t_at_distance(outstroke, outstroke_cut);
                let mut rest = cubic.subsegment(t..1.0);
                rest.p0 = last;
                path.replace_outstroke(new_outstroke_idx, rest);
            }
            _ => path.set_point(new_outstroke_idx, last),
        }

        // and finally, add any new extra paths
        for mut path in component.other_paths.into_iter() {
            path.nodes
                .iter_mut()
                .for_each(|node| *node = node.ot_round());
            self.shapes.push(Shape::Path(path));
        }

        // how far this moved the nodes after the target node along
        Ok(added_points as isize - (target_idx - point_idx) as isize)
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

    /// The segments either side of the line from the node at `idx`, if it starts one.
    fn around_line_from(&self, idx: usize) -> Option<(PathSeg, PathSeg)> {
        let next = self.next_idx(idx);
        if !self.closed
            || self.nodes.len() < 4
            || self.nodes[idx].node_type == NodeType::OffCurve
            || self.nodes[next].node_type == NodeType::OffCurve
        {
            return None;
        }
        let one = line_or_cubic(self.get_previous_segment(idx)).ok()?;
        let two = line_or_cubic(self.get_next_segment(next)).ok()?;
        Some((one, two))
    }

    /// Erase the open corner made by the line from the node at `idx`, if there is one.
    ///
    /// The node moves to where the segments either side of the line cross,
    /// and the line's end node is removed; returns that node's index.
    fn erase_open_corner_at(&mut self, idx: usize) -> Option<usize> {
        let (one, two) = self.around_line_from(idx)?;
        let crossing = open_corner(one, two)?;
        let next = self.next_idx(idx);
        if let PathSeg::Cubic(cubic) = one {
            let before = cubic.subsegment(0.0..crossing.t0);
            let p2 = self.prev_idx(idx);
            let p1 = self.prev_idx(p2);
            self.nodes[p1].pt = before.p1;
            self.nodes[p2].pt = before.p2;
        }
        self.nodes[idx].pt = one.eval(crossing.t0);
        if let PathSeg::Cubic(cubic) = two {
            let after = cubic.subsegment(crossing.t1..1.0);
            let p1 = self.next_idx(next);
            let p2 = self.next_idx(p1);
            self.nodes[p1].pt = after.p1;
            self.nodes[p2].pt = after.p2;
        }
        self.nodes.remove(next);
        Some(next)
    }

    /// Make the cubic ending at `point_idx` a line, ending at `point`.
    ///
    /// Returns the node's index once the handles are gone.
    fn straighten_instroke(&mut self, point_idx: usize, point: Point) -> usize {
        let p2 = self.prev_idx(point_idx);
        let p1 = self.prev_idx(p2);
        let removed_before = [p1, p2].into_iter().filter(|&i| i < point_idx).count();
        self.nodes.remove(p1.max(p2));
        self.nodes.remove(p1.min(p2));
        let idx = point_idx - removed_before;
        self.nodes[idx].node_type = NodeType::Line;
        self.set_point(idx, point);
        idx
    }

    /// Replace the cubic ending at `point_idx`, keeping its start.
    fn replace_instroke(&mut self, point_idx: usize, cubic: CubicBez) {
        let p2 = self.prev_idx(point_idx);
        let p1 = self.prev_idx(p2);
        self.set_point(p1, cubic.p1);
        self.set_point(p2, cubic.p2);
        self.set_point(point_idx, cubic.p3);
    }

    /// Replace the cubic starting at `point_idx`, keeping its end.
    fn replace_outstroke(&mut self, point_idx: usize, cubic: CubicBez) {
        let p1 = self.next_idx(point_idx);
        let p2 = self.next_idx(p1);
        self.set_point(point_idx, cubic.p0);
        self.set_point(p1, cubic.p1);
        self.set_point(p2, cubic.p2);
    }

    /// Reverse the direction of the path.
    ///
    /// Like fontTools' `ReverseContourPen`, a closed path keeps its first node
    /// (which in a .glyphs file is the last one).
    fn reverse_direction(&mut self) {
        self.nodes.reverse();
        if self.closed {
            self.nodes.rotate_left(1);
        }
        // a node's type describes the segment ending at it
        let n = self.nodes.len();
        let types: Vec<_> = (0..n)
            .map(|i| {
                let node = &self.nodes[i];
                let prev = match i {
                    0 if !self.closed => None,
                    _ => Some(&self.nodes[(i + n - 1) % n]),
                };
                let after_off_curve = prev.is_some_and(|p| p.node_type == NodeType::OffCurve);
                let smooth = matches!(node.node_type, NodeType::LineSmooth | NodeType::CurveSmooth);
                match (node.node_type, after_off_curve, smooth) {
                    (NodeType::OffCurve, ..) => NodeType::OffCurve,
                    (_, true, true) => NodeType::CurveSmooth,
                    (_, true, false) => NodeType::Curve,
                    (_, false, true) => NodeType::LineSmooth,
                    (_, false, false) => NodeType::Line,
                }
            })
            .collect();
        for (node, node_type) in self.nodes.iter_mut().zip(types) {
            node.node_type = node_type;
        }
    }
}

#[derive(Clone, Copy)]
enum End {
    First,
    Last,
}

struct CornerComponent {
    corner_path: Path,
    other_paths: Vec<Path>,
    left: Option<Point>,
    right: Option<Point>,
    // the corner's x and y axes, which are mirrored along with it
    axes: [Vec2; 2],
    // the mirror, if the corner was mirrored to fit
    mirror: Option<Affine>,
}

/// What decides how a corner fits the strokes either side of its node.
struct Fitting {
    alignment: Alignment,
    /// The vectors the corner points along the instroke and the outstroke.
    ends: [Vec2; 2],
    /// -1 where an unaligned corner's ends turn the other way, otherwise 1.
    sign: f64,
}

impl CornerComponent {
    fn new(corner_layer: &Layer) -> Result<Self, BadCornerComponentReason> {
        let origin = corner_layer
            .get_anchor_pt("origin")
            .unwrap_or_default()
            .to_vec2();
        let left = corner_layer.get_anchor_pt("left").map(|pt| pt - origin);
        let right = corner_layer.get_anchor_pt("right").map(|pt| pt - origin);

        // Extract the main path and other paths
        let mut path_iter = corner_layer
            .shapes
            .iter()
            .filter_map(Shape::as_path)
            .cloned()
            // apply the origin here
            .map(|mut path| {
                path.nodes.iter_mut().for_each(|node| node.pt -= origin);
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
            axes: [Vec2::new(1.0, 0.0), Vec2::new(0.0, 1.0)],
            mirror: None,
        })
    }

    fn paths_mut(&mut self) -> impl Iterator<Item = &mut Path> {
        std::iter::once(&mut self.corner_path).chain(self.other_paths.iter_mut())
    }

    fn apply_transform(&mut self, transform: Affine) {
        for node in self.paths_mut().flat_map(|path| path.nodes.iter_mut()) {
            node.pt = transform * node.pt;
        }
        for anchor in [&mut self.left, &mut self.right].into_iter().flatten() {
            *anchor = transform * *anchor;
        }
    }

    fn ends(&self) -> (Point, Point) {
        // by construction we have at least two nodes
        let nodes = &self.corner_path.nodes;
        (nodes[0].pt, nodes[nodes.len() - 1].pt)
    }

    /// The vectors the corner points along the instroke and the outstroke.
    fn pointing_ends(&self) -> (Vec2, Vec2) {
        let (first, last) = self.ends();
        (
            self.left.unwrap_or(first).to_vec2(),
            self.right.unwrap_or(last).to_vec2(),
        )
    }

    fn reverse_paths(&mut self) {
        self.paths_mut().for_each(Path::reverse_direction);
    }

    /// Reflect across the line from the origin to the first node.
    ///
    /// Unlike flipping with a negative scale, the path keeps its direction.
    fn mirror(&mut self) {
        let angle = self.ends().0.to_vec2().atan2();
        let mirror =
            Affine::rotate(angle) * Affine::scale_non_uniform(1.0, -1.0) * Affine::rotate(-angle);
        self.apply_transform(mirror);
        self.axes = self.axes.map(|axis| (mirror * axis.to_point()).to_vec2());
        self.mirror = Some(mirror);
    }

    /// The indices of an end node and its neighbour in the corner path.
    fn end_indices(&self, end: End) -> (usize, usize) {
        let n = self.corner_path.nodes.len();
        match end {
            End::First => (0, 1),
            End::Last => (n - 1, n - 2),
        }
    }

    /// How far the corner turns as a whole to fit strokes aimed along `directions`.
    fn rotation(&self, fitting: &Fitting, directions: [Vec2; 2]) -> f64 {
        let [x_axis, y_axis] = self.axes;
        // an end on the origin points along the corner's y axis
        let [in_turn, out_turn] = [0, 1].map(|i| {
            let end = match fitting.ends[i] {
                Vec2::ZERO => y_axis,
                end => end,
            };
            turn_towards(end, directions[i])
        });
        match fitting.alignment {
            Alignment::OutStroke => out_turn,
            Alignment::InStroke => in_turn,
            // with an end on the origin, Glyphs turns the corner's x axis to
            // the host's bisector
            Alignment::Middle if fitting.ends.contains(&Vec2::ZERO) => {
                let [a, b] = directions.map(Vec2::atan2);
                a + ieee_remainder(b - a, std::f64::consts::TAU) / 2.0 - x_axis.atan2()
            }
            Alignment::Middle => {
                in_turn + ieee_remainder(out_turn - in_turn, std::f64::consts::TAU) / 2.0
            }
            _ => 0.0,
        }
    }

    /// Find how Glyphs turns one end of the corner path to fit its stroke.
    ///
    /// The corner as a whole turns to fit the strokes, which point along
    /// `directions`, and the end does the rest of the turning. If the end's
    /// segment runs within 30 degrees of the line from the origin to the end's
    /// anchor (or without one, to the end node), the end node turns around the
    /// anchor (or the origin), taking its handle with it; so does an end on
    /// the origin. Otherwise Glyphs shears the end instead, along whichever of
    /// the corner's axes is nearer that line: points keep their distance from
    /// the anchor along the axis, and the axis turns to fit.
    ///
    /// When `aiming` an end again, Glyphs turns the corner about the end's
    /// anchor rather than the origin, and moves the unit vectors along the
    /// end's line and segment as though they were points.
    ///
    /// Returns `None` if the end doesn't move.
    fn end_fit(
        &self,
        end: End,
        fitting: &Fitting,
        directions: [Vec2; 2],
        aiming: bool,
    ) -> Option<Affine> {
        let (end_idx, neighbour_idx) = self.end_indices(end);
        let (i, anchor) = match end {
            End::First => (0, self.left),
            End::Last => (1, self.right),
        };
        let nodes = &self.corner_path.nodes;
        let segment = nodes[neighbour_idx].pt - nodes[end_idx].pt;
        if segment == Vec2::ZERO {
            return None;
        }
        let pivot = anchor.unwrap_or_default();
        let towards = fitting.ends[i];
        let (turn, along) = if towards == Vec2::ZERO {
            // an end on the origin does all of its turning itself
            (turn_towards(self.axes[1], directions[i]), true)
        } else {
            let turned = Affine::rotate(self.rotation(fitting, directions));
            let mut line = (turned * towards.normalize().to_point()).to_vec2();
            let mut segment = (turned * segment.normalize().to_point()).to_vec2();
            if aiming && pivot != Point::ZERO {
                let offset = self.anchor_turn_offset(turned, pivot);
                line += offset;
                segment += offset;
            }
            (
                turn_towards(line, directions[i]),
                line.cross(segment).abs() < 0.5,
            )
        };
        let turn = fitting.sign * turn;
        let fit = if along {
            Affine::rotate(turn)
        } else if turn.cos().abs() <= 1e-4 {
            // the axis would turn parallel to its stroke
            return None;
        } else {
            let [x_axis, y_axis] = self.axes;
            let axis = if y_axis.dot(towards).abs() > x_axis.dot(towards).abs() {
                y_axis
            } else {
                x_axis
            };
            shear_across(axis, turn)
        };
        let pivot = pivot.to_vec2();
        Some(Affine::translate(pivot) * fit * Affine::translate(-pivot))
    }

    /// Move an end node by `fit`, along with its handle if it has one.
    fn apply_end_fit(&mut self, end: End, fit: Affine) {
        let (end_idx, neighbour_idx) = self.end_indices(end);
        let moving = if self.corner_path.nodes[neighbour_idx].node_type == NodeType::OffCurve {
            &[end_idx, neighbour_idx][..]
        } else {
            &[end_idx][..]
        };
        for &idx in moving {
            let node = &mut self.corner_path.nodes[idx];
            node.pt = fit * node.pt;
        }
    }

    /// How much further Glyphs moves points turning about `anchor`.
    ///
    /// `turned` turns the corner about the origin. For a corner it mirrors to
    /// fit, Glyphs takes the anchor from before the mirror, and adds that
    /// anchor mirrored across the y axis and turned, rather than subtracting
    /// the turned anchor.
    fn anchor_turn_offset(&self, turned: Affine, anchor: Point) -> Vec2 {
        match self.mirror {
            None => anchor - turned * anchor,
            Some(mirror) => {
                let anchor = mirror * anchor;
                anchor.to_vec2() + (turned * mirror * Point::new(-anchor.x, anchor.y)).to_vec2()
            }
        }
    }

    /// Turn the corner by `rotation` and move its origin onto the target node.
    ///
    /// Glyphs then slides the corner along the stroke it is aligned to until
    /// its left anchor sits on the instroke, or its right anchor on the
    /// outstroke.
    fn place(
        &mut self,
        alignment: Alignment,
        node: Point,
        rotation: f64,
        instroke_dir: Vec2,
        outstroke_dir: Vec2,
    ) {
        let mut transform = Affine::translate(node.to_vec2()) * Affine::rotate(rotation);
        // the other stroke is taken to run straight from the node along its aim
        let slide = match alignment {
            Alignment::OutStroke => self.left.map(|left| (left, outstroke_dir, instroke_dir)),
            Alignment::InStroke => self.right.map(|right| (right, instroke_dir, outstroke_dir)),
            _ => None,
        };
        if let Some((anchor, along, onto)) = slide
            && along != Vec2::ZERO
            && let Some(distance) =
                distance_to_line(transform * anchor, along, Line::new(node, node + onto))
        {
            transform = Affine::translate(along * distance) * transform;
        }
        self.apply_transform(transform);
    }
}

fn line_or_cubic(seg: Option<PathSeg>) -> Result<PathSeg, BadCornerComponentReason> {
    match seg {
        Some(PathSeg::Quad(_)) => Err(BadCornerComponentReason::Quadratic),
        Some(seg) => Ok(seg),
        None => Err(BadCornerComponentReason::PathTooShort),
    }
}

/// The unit vector a corner end aims along, `distance` along `stroke`.
///
/// On a curve, the end aims at the point that far along it. An end at the
/// origin would aim at the stroke's start itself, so there Glyphs aims along
/// the stroke as it leaves its start instead, as it always does on a line.
/// Along a stroke of no length, Glyphs aims straight up.
fn aim_along(stroke: PathSeg, distance: f64) -> Vec2 {
    let cubic = stroke.to_cubic();
    let target = match stroke {
        PathSeg::Line(_) => cubic.p0,
        curve => curve.eval(t_at_distance(curve, distance)),
    };
    [target, cubic.p1, cubic.p2, cubic.p3]
        .into_iter()
        .map(|pt| pt - cubic.p0)
        .find(|dir| *dir != Vec2::ZERO)
        .map(Vec2::normalize)
        .unwrap_or(Vec2::new(0.0, 1.0))
}

/// The angle that turns `vector` to point along `direction`.
fn turn_towards(vector: Vec2, direction: Vec2) -> f64 {
    if direction == Vec2::ZERO {
        return 0.0;
    }
    ieee_remainder(direction.atan2() - vector.atan2(), std::f64::consts::TAU)
}

/// Shear across the unit vector `axis`, so that it turns by `angle`.
///
/// Points keep their distance along the axis.
fn shear_across(axis: Vec2, angle: f64) -> Affine {
    let Vec2 { x, y } = axis;
    let k = angle.tan();
    Affine::new([
        1.0 - k * x * y,
        k * x * x,
        -k * y * y,
        1.0 + k * x * y,
        0.0,
        0.0,
    ])
}

/// The transform that puts `start` at the origin and `end` on the positive x axis.
fn alignment_transform(start: Point, end: Point) -> Affine {
    Affine::rotate(-(end - start).atan2()) * Affine::translate(-start.to_vec2())
}

/// Glyphs' measure of a cubic's length: the sum of ten chords, evenly spaced in t.
fn glyphs_length(cubic: CubicBez) -> f64 {
    (1..=10)
        .map(|i| {
            cubic
                .eval((i - 1) as f64 / 10.0)
                .distance(cubic.eval(i as f64 / 10.0))
        })
        .sum()
}

/// Find the t at which Glyphs puts a point `distance` along `cubic`.
///
/// This matches GlyphsCore's `GSTForDistance`, which starts from the
/// proportion of the length and refines it four times.
fn glyphs_t_for_distance(cubic: CubicBez, distance: f64) -> f64 {
    let total = glyphs_length(cubic);
    if distance >= total {
        return 1.0;
    }
    if distance <= 0.0 {
        return 0.0;
    }
    let mut t = distance / total;
    for _ in 0..4 {
        t *= (1.0 + distance / glyphs_length(cubic.subsegment(0.0..t))) / 2.0;
    }
    t
}

/// Find the t at which `seg` is `distance` along from its start.
///
/// A line is extended past its ends; a curve stops at them.
fn t_at_distance(seg: PathSeg, distance: f64) -> f64 {
    match seg {
        PathSeg::Line(line) => match line.length() {
            0.0 => 0.0,
            length => distance / length,
        },
        curve => glyphs_t_for_distance(curve.to_cubic(), distance),
    }
}

/// Find how far `point` must move along `direction` to land on `line`.
///
/// `direction` must be a unit vector, and the line is treated as unbounded.
/// The distance may be negative. Returns `None` if `direction` is parallel
/// to the line.
fn distance_to_line(point: Point, direction: Vec2, line: Line) -> Option<f64> {
    let Line { p0, p1 } = alignment_transform(point, point + direction) * line;
    if py_is_close(p0.y, p1.y) {
        return None;
    }
    let t = p0.y / (p0.y - p1.y);
    Some(p0.x + (p1.x - p0.x) * t)
}

fn ieee_remainder(x: f64, y: f64) -> f64 {
    x - (x / y).round_ties_even() * y
}

// https://docs.python.org/3.14/library/math.html#math.isclose
fn py_is_close(a: f64, b: f64) -> bool {
    // abs(a-b) <= max(rel_tol * max(abs(a), abs(b)), abs_tol).
    const REL_TOL: f64 = 1e-09;
    (a - b).abs() <= REL_TOL * a.abs().max(b.abs())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::{Anchor, Font, Scale};
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

    // Each case in CornerComponents.glyphs has a <case>.expectation glyph with
    // Glyphs 3.5's own decomposition of its corners, made by glyphsLib's
    // tests/tools/corner_components_expectations.py.
    //
    // Cases where we don't yet match Glyphs:
    const MISMATCHES: &[&str] = &[];

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

    fn nodes(nodes: &[(i32, i32, char)]) -> Vec<Node> {
        nodes
            .iter()
            .map(|&(x, y, node_type)| Node {
                pt: Point::new(x as f64, y as f64),
                node_type: match node_type {
                    'l' => NodeType::Line,
                    'o' => NodeType::OffCurve,
                    'c' => NodeType::Curve,
                    _ => panic!("unknown node type '{node_type}'"),
                },
            })
            .collect()
    }

    fn corner_hint(node_index: usize) -> Hint {
        Hint {
            type_: HintType::Corner,
            name: "_corner.test".into(),
            shape_index: 0,
            node_index,
            scale: Scale::default(),
            alignment: Alignment::OutStroke,
        }
    }

    fn scale(x: f64, y: f64) -> Scale {
        Scale {
            x: x.into(),
            y: y.into(),
        }
    }

    /// Apply `hints` for a corner drawn as `corner` to the closed path `host`.
    ///
    /// Paths are given as in a .glyphs file, so a closed path starts after
    /// what a UFO would have as its first node.
    fn apply_corners(
        corner: &[(i32, i32, char)],
        anchors: &[(&str, (i32, i32))],
        host: &[(i32, i32, char)],
        hints: &[Hint],
    ) -> Vec<Node> {
        apply_corners_to_paths(corner, anchors, &[host], hints).remove(0)
    }

    /// Apply `hints` for a corner drawn as `corner` to the closed paths `hosts`.
    fn apply_corners_to_paths(
        corner: &[(i32, i32, char)],
        anchors: &[(&str, (i32, i32))],
        hosts: &[&[(i32, i32, char)]],
        hints: &[Hint],
    ) -> Vec<Vec<Node>> {
        let corner = Layer {
            layer_id: "m01".into(),
            shapes: vec![Shape::Path(Path {
                closed: false,
                nodes: nodes(corner),
                ..Default::default()
            })],
            anchors: anchors
                .iter()
                .map(|&(name, (x, y))| Anchor {
                    name: name.into(),
                    pos: Point::new(x as f64, y as f64),
                })
                .collect(),
            ..Default::default()
        };
        let glyphs = BTreeMap::from([(
            SmolStr::new("_corner.test"),
            Glyph {
                name: "_corner.test".into(),
                layers: vec![corner],
                ..Default::default()
            },
        )]);
        let mut layer = Layer {
            layer_id: "m01".into(),
            shapes: hosts
                .iter()
                .map(|host| {
                    Shape::Path(Path {
                        closed: true,
                        nodes: nodes(host),
                        ..Default::default()
                    })
                })
                .collect(),
            hints: hints.to_vec(),
            ..Default::default()
        };
        insert_corner_components_for_layer(&mut layer, &glyphs).unwrap();
        layer
            .shapes
            .iter()
            .filter_map(Shape::as_path)
            .map(|path| path.nodes.clone())
            .collect()
    }

    /// Assert that `run` appears in `nodes`, wrapping around the end.
    fn assert_run<'a>(nodes: impl IntoIterator<Item = &'a Node>, run: &[(i32, i32)]) {
        let points: Vec<_> = nodes
            .into_iter()
            .map(|node| (node.pt.x as i32, node.pt.y as i32))
            .collect();
        let found = (0..points.len())
            .any(|start| (0..run.len()).all(|i| points[(start + i) % points.len()] == run[i]));
        assert!(found, "{run:?} not in {points:?}");
    }

    // Drawn a quarter turn from the usual orientation, like the serif in
    // Alkatra, so the last point has a small negative x.
    #[rstest]
    #[case::line(
        &[(300, 0, 'l'), (300, 300, 'l'), (100, 300, 'l'), (100, 0, 'l')],
        &[(310, 0), (309, 15), (300, 40)],
    )]
    #[case::curve(
        &[(300, 0, 'l'), (310, 100, 'o'), (310, 200, 'o'), (300, 300, 'c'), (100, 300, 'l'), (100, 0, 'l')],
        &[(310, 0), (311, 15), (303, 40)],
    )]
    fn corner_ending_behind_origin(
        #[case] host: &[(i32, i32, char)],
        #[case] expected: &[(i32, i32)],
    ) {
        let corner = [(40, 0, 'l'), (-10, 0, 'l'), (-10, -15, 'l'), (-2, -40, 'l')];
        let result = apply_corners(&corner, &[], host, &[corner_hint(0)]);
        assert_eq!(result.len(), host.len() + corner.len() - 1);
        assert_run(&result, expected);
    }

    // The corner's first handle runs along the stem, so when the stem is a
    // (straight) curve, the first segment never crosses it.
    #[rstest]
    #[case::line(&[(300, 300, 'l'), (100, 300, 'l'), (100, 0, 'l'), (300, 0, 'l')], 2)]
    #[case::curve(
        &[(300, 300, 'l'), (100, 300, 'l'), (100, 200, 'o'), (100, 100, 'o'), (100, 0, 'c'), (300, 0, 'l')],
        4,
    )]
    fn corner_leaving_along_instroke(#[case] host: &[(i32, i32, char)], #[case] node_index: usize) {
        let corner = [
            (0, 30, 'l'),
            (0, 15, 'o'),
            (-10, 0, 'o'),
            (-20, 0, 'c'),
            (10, 0, 'l'),
        ];
        let result = apply_corners(&corner, &[], host, &[corner_hint(node_index)]);
        assert_eq!(result.len(), host.len() + corner.len() - 1);
        assert_run(&result, &[(100, 30), (100, 15), (90, 0), (80, 0), (110, 0)]);
    }

    const SERIF: &[(i32, i32, char)] = &[(0, 96, 'l'), (-68, 96, 'l'), (-68, 0, 'l'), (9, 0, 'l')];

    // Corners taken from real fonts, with the on-curve points Glyphs gave them
    #[rstest]
    // The serif slides along the outstroke until its left anchor is on the
    // instroke. (Glyphs 2 decomposing Montagu Slab's K)
    #[case::left_anchor(
        SERIF,
        &[("left", (0, 45))],
        &[(800, 682, 'l'), (629, 682, 'l'), (184, 283, 'l'), (257, 198, 'l')],
        Hint { scale: scale(0.02, 1.0), ..corner_hint(0) },
        &[(692, 586), (751, 586), (751, 682)],
    )]
    // Flipped, the left anchor becomes the right one, and aligned to the
    // instroke, the serif slides along it until that anchor is on the
    // outstroke. (Ditto)
    #[case::right_anchor(
        SERIF,
        &[("left", (0, 45))],
        &[(796, 0, 'l'), (462, 428, 'l'), (326, 307, 'l'), (558, 0, 'l')],
        Hint { alignment: Alignment::InStroke, scale: scale(-0.2, 1.0), ..corner_hint(0) },
        &[(774, 0), (774, 96), (721, 96)],
    )]
    // The serif turns the other way from the stem, so Glyphs mirrors it.
    // (Glyphs' export of Aoboshi One's L)
    #[case::mirrored(
        &[(0, 30, 'l'), (0, 22, 'o'), (15, 19, 'o'), (22, 19, 'c'), (22, 0, 'l'), (-28, 0, 'l')],
        &[],
        &[(207, 0, 'l'), (207, 750, 'l'), (73, 750, 'l'), (73, 0, 'l')],
        corner_hint(3),
        &[(73, 30), (51, 19), (51, 0)],
    )]
    // Unaligned, the serif isn't mirrored, though it turns the other way from
    // the inside corner of this L. (Glyphs 3.5)
    #[case::unaligned_not_mirrored(
        &[(0, 60, 'l'), (-70, 60, 'l'), (-70, 0, 'l'), (30, 0, 'l')],
        &[],
        &[(100, 100, 'l'), (500, 100, 'l'), (500, 250, 'l'), (250, 250, 'l'), (250, 600, 'l'), (100, 600, 'l')],
        Hint { alignment: Alignment::Unaligned, scale: scale(-1.0, 1.0), ..corner_hint(3) },
        &[(280, 250), (320, 250), (320, 310), (250, 310)],
    )]
    // Aligned to the instroke, a last segment that curves in along the
    // outstroke ends on it, |last node| along. (Aoboshi One's x)
    #[case::curved_end_on_outstroke(
        &[(28, 0, 'l'), (-42, 0, 'l'), (-42, 19, 'l'), (-25, 19, 'o'), (0, 32, 'o'), (0, 40, 'c')],
        &[],
        &[(170, 0, 'l'), (301, 171, 'l'), (344, 222, 'l'), (526, 460, 'l'), (386, 460, 'l'), (31, 0, 'l')],
        Hint { alignment: Alignment::InStroke, scale: scale(1.0, 0.97), ..corner_hint(4) },
        &[(344, 460), (344, 442), (362, 429)],
    )]
    // Drawn a quarter turn round, leaving along the instroke, with a left
    // anchor and a curved outstroke. (Glyphs 3.5's export of Alkatra's l)
    #[case::quarter_turn(
        &[
            (175, 491, 'l'), (158, 491, 'o'), (57, 473, 'o'), (57, 444, 'c'), (57, 432, 'o'),
            (65, 422, 'o'), (69, 410, 'c'), (76, 392, 'o'), (81, 361, 'o'), (78, 330, 'c'),
        ],
        &[("origin", (95, 491)), ("left", (175, 491)), ("right", (75, 304))],
        &[(188, -14, 'l'), (195, 72, 'o'), (201, 177, 'o'), (214, 273, 'c'), (58, 233, 'l'), (101, -14, 'l')],
        corner_hint(0),
        &[(108, -14), (225, 36), (212, 70), (201, 150)],
    )]
    // Drawn tilted and mirrored to fit the inside corner of an L, the serif is
    // sheared along its own axes, which are mirrored with it. (Glyphs 3.5)
    #[case::mirrored_axes(
        &[(-36, 48, 'l'), (-92, 6, 'l'), (-56, -42, 'l'), (24, 18, 'l')],
        &[("left", (-26, 18))],
        &[(100, 100, 'l'), (500, 100, 'l'), (500, 250, 'l'), (250, 250, 'l'), (250, 600, 'l'), (100, 600, 'l')],
        corner_hint(3),
        &[(313, 262), (310, 190), (250, 190), (250, 290)],
    )]
    // With its first node on the origin, the corner counts as an outside
    // corner, so Glyphs mirrors it to fit the inside corner of an L. (Glyphs
    // 3.5)
    #[case::origin_end_mirrored(
        &[(0, 0, 'l'), (-30, 50, 'l'), (20, 70, 'l'), (60, 0, 'l')],
        &[],
        &[(100, 100, 'l'), (500, 100, 'l'), (500, 250, 'l'), (250, 250, 'l'), (250, 600, 'l'), (100, 600, 'l')],
        corner_hint(3),
        &[(250, 250), (300, 220), (320, 270), (250, 310)],
    )]
    // Ditto with its right anchor on the origin; in the middle, its mirrored x
    // axis turns to the bisector of the L's inside corner. (Glyphs 3.5)
    #[case::origin_anchor_middle(
        &[(0, 60, 'l'), (-70, 60, 'l'), (-70, 0, 'l'), (30, 0, 'l')],
        &[("right", (0, 0))],
        &[(100, 100, 'l'), (500, 100, 'l'), (500, 250, 'l'), (250, 250, 'l'), (250, 600, 'l'), (100, 600, 'l')],
        Hint { alignment: Alignment::Middle, ..corner_hint(3) },
        &[(335, 250), (243, 158), (201, 201), (271, 271)],
    )]
    // The corner's first node reaches past the start of the short curved
    // instroke, so Glyphs straightens it, and ends it at the corner's first
    // node like a line. (Iansui's uni5320)
    #[case::straightened_instroke(
        &[
            (0, 168, 'l'), (-1, 47, 'l'), (-1, 26, 'o'), (4, 7, 'o'), (21, -2, 'c'), (27, -6, 'o'),
            (33, -7, 'o'), (39, -7, 'c'), (48, -7, 'o'), (57, -3, 'o'), (69, -3, 'c'), (184, 0, 'l'),
        ],
        &[],
        &[
            (913, -31, 'l'), (913, 300, 'l'), (159, 300, 'l'), (159, 72, 'l'), (159, 56, 'o'),
            (153, -36, 'o'), (158, -52, 'c'),
        ],
        corner_hint(6),
        &[(159, 72), (159, 116), (156, -5), (179, -53), (197, -58), (227, -53), (342, -47), (913, -31)],
    )]
    fn corner_matches_glyphs(
        #[case] corner: &[(i32, i32, char)],
        #[case] anchors: &[(&str, (i32, i32))],
        #[case] host: &[(i32, i32, char)],
        #[case] hint: Hint,
        #[case] expected: &[(i32, i32)],
    ) {
        let result = apply_corners(corner, anchors, host, &[hint]);
        let on_curves = result
            .iter()
            .filter(|node| node.node_type != NodeType::OffCurve);
        assert_run(on_curves, expected);
    }

    // The first corner replaces the node, so in Glyphs a second one on the
    // same node does nothing. (Aoboshi One's a.ss01)
    #[test]
    fn second_corner_on_a_node_is_ignored() {
        let corner = [(0, 50, 'l'), (-50, 50, 'l'), (-50, 0, 'l')];
        let host = [(100, 100, 'l'), (500, 100, 'l'), (100, 500, 'l')];
        assert_eq!(
            apply_corners(&corner, &[], &host, &[corner_hint(0), corner_hint(0)]),
            apply_corners(&corner, &[], &host, &[corner_hint(0)]),
        );
    }

    // Hints that point at an off-curve node (Iansui's uni7BE1) or at a path
    // that isn't there (Aoboshi One's uni5B57) are skipped, not fatal.
    #[rstest]
    #[case::off_curve_node(Hint { node_index: 2, ..corner_hint(0) })]
    #[case::missing_path(Hint { shape_index: 9, ..corner_hint(0) })]
    fn bad_hint_is_skipped(#[case] hint: Hint) {
        let corner = [(0, 50, 'l'), (-50, 50, 'l'), (-50, 0, 'l')];
        let host = [
            (100, 100, 'l'),
            (500, 100, 'l'),
            (500, 300, 'o'),
            (300, 500, 'o'),
            (100, 500, 'c'),
        ];
        let result = apply_corners(&corner, &[], &host, &[hint]);
        assert_eq!(result, nodes(&host));
    }

    // An open corner: the stroke up overshoots to (500,340), the line goes on
    // to (520,320), and the stroke back left crosses it at (500,320).
    // Glyphs erases it before applying corners, so a corner on the line's end
    // goes with that node.
    #[test]
    fn corner_on_open_corner_end_is_lost() {
        let corner = [(0, 50, 'l'), (-50, 50, 'l'), (-50, 0, 'l')];
        let host = [
            (100, 100, 'l'),
            (500, 100, 'l'),
            (500, 340, 'l'),
            (520, 320, 'l'),
            (100, 320, 'l'),
        ];
        let result = apply_corners(&corner, &[], &host, &[corner_hint(3)]);
        let erased = [
            (100, 100, 'l'),
            (500, 100, 'l'),
            (500, 320, 'l'),
            (100, 320, 'l'),
        ];
        assert_eq!(result, nodes(&erased));
    }

    // Once a glyph has a corner to apply, Glyphs erases the open corners in
    // all of its paths, not just the ones with corners. (Glyphs 3.5)
    #[test]
    fn open_corners_erased_in_every_path() {
        let corner = [(0, 50, 'l'), (-50, 50, 'l'), (-50, 0, 'l')];
        let host: &[_] = &[
            (100, 100, 'l'),
            (500, 100, 'l'),
            (500, 340, 'l'),
            (520, 320, 'l'),
            (100, 320, 'l'),
        ];
        let result = apply_corners_to_paths(&corner, &[], &[host, host], &[corner_hint(0)]);
        let erased = [
            (100, 100, 'l'),
            (500, 100, 'l'),
            (500, 320, 'l'),
            (100, 320, 'l'),
        ];
        assert_eq!(result[1], nodes(&erased));
    }

    #[rstest]
    #[case::open(
        false,
        &[(0, 0, 'l'), (10, 0, 'l'), (20, 0, 'o'), (30, 10, 'o'), (30, 20, 'c'), (30, 30, 'l')],
        &[(30, 30, 'l'), (30, 20, 'l'), (30, 10, 'o'), (20, 0, 'o'), (10, 0, 'c'), (0, 0, 'l')],
    )]
    // keeps the node a UFO would start with, which .glyphs has last
    #[case::closed(
        true,
        &[(0, 0, 'l'), (10, 0, 'l'), (20, 0, 'o'), (30, 10, 'o'), (30, 20, 'c')],
        &[(30, 10, 'o'), (20, 0, 'o'), (10, 0, 'c'), (0, 0, 'l'), (30, 20, 'l')],
    )]
    fn reverse_direction(
        #[case] closed: bool,
        #[case] before: &[(i32, i32, char)],
        #[case] after: &[(i32, i32, char)],
    ) {
        let mut path = Path {
            closed,
            nodes: nodes(before),
            ..Default::default()
        };
        path.reverse_direction();
        assert_eq!(path.nodes, nodes(after));
    }
}
