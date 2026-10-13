//! Corner components in UFO sources
//!
//! glyphsLib keeps Glyphs' corner components in a UFO as `com.schriftgestaltung.hints`
//! in the lib of the host glyph, next to the `_corner.*` glyphs that hold the
//! corners themselves, and lists `glyphsLib.filters.cornerComponents` in the
//! ufo2ft filters so that ufo2ft applies them.
//!
//! We apply them with the code we use for .glyphs files, by converting the
//! contours to the types of glyphs-reader and back. See
//! <https://github.com/googlefonts/glyphsLib/blob/main/Lib/glyphsLib/filters/cornerComponents.py>

use std::collections::{HashMap, HashSet};

use glyphs_reader::{
    Alignment, Anchor, BadCornerComponent, Hint, HintType, Layer, Node, NodeType, Path, Scale,
    Shape, apply_corner_components,
};
use kurbo::Point;
use norad::{Contour, ContourPoint, Glyph, LayerReader, PointType};
use ordered_float::OrderedFloat;
use plist::{Dictionary, Value};
use smol_str::SmolStr;

/// <https://github.com/googlefonts/glyphsLib/blob/de5b4e34/Lib/glyphsLib/builder/constants.py#L62>
const HINTS_LIB_KEY: &str = "com.schriftgestaltung.hints";
/// One character per shape of a glyph that mixes paths and components, 'P' or 'C'
const SHAPE_ORDER_LIB_KEY: &str = "com.schriftgestaltung.Glyphs.shapeOrder";

/// Apply the corner components in the lib of `glyph` to its contours.
///
/// The `_corner.*` glyphs are looked up in `corner_glyphs`, which is the
/// default layer of the UFO, as ufo2ft does.
///
/// Returns true if this also erased the open corners of the glyph, which
/// shouldn't then be erased again.
pub(crate) fn apply(
    glyph: &mut Glyph,
    corner_glyphs: Option<LayerReader<'_>>,
) -> Result<bool, BadCornerComponent> {
    let Some(hints) = glyph.lib.get(HINTS_LIB_KEY).and_then(Value::as_array) else {
        return Ok(false);
    };
    if glyph.contours.is_empty() {
        return Ok(false);
    }

    let path_indices = path_indices(glyph);
    let mut corners: HashMap<SmolStr, Layer> = HashMap::new();
    let mut corner_nodes = HashSet::new();
    let mut applicable = Vec::new();
    for hint in hints
        .iter()
        .filter_map(Value::as_dictionary)
        .filter_map(corner_hint)
    {
        let (shape_index, node_index) = (hint.shape_index, hint.node_index);
        // Glyphs ignores a corner on a component, or on a shape that isn't there any more
        let Some(path_index) = path_indices
            .get(shape_index)
            .copied()
            .flatten()
            .filter(|index| *index < glyph.contours.len())
        else {
            log::warn!(
                "ignoring corner component '{}' in '{}': shape {shape_index} is not a path",
                hint.name,
                glyph.name()
            );
            continue;
        };
        let contour = &glyph.contours[path_index];
        // glyphsLib starts a closed UFO contour at the last node of the path in
        // Glyphs, but not an open one. Its filter takes the node after the
        // hint's for both, which puts a corner on an open path one node late;
        // we use the hint's node, as we do for a .glyphs file.
        let target_index = if contour.is_closed() {
            node_index + 1
        } else {
            node_index
        };
        let target = contour
            .points
            .get(target_index % contour.points.len().max(1));
        if target.is_none_or(|pt| pt.typ == PointType::OffCurve) {
            log::warn!(
                "ignoring corner component '{}' in '{}': node {node_index} of shape \
                 {shape_index} is an off-curve point",
                hint.name,
                glyph.name()
            );
            continue;
        }
        if !corners.contains_key(&hint.name) {
            let Some(corner) = corner_glyph(corner_glyphs, &hint.name) else {
                log::warn!(
                    "corner component '{}' in '{}' not found",
                    hint.name,
                    glyph.name()
                );
                continue;
            };
            corners.insert(hint.name.clone(), corner);
        }
        // A corner replaces the node it is attached to, so like Glyphs we
        // only apply the first one on each node.
        if !corner_nodes.insert((path_index, node_index)) {
            log::warn!(
                "ignoring corner component '{}' in '{}': there is already a corner on that node",
                hint.name,
                glyph.name()
            );
            continue;
        }
        applicable.push(Hint {
            shape_index: path_index,
            ..hint
        });
    }
    if applicable.is_empty() {
        return Ok(false);
    }

    let mut layer = Layer {
        shapes: glyph
            .contours
            .iter()
            .map(to_path)
            .map(Shape::Path)
            .collect(),
        ..Default::default()
    };
    apply_corner_components(&mut layer, applicable, |name| {
        // we only apply hints for corners we found
        Ok(&corners[name])
    })?;

    let mut new_contours = layer
        .shapes
        .iter()
        .filter_map(|shape| match shape {
            Shape::Path(path) => Some(path),
            _ => None,
        })
        .map(to_contour);
    for contour in glyph.contours.iter_mut() {
        if let Some(new) = new_contours.next() {
            contour.points = new.points;
        }
    }
    // and any extra contours of the corners
    glyph.contours.extend(new_contours);
    Ok(layer.open_corners_erased)
}

/// For each shape of the glyph, the index of the contour it is, if it is one.
fn path_indices(glyph: &Glyph) -> Vec<Option<usize>> {
    let Some(shape_order) = glyph
        .lib
        .get(SHAPE_ORDER_LIB_KEY)
        .and_then(Value::as_string)
    else {
        // all contours, as there are no components to be told apart
        return (0..glyph.contours.len()).map(Some).collect();
    };
    let mut n_paths = 0;
    shape_order
        .chars()
        .map(|c| {
            (c == 'P').then(|| {
                n_paths += 1;
                n_paths - 1
            })
        })
        .collect()
}

/// A corner component hint from a glyph lib; any other kind of hint is `None`.
fn corner_hint(hint: &Dictionary) -> Option<Hint> {
    if !hint
        .get("type")?
        .as_string()?
        .eq_ignore_ascii_case("corner")
    {
        return None;
    }
    let name = hint.get("name")?.as_string()?;
    let origin = hint.get("origin")?.as_array()?;
    let (shape_index, node_index) = (
        usize::try_from(plist_integer(origin.first()?)?).ok()?,
        usize::try_from(plist_integer(origin.get(1)?)?).ok()?,
    );
    let options = hint.get("options").and_then(plist_integer).unwrap_or(0);
    let scale = hint
        .get("scale")
        .and_then(Value::as_array)
        .and_then(|scale| Some((plist_number(scale.first()?)?, plist_number(scale.get(1)?)?)))
        .map(|(x, y)| Scale {
            x: OrderedFloat(x),
            y: OrderedFloat(y),
        })
        .unwrap_or_default();
    Some(Hint {
        type_: HintType::Corner,
        name: name.into(),
        shape_index,
        node_index,
        scale,
        alignment: Alignment::from_option(options),
    })
}

fn plist_integer(value: &Value) -> Option<i64> {
    value
        .as_signed_integer()
        .or_else(|| i64::try_from(value.as_unsigned_integer()?).ok())
}

fn plist_number(value: &Value) -> Option<f64> {
    value
        .as_real()
        .or_else(|| plist_integer(value).map(|i| i as f64))
}

/// The corner component glyph `name`, with its anchors, as a layer.
fn corner_glyph(corner_glyphs: Option<LayerReader<'_>>, name: &str) -> Option<Layer> {
    let glyph = corner_glyphs?.load_glyph(name)?;
    let glyph = match glyph {
        Ok(glyph) => glyph,
        Err(e) => {
            log::warn!("unable to load corner component '{name}': {e}");
            return None;
        }
    };
    Some(Layer {
        shapes: glyph
            .contours
            .iter()
            .map(to_path)
            .map(Shape::Path)
            .collect(),
        anchors: glyph
            .anchors
            .iter()
            .filter_map(|anchor| {
                Some(Anchor {
                    name: anchor.name.as_ref()?.as_str().into(),
                    pos: Point::new(anchor.x, anchor.y),
                })
            })
            .collect(),
        ..Default::default()
    })
}

/// A contour as a path of a .glyphs file.
///
/// Glyphs stores the start of a closed path as its last node, and glyphsLib
/// writes it out first, so we rotate the nodes back.
fn to_path(contour: &Contour) -> Path {
    let closed = contour.is_closed();
    let mut nodes: Vec<_> = contour
        .points
        .iter()
        .map(|pt| Node {
            pt: Point::new(pt.x, pt.y),
            node_type: match (pt.typ, pt.smooth) {
                (PointType::OffCurve, _) => NodeType::OffCurve,
                (PointType::Move | PointType::Line, false) => NodeType::Line,
                (PointType::Move | PointType::Line, true) => NodeType::LineSmooth,
                (PointType::Curve, false) => NodeType::Curve,
                (PointType::Curve, true) => NodeType::CurveSmooth,
                (PointType::QCurve, false) => NodeType::QCurve,
                (PointType::QCurve, true) => NodeType::QCurveSmooth,
            },
        })
        .collect();
    if closed {
        nodes.rotate_left(1);
    }
    Path {
        closed,
        nodes,
        ..Default::default()
    }
}

/// A path as a contour, the opposite of [`to_path`].
fn to_contour(path: &Path) -> Contour {
    let mut nodes: Vec<_> = path.nodes.iter().collect();
    if path.closed {
        nodes.rotate_right(1);
    }
    let points = nodes
        .into_iter()
        .enumerate()
        .map(|(i, node)| {
            let (typ, smooth) = match node.node_type {
                NodeType::OffCurve => (PointType::OffCurve, false),
                // the start of an open path
                _ if i == 0 && !path.closed => (PointType::Move, false),
                NodeType::Line => (PointType::Line, false),
                NodeType::LineSmooth => (PointType::Line, true),
                NodeType::Curve => (PointType::Curve, false),
                NodeType::CurveSmooth => (PointType::Curve, true),
                NodeType::QCurve => (PointType::QCurve, false),
                NodeType::QCurveSmooth => (PointType::QCurve, true),
            };
            ContourPoint::new(node.pt.x, node.pt.y, typ, smooth, None, None)
        })
        .collect();
    Contour::new(points, None)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn point(x: f64, y: f64, typ: PointType, smooth: bool) -> ContourPoint {
        ContourPoint::new(x, y, typ, smooth, None, None)
    }

    #[test]
    fn closed_contours_start_at_the_last_node() {
        let contour = Contour::new(
            vec![
                point(0.0, 0.0, PointType::Line, false),
                point(10.0, 0.0, PointType::Line, false),
                point(10.0, 10.0, PointType::Curve, true),
            ],
            None,
        );
        let path = to_path(&contour);
        assert!(path.closed);
        assert_eq!(
            path.nodes.iter().map(|n| n.pt).collect::<Vec<_>>(),
            vec![
                Point::new(10.0, 0.0),
                Point::new(10.0, 10.0),
                Point::new(0.0, 0.0)
            ]
        );
        assert_eq!(path.nodes[1].node_type, NodeType::CurveSmooth);
        assert_eq!(to_contour(&path), contour);
    }

    #[test]
    fn open_contours_keep_their_order() {
        let contour = Contour::new(
            vec![
                point(0.0, 0.0, PointType::Move, false),
                point(10.0, 0.0, PointType::Line, false),
            ],
            None,
        );
        let path = to_path(&contour);
        assert!(!path.closed);
        assert_eq!(path.nodes[0].pt, Point::new(0.0, 0.0));
        assert_eq!(to_contour(&path), contour);
    }

    fn hint(name: &str, origin: [i64; 2]) -> Dictionary {
        let mut hint = Dictionary::new();
        hint.insert("type".into(), "Corner".into());
        hint.insert("name".into(), name.into());
        hint.insert(
            "origin".into(),
            Value::Array(origin.map(Value::from).to_vec()),
        );
        hint
    }

    #[test]
    fn corner_hints() {
        let mut dict = hint("_corner.a", [1, 2]);
        dict.insert("options".into(), 1.into());
        dict.insert(
            "scale".into(),
            Value::Array(vec![Value::Real(1.5), Value::from(2)]),
        );
        let hint = corner_hint(&dict).unwrap();
        assert_eq!(hint.name, "_corner.a");
        assert_eq!((hint.shape_index, hint.node_index), (1, 2));
        assert_eq!(hint.alignment, Alignment::InStroke);
        assert_eq!((hint.scale.x.0, hint.scale.y.0), (1.5, 2.0));
    }

    #[test]
    fn only_corner_hints_count() {
        let mut dict = hint("_cap.a", [0, 0]);
        dict.insert("type".into(), "Cap".into());
        assert!(corner_hint(&dict).is_none());
        // glyphsLib compares the type case insensitively
        let mut dict = hint("_corner.a", [0, 0]);
        dict.insert("type".into(), "CORNER".into());
        assert!(corner_hint(&dict).is_some());
    }

    // glyphsLib logs a warning and ignores a corner on a shape that isn't there
    #[test]
    fn stale_shape_order_does_not_panic() {
        let mut glyph = Glyph::new("a");
        glyph.contours = vec![Contour::new(
            vec![
                point(0.0, 0.0, PointType::Line, false),
                point(10.0, 0.0, PointType::Line, false),
                point(10.0, 10.0, PointType::Line, false),
            ],
            None,
        )];
        glyph
            .lib
            .insert(SHAPE_ORDER_LIB_KEY.into(), Value::from("PP"));
        glyph.lib.insert(
            HINTS_LIB_KEY.into(),
            Value::Array(vec![Value::Dictionary(hint("_corner.a", [1, 0]))]),
        );
        assert!(!apply(&mut glyph, None).unwrap());
    }

    #[test]
    fn shape_order_maps_shapes_to_contours() {
        let mut glyph = Glyph::new("a");
        glyph.contours = vec![Contour::default(), Contour::default()];
        assert_eq!(path_indices(&glyph), vec![Some(0), Some(1)]);
        glyph
            .lib
            .insert(SHAPE_ORDER_LIB_KEY.into(), Value::from("PCP"));
        assert_eq!(path_indices(&glyph), vec![Some(0), None, Some(1)]);
    }
}
