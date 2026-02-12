use std::{
    collections::{BTreeMap, HashMap},
    str::FromStr,
};
use thiserror::Error;

use fontdrasil::{
    coords::{NormalizedCoord, NormalizedLocation},
    types::Tag,
    variations::{RoundingBehaviour, VariationModel},
};
use kurbo::{Affine, Point, Vec2};
use smol_str::SmolStr;

use crate::{Anchor, Component, Glyph, Layer, Node, Shape, font::AxisPole};

/// Result of instantiating a smart component.
///
/// Contains both the interpolated shapes and anchors from the smart component.
pub(crate) struct SmartComponentInstance {
    pub shapes: Vec<Shape>,
    pub anchors: Vec<Anchor>,
}

/// Things that can go wrong when instantiating a smart component
#[derive(Debug, Error)]
pub enum BadSmartComponent {
    #[error("unknown component axis '{0}'")]
    UnknownAxis(SmolStr),
    #[error("strange mapping for axis '{axis}': base {base:?} child {child:?}")]
    StrangeAxisMapping {
        base: Option<AxisPole>,
        child: AxisPole,
        axis: SmolStr,
    },
    #[error("Smart component layers are not interpolation compatible")]
    IncoherentLayers,
    #[error("Axis '{0}' is not defined for the default layer")]
    AxisUndefinedForDefaultLayer(SmolStr),
    #[error("No layers exist with associated id '{0}'")]
    NoLayer(String),
}

/// Instantiate an instance of a smart component.
///
/// A smart component is a glyph that defines its own little variation space,
/// such that specific instances of the glyph can be included as components of
/// other glyphs.
///
/// See <https://glyphsapp.com/learn/smart-components>.
///
/// This code is based on <https://github.com/googlefonts/glyphsLib/blob/52c982399b/Lib/glyphsLib/builder/smart_components.py#L96>
pub(crate) fn instantiate_for_layer(
    layer_master_id: &str,
    component: &Component,
    ref_glyph: &Glyph,
) -> Result<SmartComponentInstance, BadSmartComponent> {
    assert!(!component.smart_component_values.is_empty());
    assert!(!ref_glyph.smart_component_axes.is_empty());
    let (axis_order, name_to_tag_map) = axes_for_glyph(ref_glyph);

    // these are the layers of the glyph that have the same associated master
    // as the layer that we are instantiating for.
    let mut relevant_layers = ref_glyph
        .layers
        .iter()
        .filter(|layer| {
            !layer.smart_component_positions.is_empty() && layer.master_id() == layer_master_id
        })
        .collect::<Vec<_>>();

    // make sure default layer is first (in python this happens in the iterator itself)
    // https://github.com/googlefonts/glyphsLib/blob/52c98239/Lib/glyphsLib/classes.py#L555
    if let Some(idx) = relevant_layers
        .iter()
        .position(|l| l.layer_id == layer_master_id)
        .filter(|idx| *idx != 0)
    {
        relevant_layers.swap(0, idx);
    }

    if relevant_layers.is_empty() {
        return Err(BadSmartComponent::NoLayer(layer_master_id.to_string()));
    }
    if relevant_layers.len() == 1 {
        log::debug!("smart component {} only has one layer?", component.name);
        let mut shapes = relevant_layers[0].shapes.clone();
        shapes
            .iter_mut()
            .for_each(|shape| shape.apply_affine(component.transform));
        let anchors = relevant_layers[0]
            .anchors
            .iter()
            .map(|a| Anchor {
                name: a.name.clone(),
                pos: component.transform * a.pos,
            })
            .collect();
        return Ok(SmartComponentInstance { shapes, anchors });
    }

    validate_relevant_layers(&relevant_layers)?;
    for axis in ref_glyph.smart_component_axes.keys() {
        if !relevant_layers[0]
            .smart_component_positions
            .contains_key(axis)
        {
            return Err(BadSmartComponent::UnknownAxis(axis.clone()));
        }
    }

    let locations = relevant_layers
        .iter()
        .map(|layer| normalized_location(layer, relevant_layers[0], &name_to_tag_map))
        .collect::<Result<_, _>>()?;

    let model = VariationModel::new_extrapolating(locations, axis_order.clone());

    let axis_tuples = ref_glyph
        .smart_component_axes
        .iter()
        .map(|(name, range)| {
            let default_value = if *relevant_layers[0]
                .smart_component_positions
                .get(name)
                .unwrap()
                == AxisPole::Min
            {
                *range.start()
            } else {
                *range.end()
            };
            (name, (*range.start(), default_value, *range.end()))
        })
        .collect::<HashMap<_, _>>();

    let location: NormalizedLocation = component
        .smart_component_values
        .iter()
        .map(|(name, value)| {
            (
                *name_to_tag_map.get(name).unwrap(),
                normalize_value_with_extrapolation(*value, *axis_tuples.get(name).unwrap()),
            )
        })
        .collect();

    log::debug!(
        "instantiating component '{}' at {location:?}",
        component.name
    );

    let point_seqs = relevant_layers
        .iter()
        .map(|layer| {
            let loc = normalized_location(layer, relevant_layers[0], &name_to_tag_map)?;
            let points = layer
                .shapes
                .iter()
                .filter_map(Shape::as_path)
                .flat_map(|path| path.nodes.iter().map(|node| node.pt))
                .collect::<Vec<_>>();
            Ok((loc, points))
        })
        .collect::<Result<HashMap<_, _>, BadSmartComponent>>()?;
    let deltas = model
        .deltas_with_rounding(&point_seqs, RoundingBehaviour::None)
        .unwrap();
    let points = model.interpolate_from_deltas(&location, &deltas);
    let mut shapes = shapes_with_new_points(relevant_layers[0], &points);
    shapes.iter_mut().for_each(|shape| {
        shape.apply_affine(component.transform);
        if component.transform.determinant() < 0.0 {
            shape.reverse();
        }
    });

    // Interpolate anchors from the smart component layers
    let anchors = interpolate_anchors(
        &model,
        &location,
        &relevant_layers,
        &name_to_tag_map,
        component.transform,
    )?;

    Ok(SmartComponentInstance { shapes, anchors })
}

fn validate_relevant_layers(layers: &[&Layer]) -> Result<(), BadSmartComponent> {
    let Some((head, tail)) = layers.split_first() else {
        return Ok(());
    };

    for layer in tail {
        if layer.shapes.len() != head.shapes.len()
            || layer
                .shapes
                .iter()
                .zip(&head.shapes)
                .any(|pair| match pair {
                    (Shape::Component(_), Shape::Component(_)) => false,
                    (Shape::Path(one), Shape::Path(two)) => one.nodes.len() != two.nodes.len(),
                    _ => true,
                })
        {
            return Err(BadSmartComponent::IncoherentLayers);
        }
    }
    Ok(())
}

// component parts just have names, not tags, but VariationModel needs tags;
// we give them tags like `ax01`, `ax02`.. etc
fn axes_for_glyph(glyph: &Glyph) -> (Vec<Tag>, BTreeMap<SmolStr, Tag>) {
    let name_to_tag_map: BTreeMap<_, _> = glyph
        .smart_component_axes
        .keys()
        .enumerate()
        .map(|(i, name)| {
            (
                name.clone(),
                Tag::from_str(&format!("ax{i:02}")).expect("only 99 axes supported!"),
            )
        })
        .collect();
    let axis_order = name_to_tag_map.values().copied().collect();

    (axis_order, name_to_tag_map)
}

fn shapes_with_new_points(layer: &Layer, points: &[Vec2]) -> Vec<Shape> {
    let mut points = points;
    layer
        .shapes
        .iter()
        .map(|shape| match shape {
            Shape::Path(path) => {
                let mut path = path.to_owned();
                for (node, newpt) in path.nodes.iter_mut().zip(points) {
                    node.pt = newpt.to_point();
                }
                points = &points[path.nodes.len()..];
                Shape::Path(path)
            }
            // we just skip components, which matches fonttools. Should we error instead?
            Shape::Component(_) => shape.clone(),
        })
        .collect()
}

/// Interpolate anchor positions from smart component layers.
///
/// Uses the same variation model as shape interpolation to compute anchor
/// positions at the target location in the smart component's axis space.
fn interpolate_anchors(
    model: &VariationModel,
    location: &NormalizedLocation,
    layers: &[&Layer],
    name_to_tag_map: &BTreeMap<SmolStr, Tag>,
    transform: Affine,
) -> Result<Vec<Anchor>, BadSmartComponent> {
    // Get anchor names from the default layer (first layer), preserving order
    let anchor_names: Vec<_> = layers[0].anchors.iter().map(|a| a.name.clone()).collect();

    if anchor_names.is_empty() {
        return Ok(Vec::new());
    }

    // Build position sequences for all anchors at each location,
    // similar to how point_seqs works for shapes
    let anchor_seqs: HashMap<NormalizedLocation, Vec<Point>> = layers
        .iter()
        .filter_map(|layer| {
            let loc = normalized_location(layer, layers[0], name_to_tag_map).ok()?;
            // None if any anchor is missing from non-default layer; the variation model
            // treats missing locations as 'sparse' sources. Better than fail altogether.
            let positions: Option<Vec<Point>> = anchor_names
                .iter()
                .map(|name| {
                    layer
                        .anchors
                        .iter()
                        .find(|a| &a.name == name)
                        .map(|a| a.pos)
                })
                .collect();
            if positions.is_none() {
                log::warn!(
                    "smart component layer '{}' missing some anchors, \
                     dropping from anchor interpolation",
                    layer.layer_id
                );
            }
            Some((loc, positions?))
        })
        .collect();

    // Interpolate all anchor positions at once
    let deltas = model
        .deltas_with_rounding(&anchor_seqs, RoundingBehaviour::None)
        .map_err(|_| BadSmartComponent::IncoherentLayers)?;
    let interpolated_positions = model.interpolate_from_deltas(location, &deltas);

    // Build result anchors with transformed positions
    // interpolate_from_deltas returns Vec<Vec2>, convert to Point and apply transform
    let result = anchor_names
        .into_iter()
        .zip(interpolated_positions)
        .map(|(name, pos)| {
            let point = Point::new(pos.x, pos.y);
            let transformed = transform * point;
            Anchor {
                name,
                pos: transformed,
            }
        })
        .collect();

    Ok(result)
}

//https://github.com/fonttools/fonttools/blob/03a3c8ed9e/Lib/fontTools/varLib/models.py#L47
fn normalize_value_with_extrapolation(val: f64, tuple: (i64, i64, i64)) -> NormalizedCoord {
    let (lower, default, upper) = tuple;
    let (lower, default, upper) = (lower as f64, default as f64, upper as f64);
    assert!(lower <= default);
    assert!(default <= upper);
    if val == default || lower == upper {
        return NormalizedCoord::default();
    }

    let val = if (val < default && lower != default) || (val > default && upper == default) {
        (val - default) / (default - lower)
    } else {
        assert!((val > default && upper != default) || (val < default && lower == default));
        (val - default) / (upper - default)
    };
    NormalizedCoord::new(val)
}

impl Shape {
    fn apply_affine(&mut self, affine: Affine) {
        match self {
            Shape::Path(path) => {
                path.nodes
                    .iter_mut()
                    .for_each(|node| node.apply_affine(affine));
            }
            Shape::Component(component) => {
                component.transform *= affine;
            }
        }
    }
}

impl Node {
    fn apply_affine(&mut self, affine: Affine) {
        self.pt = affine * self.pt;
    }
}

//https://github.com/googlefonts/glyphsLib/blob/52c982399b/Lib/glyphsLib/builder/smart_components.py#L43
fn normalized_location(
    for_layer: &Layer,
    base_layer: &Layer,
    name_to_tag_map: &BTreeMap<SmolStr, Tag>,
) -> Result<NormalizedLocation, BadSmartComponent> {
    for_layer
        .smart_component_positions
        .iter()
        .map(|(axis, pos)| {
            let tag = *name_to_tag_map
                .get(axis)
                .ok_or_else(|| BadSmartComponent::UnknownAxis(axis.clone()))?;
            let base_value = base_layer.smart_component_positions.get(axis).copied();
            let pos = match (base_value, *pos) {
                (Some(base), this) if base == this => NormalizedCoord::new(0.0),
                (None | Some(AxisPole::Min), AxisPole::Max) => NormalizedCoord::MAX,
                (None | Some(AxisPole::Max), AxisPole::Min) => NormalizedCoord::MIN,
                (base, this) => {
                    return Err(BadSmartComponent::StrangeAxisMapping {
                        axis: axis.clone(),
                        base,
                        child: this,
                    });
                }
            };
            Ok((tag, pos))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use kurbo::{BezPath, Rect, Shape as _};

    use super::*;
    use crate::{Path, Shape};

    // ported from python:
    //https://github.com/googlefonts/glyphsLib/blob/52c982399b/tests/smart_components_test.py#L37
    fn smart_glyphs(master_id: &str) -> BTreeMap<SmolStr, Glyph> {
        fn rectangle_path(x: f64, y: f64, width: f64, height: f64) -> Path {
            fn line_to(x: f64, y: f64) -> Node {
                Node {
                    pt: (x, y).into(),
                    node_type: crate::NodeType::Line,
                }
            }
            Path {
                nodes: vec![
                    line_to(x, y),
                    line_to(x + width, y),
                    line_to(x + width, y + height),
                    line_to(x, y + height),
                ],
                closed: true,
                ..Default::default()
            }
        }
        let mut glyphs = BTreeMap::new();

        // Rectangle smart component
        let mut rectangle = Glyph {
            name: "_part.rectangle".into(),
            ..Default::default()
        };

        // Three axes
        rectangle
            .smart_component_axes
            .insert(SmolStr::new("Width"), 0..=1);
        rectangle
            .smart_component_axes
            .insert(SmolStr::new("Height"), 100..=500);
        rectangle
            .smart_component_axes
            .insert(SmolStr::new("Shift"), -100..=0);

        // Four layers
        let regular = Layer {
            layer_id: master_id.into(),
            width: 300.0.into(),
            shapes: vec![Shape::Path(rectangle_path(100.0, 100.0, 100.0, 100.0))],
            smart_component_positions: [
                (SmolStr::new("Width"), AxisPole::Min), // 1 is bottom pole
                (SmolStr::new("Height"), AxisPole::Min),
                (SmolStr::new("Shift"), AxisPole::Max), // 2 is the top pole
            ]
            .into_iter()
            .collect(),
            ..Default::default()
        };

        let wide = Layer {
            layer_id: "wide".into(),
            associated_master_id: Some(master_id.into()),
            width: 700.0.into(),
            shapes: vec![Shape::Path(rectangle_path(100.0, 100.0, 500.0, 100.0))],
            smart_component_positions: [
                (SmolStr::new("Width"), AxisPole::Max),
                (SmolStr::new("Height"), AxisPole::Min),
                (SmolStr::new("Shift"), AxisPole::Max),
            ]
            .into_iter()
            .collect(),
            ..Default::default()
        };

        let tall = Layer {
            layer_id: "tall".into(),
            associated_master_id: Some(master_id.into()),
            width: 300.0.into(),
            shapes: vec![Shape::Path(rectangle_path(100.0, 100.0, 100.0, 500.0))],
            smart_component_positions: [
                (SmolStr::new("Width"), AxisPole::Min),
                (SmolStr::new("Height"), AxisPole::Max),
                (SmolStr::new("Shift"), AxisPole::Max),
            ]
            .into_iter()
            .collect(),
            ..Default::default()
        };

        let shifted = Layer {
            layer_id: "shifted".into(),
            associated_master_id: Some(master_id.into()),
            width: 100.0.into(),
            shapes: vec![Shape::Path(rectangle_path(0.0, 0.0, 100.0, 100.0))],
            smart_component_positions: [
                (SmolStr::new("Width"), AxisPole::Min),
                (SmolStr::new("Height"), AxisPole::Min),
                (SmolStr::new("Shift"), AxisPole::Min),
            ]
            .into_iter()
            .collect(),
            ..Default::default()
        };

        rectangle.layers.extend([regular, wide, tall, shifted]);
        glyphs.insert(rectangle.name.clone(), rectangle);

        // Normal glyph "a" with a component
        let mut a = Glyph {
            name: "a".into(),
            ..Default::default()
        };

        let a_layer = Layer {
            layer_id: master_id.into(),
            width: 1000.0.into(),
            shapes: vec![Shape::Component(Component {
                name: SmolStr::new("_part.rectangle"),
                smart_component_values: BTreeMap::from([("derp".into(), 52.2)]),
                ..Default::default()
            })],
            ..Default::default()
        };

        a.layers.push(a_layer);
        glyphs.insert(a.name.clone(), a);

        glyphs
    }

    #[derive(Debug, PartialEq)]
    enum PathDirection {
        Clockwise,
        Otherwise,
    }

    fn get_rectangle_data(shape: &Shape) -> (Rect, PathDirection) {
        let shape = shape.as_path().unwrap();
        let nodes = &shape.nodes;
        assert_eq!(nodes.len(), 4);
        let mut rect = BezPath::new();
        rect.move_to(nodes[0].pt);
        nodes[1..].iter().for_each(|nd| rect.line_to(nd.pt));

        let center = rect.bounding_box().center();
        let dir = if rect.winding(center) < 0 {
            PathDirection::Clockwise
        } else {
            PathDirection::Otherwise
        };
        (rect.bounding_box(), dir)
    }

    //https://github.com/googlefonts/glyphsLib/blob/52c982399b/tests/smart_components_test.py#L159
    #[test]
    fn test_smart_component_regular() {
        let master_id = "master01";
        let test_cases = [
            // Eight corners
            (
                [("Width", 0.0), ("Height", 100.0), ("Shift", 0.0)].as_slice(),
                (100.0, 100.0, 100.0, 100.0),
            ),
            (
                [("Width", 1.0), ("Height", 100.0), ("Shift", 0.0)].as_slice(),
                (100.0, 100.0, 500.0, 100.0),
            ),
            (
                [("Width", 0.0), ("Height", 500.0), ("Shift", 0.0)].as_slice(),
                (100.0, 100.0, 100.0, 500.0),
            ),
            (
                [("Width", 1.0), ("Height", 500.0), ("Shift", 0.0)].as_slice(),
                (100.0, 100.0, 500.0, 500.0),
            ),
            (
                [("Width", 0.0), ("Height", 100.0), ("Shift", -100.0)].as_slice(),
                (0.0, 0.0, 100.0, 100.0),
            ),
            (
                [("Width", 1.0), ("Height", 100.0), ("Shift", -100.0)].as_slice(),
                (0.0, 0.0, 500.0, 100.0),
            ),
            (
                [("Width", 0.0), ("Height", 500.0), ("Shift", -100.0)].as_slice(),
                (0.0, 0.0, 100.0, 500.0),
            ),
            (
                [("Width", 1.0), ("Height", 500.0), ("Shift", -100.0)].as_slice(),
                (0.0, 0.0, 500.0, 500.0),
            ),
            // Some points in the middle
            (
                [("Width", 0.5), ("Height", 300.0), ("Shift", -50.0)].as_slice(),
                (50.0, 50.0, 300.0, 300.0),
            ),
            // Extrapolation
            (
                [("Width", 0.0), ("Height", 800.0), ("Shift", 0.0)].as_slice(),
                (100.0, 100.0, 100.0, 800.0),
            ),
        ];

        let glyphs = smart_glyphs(master_id);
        let a_glyph = glyphs.get(&SmolStr::new("a")).unwrap();
        let a_layer = &a_glyph.layers[0];
        let component = a_layer.shapes[0].as_smart_component().unwrap();

        for (location, expected) in test_cases {
            // Set smart component values
            let mut modified_component = component.clone();
            modified_component.smart_component_values = location
                .iter()
                .map(|(k, v)| (SmolStr::new(k), *v))
                .collect();

            let rectangle = glyphs.get(&SmolStr::new("_part.rectangle")).unwrap();
            let instance = instantiate_for_layer(master_id, &modified_component, rectangle)
                .expect("instantiate should succeed");

            let (rect, dir) = get_rectangle_data(&instance.shapes[0]);
            let (x, y, w, h) = expected;
            let expected = Rect::new(x, y, x + w, y + h);

            assert_eq!(rect, expected, "Failed for values: {:?}", location);
            assert_eq!(
                dir,
                PathDirection::Otherwise,
                "Expected counter-clockwise winding for values: {:?}",
                location
            );
        }
    }

    //https://github.com/googlefonts/glyphsLib/blob/52c982399b/tests/smart_components_test.py#L198
    #[test]
    fn test_smart_component_regular_flipped_x() {
        let master_id = "master01";
        let test_cases = [
            // Eight corners
            (
                [("Width", 0.0), ("Height", 100.0), ("Shift", 0.0)].as_slice(),
                (-200.0, 100.0, 100.0, 100.0),
            ),
            (
                [("Width", 1.0), ("Height", 100.0), ("Shift", 0.0)].as_slice(),
                (-600.0, 100.0, 500.0, 100.0),
            ),
            (
                [("Width", 0.0), ("Height", 500.0), ("Shift", 0.0)].as_slice(),
                (-200.0, 100.0, 100.0, 500.0),
            ),
            (
                [("Width", 1.0), ("Height", 500.0), ("Shift", 0.0)].as_slice(),
                (-600.0, 100.0, 500.0, 500.0),
            ),
            (
                [("Width", 0.0), ("Height", 100.0), ("Shift", -100.0)].as_slice(),
                (-100.0, 0.0, 100.0, 100.0),
            ),
            (
                [("Width", 1.0), ("Height", 100.0), ("Shift", -100.0)].as_slice(),
                (-500.0, 0.0, 500.0, 100.0),
            ),
            (
                [("Width", 0.0), ("Height", 500.0), ("Shift", -100.0)].as_slice(),
                (-100.0, 0.0, 100.0, 500.0),
            ),
            (
                [("Width", 1.0), ("Height", 500.0), ("Shift", -100.0)].as_slice(),
                (-500.0, 0.0, 500.0, 500.0),
            ),
            // Some points in the middle
            (
                [("Width", 0.5), ("Height", 300.0), ("Shift", -50.0)].as_slice(),
                (-350.0, 50.0, 300.0, 300.0),
            ),
            // Extrapolation
            (
                [("Width", 0.0), ("Height", 800.0), ("Shift", 0.0)].as_slice(),
                (-200.0, 100.0, 100.0, 800.0),
            ),
        ];

        let glyphs = smart_glyphs(master_id);
        let a_glyph = glyphs.get(&SmolStr::new("a")).unwrap();
        let a_layer = &a_glyph.layers[0];
        let component = a_layer.shapes[0].as_smart_component().unwrap();

        for (location, expected) in test_cases {
            // Set smart component values and flip x transform
            let mut modified_component = component.clone();
            modified_component.transform = Affine::new([-1.0, 0.0, 0.0, 1.0, 0.0, 0.0]);
            assert!(modified_component.transform.determinant() < 0.0);
            modified_component.smart_component_values = location
                .iter()
                .map(|(k, v)| (SmolStr::new(k), *v))
                .collect();

            let rectangle = glyphs.get(&SmolStr::new("_part.rectangle")).unwrap();
            let instance = instantiate_for_layer(master_id, &modified_component, rectangle)
                .expect("instantiate should succeed");

            let (rect, dir) = get_rectangle_data(&instance.shapes[0]);
            let (x, y, w, h) = expected;
            let expected = Rect::new(x, y, x + w, y + h);

            assert_eq!(rect, expected, "Failed for values: {:?}", location);
            assert_eq!(
                dir,
                PathDirection::Otherwise,
                "Expected counter-clockwise winding after flipped x transform for values: {:?}",
                location
            );
        }
    }

    /// Helper to create a smart component with anchors for testing.
    /// Test data adapted from glyphsLib's test_smart_component_anchors
    /// (googlefonts/glyphsLib#1131, credit: @khaledhosny).
    fn smart_glyph_with_anchors(master_id: &str) -> Glyph {
        use crate::Anchor;

        let mut base = Glyph {
            name: "base".into(),
            ..Default::default()
        };

        base.smart_component_axes
            .insert(SmolStr::new("TEST"), -100..=100);

        // Default layer (pole Min = -100)
        let default_layer = Layer {
            layer_id: master_id.into(),
            width: 200.0.into(),
            shapes: vec![],
            anchors: vec![
                Anchor {
                    name: "top".into(),
                    pos: (23.0, 103.0).into(),
                },
                Anchor {
                    name: "bottom".into(),
                    pos: (36.0, -51.0).into(),
                },
            ],
            smart_component_positions: [(SmolStr::new("TEST"), AxisPole::Min)]
                .into_iter()
                .collect(),
            ..Default::default()
        };

        // Max layer (pole Max = 100)
        let max_layer = Layer {
            layer_id: "max".into(),
            associated_master_id: Some(master_id.into()),
            width: 200.0.into(),
            shapes: vec![],
            anchors: vec![
                Anchor {
                    name: "top".into(),
                    pos: (33.0, 123.0).into(),
                },
                Anchor {
                    name: "bottom".into(),
                    pos: (36.0, -51.0).into(),
                },
            ],
            smart_component_positions: [(SmolStr::new("TEST"), AxisPole::Max)]
                .into_iter()
                .collect(),
            ..Default::default()
        };

        base.layers.extend([default_layer, max_layer]);
        base
    }

    #[test]
    fn test_anchor_interpolation() {
        let master_id = "master01";
        let smart_comp = smart_glyph_with_anchors(master_id);

        // At min (TEST=-100): anchors unchanged from default layer
        let component = Component {
            name: "base".into(),
            smart_component_values: [("TEST".into(), -100.0)].into_iter().collect(),
            ..Default::default()
        };
        let instance = instantiate_for_layer(master_id, &component, &smart_comp)
            .expect("instantiate should succeed");
        assert_eq!(instance.anchors.len(), 2);
        let top = instance.anchors.iter().find(|a| a.name == "top").unwrap();
        let bottom = instance
            .anchors
            .iter()
            .find(|a| a.name == "bottom")
            .unwrap();
        assert_eq!((top.pos.x, top.pos.y), (23.0, 103.0));
        assert_eq!((bottom.pos.x, bottom.pos.y), (36.0, -51.0));

        // At max (TEST=100): anchors from max layer
        let component = Component {
            name: "base".into(),
            smart_component_values: [("TEST".into(), 100.0)].into_iter().collect(),
            ..Default::default()
        };
        let instance = instantiate_for_layer(master_id, &component, &smart_comp)
            .expect("instantiate should succeed");
        let top = instance.anchors.iter().find(|a| a.name == "top").unwrap();
        let bottom = instance
            .anchors
            .iter()
            .find(|a| a.name == "bottom")
            .unwrap();
        assert_eq!((top.pos.x, top.pos.y), (33.0, 123.0));
        assert_eq!((bottom.pos.x, bottom.pos.y), (36.0, -51.0));

        // At midpoint (TEST=0): interpolated
        let component = Component {
            name: "base".into(),
            smart_component_values: [("TEST".into(), 0.0)].into_iter().collect(),
            ..Default::default()
        };
        let instance = instantiate_for_layer(master_id, &component, &smart_comp)
            .expect("instantiate should succeed");
        let top = instance.anchors.iter().find(|a| a.name == "top").unwrap();
        let bottom = instance
            .anchors
            .iter()
            .find(|a| a.name == "bottom")
            .unwrap();
        assert_eq!((top.pos.x, top.pos.y), (28.0, 113.0));
        assert_eq!((bottom.pos.x, bottom.pos.y), (36.0, -51.0));

        // At midpoint (TEST=0) with component offset (20, 10)
        let component = Component {
            name: "base".into(),
            smart_component_values: [("TEST".into(), 0.0)].into_iter().collect(),
            transform: Affine::translate((20.0, 10.0)),
            ..Default::default()
        };
        let instance = instantiate_for_layer(master_id, &component, &smart_comp)
            .expect("instantiate should succeed");
        let top = instance.anchors.iter().find(|a| a.name == "top").unwrap();
        let bottom = instance
            .anchors
            .iter()
            .find(|a| a.name == "bottom")
            .unwrap();
        assert_eq!((top.pos.x, top.pos.y), (48.0, 123.0));
        assert_eq!((bottom.pos.x, bottom.pos.y), (56.0, -41.0));
    }
}
