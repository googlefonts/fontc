use std::{
    collections::{BTreeMap, HashMap},
    str::FromStr,
};
use thiserror::Error;

use fontdrasil::{
    coords::{NormalizedCoord, NormalizedLocation},
    types::Tag,
    variations::VariationModel,
};
use kurbo::{Affine, Vec2};
use smol_str::SmolStr;

use crate::{Component, Glyph, Layer, Node, Shape, font::SmartComponentValue};

/// Things that can go wrong when instantiating a smart component
#[derive(Debug, Error)]
pub enum BadSmartComponent {
    #[error("unknown component axis '{0}'")]
    UnknownAxis(SmolStr),
    #[error("strange mapping for axis '{axis}': base {base:?} child {child:?}")]
    StrangeAxisMapping {
        base: Option<SmartComponentValue>,
        child: SmartComponentValue,
        axis: SmolStr,
    },
}

// based on https://github.com/googlefonts/glyphsLib/blob/52c982399b/Lib/glyphsLib/builder/smart_components.py#L96
pub(crate) fn instantiate_for_layer(
    layer_master_id: &str,
    component: &Component,
    ref_glyph: &Glyph,
) -> Result<Vec<Shape>, BadSmartComponent> {
    assert!(!component.smart_component_values.is_empty());
    assert!(!ref_glyph.smart_component_axes.is_empty());
    let (axis_order, name_to_tag_map) = axes_for_glyph(ref_glyph);

    let relevant_layers = ref_glyph
        .layers
        .iter()
        .filter(|layer| {
            !layer.smart_component_positions.is_empty() && layer.master_id() == layer_master_id
        })
        .collect::<Vec<_>>();

    if relevant_layers.len() == 1 {
        log::debug!("smart component {} only has one layer?", component.name);
        let mut shapes = relevant_layers[0].shapes.clone();
        shapes
            .iter_mut()
            .for_each(|shape| shape.apply_affine(component.transform));
        return Ok(shapes);
    }

    let locations = relevant_layers
        .iter()
        .map(|layer| normalized_location(layer, relevant_layers[0], &name_to_tag_map))
        .collect::<Result<_, _>>()?;

    let model = VariationModel::new(locations, axis_order.clone());

    let axis_tuples = ref_glyph
        .smart_component_axes
        .iter()
        .map(|(name, range)| {
            let default_value = if relevant_layers[0]
                .smart_component_positions
                .get(name)
                .copied()
                == Some(SmartComponentValue::Min)
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
        .collect::<Result<_, BadSmartComponent>>()?;
    let deltas = model.deltas(&point_seqs).unwrap();
    let points = VariationModel::interpolate_from_deltas(&location, &deltas);
    let mut shapes = shapes_with_new_points(relevant_layers[0], &points);
    shapes
        .iter_mut()
        .for_each(|shape| shape.apply_affine(component.transform));

    Ok(shapes)
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
            //TODO: check path length of all layers ahead of time
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
                (None | Some(SmartComponentValue::Min), SmartComponentValue::Max) => {
                    NormalizedCoord::MAX
                }
                (None | Some(SmartComponentValue::Max), SmartComponentValue::Min) => {
                    NormalizedCoord::MIN
                }
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
