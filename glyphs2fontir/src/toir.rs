use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use kurbo::BezPath;
use log::trace;
use ordered_float::OrderedFloat;

use write_fonts::types::Tag;

use fontdrasil::{
    coords::{CoordConverter, DesignCoord, DesignLocation, NormalizedLocation, UserCoord},
    types::{Axes, GlyphName},
};
use fontir::{
    error::{BadGlyph, Error, PathConversionError},
    ir::{self, Condition, ConditionSet, GlyphPathBuilder},
};
use glyphs_reader::{Component, FeatureSnippet, Font, Layer, NodeType, Path, Shape};

pub(crate) fn to_ir_contours_and_components(
    glyph_name: GlyphName,
    shapes: &[Shape],
) -> Result<(Vec<BezPath>, Vec<ir::Component>), BadGlyph> {
    // For most glyphs in most fonts all the shapes are contours so it's a good guess
    let mut contours = Vec::with_capacity(shapes.len());
    let mut components = Vec::new();

    for shape in shapes.iter() {
        match shape {
            Shape::Component(component) => {
                components.push(to_ir_component(glyph_name.clone(), component))
            }
            Shape::Path(path) => contours.push(
                to_ir_path(glyph_name.clone(), path)
                    .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?,
            ),
        }
    }

    Ok((contours, components))
}

fn to_ir_component(glyph_name: GlyphName, component: &Component) -> ir::Component {
    trace!(
        "{} reuses {} with transform {:?}",
        glyph_name,
        component.name,
        component.transform
    );
    ir::Component {
        base: component.name.as_str().into(),
        transform: component.transform,
    }
}

fn add_to_path<'a>(
    path_builder: &'a mut GlyphPathBuilder,
    nodes: impl Iterator<Item = &'a glyphs_reader::Node>,
) -> Result<(), PathConversionError> {
    // Walk through the remaining points, accumulating off-curve points until we see an on-curve
    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/pens.py#L92
    for node in nodes {
        // Smooth is only relevant to editors so ignore here
        match node.node_type {
            NodeType::Line | NodeType::LineSmooth => path_builder.line_to((node.pt.x, node.pt.y)),
            NodeType::Curve | NodeType::CurveSmooth => {
                path_builder.curve_to((node.pt.x, node.pt.y))
            }
            NodeType::OffCurve => path_builder.offcurve((node.pt.x, node.pt.y)),
            NodeType::QCurve | NodeType::QCurveSmooth => {
                path_builder.qcurve_to((node.pt.x, node.pt.y))
            }
        }?
    }
    Ok(())
}

fn to_ir_path(glyph_name: GlyphName, src_path: &Path) -> Result<BezPath, PathConversionError> {
    // Based on https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/paths.py#L20
    // See also https://github.com/fonttools/ufoLib2/blob/4d8a9600148b670b0840120658d9aab0b38a9465/src/ufoLib2/pointPens/glyphPointPen.py#L16
    if src_path.nodes.is_empty() {
        return Ok(BezPath::new());
    }

    let mut path_builder = GlyphPathBuilder::new(src_path.nodes.len());

    // First is a delicate butterfly
    if !src_path.closed {
        let first = src_path.nodes.first().unwrap();
        if first.node_type == NodeType::OffCurve {
            return Err(PathConversionError::Parse(
                "Open path starts with off-curve points".into(),
            ));
        }
        path_builder.move_to((first.pt.x, first.pt.y))?;
        add_to_path(&mut path_builder, src_path.nodes[1..].iter())?;
    } else if src_path.nodes.iter().any(|node| node.is_on_curve()) {
        // In Glyphs.app, the starting node of a closed contour is always
        // stored at the end of the nodes list.
        // Rotate right by 1 by way of chaining iterators
        let last_idx = src_path.nodes.len() - 1;
        add_to_path(
            &mut path_builder,
            std::iter::once(&src_path.nodes[last_idx]).chain(&src_path.nodes[..last_idx]),
        )?;
    } else {
        // except if the contour contains only off-curve points (implied quadratic)
        // in which case we're already in the correct order (this is very rare
        // in glyphs sources and might be the result of bugs, but it exists)
        add_to_path(&mut path_builder, src_path.nodes.iter())?;
    };

    let path = path_builder.build()?;

    let path = match crate::erase_open_corners::erase_open_corners(&path) {
        Some(changes) => {
            log::debug!("erased open contours for {glyph_name}");
            changes
        }
        None => path,
    };
    trace!(
        "Built a {} entry path for {}",
        path.elements().len(),
        glyph_name
    );
    Ok(path)
}

pub(crate) fn to_ir_features(features: &[FeatureSnippet]) -> Result<ir::FeaturesSource, Error> {
    // Based on https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L74
    // TODO: token expansion
    // TODO: implement notes
    let fea_snippets: Vec<_> = features.iter().filter_map(|f| f.str_if_enabled()).collect();
    Ok(ir::FeaturesSource::Memory {
        fea_content: fea_snippets.join("\n\n"),
        include_dir: None,
    })
}

pub(crate) fn design_location(
    axes: &fontdrasil::types::Axes,
    axes_values: &[OrderedFloat<f64>],
) -> DesignLocation {
    axes.iter()
        .zip(axes_values.iter())
        .map(|(axis, pos)| (axis.tag, DesignCoord::new(*pos)))
        .collect()
}

fn find_by_design_coord(
    mappings: &[(UserCoord, DesignCoord)],
    value: DesignCoord,
    axis_name: &str,
    field: &str,
) -> Result<usize, Error> {
    mappings
        .iter()
        .position(|(_, dc)| *dc == value)
        .ok_or_else(|| Error::MissingMappingForDesignCoord {
            axis_name: axis_name.to_string(),
            field: field.to_string(),
            mappings: mappings.to_vec(),
            value,
        })
}

/// Convert .glyphs axes to IR axes.
///
///  See <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/builder/axes.py#L155>
fn to_ir_axis(
    font: &Font,
    axis_values: &[OrderedFloat<f64>],
    default_idx: usize,
    axis: &glyphs_reader::Axis,
) -> Result<fontdrasil::types::Axis, Error> {
    let min = axis_values.iter().min().unwrap();
    let max = axis_values.iter().max().unwrap();
    let default = axis_values[default_idx];

    // Given in design coords based on a sample file
    let default = DesignCoord::new(default);
    let min = DesignCoord::new(*min);
    let max = DesignCoord::new(*max);

    let converter = if font.axis_mappings.contains(&axis.name)
        && !font.axis_mappings.get(&axis.name).unwrap().is_identity()
    {
        let mappings: Vec<_> = font
            .axis_mappings
            .get(&axis.name)
            .unwrap()
            .iter()
            .map(|(u, d)| (UserCoord::new(*u), DesignCoord::new(*d)))
            .collect();
        let default_idx = find_by_design_coord(&mappings, default, axis.name.as_str(), "default")?;
        // Make sure we have min and max mappings
        find_by_design_coord(&mappings, min, axis.name.as_str(), "min")?;
        find_by_design_coord(&mappings, max, axis.name.as_str(), "max")?;
        CoordConverter::new(mappings, default_idx)
    } else {
        // There is no meaningful mapping; design == user
        let min = UserCoord::new(min.into_inner());
        let max = UserCoord::new(max.into_inner());
        let default = UserCoord::new(default.into_inner());
        CoordConverter::unmapped(min, default, max)
    };

    Ok(fontdrasil::types::Axis {
        name: axis.name.clone(),
        tag: Tag::from_str(&axis.tag).map_err(|cause| Error::InvalidTag {
            raw_tag: axis.tag.clone(),
            cause,
        })?,
        hidden: axis.hidden.unwrap_or(false),
        min: min.to_user(&converter),
        default: default.to_user(&converter),
        max: max.to_user(&converter),
        converter,
    })
}

fn ir_axes(font: &Font) -> Result<fontdrasil::types::Axes, Error> {
    // Every master should have a value for every axis
    for master in font.masters.iter() {
        if font.axes.len() != master.axes_values.len() {
            return Err(Error::InconsistentAxisDefinitions(format!(
                "Axes {:?} doesn't match axis values {:?}",
                font.axes, master.axes_values
            )));
        }
    }

    font.axes
        .iter()
        .enumerate()
        .map(|(idx, glyphs_axis)| {
            let axis_values: Vec<_> = font
                .masters
                .iter()
                .map(|m| m.axes_values[idx])
                // extend the masters' axis values with the virtual masters' if any;
                // they will be used to compute the axis min/max values
                .chain(font.virtual_masters.iter().flat_map(|vm| {
                    vm.iter().filter_map(|(axis_name, location)| {
                        if axis_name == &glyphs_axis.name {
                            Some(*location)
                        } else {
                            None
                        }
                    })
                }))
                .collect();
            to_ir_axis(font, &axis_values, font.default_master_idx, glyphs_axis)
        })
        .collect()
}

/// A [Font] with some prework to convert to IR predone.
#[derive(Debug)]
pub(crate) struct FontInfo {
    pub font: Font,
    /// Index by master id
    pub master_indices: HashMap<String, usize>,
    // Master id => location
    pub master_positions: HashMap<String, NormalizedLocation>,
    /// Axes values => location for every instance and master
    pub locations: HashMap<Vec<OrderedFloat<f64>>, NormalizedLocation>,
    pub axes: fontdrasil::types::Axes,
    /// Maps the bracket glyph name to the parent glyph
    pub bracket_glyphs: HashMap<GlyphName, GlyphName>,
}

impl TryFrom<Font> for FontInfo {
    type Error = Error;

    fn try_from(font: Font) -> Result<Self, Self::Error> {
        let master_indices: HashMap<_, _> = font
            .masters
            .iter()
            .enumerate()
            .map(|(idx, m)| (m.id.clone(), idx))
            .collect();

        let axes = ir_axes(&font)?;

        let locations: HashMap<_, _> = font
            .masters
            .iter()
            .map(|m| {
                (
                    m.axes_values.clone(),
                    design_location(&axes, &m.axes_values).to_normalized(&axes),
                )
            })
            .chain(font.instances.iter().map(|i| {
                (
                    i.axes_values.clone(),
                    design_location(&axes, &i.axes_values).to_normalized(&axes),
                )
            }))
            .collect();

        let variable_axes: HashSet<_> = axes
            .iter()
            .filter(|&a| (!a.is_point()))
            .map(|a| a.tag)
            .collect();
        let master_positions: HashMap<_, _> = font
            .masters
            .iter()
            .map(|m| (&m.id, locations.get(&m.axes_values).unwrap()))
            .map(|(id, pos)| {
                let mut pos = pos.clone();
                pos.retain(|tag, _| variable_axes.contains(tag));
                (id.clone(), pos)
            })
            .collect();

        let mut bracket_glyphs = HashMap::new();
        for glyph in font.glyphs.values() {
            for bracket_glyph_name in glyph.bracket_glyph_names(&axes).map(|(gn, _)| gn) {
                bracket_glyphs.insert(bracket_glyph_name, GlyphName::new(&glyph.name));
            }
        }

        Ok(FontInfo {
            font,
            master_indices,
            master_positions,
            locations,
            axes,
            bracket_glyphs,
        })
    }
}

/// A glyph that produces bracket glyphs.
///
/// Meant to be implemented exclusively by [`glyphs_reader::Glyph`]
/// Exists here because glyphs_reader doesn't know about IR concepts.
pub(crate) trait BracketGlyphs {
    /// The bracket glyphs that need to be produced for this glyph
    ///
    /// See <https://github.com/googlefonts/glyphsLib/blob/c4db6b981d577f/Lib/glyphsLib/builder/bracket_layers.py#L127>
    fn bracket_glyph_names(&self, axes: &Axes) -> impl Iterator<Item = (GlyphName, Vec<&Layer>)>;
}

impl BracketGlyphs for &glyphs_reader::Glyph {
    fn bracket_glyph_names(&self, axes: &Axes) -> impl Iterator<Item = (GlyphName, Vec<&Layer>)> {
        let mut seen_sets = HashMap::new();
        for layer in &self.bracket_layers {
            let condition_set = get_bracket_info(layer, axes);
            let next_alt = seen_sets.len() + 1;
            seen_sets
                .entry(condition_set)
                .or_insert_with(|| {
                    (
                        smol_str::format_smolstr!("{}.BRACKET.varAlt{next_alt:02}", self.name,)
                            .into(),
                        Vec::new(),
                    )
                })
                .1
                .push(layer);
        }
        seen_sets.into_values()
    }
}

/// <https://github.com/googlefonts/glyphsLib/blob/c4db6b981d/Lib/glyphsLib/classes.py#L3947>
fn get_bracket_info(layer: &Layer, axes: &Axes) -> ConditionSet {
    assert!(
        !layer.attributes.axis_rules.is_empty(),
        "all bracket layers have axis rules"
    );

    axes.iter()
        .zip(&layer.attributes.axis_rules)
        .map(|(axis, rule)| {
            let min = rule.min.map(|v| DesignCoord::new(v as f64));
            let max = rule.max.map(|v| DesignCoord::new(v as f64));
            Condition::new(axis.tag, min, max)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use glyphs_reader::{Node, Path};

    use super::to_ir_path;

    #[test]
    fn the_last_of_a_closed_contour_is_first() {
        // In glyph's if we start with off-curve points that means start at the *last* point
        let mut path = Path::new(true);

        // A sort of teardrop thing drawn with a single cubic
        // Offcurve, Offcurve, Oncurve should be taken to start and end at the closing Oncurve.
        path.nodes.push(Node {
            pt: (64.0, 64.0).into(),
            node_type: glyphs_reader::NodeType::OffCurve,
        });
        path.nodes.push(Node {
            pt: (64.0, 0.0).into(),
            node_type: glyphs_reader::NodeType::OffCurve,
        });
        path.nodes.push(Node {
            pt: (32.0, 32.0).into(),
            node_type: glyphs_reader::NodeType::Curve,
        });
        let bez = to_ir_path("test".into(), &path).unwrap();
        assert_eq!("M32,32 C64,64 64,0 32,32 Z", bez.to_svg());
    }

    // in a curve with only offcurves, the 'start' of the curve is the last implied
    // on-curve (the interpolation of the first and last points)
    #[test]
    fn no_on_curve_path_order() {
        let nodes = [(10., 0.), (10., 10.), (0., 10.), (0., 0.)]
            .into_iter()
            .map(|pt| Node {
                pt: pt.into(),
                node_type: glyphs_reader::NodeType::OffCurve,
            })
            .collect();
        let path = Path {
            closed: true,
            nodes,
            ..Default::default()
        };

        let bez = to_ir_path("hello".into(), &path).unwrap();
        assert_eq!(
            bez.elements().first(),
            Some(&kurbo::PathEl::MoveTo((5., 0.).into()))
        );
    }
}
