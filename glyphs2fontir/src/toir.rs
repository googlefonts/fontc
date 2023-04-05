use std::{collections::HashMap, str::FromStr};

use font_types::Tag;
use fontdrasil::types::GlyphName;
use fontir::{
    coords::{CoordConverter, DesignCoord, DesignLocation, NormalizedLocation, UserCoord},
    error::{Error, WorkError},
    ir::{self, GlyphPathBuilder},
};
use glyphs_reader::{Component, FeatureSnippet, Font, FontMaster, NodeType, Path, Shape};
use kurbo::BezPath;
use log::trace;
use ordered_float::OrderedFloat;

pub(crate) fn to_ir_contours_and_components(
    glyph_name: GlyphName,
    shapes: &[Shape],
) -> Result<(Vec<BezPath>, Vec<ir::Component>), WorkError> {
    let mut contours = Vec::new();
    let mut components = Vec::new();

    for shape in shapes.iter() {
        match shape {
            Shape::Component(component) => {
                components.push(to_ir_component(glyph_name.clone(), component))
            }
            Shape::Path(path) => contours.push(to_ir_path(glyph_name.clone(), path)?),
        }
    }

    Ok((contours, components))
}

fn to_ir_component(glyph_name: GlyphName, component: &Component) -> ir::Component {
    trace!(
        "{} reuses {} with transform {:?}",
        glyph_name,
        component.glyph_name,
        component.transform
    );
    ir::Component {
        base: component.glyph_name.as_str().into(),
        transform: component.transform,
    }
}

fn to_ir_path(glyph_name: GlyphName, src_path: &Path) -> Result<BezPath, WorkError> {
    // Based on https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/paths.py#L20
    // See also https://github.com/fonttools/ufoLib2/blob/4d8a9600148b670b0840120658d9aab0b38a9465/src/ufoLib2/pointPens/glyphPointPen.py#L16
    let mut path_builder = GlyphPathBuilder::new(glyph_name.clone());
    if src_path.nodes.is_empty() {
        return Ok(path_builder.build());
    }

    let mut nodes = &src_path.nodes[..];

    // First is a delicate butterfly
    let first = if !src_path.closed {
        let (first, elements) = nodes
            .split_first()
            .expect("Not empty and no first is a good trick");
        nodes = elements;
        first
    } else {
        // In Glyphs.app, the starting node of a closed contour is always
        // stored at the end of the nodes list.
        let (last, elements) = nodes
            .split_last()
            .expect("Not empty and no last is a good trick");
        nodes = elements;
        last
    };
    if first.node_type == NodeType::OffCurve {
        return Err(WorkError::InvalidSourceGlyph {
            glyph_name,
            message: String::from("Open path starts with off-curve points"),
        });
    }
    path_builder.move_to((first.pt.x, first.pt.y))?;

    // Walk through the remaining points, accumulating off-curve points until we see an on-curve
    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/pens.py#L92
    for node in nodes {
        // Smooth is only relevant to editors so ignore here
        match node.node_type {
            NodeType::Line | NodeType::LineSmooth => path_builder
                .line_to((node.pt.x, node.pt.y))
                .map_err(WorkError::PathConversionError)?,
            NodeType::Curve | NodeType::CurveSmooth => path_builder
                .curve_to((node.pt.x, node.pt.y))
                .map_err(WorkError::PathConversionError)?,
            NodeType::OffCurve => path_builder
                .offcurve((node.pt.x, node.pt.y))
                .map_err(WorkError::PathConversionError)?,
        }
    }

    if src_path.closed {
        path_builder.close_path()?;
    }

    let path = path_builder.build();
    trace!(
        "Built a {} entry path for {}",
        path.elements().len(),
        glyph_name
    );
    Ok(path)
}

pub(crate) fn to_ir_features(features: &[FeatureSnippet]) -> Result<ir::Features, WorkError> {
    // Based on https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L74
    // TODO: token expansion
    // TODO: implement notes and labels
    let fea_snippets: Vec<String> = features.iter().map(|f| f.as_str().to_string()).collect();
    Ok(ir::Features::Memory(fea_snippets.join("\n\n")))
}

fn design_location(axes: &[ir::Axis], master: &FontMaster) -> DesignLocation {
    axes.iter()
        .zip(master.axes_values.iter())
        .map(|(axis, pos)| (axis.name.clone(), DesignCoord::new(pos.into_inner() as f32)))
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
            value,
        })
}

fn to_ir_axis(
    font: &Font,
    axis_values: &[OrderedFloat<f64>],
    axis: &glyphs_reader::Axis,
) -> Result<ir::Axis, Error> {
    let min = axis_values
        .iter()
        .map(|v| OrderedFloat::<f32>(v.into_inner() as f32))
        .min()
        .unwrap();
    let max = axis_values
        .iter()
        .map(|v| OrderedFloat::<f32>(v.into_inner() as f32))
        .max()
        .unwrap();
    let default = OrderedFloat::<f32>(axis_values[font.default_master_idx].into_inner() as f32);

    // Given in design coords based on a sample file
    let default = DesignCoord::new(default);
    let min = DesignCoord::new(min);
    let max = DesignCoord::new(max);

    let converter = if font.axis_mappings.contains_key(&axis.name) {
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
        // There is no mapping; design == user
        let min = UserCoord::new(min.into_inner());
        let max = UserCoord::new(max.into_inner());
        let default = UserCoord::new(default.into_inner());
        CoordConverter::unmapped(min, default, max)
    };

    Ok(ir::Axis {
        name: axis.name.clone(),
        tag: Tag::from_str(&axis.tag).map_err(Error::InvalidTag)?,
        hidden: axis.hidden.unwrap_or(false),
        min: min.to_user(&converter),
        default: default.to_user(&converter),
        max: max.to_user(&converter),
        converter,
    })
}

fn ir_axes(font: &Font) -> Result<Vec<ir::Axis>, Error> {
    let mut axis_values = Vec::new();
    for master in font.masters.iter() {
        master
            .axes_values
            .iter()
            .enumerate()
            .for_each(|(idx, value)| {
                while axis_values.len() <= idx {
                    axis_values.push(Vec::new());
                }
                axis_values[idx].push(*value);
            });
    }

    if font.axes.len() != axis_values.len() || axis_values.iter().any(|v| v.is_empty()) {
        return Err(Error::InconsistentAxisDefinitions(format!(
            "Axes {:?} doesn't match axis values {:?}",
            font.axes, axis_values
        )));
    }

    let mut ir_axes = Vec::new();
    for (idx, glyphs_axis) in font.axes.iter().enumerate() {
        ir_axes.push(to_ir_axis(font, &axis_values[idx], glyphs_axis)?);
    }
    Ok(ir_axes)
}

/// A [Font] with some prework to convert to IR predone.
pub struct FontInfo {
    pub font: Font,
    /// Index by master id
    pub master_indices: HashMap<String, usize>,
    /// Location by master id
    pub master_locations: HashMap<String, NormalizedLocation>,
    pub axes: Vec<ir::Axis>,
    /// Index by tag
    pub axis_indices: HashMap<Tag, usize>,
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
        let axis_indices = axes
            .iter()
            .enumerate()
            .map(|(idx, a)| (a.tag, idx))
            .collect();

        let axes_by_name = axes.iter().map(|a| (&a.name, a)).collect();
        let master_locations: HashMap<_, _> = font
            .masters
            .iter()
            .map(|m| {
                (
                    m.id.clone(),
                    design_location(&axes, m).to_normalized(&axes_by_name),
                )
            })
            .collect();

        Ok(FontInfo {
            font,
            master_indices,
            master_locations,
            axes,
            axis_indices,
        })
    }
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
}
