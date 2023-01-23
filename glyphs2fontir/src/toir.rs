use std::collections::HashMap;

use fontdrasil::types::GlyphName;
use fontir::{
    coords::{CoordConverter, DesignCoord, DesignLocation, NormalizedLocation, UserCoord},
    error::{Error, WorkError},
    ir,
};
use glyphs_reader::{Component, FeatureSnippet, Font, FontMaster, Node, NodeType, Path, Shape};
use log::trace;
use ordered_float::OrderedFloat;

pub(crate) fn to_ir_contours_and_components(
    glyph_name: &GlyphName,
    shapes: &[Shape],
) -> Result<(Vec<ir::Contour>, Vec<ir::Component>), WorkError> {
    let mut contours = Vec::new();
    let mut components = Vec::new();

    for shape in shapes.iter() {
        match shape {
            Shape::Component(component) => components.push(to_ir_component(glyph_name, component)),
            Shape::Path(path) => contours.push(to_ir_contour(glyph_name, path)?),
        }
    }

    Ok((contours, components))
}

fn to_ir_component(glyph_name: &GlyphName, component: &Component) -> ir::Component {
    trace!(
        "{} reuses {} with transform {:?}",
        glyph_name,
        component.glyph_name,
        component.transform
    );
    ir::Component {
        base: component.glyph_name.clone(),
        transform: component.transform,
    }
}

fn to_ir_contour(glyph_name: &GlyphName, path: &Path) -> Result<ir::Contour, WorkError> {
    // Based on https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/paths.py#L20
    // See also https://github.com/fonttools/ufoLib2/blob/4d8a9600148b670b0840120658d9aab0b38a9465/src/ufoLib2/pointPens/glyphPointPen.py#L16
    let mut contour = ir::Contour::new();
    if path.nodes.is_empty() {
        return Ok(contour);
    }

    let mut nodes = &path.nodes[..];
    if !path.closed {
        let (first, elements) = nodes
            .split_first()
            .expect("Not empty and no first is a good trick");
        nodes = elements;
        if first.node_type != NodeType::Line {
            return Err(WorkError::InvalidSourceGlyph {
                glyph_name: glyph_name.clone(),
                message: String::from("Open path starts with off-curve points"),
            });
        }
        let mut pt = to_ir_point(first);
        pt.typ = ir::PointType::Move;
        contour.push(pt);
    } else {
        // In Glyphs.app, the starting node of a closed contour is always
        // stored at the end of the nodes list.
        let (last, elements) = nodes
            .split_last()
            .expect("Not empty and no last is a good trick");
        nodes = elements;
        contour.push(to_ir_point(last));
    }

    contour.extend(nodes.iter().map(to_ir_point));

    trace!("Built a {} entry contour for {}", contour.len(), glyph_name);
    Ok(contour)
}

fn to_ir_point(node: &Node) -> ir::ContourPoint {
    ir::ContourPoint {
        x: node.pt.x,
        y: node.pt.y,
        typ: to_ir_point_type(node.node_type),
    }
}

fn to_ir_point_type(node_type: NodeType) -> ir::PointType {
    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/pens.py#L92
    // Convert XSmooth to X because Smooth is only relevant to editor behavior
    match node_type {
        NodeType::Line => ir::PointType::Line,
        NodeType::Curve => ir::PointType::Curve,
        NodeType::LineSmooth => ir::PointType::Line,
        NodeType::CurveSmooth => ir::PointType::Curve,
        NodeType::OffCurve => ir::PointType::OffCurve,
    }
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
        .zip(master.axes_values.as_ref().unwrap())
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
        tag: axis.tag.clone(),
        hidden: axis.hidden.unwrap_or(false),
        min: min.to_user(&converter),
        default: default.to_user(&converter),
        max: max.to_user(&converter),
        converter,
    })
}

fn ir_axes(font: &Font) -> Result<Vec<ir::Axis>, Error> {
    let mut axis_values = Vec::new();
    for master in font.font_master.iter() {
        master
            .axes_values
            .as_ref()
            .ok_or_else(|| Error::NoAxisDefinitions(master.id.clone()))?
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
    pub axis_indices: HashMap<String, usize>,
}

impl TryFrom<Font> for FontInfo {
    type Error = Error;

    fn try_from(font: Font) -> Result<Self, Self::Error> {
        let master_indices: HashMap<_, _> = font
            .font_master
            .iter()
            .enumerate()
            .map(|(idx, m)| (m.id.clone(), idx))
            .collect();

        let axes = ir_axes(&font)?;
        let axis_indices = axes
            .iter()
            .enumerate()
            .map(|(idx, a)| (a.tag.clone(), idx))
            .collect();

        let axes_by_name = axes.iter().map(|a| (&a.name, a)).collect();
        let master_locations: HashMap<_, _> = font
            .font_master
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
