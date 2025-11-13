use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::PathBuf,
    str::FromStr,
};

use indexmap::IndexMap;
use kurbo::{BezPath, Point};
use log::trace;
use ordered_float::OrderedFloat;

use smol_str::SmolStr;
use write_fonts::types::Tag;

use fontdrasil::{
    coords::{CoordConverter, DesignCoord, DesignLocation, NormalizedLocation, UserCoord},
    types::GlyphName,
};
use fontir::{
    error::{BadGlyph, BadGlyphKind, Error, PathConversionError},
    ir::{
        self, Color, ColorStop, GlyphPathBuilder, Paint, PaintLinearGradient, PaintRadialGradient,
        PaintSolid,
    },
};
use glyphs_reader::{
    Component, FeatureSnippet, Font, Glyph, Layer, NodeType, Path, Shape, ShapeAttributes,
};

pub(crate) fn to_ir_contours_and_components(
    glyph_name: GlyphName,
    shapes: &[Shape],
    erase_open_corners: bool,
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
                to_ir_path(glyph_name.clone(), path, erase_open_corners)
                    .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?,
            ),
        }
    }

    Ok((contours, components))
}

fn to_ir_component(glyph_name: GlyphName, component: &Component) -> ir::Component {
    trace!(
        "{} reuses {} with transform {:?}",
        glyph_name, component.name, component.transform
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

fn to_ir_path(
    glyph_name: GlyphName,
    src_path: &Path,
    erase_open_corners: bool,
) -> Result<BezPath, PathConversionError> {
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

    if erase_open_corners && path_builder.erase_open_corners()? {
        log::debug!("erased open contours for {glyph_name}");
    }

    let path = path_builder.build()?;

    trace!(
        "Built a {} entry path for {glyph_name}",
        path.elements().len(),
    );
    Ok(path)
}

pub(crate) fn to_ir_features(
    features: &[FeatureSnippet],
    include_dir: Option<PathBuf>,
) -> Result<ir::FeaturesSource, Error> {
    // Based on https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L74
    // TODO: token expansion
    // TODO: implement notes
    let fea_snippets: Vec<_> = features.iter().filter_map(|f| f.str_if_enabled()).collect();
    Ok(ir::FeaturesSource::Memory {
        fea_content: fea_snippets.join("\n\n"),
        include_dir,
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
        // localized axis names from .glyphs sources aren't supported yet
        // https://forum.glyphsapp.com/t/localisable-axis-names/19028
        localized_names: Default::default(),
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
    /// Name of glyph : color glyphs split from it, if any
    pub color_glyphs: IndexMap<SmolStr, Vec<SmolStr>>,
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
            .filter(|&a| !a.is_point())
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

        let (font, color_glyphs) = split_color_glyphs(font)?;

        Ok(FontInfo {
            font,
            master_indices,
            master_positions,
            locations,
            axes,
            color_glyphs,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum PaintKey {
    NonColor,
    Solid(glyphs_reader::Color),
    Linear(Vec<glyphs_reader::ColorStop>),
    Radial(Vec<glyphs_reader::ColorStop>),
    Unknown(ShapeAttributes),
}

impl PaintKey {
    fn key_for(layer: &Layer, shape: &Shape) -> Self {
        if !layer.attributes.color {
            return PaintKey::NonColor;
        }
        let attr = shape.attributes();
        if let Some(gradient) = &attr.gradient {
            if gradient.style == "circle" {
                return PaintKey::Radial(gradient.colors.clone());
            }
            return PaintKey::Linear(gradient.colors.clone());
        }
        if let Some(fill) = attr.fill_color {
            return PaintKey::Solid(fill);
        }
        PaintKey::Unknown(attr.clone())
    }

    fn color(&self) -> bool {
        !matches!(self, Self::NonColor)
    }
}

fn split_color_glyphs(font: Font) -> Result<(Font, IndexMap<SmolStr, Vec<SmolStr>>), Error> {
    // <https://github.com/googlefonts/glyphsLib/blob/99328059ec4799956ecef3d47ebcc13ae70dacff/Lib/glyphsLib/builder/glyph.py#L309-L357>
    let mut font = font;
    let mut color_glyphs: IndexMap<SmolStr, Vec<SmolStr>> = Default::default();
    let default_master_id = font.default_master().id.clone();

    let mut additions = Vec::new();
    for (glyph_name, glyph) in font.glyphs.iter_mut() {
        // Normal path: nop, not a color glyph
        if glyph.layers.iter().all(|l| !l.attributes.color) {
            continue;
        }
        // Remember the name!
        color_glyphs.entry(glyph_name.clone()).or_default();
        let Some(default_master_layer) = glyph
            .layers
            .iter()
            .find(|l| l.layer_id == default_master_id)
        else {
            continue;
        };

        // Split into runs of the same paint type
        let mut paint_groups = default_master_layer
            .shapes
            .iter()
            .map(|s| PaintKey::key_for(default_master_layer, s))
            .collect::<Vec<_>>();
        paint_groups.dedup();
        let mut paint_groups: VecDeque<_> = paint_groups.into();

        // If there is only one run we're done
        if paint_groups.len() <= 1 {
            continue;
        }

        // There are multiple runs, we must split this glyph apart
        // The original will remain but uncolored

        // Each color run becomes a new glyph named [original].color[i]
        let mut glyph = glyph.clone();
        let mut nth = 0;
        while let Some(paint_group) = paint_groups.pop_front() {
            let new_glyph_name: SmolStr = format!("{glyph_name}.color{nth}").into();
            let mut new_glyph = Glyph {
                name: new_glyph_name.clone(),
                export: glyph.export,
                ..Default::default()
            };

            // For each layer, chop the head that matches this paint group off glyph and attach it here
            for old_layer in glyph.layers.iter_mut() {
                let mut new_layer = old_layer.clone();
                new_layer.attributes.color = paint_group.color();
                if let Some(split_at) = old_layer
                    .shapes
                    .iter()
                    .position(|s| PaintKey::key_for(&new_layer, s) != paint_group)
                {
                    // Only some of the shapes fit the current run take them
                    // Amusingly this results in the opposite arrangement than we want so swap
                    new_layer.shapes = old_layer.shapes.split_off(split_at);
                    std::mem::swap(&mut new_layer.shapes, &mut old_layer.shapes);
                } else {
                    // All the shapes fit the current run
                    old_layer.shapes.clear();
                }
                trace!(
                    "{glyph_name} {} takes {} shapes for {paint_group:?}",
                    old_layer.layer_id,
                    new_layer.shapes.len()
                );
                new_glyph.layers.push(new_layer);
            }

            color_glyphs
                .entry(glyph_name.clone())
                .or_default()
                .push(new_glyph_name.clone());
            additions.push((new_glyph_name, new_glyph));
            nth += 1;
        }
    }

    font.glyph_order
        .extend(additions.iter().map(|(gn, _)| gn.clone()));
    font.glyphs.extend(additions);

    trace!("updated glyph order {:?}", font.glyph_order);

    Ok((font, color_glyphs))
}

pub(crate) fn to_ir_color(color: glyphs_reader::Color) -> Color {
    Color {
        r: color.r as u8,
        g: color.g as u8,
        b: color.b as u8,
        a: color.a as u8,
    }
}

pub(crate) fn to_ir_color_stops(stops: &[glyphs_reader::ColorStop]) -> Vec<ColorStop> {
    stops
        .iter()
        .map(|cs| ColorStop {
            offset: (cs.stop_offset.0 as f32).into(),
            color: to_ir_color(cs.color),
            alpha: 255.0.into(),
        })
        .collect()
}

pub(crate) fn to_ir_paint(glyph_name: impl Into<GlyphName>, shape: &Shape) -> Result<Paint, Error> {
    let attr = shape.attributes();
    if let Some(color) = attr.fill_color {
        return Ok(Paint::Solid(
            PaintSolid {
                color: to_ir_color(color),
            }
            .into(),
        ));
    }

    // TODO: scaling of points and proper radius values

    if let Some(gradient) = &attr.gradient {
        // <https://github.com/googlefonts/glyphsLib/blob/99328059ec4799956ecef3d47ebcc13ae70dacff/Lib/glyphsLib/builder/color_layers.py#L72>
        let start = Point::new(gradient.start[0].0, gradient.start[1].0);
        let end = Point::new(gradient.end[0].0, gradient.end[1].0);
        return match gradient.style.as_str() {
            "circle" => Ok(Paint::RadialGradient(
                PaintRadialGradient {
                    p0: start,
                    p1: start,
                    r0: 0.0.into(),
                    r1: 1.0.into(),
                    color_line: to_ir_color_stops(&gradient.colors),
                }
                .into(),
            )),
            "" => Ok(Paint::LinearGradient(
                PaintLinearGradient {
                    p0: start,
                    p1: end,
                    p2: Point::new(start.x + end.y - start.y, start.y - (end.x - start.x)),
                    color_line: to_ir_color_stops(&gradient.colors),
                }
                .into(),
            )),
            _ => Err(Error::BadGlyph(BadGlyph::new(
                glyph_name,
                BadGlyphKind::FrontendSpecific(format!("Unrecognized gradient {}", gradient.style)),
            ))),
        };
    }

    Err(Error::BadGlyph(BadGlyph::new(
        glyph_name,
        BadGlyphKind::FrontendSpecific(format!("Unable to produce paint for {attr:?}")),
    )))
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
        let bez = to_ir_path("test".into(), &path, false).unwrap();
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

        let bez = to_ir_path("hello".into(), &path, false).unwrap();
        assert_eq!(
            bez.elements().first(),
            Some(&kurbo::PathEl::MoveTo((5., 0.).into()))
        );
    }
}
