use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
    path::PathBuf,
    str::FromStr,
    sync::OnceLock,
};

use indexmap::IndexMap;
use kurbo::{BezPath, Point};
use log::{debug, trace, warn};
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
        anchor: component.anchor.clone(),
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

    // If all masters sit at the same position on this axis, the mapping is
    // meaningless and there's no variation to map. Treat as unmapped.
    // glyphsLib handles this via reverse-map + clamp to [min(keys), max(keys)]:
    // https://github.com/googlefonts/glyphsLib/blob/044f19e4/Lib/glyphsLib/builder/axes.py#L286
    let has_non_identity_mapping = font.axis_mappings.contains(&axis.name)
        && !font.axis_mappings.get(&axis.name).unwrap().is_identity()
        && min != max;

    let (converter, user_min, user_default, user_max) = if has_non_identity_mapping {
        let mappings: Vec<_> = font
            .axis_mappings
            .get(&axis.name)
            .unwrap()
            .iter()
            .map(|(u, d)| (UserCoord::new(*u), DesignCoord::new(*d)))
            .collect();
        let default_idx = find_by_design_coord(&mappings, default, axis.name.as_str(), "default")?;
        let min_idx = find_by_design_coord(&mappings, min, axis.name.as_str(), "min")?;
        let max_idx = find_by_design_coord(&mappings, max, axis.name.as_str(), "max")?;
        // Use user-space values directly from the mapping, matching glyphsLib.
        // Don't round-trip via design_to_user which is lossy for many-to-one maps.
        let user_min = mappings[min_idx].0;
        let user_default = mappings[default_idx].0;
        let user_max = mappings[max_idx].0;
        (
            CoordConverter::new(mappings, default_idx)?,
            user_min,
            user_default,
            user_max,
        )
    } else {
        // There is no meaningful mapping; design == user
        let min = UserCoord::new(min.into_inner());
        let max = UserCoord::new(max.into_inner());
        let default = UserCoord::new(default.into_inner());
        (
            CoordConverter::unmapped(min, default, max),
            min,
            default,
            max,
        )
    };

    Ok(fontdrasil::types::Axis {
        name: axis.name.clone(),
        tag: Tag::from_str(&axis.tag).map_err(|cause| Error::InvalidTag {
            raw_tag: axis.tag.clone(),
            cause,
        })?,
        hidden: axis.hidden.unwrap_or(false),
        min: user_min,
        default: user_default,
        max: user_max,
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
    /// The kern-group partition, lazily derived once by the
    /// `FontInfo::kern_groups` accessor; per-glyph attributes make it
    /// font-global, unlike UFO sources' per-master groups.
    pub kern_groups: OnceLock<BTreeMap<ir::KernGroup, BTreeSet<GlyphName>>>,
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
                    design_location(&axes, &m.axes_values)
                        .to_normalized(&axes)
                        .unwrap(),
                )
            })
            .chain(font.instances.iter().map(|i| {
                (
                    i.axes_values.clone(),
                    design_location(&axes, &i.axes_values)
                        .to_normalized(&axes)
                        .unwrap(),
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
            kern_groups: OnceLock::new(),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Colrv1RunType {
    NonColor,
    Solid(glyphs_reader::Color),
    // (start, end, colors) - geometry matters for distinguishing gradients
    Linear(
        Vec<OrderedFloat<f64>>,
        Vec<OrderedFloat<f64>>,
        Vec<glyphs_reader::ColorStop>,
    ),
    Radial(
        Vec<OrderedFloat<f64>>,
        Vec<OrderedFloat<f64>>,
        Vec<glyphs_reader::ColorStop>,
    ),
    Unknown(ShapeAttributes),
}

#[derive(Debug)]
struct Colrv1Run {
    run_type: Colrv1RunType,
    start: usize,
    end: usize,
}

impl Colrv1Run {
    fn color(&self) -> bool {
        !matches!(self.run_type, Colrv1RunType::NonColor)
    }
}

impl Colrv1RunType {
    fn key_for(layer: &Layer, shape: &Shape) -> Self {
        // COLRv1?
        if !layer.attributes.color {
            return Colrv1RunType::NonColor;
        }
        let attr = shape.attributes();
        if let Some(gradient) = &attr.gradient {
            if gradient.style == "circle" {
                return Colrv1RunType::Radial(
                    gradient.start.clone(),
                    gradient.end.clone(),
                    gradient.colors.clone(),
                );
            }
            return Colrv1RunType::Linear(
                gradient.start.clone(),
                gradient.end.clone(),
                gradient.colors.clone(),
            );
        }
        if let Some(fill) = attr.fill_color {
            return Colrv1RunType::Solid(fill);
        }
        Colrv1RunType::Unknown(attr.clone())
    }
}

fn new_color_glyph(original: &Glyph, nth: &mut usize) -> Glyph {
    let new_glyph_name: SmolStr = format!("{}.color{nth}", original.name).into();
    let new_production_name = original
        .production_name
        .as_ref()
        .map(|production_name| format!("{}.color{nth}", production_name).into());
    let new_glyph = Glyph {
        name: new_glyph_name.clone(),
        production_name: new_production_name,
        export: original.export,
        ..Default::default()
    };
    *nth += 1;
    new_glyph
}

/// Build the split color glyphs for a COLRv0 glyph.
///
/// As in glyphsLib, color layers are matched across masters by position:
/// the i-th color layer of each master contributes that master's geometry
/// to [original].color[i], so the color glyphs interpolate. The default
/// master determines how many color glyphs are created; a master with fewer
/// color layers simply contributes no source. An intermediate (brace) color
/// layer becomes a sparse intermediate source of the color glyph whose n-th
/// color layer shares its palette index (n counted in document order per
/// location), matching glyphsLib >= 6.14.0.
/// <https://github.com/googlefonts/glyphsLib/blob/v6.14.0/Lib/glyphsLib/builder/color_layers.py#L33-L107>
fn colrv0_color_glyphs(
    original: &Glyph,
    default_master_id: &str,
    master_ids: &[String],
) -> Vec<Glyph> {
    let glyph_name = &original.name;
    let mut layers_by_master: IndexMap<&str, Vec<&Layer>> = IndexMap::new();
    // master => brace location => palette index => layers, in document order.
    // Layers grouped under an id that is not a font master are never read:
    // consumption below is keyed by master_ids
    type BracesByLocation<'a> = IndexMap<&'a [OrderedFloat<f64>], IndexMap<i64, Vec<&'a Layer>>>;
    let mut braces_by_master: IndexMap<&str, BracesByLocation> = IndexMap::new();
    for layer in original.layers.iter() {
        let Some(palette_idx) = layer.attributes.color_palette else {
            continue;
        };
        if layer.shapes.is_empty() {
            continue;
        }
        let Some(master_id) = layer.associated_master_id.as_deref() else {
            continue;
        };
        if layer.is_intermediate() {
            braces_by_master
                .entry(master_id)
                .or_default()
                .entry(layer.attributes.coordinates.as_slice())
                .or_default()
                .entry(palette_idx)
                .or_default()
                .push(layer);
        } else {
            layers_by_master.entry(master_id).or_default().push(layer);
        }
    }
    let num_color_glyphs = layers_by_master
        .get(default_master_id)
        .map(Vec::len)
        .unwrap_or_default();

    let mut nth = 0;
    // new_glyphs[i] is named [original].color{i}, aligned with the i-th color
    // layer of each master below
    let mut new_glyphs: Vec<Glyph> = (0..num_color_glyphs)
        .map(|_| new_color_glyph(original, &mut nth))
        .collect();

    for master_id in master_ids {
        // this master's brace groups, one per (location, palette index)
        let brace_groups: Vec<&IndexMap<i64, Vec<&Layer>>> = braces_by_master
            .get(master_id.as_str())
            .into_iter()
            .flat_map(IndexMap::values)
            .collect();
        // how many color layers of this master used each palette index so
        // far; the n-th intermediate with an index pairs with the n-th
        // color layer with that index (glyphsLib's seen counter)
        let mut seen: IndexMap<i64, usize> = IndexMap::new();
        for (i, &layer) in layers_by_master
            .get(master_id.as_str())
            .into_iter()
            .flatten()
            .enumerate()
        {
            let palette_idx = layer.attributes.color_palette.unwrap();
            let n = seen.entry(palette_idx).or_default();
            let nth_with_index = *n;
            *n += 1;
            let Some(new_glyph) = new_glyphs.get_mut(i) else {
                // more color layers than the default master has; no color
                // glyph to attach to
                continue;
            };
            let mut master_layer = layer.clone();
            master_layer.layer_id = master_id.clone();
            master_layer.associated_master_id = None;
            new_glyph.layers.push(master_layer);
            // attach the matching intermediate, if any, at each location;
            // it keeps its associated master and coordinates and so
            // becomes an intermediate source of the color glyph
            for by_palette in brace_groups.iter() {
                if let Some(&brace) = by_palette
                    .get(&palette_idx)
                    .and_then(|layers| layers.get(nth_with_index))
                {
                    new_glyph.layers.push(brace.clone());
                }
            }
        }
        for by_palette in brace_groups {
            for (palette_idx, brace_layers) in by_palette.iter() {
                let consumed = seen.get(palette_idx).copied().unwrap_or_default();
                for brace in brace_layers.iter().skip(consumed) {
                    warn!(
                        "{glyph_name}: intermediate color layer {} has no matching color layer and will be skipped",
                        brace.layer_id
                    );
                }
            }
        }
    }
    new_glyphs
}

fn split_colrv0_glyph(
    original: &Glyph,
    default_master_id: &str,
    master_ids: &[String],
    color_glyphs: &mut IndexMap<SmolStr, Vec<SmolStr>>,
    additions: &mut Vec<(SmolStr, Glyph)>,
) -> Result<(), Error> {
    // COLRv0 runs are just consecutive shapes by palette index
    // The original glyph becomes uncolored,
    // each color run becomes a new glyph named [original].color[i]
    let new_glyphs = colrv0_color_glyphs(original, default_master_id, master_ids);

    for new_glyph in new_glyphs {
        debug!("Add COLRv0 {}", new_glyph.name);

        color_glyphs
            .entry(original.name.clone())
            .or_default()
            .push(new_glyph.name.clone());
        additions.push((new_glyph.name.clone(), new_glyph));
    }

    // The color_glyphs entry drives ColorGlyphsWork::exec: absent = not in
    // COLR, empty = paint the base glyph itself, non-empty = paint the splits.
    // Only a color-valued master layer (which glyphsLib reuses as a color
    // layer painting the base) may reserve an empty entry; an uncolored base
    // with no splits stays absent
    if let Some(default_master_layer) = original
        .layers
        .iter()
        .find(|l| l.layer_id == default_master_id)
        && default_master_layer.is_color()
        && !default_master_layer.shapes.is_empty()
    {
        color_glyphs.entry(original.name.clone()).or_default();
    }
    Ok(())
}

fn split_colrv1_glyph(
    glyph: &Glyph,
    default_master_layer: &Layer,
    color_glyphs: &mut IndexMap<SmolStr, Vec<SmolStr>>,
    additions: &mut Vec<(SmolStr, Glyph)>,
) -> Result<(), Error> {
    let glyph_name = &glyph.name;

    // Split into runs of the same paint
    let mut runs = VecDeque::<Colrv1Run>::new();
    for (idx, shape) in default_master_layer.shapes.iter().enumerate() {
        let run_type = Colrv1RunType::key_for(default_master_layer, shape);
        if let Some(curr) = runs.back_mut()
            && curr.run_type == run_type
        {
            // Extend the current run
            curr.end = idx + 1;
        } else {
            // New run
            runs.push_back(Colrv1Run {
                run_type,
                start: idx,
                end: idx + 1,
            });
        }
    }

    // Only one run we're done
    if runs.len() <= 1 {
        return Ok(());
    }

    // There are multiple runs, we must split this glyph apart
    // The original will remain but uncolored

    // Each color run becomes a new glyph named [original].color[i]
    let mut nth = 0;
    for run in runs {
        let new_glyph_name: SmolStr = format!("{glyph_name}.color{nth}").into();
        let mut new_glyph = new_color_glyph(glyph, &mut nth);

        // For each layer, chop the head that matches this paint group off glyph and attach it here
        for old_layer in glyph.layers.iter() {
            let mut new_layer = old_layer.clone();
            new_layer.attributes.color = run.color();
            new_layer.shapes = old_layer.shapes[run.start..run.end].to_vec();
            trace!(
                "{glyph_name} {} takes {} shapes for {run:?}",
                old_layer.layer_id,
                new_layer.shapes.len()
            );
            new_glyph.layers.push(new_layer);
        }

        let mut layer_sizes = new_glyph
            .layers
            .iter()
            .map(|l| l.shapes.len())
            .collect::<Vec<_>>();
        layer_sizes.sort();
        layer_sizes.dedup();
        if layer_sizes.len() != 1 {
            return Err(Error::BadGlyph(BadGlyph::new(
                new_glyph_name,
                BadGlyphKind::FrontendSpecific(format!("Inconsistent layer sizes {layer_sizes:?}")),
            )));
        }
        if layer_sizes.first() == Some(&0) {
            return Err(Error::BadGlyph(BadGlyph::new(
                new_glyph_name,
                BadGlyphKind::FrontendSpecific("All layers are empty?!".to_string()),
            )));
        }

        color_glyphs
            .entry(glyph_name.clone())
            .or_default()
            .push(new_glyph_name.clone());
        additions.push((new_glyph_name, new_glyph));
    }
    Ok(())
}

fn split_color_glyphs(font: Font) -> Result<(Font, IndexMap<SmolStr, Vec<SmolStr>>), Error> {
    // <https://github.com/googlefonts/glyphsLib/blob/99328059ec4799956ecef3d47ebcc13ae70dacff/Lib/glyphsLib/builder/glyph.py#L309-L357>
    let mut font = font;
    let mut color_glyphs: IndexMap<SmolStr, Vec<SmolStr>> = Default::default();
    let default_master_id = font.default_master().id.clone();
    let master_ids: Vec<String> = font.masters.iter().map(|m| m.id.clone()).collect();

    let mut additions: Vec<(SmolStr, Glyph)> = Vec::new();
    for glyph in font.glyphs.values_mut() {
        if let Some(default_master_layer) = glyph
            .layers
            .iter()
            .find(|l| l.layer_id == default_master_id)
        {
            // If 1..N layers with palette indices are associated this is COLRv0
            // See <https://github.com/googlefonts/glyphsLib/blob/99328059ec4799956ecef3d47ebcc13ae70dacff/Lib/glyphsLib/builder/glyph.py#L289-L292>
            if glyph.layers.iter().any(|l| {
                l.attributes.color_palette.is_some()
                    && l.associated_master_id.as_deref() == Some(default_master_id.as_str())
            }) {
                split_colrv0_glyph(
                    glyph,
                    &default_master_id,
                    &master_ids,
                    &mut color_glyphs,
                    &mut additions,
                )?;
            } else if default_master_layer.is_color() {
                split_colrv1_glyph(
                    glyph,
                    default_master_layer,
                    &mut color_glyphs,
                    &mut additions,
                )?;
                // For COLRv1 single-run glyphs (i.e. no split glyphs created, shapes in default layer),
                // reserve an entry with empty vec so it gets included in COLR (see ColorGlyphsWork::exec).
                // For v1 multi-run, an non-empty vec already exists from split_colrv1_glyph.
                if !default_master_layer.shapes.is_empty() {
                    color_glyphs.entry(glyph.name.clone()).or_default();
                }
            }
        }

        // Palette-valued intermediates belong to split color glyphs only;
        // left on the glyph, GlyphIrWork's is_intermediate() filter would
        // admit them into its own variation (as glyphsLib, which excludes
        // them from ordinary intermediate handling). Unconditional: one
        // associated with a non-default master trips no detection above
        glyph
            .layers
            .retain(|l| l.attributes.color_palette.is_none() || !l.is_intermediate());
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

pub(crate) fn to_ir_paint(
    palette: Option<&[glyphs_reader::Color]>,
    glyph_name: impl Into<GlyphName>,
    layer: &Layer,
    attr: &ShapeAttributes,
) -> Result<Paint, Error> {
    if let Some(palette_idx) = layer.attributes.color_palette {
        // 0xFFFF is a special COLR palette index meaning "use the text foreground color"
        if palette_idx == 0xFFFF {
            return Ok(Paint::Solid(PaintSolid { color: None }.into()));
        }
        let Some(palette) = palette else {
            return Err(Error::BadGlyph(BadGlyph::new(
                glyph_name,
                BadGlyphKind::FrontendSpecific("Uses palette but there isn't one".to_string()),
            )));
        };
        let Some(color) = palette.get(palette_idx as usize) else {
            return Err(Error::BadGlyph(BadGlyph::new(
                glyph_name,
                BadGlyphKind::FrontendSpecific(format!(
                    "Out of bounds palette index {palette_idx}"
                )),
            )));
        };
        return Ok(Paint::Solid(
            PaintSolid {
                color: Some(to_ir_color(*color)),
            }
            .into(),
        ));
    }
    if let Some(color) = attr.fill_color {
        return Ok(Paint::Solid(
            PaintSolid {
                color: Some(to_ir_color(color)),
            }
            .into(),
        ));
    }

    // Note: Gradient coordinates from Glyphs are percentages (0.0-1.0) of the layer's bounding box.
    // The scaling to absolute coordinates is done later in fontbe/src/colr.rs, in order to reuse
    // the already-computed glyf bounding boxes and avoid redundant work.
    if let Some(gradient) = &attr.gradient {
        // See <https://github.com/googlefonts/glyphsLib/blob/99328059ec4799956ecef3d47ebcc13ae70dacff/Lib/glyphsLib/builder/color_layers.py#L72>
        let start = Point::new(gradient.start[0].0, gradient.start[1].0);
        let end = Point::new(gradient.end[0].0, gradient.end[1].0);
        return match gradient.style.as_str() {
            "circle" => {
                // Glyphs radial gradient only has a single circle centered at 'start'
                // with the radius calculated as % of the max distance to bbox corners.
                Ok(Paint::RadialGradient(
                    PaintRadialGradient {
                        p0: start,
                        p1: start,
                        r0: None, // Defaults to 0
                        r1: None, // Calculated in backend
                        color_line: to_ir_color_stops(&gradient.colors),
                    }
                    .into(),
                ))
            }
            "" => {
                // p2 is calculated in backend after scaling to absolute coordinates
                // (rotation works differently in percentage vs absolute space).
                Ok(Paint::LinearGradient(
                    PaintLinearGradient {
                        p0: start,
                        p1: end,
                        p2: None,
                        color_line: to_ir_color_stops(&gradient.colors),
                    }
                    .into(),
                ))
            }
            _ => Err(Error::BadGlyph(BadGlyph::new(
                glyph_name,
                BadGlyphKind::FrontendSpecific(format!("Unrecognized gradient {}", gradient.style)),
            ))),
        };
    }

    Err(Error::BadGlyph(BadGlyph::new(
        glyph_name,
        BadGlyphKind::FrontendSpecific(format!(
            "Unable to produce paint for {:?}, {attr:?}",
            layer.attributes
        )),
    )))
}

#[cfg(test)]
mod tests {
    use glyphs_reader::{Font, Glyph, Layer, LayerAttributes, Node, Path};
    use std::path::PathBuf;
    use std::str::FromStr;

    use super::{FontInfo, split_color_glyphs, to_ir_path};

    fn testdata_dir() -> PathBuf {
        let dir = PathBuf::from("../resources/testdata");
        assert!(dir.is_dir(), "{dir:?} isn't a dir");
        dir
    }

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

    /// Test that glyphs with empty color palette layers are NOT added to color_glyphs.
    ///
    /// This reproduces a bug where a non-printing glyph like "CR" may nominally contain
    /// palette layers that trigger the COLRv0 code path, but none of the layers have shapes.
    /// The glyph was incorrectly added to color_glyphs, causing a panic when trying to access
    /// layer.shapes[0].
    #[test]
    fn colrv0_glyph_with_empty_palette_layers_is_skipped() {
        let mut font = Font::load(&testdata_dir().join("glyphs3/COLRv0-1layer.glyphs")).unwrap();
        let master_id = font.default_master().id.clone();

        // Add a glyph "CR" with palette layers but no shapes
        let cr_glyph = Glyph {
            name: "CR".into(),
            export: true,
            layers: vec![
                // Default master layer with empty shapes
                Layer {
                    layer_id: master_id.clone(),
                    associated_master_id: None,
                    width: 0.0.into(),
                    shapes: vec![], // Empty!
                    anchors: vec![],
                    attributes: LayerAttributes::default(),
                    ..Default::default()
                },
                // Palette layer has color_palette but empty shapes
                Layer {
                    layer_id: "palette-layer-1".to_string(),
                    associated_master_id: Some(master_id.clone()),
                    width: 0.0.into(),
                    shapes: vec![], // Empty!
                    anchors: vec![],
                    attributes: LayerAttributes {
                        color_palette: Some(0), // This triggers COLRv0 path
                        ..Default::default()
                    },
                    ..Default::default()
                },
            ],
            ..Default::default()
        };

        font.glyphs.insert("CR".into(), cr_glyph);
        font.glyph_order.push("CR".into());

        // this would panic with the old code
        let (_, color_glyphs) = split_color_glyphs(font).unwrap();

        // The glyph should NOT be in color_glyphs because it has no color content
        assert!(
            !color_glyphs.contains_key("CR"),
            "Glyph with empty palette layers should not be added to color_glyphs"
        );
    }

    /// The i-th color layer of every master, not just the default master's,
    /// contributes to split glyph [glyph].color[i], so color glyphs interpolate.
    #[test]
    fn colrv0_split_keeps_color_layers_of_all_masters() {
        let font =
            Font::load(&testdata_dir().join("glyphs3/COLRv0-2masters-brace.glyphs")).unwrap();
        let (font, color_glyphs) = split_color_glyphs(font).unwrap();

        assert_eq!(
            color_glyphs.get("A").map(Vec::as_slice),
            Some(["A.color0".into(), "A.color1".into()].as_slice())
        );

        let original = font.glyphs.get("A").unwrap();
        // (split glyph, palette index, id of the Bold master's color layer)
        for (split_name, palette_idx, bold_layer_id) in
            [("A.color0", 1, "c03"), ("A.color1", 0, "c04")]
        {
            // the master layers; A.color0 also carries the intermediate,
            // pinned by the test below
            let split_glyph = font.glyphs.get(split_name).unwrap();
            let masters: Vec<&Layer> = split_glyph
                .layers
                .iter()
                .filter(|l| l.is_master())
                .collect();
            assert_eq!(masters.len(), 2, "{split_name}");
            for (layer, expected_id) in masters.iter().zip(["m01", "m02"]) {
                assert_eq!(layer.layer_id, expected_id, "{split_name}");
                assert_eq!(
                    layer.attributes.color_palette,
                    Some(palette_idx),
                    "{split_name} {expected_id}"
                );
            }
            // the Bold layer must carry the Bold color layer's geometry
            let expected_shapes = &original
                .layers
                .iter()
                .find(|l| l.layer_id == bold_layer_id)
                .unwrap()
                .shapes;
            assert_eq!(&masters[1].shapes, expected_shapes, "{split_name}");
        }
    }

    /// Test that intermediate (brace) color layers are attached to the split
    /// color glyph with the matching palette index, as sparse intermediate
    /// sources, instead of leaking into the base glyph.
    ///
    /// glyphsLib (>= 6.14.0) matches an intermediate color layer to the n-th
    /// color layer sharing its palette index, in document order.
    #[test]
    fn colrv0_split_attaches_intermediate_color_layers() {
        let font =
            Font::load(&testdata_dir().join("glyphs3/COLRv0-2masters-brace.glyphs")).unwrap();
        let brace_shapes = font
            .glyphs
            .get("A")
            .unwrap()
            .layers
            .iter()
            .find(|l| l.layer_id == "cbrace")
            .unwrap()
            .shapes
            .clone();
        let (font, color_glyphs) = split_color_glyphs(font).unwrap();

        assert_eq!(
            color_glyphs.get("A").map(Vec::as_slice),
            Some(["A.color0".into(), "A.color1".into()].as_slice())
        );

        // the intermediate has colorPalette = 1, so it belongs to A.color0
        let color0 = font.glyphs.get("A.color0").unwrap();
        let brace = color0
            .layers
            .iter()
            .find(|l| l.is_intermediate())
            .expect("A.color0 should have an intermediate layer");
        assert_eq!(brace.associated_master_id.as_deref(), Some("m01"));
        assert_eq!(
            brace
                .attributes
                .coordinates
                .iter()
                .map(|c| c.0)
                .collect::<Vec<_>>(),
            vec![550.0]
        );
        assert_eq!(brace.shapes, brace_shapes);
        assert_eq!(color0.layers.len(), 3);

        // no intermediate with palette 0, so A.color1 has master layers only
        let color1 = font.glyphs.get("A.color1").unwrap();
        assert!(color1.layers.iter().all(|l| !l.is_intermediate()));
        assert_eq!(color1.layers.len(), 2);

        // the intermediate color layer must not remain a layer of the base
        // glyph, where it would pollute the base glyph's own variation
        let original = font.glyphs.get("A").unwrap();
        assert!(
            original
                .layers
                .iter()
                .all(|l| !(l.is_intermediate() && l.attributes.color_palette.is_some())),
            "base glyph still carries an intermediate color layer"
        );
    }

    fn square_path(dx: f64) -> glyphs_reader::Shape {
        let mut path = Path::new(true);
        for (x, y) in [
            (dx, 0.0),
            (dx + 100.0, 0.0),
            (dx + 100.0, 100.0),
            (dx, 100.0),
        ] {
            path.nodes.push(Node {
                pt: (x, y).into(),
                node_type: glyphs_reader::NodeType::Line,
            });
        }
        glyphs_reader::Shape::Path(path)
    }

    fn master_layer(master_id: &str, dx: f64) -> Layer {
        Layer {
            layer_id: master_id.to_string(),
            shapes: vec![square_path(dx)],
            ..Default::default()
        }
    }

    /// A color layer associated with a master; non-empty `coords` makes it an
    /// intermediate (brace) layer
    fn palette_layer(id: &str, master_id: &str, dx: f64, palette: i64, coords: &[f64]) -> Layer {
        Layer {
            layer_id: id.to_string(),
            associated_master_id: Some(master_id.to_string()),
            shapes: vec![square_path(dx)],
            attributes: LayerAttributes {
                color_palette: Some(palette),
                coordinates: coords.iter().map(|c| (*c).into()).collect(),
                ..Default::default()
            },
            ..Default::default()
        }
    }

    fn insert_glyph(font: &mut Font, name: &str, layers: Vec<Layer>) {
        let glyph = Glyph {
            name: name.into(),
            export: true,
            layers,
            ..Default::default()
        };
        font.glyphs.insert(name.into(), glyph);
        font.glyph_order.push(name.into());
    }

    /// A glyph whose only palette layer is an unmatched intermediate produces
    /// no split glyphs and must stay out of color_glyphs: an empty entry means
    /// "paint the (uncolored) base glyph itself" downstream. Palette
    /// intermediates are stripped from the base glyph either way, including
    /// when they never trip COLRv0 detection (glyph "B2": associated with a
    /// non-default master only).
    #[test]
    fn colrv0_unmatched_intermediate_does_not_make_base_a_color_glyph() {
        let mut font =
            Font::load(&testdata_dir().join("glyphs3/COLRv0-2masters-brace.glyphs")).unwrap();
        let master_id = font.default_master().id.clone();
        insert_glyph(
            &mut font,
            "B",
            vec![
                master_layer(&master_id, 0.0),
                palette_layer("bbrace", &master_id, 10.0, 0, &[550.0]),
            ],
        );
        insert_glyph(
            &mut font,
            "B2",
            vec![
                master_layer(&master_id, 0.0),
                palette_layer("b2brace", "m02", 10.0, 0, &[550.0]),
            ],
        );
        let (font, color_glyphs) = split_color_glyphs(font).unwrap();

        assert!(!color_glyphs.contains_key("B"));
        assert!(!font.glyphs.contains_key("B.color0"));
        for name in ["B", "B2"] {
            assert!(
                font.glyphs
                    .get(name)
                    .unwrap()
                    .layers
                    .iter()
                    .all(|l| !l.is_intermediate()),
                "{name} still carries an intermediate color layer"
            );
        }
    }

    /// A master layer that is itself a palette layer (glyphsLib reuses it as a
    /// color layer painting the base glyph) must keep its COLR entry even when
    /// the COLRv0 split produces no color glyphs.
    #[test]
    fn colrv0_unmatched_intermediate_keeps_colored_master_base() {
        let mut font =
            Font::load(&testdata_dir().join("glyphs3/COLRv0-2masters-brace.glyphs")).unwrap();
        let master_id = font.default_master().id.clone();
        insert_glyph(
            &mut font,
            "D",
            vec![
                Layer {
                    attributes: LayerAttributes {
                        color_palette: Some(0),
                        ..Default::default()
                    },
                    ..master_layer(&master_id, 0.0)
                },
                palette_layer("dbrace", &master_id, 10.0, 0, &[550.0]),
            ],
        );

        let (font, color_glyphs) = split_color_glyphs(font).unwrap();

        // an empty entry means "paint the base glyph itself", correct here
        // because the base is color-valued
        assert_eq!(color_glyphs.get("D"), Some(&vec![]));
        assert!(!font.glyphs.contains_key("D.color0"));
    }

    /// When several color layers share a palette index, the n-th intermediate
    /// with that index pairs with the n-th color layer with that index, in
    /// document order (glyphsLib's seen counter) -- not by index alone.
    #[test]
    fn colrv0_duplicate_palette_indices_pair_intermediates_by_occurrence() {
        let mut font =
            Font::load(&testdata_dir().join("glyphs3/COLRv0-2masters-brace.glyphs")).unwrap();
        let master_id = font.default_master().id.clone();
        insert_glyph(
            &mut font,
            "C",
            vec![
                master_layer(&master_id, 0.0),
                // intermediates listed before the color layers: document order
                // within each group drives the pairing, not adjacency
                palette_layer("brace0", &master_id, 10.0, 1, &[550.0]),
                palette_layer("brace1", &master_id, 20.0, 1, &[550.0]),
                palette_layer("c0", &master_id, 30.0, 1, &[]),
                palette_layer("c1", &master_id, 40.0, 1, &[]),
            ],
        );

        let (font, color_glyphs) = split_color_glyphs(font).unwrap();

        assert_eq!(
            color_glyphs.get("C").map(Vec::as_slice),
            Some(["C.color0".into(), "C.color1".into()].as_slice())
        );
        for (split_name, color_dx, brace_dx) in [("C.color0", 30.0, 10.0), ("C.color1", 40.0, 20.0)]
        {
            let layers = &font.glyphs.get(split_name).unwrap().layers;
            assert_eq!(layers.len(), 2, "{split_name}");
            assert_eq!(
                layers[0].shapes,
                vec![square_path(color_dx)],
                "{split_name}"
            );
            assert!(layers[1].is_intermediate(), "{split_name}");
            assert_eq!(
                layers[1].shapes,
                vec![square_path(brace_dx)],
                "{split_name}"
            );
        }
    }

    /// Test that COLRv1 glyphs with empty color layers are not added to color_glyphs.
    ///
    /// This is similar to the COLRv0 test but for the COLRv1 code path.
    #[test]
    fn colrv1_glyph_with_empty_color_layer_is_skipped() {
        let mut font = Font::load(&testdata_dir().join("glyphs3/COLRv1-gradient.glyphs")).unwrap();
        let master_id = font.default_master().id.clone();

        // Add a glyph "empty_color" with a color layer but no shapes
        let empty_glyph = Glyph {
            name: "empty_color".into(),
            export: true,
            layers: vec![
                // Default master layer - marked as color but empty shapes
                Layer {
                    layer_id: master_id.clone(),
                    associated_master_id: None,
                    width: 0.0.into(),
                    shapes: vec![], // Empty!
                    anchors: vec![],
                    attributes: LayerAttributes {
                        color: true, // This triggers COLRv1 path
                        ..Default::default()
                    },
                    ..Default::default()
                },
            ],
            ..Default::default()
        };

        font.glyphs.insert("empty_color".into(), empty_glyph);
        font.glyph_order.push("empty_color".into());

        // this would add the glyph incorrectly with old code
        let (_, color_glyphs) = split_color_glyphs(font).unwrap();

        // The glyph should NOT be in color_glyphs because it has no shapes
        assert!(
            !color_glyphs.contains_key("empty_color"),
            "COLRv1 glyph with empty color layer should not be added to color_glyphs"
        );
    }

    /// When multiple user-space values map to the same design-space value
    /// (a many-to-one axis map), the axis max should reflect the largest
    /// user-space value, not the result of a lossy design-to-user round-trip.
    /// https://github.com/googlefonts/ufo2ft/issues/978
    #[test]
    fn many_to_one_axis_map_preserves_max() {
        let font = Font::load(&testdata_dir().join("glyphs3/ManyToOneAxisMap.glyphs")).unwrap();
        let font_info = FontInfo::try_from(font).unwrap();
        let wght_tag = write_fonts::types::Tag::from_str("wght").unwrap();
        let wght = font_info.axes.get(&wght_tag).unwrap();
        // user=900 and user=1000 both map to design=1000;
        // axis max must be 1000 (the largest user value), not 900
        assert_eq!(wght.max, fontdrasil::coords::UserCoord::new(1000.0));
    }

    /// Test that a layer with palette index 0xFFFF produces a PaintSolid with color `None`.
    #[test]
    fn palette_index_0xffff() {
        use super::to_ir_paint;
        use fontir::ir::Paint;
        use glyphs_reader::ShapeAttributes;

        let layer = Layer {
            attributes: LayerAttributes {
                color_palette: Some(0xFFFF),
                ..Default::default()
            },
            ..Default::default()
        };
        let attr = ShapeAttributes::default();
        let paint = to_ir_paint(None, "test", &layer, &attr).unwrap();
        match paint {
            Paint::Solid(solid) => {
                assert_eq!(solid.color, None, "expected foreground paint (color: None)");
            }
            other => panic!("expected Paint::Solid, got {other:?}"),
        }
    }
}
