//! Generates a [COLR](https://learn.microsoft.com/en-us/typography/opentype/spec/colr) table.

use std::collections::HashSet;

use kurbo::{Affine, Point, Rect};

use crate::{
    error::{Error, GlyphProblem},
    glyphs::affine_for,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};
use fontdrasil::{
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{
    ir::{self, ColorPalettes, GlyphOrder},
    orchestration::WorkId as FeWorkId,
};
use write_fonts::{
    OtRound,
    tables::{
        colr::{
            Affine2x3, BaseGlyph, BaseGlyphList, BaseGlyphPaint, Clip, ClipBox, ClipList,
            ColorLine, ColorStop, Colr, Extend, Layer, LayerList, Paint, PaintColrGlyph,
            PaintColrLayers, PaintGlyph, PaintLinearGradient, PaintRadialGradient, PaintSolid,
            PaintTransform, PaintTranslate,
        },
        glyf::{Bbox, Glyph as RawGlyph},
    },
    types::{F2Dot14, FWord, Fixed, GlyphId16},
};

#[derive(Debug)]
struct ColrWork {}

pub fn create_colr_work() -> Box<BeWork> {
    Box::new(ColrWork {})
}

fn to_colr_line(
    palette: &ColorPalettes,
    glyph_name: &GlyphName,
    stops: &[ir::ColorStop],
    extend: Extend,
) -> Result<ColorLine, Error> {
    let mut color_stops = Vec::with_capacity(stops.len());
    for stop in stops {
        color_stops.push(ColorStop::new(
            F2Dot14::from_f32(stop.offset.0),
            palette_index(palette, glyph_name, stop.color)?,
            F2Dot14::from_f32(stop.alpha.0),
        ));
    }

    Ok(ColorLine::new(extend, stops.len() as u16, color_stops))
}

fn palette_index(
    palette: &ColorPalettes,
    glyph_name: &GlyphName,
    color: ir::ColorReference,
) -> Result<u16, Error> {
    match color {
        ir::ColorReference::PaletteIndex(index) => Ok(index),
        ir::ColorReference::Color(color) => {
            palette.index_of(color).map(|i| i as u16).ok_or_else(|| {
                Error::GlyphError(glyph_name.clone(), GlyphProblem::NotInColorPalette(color))
            })
        }
    }
}

/// Calculate the radius for a radial gradient, matching glyphsLib's behavior.
///
/// Emulates how AppKit's "drawInRect:relativeCenterPosition:" calculates the radius.
/// The center point is given as percentages (0-1) of the bounding box dimensions.
/// The radius is the maximum distance from the center to any of the four corners.
///
/// See <https://github.com/googlefonts/glyphsLib/blob/99328059ec4799956ecef3d47ebcc13ae70dacff/Lib/glyphsLib/builder/color_layers.py#L57-L69>
fn scale_gradient_radius(bbox: &Bbox, center_pct_x: f64, center_pct_y: f64) -> u16 {
    let width = bbox.x_max as f64 - bbox.x_min as f64;
    let height = bbox.y_max as f64 - bbox.y_min as f64;

    // Convert center from percentage to absolute within the bbox dimensions (not yet offset)
    let center_x = width * center_pct_x;
    let center_y = height * center_pct_y;

    // Calculate distance to all 4 corners (relative to bbox origin at 0,0)
    let corners = [(0.0, 0.0), (width, 0.0), (0.0, height), (width, height)];

    let max_dist_squared = corners
        .iter()
        .map(|(x, y)| {
            let dx = x - center_x;
            let dy = y - center_y;
            dx * dx + dy * dy
        })
        .fold(0.0f64, f64::max);

    max_dist_squared.sqrt().ot_round()
}

/// Scale a gradient coordinate from percentage (0.0-1.0) to absolute coordinates
/// within the given bounding box.
///
/// Glyphs gradient coordinates are percentages of the layer's bounding box.
/// Returns floats - use OtRound when converting to i16.
/// See <https://github.com/googlefonts/glyphsLib/blob/99328059ec4799956ecef3d47ebcc13ae70dacff/Lib/glyphsLib/builder/color_layers.py#L72-L81>
fn scale_gradient_point(bbox: &Bbox, x_pct: f64, y_pct: f64) -> (f64, f64) {
    let x_abs = bbox.x_min as f64 + ((bbox.x_max as f64 - bbox.x_min as f64) * x_pct);
    let y_abs = bbox.y_min as f64 + ((bbox.y_max as f64 - bbox.y_min as f64) * y_pct);
    (x_abs, y_abs)
}

fn gradient_point(bbox: &Bbox, coordinates: ir::GradientCoordinates, point: Point) -> (f64, f64) {
    match coordinates {
        ir::GradientCoordinates::Relative => scale_gradient_point(bbox, point.x, point.y),
        ir::GradientCoordinates::Absolute => (point.x, point.y),
    }
}

/// Round a float to FWord using OtRound (round half up).
#[inline]
fn round_fword(value: f64) -> FWord {
    OtRound::<i16>::ot_round(value).into()
}

fn round_fixed(value: f64) -> Fixed {
    let rounded: f64 = (value * 65536.0).ot_round();
    Fixed::from_bits(rounded as i32)
}

/// Calculate the quantization factor for COLR ClipBoxes.
///
/// This quantizes to 1/10th of the font's upem, rounded to nearest multiple of 10.
/// E.g., 100 unit intervals for 1000 upem, 200 units for 2048 upem, etc.
/// This matches ufo2ft's behavior to maximize clipbox reuse.
///
/// See <https://github.com/googlefonts/ufo2ft/blob/1315f37d/Lib/ufo2ft/util.py#L646-L6600>
fn colr_clip_box_quantization(upem: u16) -> i16 {
    let upem_f = upem as f64;
    let factor = upem_f / 10.0;
    // Round to nearest 10
    (factor / 10.0).round() as i16 * 10
}

/// Quantize a bounding box to multiples of the given factor.
///
/// Expands the bbox by rounding xMin/yMin down and xMax/yMax up to the nearest
/// multiple of the factor. This matches fontTools' quantizeRect behavior.
///
/// See <https://github.com/fonttools/fonttools/blob/3b9a9f6d7ad146c30c9161e527bb3cd07aa9c57b/Lib/fontTools/misc/arrayTools.py#L287-L305>
fn quantize_bbox(bbox: &Bbox, factor: i16) -> Bbox {
    if factor <= 1 {
        return *bbox;
    }
    let factor = factor as i32;
    let floor = |v: i16| ((v as i32).div_euclid(factor) * factor).max(i16::MIN as i32) as i16;
    let ceil = |v: i16| (-(-(v as i32)).div_euclid(factor) * factor).min(i16::MAX as i32) as i16;
    Bbox {
        x_min: floor(bbox.x_min),
        y_min: floor(bbox.y_min),
        x_max: ceil(bbox.x_max),
        y_max: ceil(bbox.y_max),
    }
}

fn to_colr_paint(
    context: &Context,
    glyph_order: &GlyphOrder,
    palette: &ColorPalettes,
    glyph_name: &GlyphName,
    bbox: &Bbox,
    layer_list: &mut LayerList,
    ir_paint: &ir::Paint,
) -> Result<Paint, Error> {
    match ir_paint {
        ir::Paint::Glyph(paint) => {
            let glyph_id = glyph_order
                .glyph_id(&paint.name)
                .ok_or_else(|| Error::MissingGlyphId(paint.name.clone()))?;
            // Fetch the bbox of the referenced glyph for proper gradient scaling
            let ref_glyph = context
                .glyphs
                .get(&WorkId::GlyfFragment(paint.name.clone()).into());
            let bbox = ref_glyph.data.bbox().unwrap_or_default();

            Ok(Paint::Glyph(PaintGlyph {
                paint: to_colr_paint(
                    context,
                    glyph_order,
                    palette,
                    &paint.name,
                    &bbox,
                    layer_list,
                    &paint.paint,
                )?
                .into(),
                glyph_id,
            }))
        }
        ir::Paint::Solid(paint) => Ok(Paint::Solid(PaintSolid {
            palette_index: palette_index(palette, glyph_name, paint.color)?,
            alpha: F2Dot14::from_f32(paint.alpha.0),
        })),
        ir::Paint::LinearGradient(linear) => {
            // Scale gradient points from relative 0-1 to absolute coordinates
            let (x0, y0) = gradient_point(bbox, linear.coordinates, linear.p0);
            let (x1, y1) = gradient_point(bbox, linear.coordinates, linear.p1);
            let (x2, y2) = if let Some(p2) = linear.p2 {
                gradient_point(bbox, linear.coordinates, p2)
            } else {
                // Calculate perpendicular point: rotate p0-p1 vector by -90° around p0
                (x0 + (y1 - y0), y0 - (x1 - x0))
            };

            Ok(Paint::LinearGradient(PaintLinearGradient::new(
                to_colr_line(palette, glyph_name, &linear.color_line, linear.extend)?,
                round_fword(x0),
                round_fword(y0),
                round_fword(x1),
                round_fword(y1),
                round_fword(x2),
                round_fword(y2),
            )))
        }
        ir::Paint::RadialGradient(radial) => {
            // Scale gradient points from relative 0-1 to absolute coordinates
            let (x0, y0) = gradient_point(bbox, radial.coordinates, radial.p0);
            let (x1, y1) = gradient_point(bbox, radial.coordinates, radial.p1);
            // UFO radii are explicit font units; Glyphs uses implicit radii.
            let r0 = radial.r0.map(|r| r.0.ot_round()).unwrap_or(0);
            let r1 = if let Some(r) = radial.r1 {
                r.0.ot_round()
            } else {
                // Calculate radius from bbox dimensions, matching glyphsLib behavior
                scale_gradient_radius(bbox, radial.p1.x, radial.p1.y)
            };

            Ok(Paint::RadialGradient(PaintRadialGradient::new(
                to_colr_line(palette, glyph_name, &radial.color_line, radial.extend)?,
                round_fword(x0),
                round_fword(y0),
                r0.into(),
                round_fword(x1),
                round_fword(y1),
                r1.into(),
            )))
        }
        ir::Paint::Layers(layers) => {
            if layers.is_empty() {
                return Err(Error::InvalidColorGlyph(
                    glyph_name.clone(),
                    "no color layers".into(),
                ));
            }
            let mut paints = Vec::with_capacity(layers.len());
            for ir_paint in layers.iter() {
                let paint = to_colr_paint(
                    context,
                    glyph_order,
                    palette,
                    glyph_name,
                    bbox,
                    layer_list,
                    ir_paint,
                )?;
                paints.push(paint);
            }
            // Lower descendants before appending this node's direct children: its
            // LayerList slice must remain contiguous even when children have layers.
            Ok(append_layers(paints, layer_list))
        }
        ir::Paint::ColrGlyph(name) => Ok(Paint::ColrGlyph(PaintColrGlyph::new(
            glyph_order
                .glyph_id(name)
                .ok_or_else(|| Error::MissingGlyphId(name.clone()))?,
        ))),
        ir::Paint::Transform(paint) => {
            let [xx, yx, xy, yy, dx, dy] = paint.transform.as_coeffs().map(round_fixed);
            Ok(Paint::Transform(PaintTransform::new(
                to_colr_paint(
                    context,
                    glyph_order,
                    palette,
                    glyph_name,
                    bbox,
                    layer_list,
                    &paint.paint,
                )?,
                Affine2x3::new(xx, yx, xy, yy, dx, dy),
            )))
        }
        ir::Paint::Translate(paint) => Ok(Paint::Translate(PaintTranslate::new(
            to_colr_paint(
                context,
                glyph_order,
                palette,
                glyph_name,
                bbox,
                layer_list,
                &paint.paint,
            )?,
            round_fword(paint.dx),
            round_fword(paint.dy),
        ))),
    }
}

/// Build a tree when a source layer list exceeds PaintColrLayers' uint8 count.
fn append_layers(mut paints: Vec<Paint>, layer_list: &mut LayerList) -> Paint {
    while paints.len() > u8::MAX as usize {
        let mut remaining = paints.into_iter();
        let mut grouped = Vec::new();
        while remaining.len() > 0 {
            grouped.push(append_layers(
                remaining.by_ref().take(u8::MAX as usize).collect(),
                layer_list,
            ));
        }
        paints = grouped;
    }
    if paints.len() == 1 {
        return paints.pop().unwrap();
    }
    let first_layer_index = layer_list.paints.len() as u32;
    let count = paints.len() as u8;
    layer_list.paints.extend(paints.into_iter().map(Into::into));
    Paint::ColrLayers(PaintColrLayers::new(count, first_layer_index))
}

fn add_or_extend_clip(clips: &mut Vec<Clip>, quantization: i16, gid: GlyphId16, bounds: Rect) {
    // A bounded graph can exceed the range of a ClipBox. In that case omit the
    // optional box rather than truncate the artwork to fit an FWORD.
    if bounds.x0 < i16::MIN as f64
        || bounds.y0 < i16::MIN as f64
        || bounds.x1 > i16::MAX as f64
        || bounds.y1 > i16::MAX as f64
    {
        return;
    }
    let bbox = Bbox {
        x_min: bounds.x0.floor() as i16,
        y_min: bounds.y0.floor() as i16,
        x_max: bounds.x1.ceil() as i16,
        y_max: bounds.y1.ceil() as i16,
    };
    let next_clip = quantize_bbox(&bbox, quantization);
    let next_clip = ClipBox::format_1(
        next_clip.x_min.into(),
        next_clip.y_min.into(),
        next_clip.x_max.into(),
        next_clip.y_max.into(),
    );
    if let Some(curr) = clips.last_mut()
        && curr.end_glyph_id.to_u32() + 1 == gid.to_u32()
        && *curr.clip_box == next_clip
    {
        // wow wow wow, a run!
        curr.end_glyph_id = gid;
        return; // DONE
    }

    // Evidently we didn't make a run
    clips.push(Clip::new(gid, gid, next_clip));
}

fn is_paint_glyph_solid(paint: &ir::Paint) -> bool {
    if let ir::Paint::Glyph(paint_glyph) = paint
        && let ir::Paint::Solid(solid) = &paint_glyph.paint
    {
        solid.alpha.0 == 1.0
    } else {
        false
    }
}

fn colr_v0_compatible(paint: &ir::Paint) -> bool {
    if is_paint_glyph_solid(paint) {
        return true;
    }
    if let ir::Paint::Layers(layers) = paint
        && !layers.is_empty()
        && layers.len() <= u16::MAX as usize
        && layers.iter().all(is_paint_glyph_solid)
    {
        return true;
    }
    false
}

/// Invariants:
///
/// * All glyph names are valid
/// * All paint are PaintGlyph whose nested paint is PaintSolid (e.g. are COLRv0 compatible)
fn new_colr0(
    palette: &ColorPalettes,
    glyph_order: &GlyphOrder,
    glyph_name: &GlyphName,
    paints: &[&ir::PaintGlyph],
    layers: &mut Vec<Layer>,
) -> Result<BaseGlyph, Error> {
    let gid = glyph_order
        .glyph_id(glyph_name)
        .ok_or_else(|| Error::MissingGlyphId(glyph_name.clone()))?;
    let candidate_layers = paints
        .iter()
        .map(|p| {
            let ir::Paint::Solid(solid) = &p.paint else {
                unreachable!("We prevalidated, what is {p:?}?!");
            };
            let palette_idx = palette_index(palette, glyph_name, solid.color)?;
            Ok(Layer::new(
                glyph_order
                    .glyph_id(&p.name)
                    .ok_or_else(|| Error::MissingGlyphId(p.name.clone()))?,
                palette_idx,
            ))
        })
        .collect::<Result<Vec<_>, Error>>()?;
    let first_idx = if let Some(pos) = layers
        .windows(candidate_layers.len())
        .position(|window| candidate_layers.as_slice() == window)
    {
        pos
    } else {
        let pos = layers.len();
        layers.extend(candidate_layers);
        pos
    };
    Ok(BaseGlyph::new(gid, first_idx as u16, paints.len() as u16))
}

/// Find bases that must remain in BaseGlyphList even if their paint fits v0.
fn color_references(paint: &ir::Paint, referenced: &mut HashSet<GlyphName>) {
    match paint {
        ir::Paint::ColrGlyph(name) => {
            referenced.insert(name.clone());
        }
        ir::Paint::Glyph(glyph) => color_references(&glyph.paint, referenced),
        ir::Paint::Transform(transform) => color_references(&transform.paint, referenced),
        ir::Paint::Translate(translate) => color_references(&translate.paint, referenced),
        ir::Paint::Layers(layers) => {
            for paint in layers.iter() {
                color_references(paint, referenced);
            }
        }
        _ => (),
    }
}

/// Control bounds of the outlines a paint graph draws, after the transforms
/// written to COLR, as fontTools computes automatic clip boxes.
///
/// Paints that draw no outline contribute nothing, so a graph without any
/// PaintGlyph has no bounds.
struct ClipBounds<'a> {
    context: &'a Context,
    glyph_order: &'a GlyphOrder,
    graph: &'a ir::ColorGlyphs,
    visiting: HashSet<GlyphName>,
}

impl ClipBounds<'_> {
    fn base(&mut self, name: &GlyphName) -> Result<Option<Rect>, Error> {
        let mut bounds = None;
        self.base_into(name, Affine::IDENTITY, &mut bounds)?;
        Ok(bounds)
    }

    fn base_into(
        &mut self,
        name: &GlyphName,
        affine: Affine,
        bounds: &mut Option<Rect>,
    ) -> Result<(), Error> {
        if !self.visiting.insert(name.clone()) {
            return Err(Error::InvalidColorGlyph(
                name.clone(),
                "cyclic PaintColrGlyph reference".into(),
            ));
        }
        let paint = self.graph.base_glyphs.get(name).ok_or_else(|| {
            Error::InvalidColorGlyph(name.clone(), "missing PaintColrGlyph target".into())
        })?;
        self.paint(paint, affine, bounds)?;
        self.visiting.remove(name);
        Ok(())
    }

    fn paint(
        &mut self,
        paint: &ir::Paint,
        affine: Affine,
        bounds: &mut Option<Rect>,
    ) -> Result<(), Error> {
        match paint {
            ir::Paint::Glyph(glyph) => self.outline(&glyph.name, affine, bounds),
            ir::Paint::ColrGlyph(name) => self.base_into(name, affine, bounds),
            ir::Paint::Layers(layers) => {
                for layer in layers.iter() {
                    self.paint(layer, affine, bounds)?;
                }
                Ok(())
            }
            ir::Paint::Transform(transform) => {
                let inner = Affine::new(
                    transform
                        .transform
                        .as_coeffs()
                        .map(|v| round_fixed(v).to_f64()),
                );
                self.paint(&transform.paint, affine * inner, bounds)
            }
            ir::Paint::Translate(translate) => {
                let inner = Affine::translate((
                    OtRound::<i16>::ot_round(translate.dx) as f64,
                    OtRound::<i16>::ot_round(translate.dy) as f64,
                ));
                self.paint(&translate.paint, affine * inner, bounds)
            }
            ir::Paint::Solid(_) | ir::Paint::LinearGradient(_) | ir::Paint::RadialGradient(_) => {
                Ok(())
            }
        }
    }

    /// Extend `bounds` with the glyph's transformed control points, like fontTools' ControlBoundsPen.
    fn outline(
        &self,
        name: &GlyphName,
        affine: Affine,
        bounds: &mut Option<Rect>,
    ) -> Result<(), Error> {
        let glyph = self
            .context
            .glyphs
            .get(&WorkId::GlyfFragment(name.clone()).into());
        match &glyph.data {
            RawGlyph::Empty => (),
            RawGlyph::Simple(simple) => {
                for point in simple.contours.iter().flat_map(|c| c.iter()) {
                    let point = affine * Point::new(point.x as f64, point.y as f64);
                    *bounds = Some(bounds.map_or(Rect::from_points(point, point), |bounds| {
                        bounds.union_pt(point)
                    }));
                }
            }
            RawGlyph::Composite(composite) => {
                for component in composite.components() {
                    let gid = component.glyph.to_u32();
                    let component_name =
                        self.glyph_order.glyph_name(gid as usize).ok_or_else(|| {
                            Error::InvalidColorGlyph(
                                name.clone(),
                                format!("component glyph id {gid} is not in the glyph order"),
                            )
                        })?;
                    self.outline(component_name, affine * affine_for(component), bounds)?;
                }
            }
        }
        Ok(())
    }
}

impl Work<Context, AnyWorkId, Error> for ColrWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Colr.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::PaintGraph)
            .variant(FeWorkId::ColorPalettes)
            .variant(WorkId::ALL_GLYF_FRAGMENTS)
            .specific_instance(FeWorkId::GlyphOrder)
            .specific_instance(FeWorkId::StaticMetadata)
            .build()
    }

    /// Generate [COLR](https://learn.microsoft.com/en-us/typography/opentype/spec/colr)
    #[tracing::instrument(name = "fontbe::ColrWork::exec", skip_all)]
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let Some(paint_graph) = context.ir.paint_graph.try_get() else {
            return Ok(());
        };
        let palette = context.ir.colors.try_get().unwrap_or_default();
        let glyph_order = context.ir.glyph_order.get();
        let static_metadata = context.ir.static_metadata.get();
        let quantization = colr_clip_box_quantization(static_metadata.units_per_em);
        let is_variable = !static_metadata.axes.is_empty();
        let mut referenced_bases = HashSet::new();
        for name in glyph_order.names() {
            if let Some(paint) = paint_graph.base_glyphs.get(name) {
                color_references(paint, &mut referenced_bases);
            }
        }
        for name in &referenced_bases {
            if !glyph_order.contains(name) {
                return Err(Error::MissingGlyphId(name.clone()));
            }
            if !paint_graph.base_glyphs.contains_key(name) {
                return Err(Error::InvalidColorGlyph(
                    name.clone(),
                    "missing PaintColrGlyph target".into(),
                ));
            }
        }
        let mut clip_bounds = ClipBounds {
            context,
            glyph_order: &glyph_order,
            graph: &paint_graph,
            visiting: HashSet::new(),
        };

        let mut colr_v0_glyphs = Vec::new();
        let mut colr_v0_layers = Vec::new();
        let mut colr_v1_glyphs = Vec::with_capacity(paint_graph.base_glyphs.len());
        let mut colr_v1_layers = LayerList::default();
        let mut clips = Vec::new();

        for glyph_name in glyph_order.names() {
            let Some(paint) = paint_graph.base_glyphs.get(glyph_name) else {
                continue;
            };
            if colr_v0_compatible(paint)
                && !referenced_bases.contains(glyph_name)
                && !paint_graph.clip_boxes.contains_key(glyph_name)
            {
                // This can be a COLRv0 glyph!
                if let ir::Paint::Glyph(paint_glyph) = paint {
                    colr_v0_glyphs.push(new_colr0(
                        &palette,
                        &glyph_order,
                        glyph_name,
                        &[paint_glyph],
                        &mut colr_v0_layers,
                    )?);
                } else if let ir::Paint::Layers(layers) = paint {
                    let mut paint_glyphs: Vec<&ir::PaintGlyph> = Vec::with_capacity(layers.len());
                    for paint in layers.iter() {
                        let ir::Paint::Glyph(paint_glyph) = paint else {
                            unreachable!("We *just* checked for this! What is {paint:#?}");
                        };
                        paint_glyphs.push(paint_glyph);
                    }
                    colr_v0_glyphs.push(new_colr0(
                        &palette,
                        &glyph_order,
                        glyph_name,
                        &paint_glyphs,
                        &mut colr_v0_layers,
                    )?);
                } else {
                    unreachable!("We *just* checked for this! What is {paint:#?}");
                };
            } else {
                // This is too complicated and fiddly to be a COLRv0, use v1
                // Fetch the glyph's bounding box for gradient coordinate scaling
                let glyph = context
                    .glyphs
                    .get(&WorkId::GlyfFragment(glyph_name.clone()).into());
                let bbox = glyph.data.bbox().unwrap_or_default();

                colr_v1_glyphs.push(BaseGlyphPaint::new(
                    glyph_order
                        .glyph_id(glyph_name)
                        .ok_or_else(|| Error::MissingGlyphId(glyph_name.clone()))?,
                    to_colr_paint(
                        context,
                        &glyph_order,
                        &palette,
                        glyph_name,
                        &bbox,
                        &mut colr_v1_layers,
                        paint,
                    )?,
                ));
                let gid = glyph_order.glyph_id(glyph_name).unwrap();
                if let Some(bounds) = paint_graph.clip_boxes.get(glyph_name) {
                    add_or_extend_clip(&mut clips, 1, gid, *bounds);
                } else if !is_variable && paint_graph.clip_boxes.is_empty() {
                    // As in ufo2ft, explicit source clips replace automatic clips,
                    // and a graph that draws no outline gets none.
                    if let Some(bounds) = clip_bounds.base(glyph_name)? {
                        add_or_extend_clip(&mut clips, quantization, gid, bounds);
                    }
                }
            }
        }

        // Create the COLR table
        let num_v0_glyphs = colr_v0_glyphs.len() as u16;
        let num_colr_v0_layers = colr_v0_layers.len() as u16;
        let base_glyph_records = (!colr_v0_glyphs.is_empty()).then_some(colr_v0_glyphs);
        let layer_records = (!colr_v0_layers.is_empty()).then_some(colr_v0_layers);
        let mut colr = Colr::new(
            num_v0_glyphs,
            base_glyph_records,
            layer_records,
            num_colr_v0_layers,
        );
        if !colr_v1_glyphs.is_empty() {
            colr.base_glyph_list =
                BaseGlyphList::new(colr_v1_glyphs.len() as u32, colr_v1_glyphs).into();
        }
        if !colr_v1_layers.paints.is_empty() {
            colr_v1_layers.num_layers = colr_v1_layers.paints.len() as u32;
            colr.layer_list = colr_v1_layers.into();
        }
        if !clips.is_empty() {
            colr.clip_list = ClipList::new(1, clips.len() as u32, clips).into();
        }

        // All done, claim victory!
        context.colr.set(colr);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fontdrasil::types::GlyphName;

    #[test]
    fn clips_expand_negative_bounds_without_wrapping() {
        for (bounds, expected) in [
            (Rect::new(-1.0, -101.0, 101.0, -1.0), [-100, -200, 200, 0]),
            (
                Rect::new(-32768.0, -1.0, 32767.0, 1.0),
                [-32768, -100, 32767, 100],
            ),
        ] {
            let mut clips = Vec::new();
            add_or_extend_clip(&mut clips, 100, GlyphId16::new(1), bounds);
            assert_eq!(
                *clips[0].clip_box,
                ClipBox::format_1(
                    expected[0].into(),
                    expected[1].into(),
                    expected[2].into(),
                    expected[3].into(),
                )
            );
        }
        let mut clips = Vec::new();
        add_or_extend_clip(
            &mut clips,
            100,
            GlyphId16::new(1),
            Rect::new(0.0, 0.0, 40000.0, 100.0),
        );
        assert!(
            clips.is_empty(),
            "omit an unrepresentable automatic clip rather than crop the drawing"
        );
    }

    #[test]
    fn v1_large_layer_lists_preserve_paint_order() {
        let ir_ctx = fontir::orchestration::Context::new_root(Default::default());
        let context = Context::new_root(Default::default(), None, None, false, &ir_ctx);
        // 256 needs one grouping level; 65026 needs two.
        for count in [256, 65026] {
            let mut layer_list = LayerList::default();
            let paints = (0..count)
                .map(|index| {
                    ir::Paint::Solid(
                        ir::PaintSolid {
                            color: ir::ColorReference::PaletteIndex(index as u16),
                            alpha: 1.0.into(),
                        }
                        .into(),
                    )
                })
                .collect::<Vec<_>>();
            let result = to_colr_paint(
                &context,
                &GlyphOrder::new(),
                &ColorPalettes::default(),
                &GlyphName::new("test"),
                &Bbox::default(),
                &mut layer_list,
                &ir::Paint::Layers(paints.into()),
            )
            .unwrap();
            let mut pending = vec![&result];
            let mut colors = Vec::new();
            while let Some(paint) = pending.pop() {
                match paint {
                    Paint::Solid(solid) => colors.push(solid.palette_index),
                    Paint::ColrLayers(layers) => {
                        assert!(layers.num_layers > 0);
                        let start = layers.first_layer_index as usize;
                        pending.extend(
                            layer_list.paints[start..start + layers.num_layers as usize]
                                .iter()
                                .rev()
                                .map(|paint| &**paint),
                        );
                    }
                    _ => panic!("unexpected paint {paint:?}"),
                }
            }
            assert_eq!(colors, (0..count as u16).collect::<Vec<_>>());
        }
    }

    #[test]
    fn v0_palette_index_0xffff() {
        let palette = ColorPalettes::default();
        let mut glyph_order = GlyphOrder::new();
        let glyph_name = GlyphName::new("test");
        glyph_order.insert(glyph_name.clone());

        let paints = [ir::PaintGlyph {
            name: glyph_name.clone(),
            paint: ir::Paint::Solid(
                ir::PaintSolid {
                    color: ir::ColorReference::PaletteIndex(0xFFFF),
                    alpha: 1.0.into(),
                }
                .into(),
            ),
        }];

        let refs: Vec<&ir::PaintGlyph> = paints.iter().collect();

        let mut layers = Vec::new();
        let _base_glyph =
            new_colr0(&palette, &glyph_order, &glyph_name, &refs, &mut layers).unwrap();

        assert_eq!(layers.len(), 1);
        assert_eq!(layers[0].palette_index, 0xFFFF);
    }

    #[test]
    fn v1_palette_index_0xffff() {
        use fontir::orchestration::Context as IrContext;

        let ir_ctx = IrContext::new_root(Default::default());
        let context = Context::new_root(Default::default(), None, None, false, &ir_ctx);

        let palette = ColorPalettes::default();
        let mut glyph_order = GlyphOrder::new();
        let glyph_name = GlyphName::new("test");
        glyph_order.insert(glyph_name.clone());

        let bbox = Bbox {
            x_min: 0,
            y_min: 0,
            x_max: 100,
            y_max: 100,
        };
        let mut layer_list = LayerList::default();

        let ir_paint = ir::Paint::Solid(
            ir::PaintSolid {
                color: ir::ColorReference::PaletteIndex(0xFFFF),
                alpha: 1.0.into(),
            }
            .into(),
        );

        let paint = to_colr_paint(
            &context,
            &glyph_order,
            &palette,
            &glyph_name,
            &bbox,
            &mut layer_list,
            &ir_paint,
        )
        .unwrap();

        match paint {
            Paint::Solid(solid) => {
                assert_eq!(solid.palette_index, 0xFFFF);
            }
            _ => panic!("Expected Paint::Solid"),
        }
    }
}
