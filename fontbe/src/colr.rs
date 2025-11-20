//! Generates a [COLR](https://learn.microsoft.com/en-us/typography/opentype/spec/colr) table.

use crate::{
    error::{Error, GlyphProblem},
    orchestration::{AnyWorkId, BeWork, Context, Glyph, WorkId},
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
            BaseGlyph, BaseGlyphList, BaseGlyphPaint, Clip, ClipBox, ClipList, ColorLine,
            ColorStop, Colr, Extend, Layer, LayerList, Paint, PaintColrLayers, PaintGlyph,
            PaintLinearGradient, PaintRadialGradient, PaintSolid,
        },
        glyf::Bbox,
    },
    types::{F2Dot14, FWord, GlyphId16},
};

static OPAQUE: F2Dot14 = F2Dot14::ONE;

#[derive(Debug)]
struct ColrWork {}

pub fn create_colr_work() -> Box<BeWork> {
    Box::new(ColrWork {})
}

fn to_colr_line(
    palette: &ColorPalettes,
    glyph_name: &GlyphName,
    stops: &[ir::ColorStop],
) -> Result<ColorLine, Error> {
    let mut color_stops = Vec::with_capacity(stops.len());
    for stop in stops {
        color_stops.push(ColorStop::new(
            F2Dot14::from_f32(stop.offset.0),
            palette.index_of(stop.color).ok_or_else(|| {
                Error::GlyphError(
                    glyph_name.clone(),
                    GlyphProblem::NotInColorPalette(stop.color),
                )
            })? as u16,
            OPAQUE,
        ));
    }

    Ok(ColorLine::new(Extend::Pad, stops.len() as u16, color_stops))
}

/// Calculate the radius for a radial gradient, matching glyphsLib's behavior.
///
/// Emulates how AppKit's "drawInRect:relativeCenterPosition:" calculates the radius.
/// The center point is given as percentages (0-1) of the bounding box dimensions.
/// The radius is the maximum distance from the center to any of the four corners.
///
/// See <https://github.com/googlefonts/glyphsLib/blob/99328059ec4799956ecef3d47ebcc13ae70dacff/Lib/glyphsLib/builder/color_layers.py#L57-L69>
fn scale_gradient_radius(bbox: &Bbox, center_pct_x: f64, center_pct_y: f64) -> u16 {
    let width = (bbox.x_max - bbox.x_min) as f64;
    let height = (bbox.y_max - bbox.y_min) as f64;

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
    let x_abs = bbox.x_min as f64 + ((bbox.x_max - bbox.x_min) as f64 * x_pct);
    let y_abs = bbox.y_min as f64 + ((bbox.y_max - bbox.y_min) as f64 * y_pct);
    (x_abs, y_abs)
}

/// Round a float to FWord using OtRound (round half up).
#[inline]
fn round_fword(value: f64) -> FWord {
    OtRound::<i16>::ot_round(value).into()
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
    Bbox {
        x_min: ((bbox.x_min as i32 / factor) * factor) as i16,
        y_min: ((bbox.y_min as i32 / factor) * factor) as i16,
        x_max: (((bbox.x_max as i32 + factor - 1) / factor) * factor) as i16,
        y_max: (((bbox.y_max as i32 + factor - 1) / factor) * factor) as i16,
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
                glyph_id: glyph_order
                    .glyph_id(&paint.name)
                    .expect("Validated earlier"),
            }))
        }
        ir::Paint::Solid(paint) => Ok(Paint::Solid(PaintSolid {
            palette_index: palette.index_of(paint.color).ok_or_else(|| {
                Error::GlyphError(
                    glyph_name.clone(),
                    GlyphProblem::NotInColorPalette(paint.color),
                )
            })? as u16,
            alpha: OPAQUE,
        })),
        ir::Paint::LinearGradient(linear) => {
            // Scale gradient points from relative 0-1 to absolute coordinates
            let (x0, y0) = scale_gradient_point(bbox, linear.p0.x, linear.p0.y);
            let (x1, y1) = scale_gradient_point(bbox, linear.p1.x, linear.p1.y);
            let (x2, y2) = if let Some(p2) = linear.p2 {
                scale_gradient_point(bbox, p2.x, p2.y)
            } else {
                // Calculate perpendicular point: rotate p0-p1 vector by -90° around p0
                (x0 + (y1 - y0), y0 - (x1 - x0))
            };

            Ok(Paint::LinearGradient(PaintLinearGradient::new(
                to_colr_line(palette, glyph_name, &linear.color_line)?,
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
            let (x0, y0) = scale_gradient_point(bbox, radial.p0.x, radial.p0.y);
            let (x1, y1) = scale_gradient_point(bbox, radial.p1.x, radial.p1.y);
            // Handle optional radii
            let r0 = radial.r0.map(|r| r.0 as u16).unwrap_or(0); // default to 0
            let r1 = if let Some(r) = radial.r1 {
                // TODO: Semantics of explicit radius values are unclear. Are they absolute font units,
                // or percentages of bbox dimensions? For now treat as absolute, revisit when we have
                // a source format that actually provides explicit radii.
                r.0 as u16
            } else {
                // Calculate radius from bbox dimensions, matching glyphsLib behavior
                scale_gradient_radius(bbox, radial.p1.x, radial.p1.y)
            };

            Ok(Paint::RadialGradient(PaintRadialGradient::new(
                to_colr_line(palette, glyph_name, &radial.color_line)?,
                round_fword(x0),
                round_fword(y0),
                r0.into(),
                round_fword(x1),
                round_fword(y1),
                r1.into(),
            )))
        }
        ir::Paint::Layers(layers) => {
            let start_idx = layer_list.paints.len() as u32;
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
                layer_list.paints.push(paint.into());
            }
            Ok(Paint::ColrLayers(PaintColrLayers::new(
                layers.len() as u8,
                start_idx,
            )))
        }
    }
}

fn add_or_extend_clip(clips: &mut Vec<Clip>, quantization: i16, gid: GlyphId16, glyph: &Glyph) {
    let bbox = glyph.data.bbox().unwrap_or_default();
    // Quantize the bbox to maximize clipbox reuse
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
        && let ir::Paint::Solid(_) = &paint_glyph.paint
    {
        true
    } else {
        false
    }
}

fn colr_v0_compatible(paint: &ir::Paint) -> bool {
    if is_paint_glyph_solid(paint) {
        return true;
    }
    if let ir::Paint::Layers(layers) = paint
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
    let gid = glyph_order.glyph_id(glyph_name).expect("Prevalidated");
    let candidate_layers = paints
        .iter()
        .map(|p| {
            let ir::Paint::Solid(solid) = &p.paint else {
                unreachable!("We prevalidated, what is {p:?}?!");
            };
            let palette_idx = palette.index_of(solid.color).expect("Prevalidated");
            Layer::new(
                glyph_order.glyph_id(&p.name).expect("Prevalidated"),
                palette_idx as u16,
            )
        })
        .collect::<Vec<_>>();
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
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let Some(paint_graph) = context.ir.paint_graph.try_get() else {
            return Ok(());
        };
        let palette = context.ir.colors.try_get().unwrap_or_default();
        let glyph_order = context.ir.glyph_order.get();
        let static_metadata = context.ir.static_metadata.get();
        let quantization = colr_clip_box_quantization(static_metadata.units_per_em);

        let mut colr_v0_glyphs = Vec::new();
        let mut colr_v0_layers = Vec::new();
        let mut colr_v1_glyphs = Vec::with_capacity(paint_graph.base_glyphs.len());
        let mut colr_v1_layers = LayerList::default();
        let mut clips = Vec::new();

        for glyph_name in glyph_order.names() {
            let Some(paint) = paint_graph.base_glyphs.get(glyph_name) else {
                continue;
            };
            if colr_v0_compatible(paint) {
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
                add_or_extend_clip(
                    &mut clips,
                    quantization,
                    glyph_order.glyph_id(glyph_name).expect("Prevalidated"),
                    &glyph,
                );
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
