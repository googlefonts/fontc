//! Generates a [COLR](https://learn.microsoft.com/en-us/typography/opentype/spec/colr) table.

use crate::{
    error::{Error, GlyphProblem},
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
    tables::{
        colr::{
            BaseGlyphList, BaseGlyphPaint, Clip, ClipBox, ClipList, ColorLine, ColorStop, Colr,
            Extend, LayerList, Paint, PaintColrLayers, PaintGlyph, PaintLinearGradient,
            PaintRadialGradient, PaintSolid,
        },
        glyf::Bbox,
    },
    types::F2Dot14,
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

    max_dist_squared.sqrt().round() as u16
}

/// Scale a gradient coordinate from percentage (0.0-1.0) to absolute coordinates
/// within the given bounding box.
///
/// Glyphs gradient coordinates are percentages of the layer's bounding box.
/// See <https://github.com/googlefonts/glyphsLib/blob/99328059ec4799956ecef3d47ebcc13ae70dacff/Lib/glyphsLib/builder/color_layers.py#L72-L81>
fn scale_gradient_point(bbox: &Bbox, x_pct: f64, y_pct: f64) -> (i16, i16) {
    let x_abs = bbox.x_min as f64 + ((bbox.x_max - bbox.x_min) as f64 * x_pct);
    let y_abs = bbox.y_min as f64 + ((bbox.y_max - bbox.y_min) as f64 * y_pct);
    (x_abs.round() as i16, y_abs.round() as i16)
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
                x0.into(),
                y0.into(),
                x1.into(),
                y1.into(),
                x2.into(),
                y2.into(),
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
                x0.into(),
                y0.into(),
                r0.into(),
                x1.into(),
                y1.into(),
                r1.into(),
            )))
        }
        ir::Paint::Layers(layers) => {
            let start_idx = layer_list.num_layers;
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
            .build()
    }

    /// Generate [COLR](https://learn.microsoft.com/en-us/typography/opentype/spec/colr)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let Some(paint_graph) = context.ir.paint_graph.try_get() else {
            return Ok(());
        };
        let palette = context.ir.colors.try_get().unwrap_or_default();
        let glyph_order = context.ir.glyph_order.get();
        let mut colr = Colr::new(0, None, None, 0);
        let mut base_glyphs = Vec::with_capacity(paint_graph.base_glyphs.len());
        let mut layer_list = LayerList::default();
        for (glyph_name, paint) in paint_graph.base_glyphs.iter() {
            // Fetch the glyph's bounding box for gradient coordinate scaling
            let glyph = context
                .glyphs
                .get(&WorkId::GlyfFragment(glyph_name.clone()).into());
            let bbox = glyph.data.bbox().unwrap_or_default();

            base_glyphs.push(BaseGlyphPaint::new(
                glyph_order
                    .glyph_id(glyph_name)
                    .ok_or_else(|| Error::MissingGlyphId(glyph_name.clone()))?,
                to_colr_paint(
                    context,
                    &glyph_order,
                    &palette,
                    glyph_name,
                    &bbox,
                    &mut layer_list,
                    paint,
                )?,
            ));
        }

        colr.base_glyph_list =
            BaseGlyphList::new(paint_graph.base_glyphs.len() as u32, base_glyphs).into();
        if !layer_list.paints.is_empty() {
            layer_list.num_layers = layer_list.paints.len() as u32;
            colr.layer_list = layer_list.into();
        }

        let mut clips = Vec::<Clip>::new();
        for glyph_name in paint_graph.base_glyphs.iter().map(|(g, _)| g) {
            let next_gid = glyph_order.glyph_id(glyph_name).expect("Validated earlier");
            let next_glyph = context
                .glyphs
                .get(&WorkId::GlyfFragment(glyph_name.clone()).into());
            let next_clip = next_glyph.data.bbox().unwrap_or_default();
            let next_clip = ClipBox::format_1(
                next_clip.x_min.into(),
                next_clip.y_min.into(),
                next_clip.x_max.into(),
                next_clip.y_max.into(),
            );
            if let Some(curr) = clips.last_mut()
                && curr.end_glyph_id.to_u32() + 1 == next_gid.to_u32()
                && *curr.clip_box == next_clip
            {
                // wow wow wow, a run!
                curr.end_glyph_id = next_gid;
                continue; // DONE
            }

            // Evidently we didn't make a run
            clips.push(Clip::new(next_gid, next_gid, next_clip));
        }
        colr.clip_list = ClipList::new(1, clips.len() as u32, clips).into();

        // All done, claim victory!
        context.colr.set(colr);
        Ok(())
    }
}
