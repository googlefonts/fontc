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
    tables::colr::{
        BaseGlyphList, BaseGlyphPaint, Clip, ClipBox, ClipList, ColorLine, ColorStop, Colr, Extend,
        LayerList, Paint, PaintColrLayers, PaintGlyph, PaintLinearGradient, PaintRadialGradient,
        PaintSolid,
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

fn to_colr_paint(
    glyph_order: &GlyphOrder,
    palette: &ColorPalettes,
    glyph_name: &GlyphName,
    layer_list: &mut LayerList,
    ir_paint: &ir::Paint,
) -> Result<Paint, Error> {
    match ir_paint {
        ir::Paint::Glyph(paint) => Ok(Paint::Glyph(PaintGlyph {
            paint: to_colr_paint(glyph_order, palette, glyph_name, layer_list, &paint.paint)?
                .into(),
            glyph_id: glyph_order
                .glyph_id(&paint.name)
                .expect("Validated earlier"),
        })),
        ir::Paint::Solid(paint) => Ok(Paint::Solid(PaintSolid {
            palette_index: palette.index_of(paint.color).ok_or_else(|| {
                Error::GlyphError(
                    glyph_name.clone(),
                    GlyphProblem::NotInColorPalette(paint.color),
                )
            })? as u16,
            alpha: OPAQUE,
        })),
        ir::Paint::LinearGradient(linear) => Ok(Paint::LinearGradient(PaintLinearGradient::new(
            to_colr_line(palette, glyph_name, &linear.color_line)?,
            (linear.p0.x as i16).into(),
            (linear.p0.y as i16).into(),
            (linear.p1.x as i16).into(),
            (linear.p1.y as i16).into(),
            (linear.p2.x as i16).into(),
            (linear.p2.y as i16).into(),
        ))),
        ir::Paint::RadialGradient(radial) => Ok(Paint::RadialGradient(PaintRadialGradient::new(
            to_colr_line(palette, glyph_name, &radial.color_line)?,
            (radial.p0.x as i16).into(),
            (radial.p0.y as i16).into(),
            (radial.r0.0 as u16).into(),
            (radial.p1.x as i16).into(),
            (radial.p1.y as i16).into(),
            (radial.r1.0 as u16).into(),
        ))),
        ir::Paint::Layers(layers) => {
            let start_idx = layer_list.num_layers;
            for ir_paint in layers.iter() {
                let paint = to_colr_paint(glyph_order, palette, glyph_name, layer_list, ir_paint)?;
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
            base_glyphs.push(BaseGlyphPaint::new(
                glyph_order
                    .glyph_id(glyph_name)
                    .ok_or_else(|| Error::MissingGlyphId(glyph_name.clone()))?,
                to_colr_paint(&glyph_order, &palette, glyph_name, &mut layer_list, paint)?,
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
