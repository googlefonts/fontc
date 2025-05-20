//! Generates the [vmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/vmtx),
//! and [vhea](https://learn.microsoft.com/en-us/typography/opentype/spec/vhea) tables.

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;
use log::trace;
use write_fonts::{
    dump_table,
    tables::{vhea::Vhea, vmtx::Vmtx},
    types::FWord,
    OtRound,
};

use crate::{
    error::Error,
    metrics_and_limits::MetricsBuilder,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct VerticalMetricsWork {}

pub fn create_vertical_metrics_work() -> Box<BeWork> {
    Box::new(VerticalMetricsWork {})
}

impl Work<Context, AnyWorkId, Error> for VerticalMetricsWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Vmtx.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::GlobalMetrics)
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::ALL_GLYPHS)
            .variant(WorkId::ALL_GLYF_FRAGMENTS)
            // We need composite bboxes to be calculated:
            .variant(WorkId::Glyf)
            .build()
    }

    fn write_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(WorkId::Vmtx)
            .variant(WorkId::Vhea)
            .build()
    }

    fn also_completes(&self) -> Vec<AnyWorkId> {
        vec![WorkId::Vhea.into()]
    }

    /// Generate:
    ///
    /// * [vmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/vmtx)
    /// * [vhea](https://learn.microsoft.com/en-us/typography/opentype/spec/vhea)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();

        if !static_metadata.build_vertical {
            trace!("Skip vmtx and vhea; this is not a vertical font");
            return Ok(());
        }

        let glyph_order = context.ir.glyph_order.get();
        let default_metrics = context
            .ir
            .global_metrics
            .get()
            .at(static_metadata.default_location());

        // Collate vertical metrics
        let builder =
            glyph_order
                .iter()
                .fold(MetricsBuilder::default(), |mut builder, (_gid, gn)| {
                    let glyph = context.ir.get_glyph(gn.clone());
                    let instance = glyph.default_instance();

                    // https://github.com/googlefonts/ufo2ft/blob/2f11b0ff/Lib/ufo2ft/outlineCompiler.py#L882-L890
                    let advance = instance.height(&default_metrics);
                    let vertical_origin = instance.vertical_origin(&default_metrics);

                    let glyph = context.glyphs.get(&WorkId::GlyfFragment(gn.clone()).into());

                    let side_bearing = vertical_origin
                        - glyph.data.bbox().map(|bbox| bbox.y_max).unwrap_or_default();
                    let bounds_advance = glyph
                        .data
                        .bbox()
                        .map(|bbox| bbox.y_max as i32 - bbox.y_min as i32);

                    builder.update(advance, side_bearing, bounds_advance);
                    builder
                });

        let metrics = builder.build();

        // Build and send vertical metrics tables out into the world
        let vhea = Vhea {
            ascender: FWord::new(default_metrics.vhea_ascender.into_inner().ot_round()),
            descender: FWord::new(default_metrics.vhea_descender.into_inner().ot_round()),
            line_gap: FWord::new(default_metrics.vhea_line_gap.into_inner().ot_round()),
            advance_height_max: metrics.advance_max,
            min_top_side_bearing: metrics.min_first_side_bearing,
            min_bottom_side_bearing: metrics.min_second_side_bearing,
            y_max_extent: metrics.max_extent,
            caret_slope_rise: default_metrics
                .vhea_caret_slope_rise
                .into_inner()
                .ot_round(),
            caret_slope_run: default_metrics.vhea_caret_slope_run.into_inner().ot_round(),
            caret_offset: default_metrics.vhea_caret_offset.into_inner().ot_round(),
            number_of_long_ver_metrics: metrics.long_metrics.len().try_into().map_err(|_| {
                Error::OutOfBounds {
                    what: "number_of_long_metrics".into(),
                    value: format!("{}", metrics.long_metrics.len()),
                }
            })?,
        };
        context.vhea.set(vhea);

        let vmtx = Vmtx::new(metrics.long_metrics, metrics.first_side_bearings);
        let raw_vmtx = dump_table(&vmtx)
            .map_err(|e| Error::DumpTableError {
                e,
                context: "vmtx".into(),
            })?
            .into();
        context.vmtx.set(raw_vmtx);

        Ok(())
    }
}
