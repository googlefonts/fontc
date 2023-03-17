//! Generates the [hmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx) and
//! [hhea](https://learn.microsoft.com/en-us/typography/opentype/spec/hhea) tables

use std::cmp::{max, min};

use fontdrasil::orchestration::Work;
use write_fonts::{
    dump_table,
    tables::{hhea::Hhea, hmtx::Hmtx, vmtx::LongMetric},
    OtRound,
};

use crate::{
    error::Error,
    orchestration::{BeWork, Bytes, Context},
};

struct HMetricWork {}

pub fn create_hmetric_work() -> Box<BeWork> {
    Box::new(HMetricWork {})
}

impl Work<Context, Error> for HMetricWork {
    /// Generate [hmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx)
    /// and [hhea](https://learn.microsoft.com/en-us/typography/opentype/spec/hhea)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_final_static_metadata();

        let mut min_left_side_bearing = None;
        let mut min_right_side_bearing = None;
        let mut x_max_extent = None;

        let mut long_metrics: Vec<LongMetric> = static_metadata
            .glyph_order
            .iter()
            .map(|gn| {
                let glyph = context.get_glyph(gn);
                let bbox = glyph.bbox();
                let ir_glyph = context.ir.get_glyph_ir(gn);
                let ir_instance = ir_glyph.default_instance();
                let advance: u16 = ir_instance.width.ot_round();
                let left_side_bearing = bbox.x_min;
                // aw - (lsb + xMax - xMin) ... but if lsb == xMin then just advance - xMax?
                let right_side_bearing = advance.saturating_sub(bbox.x_max.try_into().unwrap());

                // min side bearings are only for non-empty glyphs
                // we will presume only simple glyphs with no contours are empty
                if !glyph.is_empty() {
                    min_left_side_bearing = min_left_side_bearing
                        .map(|v| min(v, left_side_bearing))
                        .or(Some(left_side_bearing));
                    min_right_side_bearing = min_right_side_bearing
                        .map(|v| min(v, right_side_bearing))
                        .or(Some(right_side_bearing));
                    x_max_extent = x_max_extent
                        .map(|v| max(v, bbox.x_max))
                        .or(Some(bbox.x_max));
                }

                LongMetric {
                    advance,
                    side_bearing: left_side_bearing,
                }
            })
            .collect();

        // If there's a run at the end with matching advances we can save some bytes
        let num_lsb_only = if !long_metrics.is_empty() {
            let last_advance = long_metrics.last().unwrap().advance;
            let mut lsb_run = 0;
            for metric in long_metrics.iter().rev() {
                if metric.advance != last_advance {
                    break;
                }
                lsb_run += 1;
            }

            // Carve 1 less than the length of the run off so the last metric retained has the advance
            // that repeats
            lsb_run - 1
        } else {
            0
        };

        let lsbs = long_metrics
            .split_off(long_metrics.len() - num_lsb_only)
            .into_iter()
            .map(|metric| metric.side_bearing)
            .collect();

        // Before we cede ownership of Hmtx grab a few notes for Hhea
        let min_left_side_bearing = min_left_side_bearing.unwrap_or_default().into();
        let min_right_side_bearing: i16 = min_right_side_bearing
            .unwrap_or_default()
            .try_into()
            .map_err(|_| Error::OutOfBounds {
                what: "min_right_side_bearing".into(),
                value: format!("{min_right_side_bearing:?}"),
            })?;
        let min_right_side_bearing = min_right_side_bearing.into();
        let x_max_extent = x_max_extent.unwrap_or_default().into();
        let hhea = Hhea {
            advance_width_max: long_metrics
                .iter()
                .map(|m| m.advance)
                .max()
                .unwrap_or_default()
                .into(),
            min_left_side_bearing,
            min_right_side_bearing,
            x_max_extent,
            number_of_long_metrics: long_metrics.len().try_into().map_err(|_| {
                Error::OutOfBounds {
                    what: "number_of_long_metrics".into(),
                    value: format!("{}", long_metrics.len()),
                }
            })?,
            ..Default::default()
        };
        context.set_hhea(hhea);

        // Send hmtx out into the world
        let hmtx = Hmtx::new(long_metrics, lsbs);
        let raw_hmtx = Bytes::new(dump_table(&hmtx).map_err(|report| Error::DumpTableError {
            report,
            context: "hmtx".into(),
        })?);
        context.set_hmtx(raw_hmtx);

        Ok(())
    }
}
