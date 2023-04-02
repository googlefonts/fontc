//! Generates the [hmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx) and
//! [hhea](https://learn.microsoft.com/en-us/typography/opentype/spec/hhea) tables

use std::cmp::{max, min};

use fontdrasil::orchestration::Work;
use read_fonts::types::FWord;
use write_fonts::{
    dump_table,
    tables::{glyf::Contour, hhea::Hhea, hmtx::Hmtx, vmtx::LongMetric},
    OtRound,
};

use crate::{
    error::Error,
    orchestration::{BeWork, Bytes, Context, Glyph},
};

struct MetricAndLimitWork {}

pub fn create_metric_and_limit_work() -> Box<BeWork> {
    Box::new(MetricAndLimitWork {})
}

#[derive(Debug, Default)]
struct GlyphLimits {
    min_left_side_bearing: Option<i16>,
    min_right_side_bearing: Option<u16>,
    x_max_extent: Option<i16>,
    advance_width_max: u16,
    max_points: u16,
    max_contours: u16,
    max_component_elements: u16,
}

impl GlyphLimits {
    fn update(&mut self, advance: u16, glyph: &Glyph) {
        // min side bearings are only for non-empty glyphs
        // we will presume only simple glyphs with no contours are empty
        if glyph.is_empty() {
            return;
        }

        let bbox = glyph.bbox();
        let left_side_bearing = bbox.x_min;
        // aw - (lsb + xMax - xMin) ... but if lsb == xMin then just advance - xMax?
        let right_side_bearing = advance.saturating_sub(bbox.x_max.try_into().unwrap());
        self.min_left_side_bearing = self
            .min_left_side_bearing
            .map(|v| min(v, left_side_bearing))
            .or(Some(left_side_bearing));
        self.min_right_side_bearing = self
            .min_right_side_bearing
            .map(|v| min(v, right_side_bearing))
            .or(Some(right_side_bearing));
        self.x_max_extent = self
            .x_max_extent
            .map(|v| max(v, bbox.x_max))
            .or(Some(bbox.x_max));
        self.advance_width_max = max(self.advance_width_max, advance);

        match glyph {
            Glyph::Simple(simple) => {
                let points = simple.contours().iter().map(Contour::len).sum::<usize>() as u16;
                self.max_points = max(self.max_points, points);
                self.max_contours = max(self.max_contours, simple.contours().len() as u16);
            }
            Glyph::Composite(composite) => {
                let num_components = composite.components().len() as u16;
                self.max_component_elements = max(self.max_component_elements, num_components);
            }
        };
    }
}

impl Work<Context, Error> for MetricAndLimitWork {
    /// Generate [hmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx)
    /// and [hhea](https://learn.microsoft.com/en-us/typography/opentype/spec/hhea)
    ///
    /// Touchup [maxp](https://learn.microsoft.com/en-us/typography/opentype/spec/maxp)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_final_static_metadata();
        let default_metrics = context
            .ir
            .get_global_metrics()
            .at(static_metadata.default_location());

        let mut glyph_limits = GlyphLimits::default();

        let mut long_metrics: Vec<LongMetric> = static_metadata
            .glyph_order
            .iter()
            .map(|gn| {
                let advance: u16 = context
                    .ir
                    .get_glyph_ir(gn)
                    .default_instance()
                    .width
                    .ot_round();
                let glyph = context.get_glyph(gn);
                glyph_limits.update(advance, &glyph);
                LongMetric {
                    advance,
                    side_bearing: glyph.bbox().x_min,
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
        let min_left_side_bearing = glyph_limits
            .min_left_side_bearing
            .unwrap_or_default()
            .into();
        let min_right_side_bearing: i16 = glyph_limits
            .min_right_side_bearing
            .unwrap_or_default()
            .try_into()
            .map_err(|_| Error::OutOfBounds {
                what: "min_right_side_bearing".into(),
                value: format!("{:?}", glyph_limits.min_right_side_bearing),
            })?;
        let min_right_side_bearing = min_right_side_bearing.into();
        let x_max_extent = glyph_limits.x_max_extent.unwrap_or_default().into();
        let hhea = Hhea {
            ascender: FWord::new(default_metrics.ascender.into_inner().ot_round()),
            descender: FWord::new(default_metrics.descender.into_inner().ot_round()),
            advance_width_max: glyph_limits.advance_width_max.into(),
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

        // Tell maxp a little more about the world
        let mut maxp = (*context.get_maxp()).clone();
        maxp.max_points = Some(glyph_limits.max_points);
        maxp.max_contours = Some(glyph_limits.max_contours);
        maxp.max_component_elements = Some(glyph_limits.max_component_elements);
        // TODO: set me properly
        if maxp.max_component_elements.unwrap() > 0 {
            maxp.max_component_depth = Some(1); // it's _at least_ 1 so let's start there for now
        }
        context.set_maxp(maxp);

        Ok(())
    }
}
