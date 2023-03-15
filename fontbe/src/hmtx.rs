//! Generates a [hmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx) table.

use fontdrasil::orchestration::Work;
use write_fonts::{
    dump_table,
    tables::{hmtx::Hmtx, vmtx::LongMetric},
};

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

pub struct RawHmtx {
    pub buf: Vec<u8>,
}

struct HmtxWork {}

pub fn create_hmtx_work() -> Box<BeWork> {
    Box::new(HmtxWork {})
}

impl Work<Context, Error> for HmtxWork {
    /// Generate [hmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_final_static_metadata();
        let var_model = &static_metadata.variation_model;
        let default_location = var_model.default_location();

        let mut long_metrics: Vec<LongMetric> = static_metadata.glyph_order.iter()
            .map(|gn| {
                let bbox = context.get_glyph(gn).bbox();
                let ir_glyph = context.ir.get_glyph_ir(gn);
                let Some(ir_instance) = ir_glyph.sources.get(default_location) else {
                    panic!("{gn} is not defined at the default location.");
                };
                let advance: u16 = ir_instance.width as u16;

                LongMetric {
                    advance,
                    side_bearing: bbox.x_min
                }
            }).collect();

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

        let hmtx = Hmtx::new(long_metrics, lsbs);
        let raw_hmtx = RawHmtx {
            buf: dump_table(&hmtx).map_err(|report| Error::DumpTableError {
                report,
                context: "hmtx".into(),
            })?,
        };
        context.set_hmtx(raw_hmtx);
        Ok(())
    }
}
