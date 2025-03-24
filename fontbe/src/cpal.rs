//! Generates a [CPAL](https://learn.microsoft.com/en-us/typography/opentype/spec/cpal) table.

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};
use fontdrasil::orchestration::{Access, Work};
use fontir::{ir, orchestration::WorkId as FeWorkId};
use log::debug;
use write_fonts::{tables::cpal::ColorRecord, tables::cpal::Cpal, NullableOffsetMarker};

#[derive(Debug)]
struct CpalWork {}

pub fn create_cpal_work() -> Box<BeWork> {
    Box::new(CpalWork {})
}

fn to_cpal_color(c: &ir::Color) -> ColorRecord {
    ColorRecord {
        red: c.r,
        green: c.g,
        blue: c.b,
        alpha: c.a,
    }
}

impl Work<Context, AnyWorkId, Error> for CpalWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Cpal.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Variant(AnyWorkId::Fe(FeWorkId::ColorPalettes))
    }

    /// Generate [CPAL](https://learn.microsoft.com/en-us/typography/opentype/spec/cpal)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // Guard clause: no colors
        let Some(colors) = context.ir.colors.try_get() else {
            return Ok(());
        };

        let color_records = colors
            .palettes
            .iter()
            .flat_map(|p| p.iter())
            .map(to_cpal_color)
            .collect::<Vec<_>>();

        if color_records.len() > u16::MAX as usize {
            return Err(Error::OutOfBounds {
                what: "Too many CPAL colorRecords".to_string(),
                value: format!("{}", color_records.len()),
            });
        }

        let entries_per_palette = colors.palettes[0].len();
        debug!(
            "CPAL has {} color records in {} palette(s) of {}",
            color_records.len(),
            colors.palettes.len(),
            entries_per_palette
        );
        context.cpal.set(Cpal {
            num_palette_entries: entries_per_palette as u16,
            num_palettes: colors.palettes.len() as u16,
            num_color_records: color_records.len() as u16,
            color_records_array: NullableOffsetMarker::new(Some(color_records)),
            color_record_indices: (0..colors.palettes.len())
                .map(|i| (i * entries_per_palette) as u16)
                .collect(),
            ..Default::default()
        });

        Ok(())
    }
}
