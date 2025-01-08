//! Generates a [gasp](https://learn.microsoft.com/en-us/typography/opentype/spec/gasp) table.

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;

use write_fonts::tables::gasp::Gasp;

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct GaspWork {}

pub fn create_gasp_work() -> Box<BeWork> {
    Box::new(GaspWork {})
}

impl Work<Context, AnyWorkId, Error> for GaspWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Gasp.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .specific_instance(FeWorkId::StaticMetadata)
            .build()
    }

    /// Generate [gasp](https://learn.microsoft.com/en-us/typography/opentype/spec/gasp) if necessary
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let mut gasp_ranges = static_metadata.misc.gasp.clone();
        gasp_ranges.sort_by_key(|gr| gr.range_max_ppem);

        if gasp_ranges.is_empty() {
            return Ok(());
        }
        if gasp_ranges.len() > u16::MAX as usize {
            return Err(Error::OutOfBounds {
                what: "gasp".to_string(),
                value: format!("Too many records: {}", gasp_ranges.len()),
            });
        }

        let gasp = Gasp {
            version: 1, // "set to 1 in new fonts)" <https://learn.microsoft.com/en-us/typography/opentype/spec/gasp#gasp-table-formats>
            num_ranges: gasp_ranges.len() as u16,
            gasp_ranges,
        };
        context.gasp.set(gasp);

        Ok(())
    }
}
