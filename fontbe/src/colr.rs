//! Generates a [COLR](https://learn.microsoft.com/en-us/typography/opentype/spec/colr) table.

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};
use fontdrasil::orchestration::{Access, Work};
use fontir::orchestration::WorkId as FeWorkId;

#[derive(Debug)]
struct ColrWork {}

pub fn create_colr_work() -> Box<BeWork> {
    Box::new(ColrWork {})
}

impl Work<Context, AnyWorkId, Error> for ColrWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Colr.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Variant(FeWorkId::ColorPalettes.into())
    }

    /// Generate [COLR](https://learn.microsoft.com/en-us/typography/opentype/spec/colr)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // Temporary: if there is a palette generate an empty COLR, just so we can easily see a ttx_diff
        if context.ir.colors.try_get().is_some() {
            context.colr.set(Default::default());
        }
        Ok(())
    }
}
