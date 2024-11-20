//! Generates a [meta](https://learn.microsoft.com/en-us/typography/opentype/spec/meta) table.

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::tables::meta::{DataMapRecord, Meta};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct MetaWork {}

pub fn create_meta_work() -> Box<BeWork> {
    Box::new(MetaWork {})
}

impl Work<Context, AnyWorkId, Error> for MetaWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Meta.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata) // For meta records
            .build()
    }

    /// Generate [meta](https://learn.microsoft.com/en-us/typography/opentype/spec/meta)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // Build from static metadata, if at least one record is provided.
        let meta = {
            let static_metadata = context.ir.static_metadata.get();
            let records = &static_metadata.misc.meta;

            if records.is_empty() {
                None
            } else {
                // Instantiate DataMapRecords from tuples.
                Some(Meta::new(
                    records
                        .iter()
                        .map(|(tag, metadata)| DataMapRecord::new(*tag, metadata.clone()))
                        .collect(),
                ))
            }
        };

        if let Some(meta) = meta {
            context.meta.set(meta);
        };

        Ok(())
    }
}
