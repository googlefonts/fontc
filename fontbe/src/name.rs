//! Generates a [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name) table.

use fontdrasil::orchestration::{Access, Work};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::{
    tables::name::{Name, NameRecord},
    OffsetMarker,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct NameWork {}

pub fn create_name_work() -> Box<BeWork> {
    Box::new(NameWork {})
}

impl Work<Context, AnyWorkId, Error> for NameWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Name.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Variant(AnyWorkId::Fe(FeWorkId::StaticMetadata))
    }

    /// Generate [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();

        let name_records = static_metadata
            .names
            .iter()
            .map(|(key, value)| NameRecord {
                name_id: key.name_id,
                platform_id: key.platform_id,
                encoding_id: key.encoding_id,
                language_id: key.lang_id,
                string: OffsetMarker::new(value.clone()),
            })
            .collect::<Vec<_>>();

        context
            .name
            .set_unconditionally(Name::new(name_records.into_iter().collect()).into());
        Ok(())
    }
}
