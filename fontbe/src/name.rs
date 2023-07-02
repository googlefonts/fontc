//! Generates a [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name) table.

use fontdrasil::orchestration::Work;
use write_fonts::{
    tables::name::{Name, NameRecord},
    OffsetMarker,
};

use crate::{
    error::Error,
    orchestration::{BeWork, Context, WorkId},
};

struct NameWork {}

pub fn create_name_work() -> Box<BeWork> {
    Box::new(NameWork {})
}

impl Work<Context, WorkId, Error> for NameWork {
    fn id(&self) -> WorkId {
        WorkId::Name
    }

    /// Generate [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_init_static_metadata();

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

        context.set_name(Name::new(name_records.into_iter().collect()));
        Ok(())
    }
}
