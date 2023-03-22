//! Generates a [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name) table.

use fontdrasil::orchestration::Work;
use write_fonts::{
    tables::name::{Name, NameRecord},
    OffsetMarker,
};

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct NameWork {}

pub fn create_name_work() -> Box<BeWork> {
    Box::new(NameWork {})
}

impl Work<Context, Error> for NameWork {
    /// Generate [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_init_static_metadata();
        
        let name_records = static_metadata
            .names
            .iter()
            .map(|(key, value)| {
                let name_id: u16 = key.name_id.into();
                NameRecord {
                    name_id: name_id.into(),
                    platform_id: key.platform_id,
                    encoding_id: key.encoding_id,
                    language_id: key.lang_id,
                    string: OffsetMarker::new(value.clone()),
                }
            })
            .collect();

        context.set_name(Name::new(name_records));
        Ok(())
    }
}
