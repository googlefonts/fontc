//! Generates a [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name) table.

use std::collections::BTreeMap;

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::{
    tables::name::{Name, NameRecord},
    types::NameId,
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
        AccessBuilder::new()
            .variant(AnyWorkId::Fe(FeWorkId::StaticMetadata))
            .variant(AnyWorkId::Be(WorkId::ExtraFeaTables))
            .build()
    }

    /// Generate [name](https://learn.microsoft.com/en-us/typography/opentype/spec/name)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let extra_tables = context.extra_fea_tables.try_get();

        let mut name_records = static_metadata
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

        if let Some(name) = extra_tables
            .as_ref()
            .and_then(|tables| tables.name.as_ref())
        {
            log::info!("merging name records from FEA");
            name_records = merge_name_records(name_records, name);
        } else {
            name_records.sort();
        }

        context.name.set(Name::new(name_records));
        Ok(())
    }
}

// records from fea overwrite records derived elsewhere:
// https://github.com/fonttools/fonttools/blob/b90ac3c29f6030ec/Lib/fontTools/feaLib/builder.py#L452
fn merge_name_records(records: Vec<NameRecord>, fea_names: &Name) -> Vec<NameRecord> {
    fn split_key_and_string(name_record: NameRecord) -> ((u16, u16, u16, NameId), String) {
        let NameRecord {
            platform_id,
            encoding_id,
            language_id,
            name_id,
            string,
        } = name_record;
        (
            (platform_id, encoding_id, language_id, name_id),
            string.into_inner(),
        )
    }

    let sorted_and_deduped = records
        .into_iter()
        .chain(fea_names.name_record.iter().cloned())
        .map(split_key_and_string)
        .collect::<BTreeMap<_, _>>();

    sorted_and_deduped
        .into_iter()
        .map(|((platform, encoding, language, name), string)| {
            NameRecord::new(platform, encoding, language, name, string.into())
        })
        .collect()
}
