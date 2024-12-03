//! Generate a [meta](https://learn.microsoft.com/en-us/typography/opentype/spec/meta) table

use fontdrasil::orchestration::{Access, Work};
use fontir::orchestration::WorkId as FeWorkId;

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

use write_fonts::{
    tables::meta::{DataMapRecord, Meta, Metadata, ScriptLangTag},
    types::Tag,
};

const DLNG: Tag = Tag::new(b"dlng");
const SLNG: Tag = Tag::new(b"slng");

#[derive(Debug)]
struct MetaWork;

pub fn create_meta_work() -> Box<BeWork> {
    Box::new(MetaWork)
}

impl Work<Context, AnyWorkId, Error> for MetaWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Meta.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Variant(FeWorkId::StaticMetadata.into())
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let Some(meta_table) = static_metadata.misc.meta_table.as_ref() else {
            return Ok(());
        };

        let records = [(DLNG, &meta_table.dlng), (SLNG, &meta_table.slng)]
            .into_iter()
            .filter(|(_, items)| !items.is_empty())
            .map(|(tag, scriptlangs)| {
                let scriptlangs = scriptlangs
                    .iter()
                    .filter_map(
                        |scriptlang| match ScriptLangTag::new(scriptlang.to_string()) {
                            Ok(scriptlang) => Some(scriptlang),
                            Err(bad) => {
                                log::warn!("Invalid ScriptLangTag '{scriptlang}': '{bad}'");
                                None
                            }
                        },
                    )
                    .collect();
                DataMapRecord::new(tag, Metadata::ScriptLangTags(scriptlangs))
            })
            .collect();

        let meta = Meta::new(records);
        context.meta.set(meta);

        Ok(())
    }
}
