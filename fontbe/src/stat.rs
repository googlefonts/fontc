//! Generates a [stat](https://learn.microsoft.com/en-us/typography/opentype/spec/stat) table.

use std::collections::HashMap;

use log::trace;

use fontdrasil::orchestration::{Access, Work};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::{
    tables::stat::{AxisRecord, Stat},
    types::NameId,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct StatWork {}

pub fn create_stat_work() -> Box<BeWork> {
    Box::new(StatWork {})
}

impl Work<Context, AnyWorkId, Error> for StatWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Stat.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Variant(AnyWorkId::Fe(FeWorkId::StaticMetadata))
    }

    /// Generate [stat](https://learn.microsoft.com/en-us/typography/opentype/spec/stat)
    ///
    /// See <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/builder.py#L2688-L2810>
    /// Note that we support only a very simple STAT at time of writing.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();

        // Guard clause: don't produce fvar for a static font
        if static_metadata.axes.is_empty() {
            trace!("Skip stat; this is not a variable font");
            return Ok(());
        }

        let reverse_names: HashMap<_, _> = static_metadata
            .names
            .iter()
            // To match fontmake we should use the font-specific name range and not reuse
            // a well-known name, even if the name matches.
            .filter(|(key, _)| key.name_id.to_u16() > 255)
            .map(|(key, name)| (name, key.name_id))
            .collect();

        context.stat.set_unconditionally(
            Stat {
                design_axes: static_metadata
                    .axes
                    .iter()
                    .enumerate()
                    .map(|(idx, a)| AxisRecord {
                        axis_tag: a.tag,
                        axis_name_id: *reverse_names.get(&a.name).unwrap(),
                        axis_ordering: idx as u16,
                    })
                    .collect::<Vec<_>>()
                    .into(),
                elided_fallback_name_id: Some(NameId::SUBFAMILY_NAME),
                ..Default::default()
            }
            .into(),
        );

        Ok(())
    }
}
