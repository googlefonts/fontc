//! Generates a [stat](https://learn.microsoft.com/en-us/typography/opentype/spec/stat) table.

use std::collections::HashMap;

use log::trace;

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::{ir::StaticMetadata, orchestration::WorkId as FeWorkId};
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
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(WorkId::ExtraFeaTables)
            .build()
    }

    /// Generate [stat](https://learn.microsoft.com/en-us/typography/opentype/spec/stat)
    ///
    /// See <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/builder.py#L2688-L2810>
    /// Note that we support only a very simple STAT at time of writing.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let stat = match context
            .extra_fea_tables
            .try_get()
            .and_then(|tables| tables.stat.clone())
        {
            Some(stat) => {
                log::info!("Using STAT table from FEA");
                stat
            }
            // Guard clause: don't produce fvar for a static font
            None if static_metadata.axes.is_empty() => {
                trace!("Skip stat; this is not a variable font");
                return Ok(());
            }
            None => make_stat(&static_metadata),
        };

        context.stat.set(stat);
        Ok(())
    }
}

fn make_stat(static_metadata: &StaticMetadata) -> Stat {
    // Reuse an existing name record for the axis names if possible, but only in the
    // font-specific range (nameID >= 256), to match the behavior of fonttools:
    // https://github.com/fonttools/fonttools/blob/0bc8c028/Lib/fontTools/otlLib/builder.py#L3048-L3050
    let min_font_specific_name_id = NameId::new(256);
    let reusable_names: HashMap<&str, NameId> = static_metadata
        .reverse_names()
        .into_iter()
        .filter_map(|(name, ids)| {
            ids.into_iter()
                .find(|&id| id >= min_font_specific_name_id)
                .map(|id| (name, id))
        })
        .collect();

    Stat {
        design_axes: static_metadata
            .axes
            .iter()
            .enumerate()
            .map(|(idx, a)| AxisRecord {
                axis_tag: a.tag,
                axis_name_id: *reusable_names.get(a.ui_label_name()).unwrap(),
                axis_ordering: idx as u16,
            })
            .collect::<Vec<_>>()
            .into(),
        elided_fallback_name_id: Some(NameId::SUBFAMILY_NAME),
        ..Default::default()
    }
}
