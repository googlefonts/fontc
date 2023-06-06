//! Generates a [stat](https://learn.microsoft.com/en-us/typography/opentype/spec/stat) table.

use std::collections::HashMap;

use font_types::NameId;
use fontdrasil::orchestration::Work;
use log::trace;
use write_fonts::tables::stat::{AxisRecord, Stat};

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct StatWork {}

pub fn create_stat_work() -> Box<BeWork> {
    Box::new(StatWork {})
}

impl Work<Context, Error> for StatWork {
    /// Generate [stat](https://learn.microsoft.com/en-us/typography/opentype/spec/stat)
    ///
    /// See <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/builder.py#L2688-L2810>
    /// Note that we support only a very simple STAT at time of writing.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_init_static_metadata();

        // Guard clause: don't produce fvar for a static font
        if static_metadata.variable_axes.is_empty() {
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

        context.set_stat(Stat {
            design_axes: static_metadata
                .variable_axes
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
        });

        Ok(())
    }
}
