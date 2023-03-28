//! Generates a [fvar](https://learn.microsoft.com/en-us/typography/opentype/spec/fvar) table.

use std::collections::HashMap;

use fontdrasil::orchestration::Work;
use log::trace;
use read_fonts::types::MajorMinor;
use write_fonts::tables::fvar::{AxisInstanceArrays, Fvar, VariationAxisRecord};

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

const HIDDEN_AXIS: u16 = 0x0001;

struct FvarWork {}

pub fn create_fvar_work() -> Box<BeWork> {
    Box::new(FvarWork {})
}

impl Work<Context, Error> for FvarWork {
    /// Generate [fvar](https://learn.microsoft.com/en-us/typography/opentype/spec/fvar)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_init_static_metadata();
        // Guard clause: don't produce fvar for a static font
        if static_metadata.axes.is_empty() {
            trace!("Skip fvar; this is not a variable font");
            return Ok(());
        }

        let reverse_names: HashMap<_, _> = static_metadata
            .names
            .iter()
            .map(|(key, name)| (name, key.name_id))
            .collect();

        let axes_and_instances = AxisInstanceArrays::new(
            static_metadata
                .axes
                .iter()
                .map(|ir_axis| {
                    let mut var = VariationAxisRecord {
                        axis_tag: ir_axis.tag,
                        min_value: ir_axis.min.into(),
                        default_value: ir_axis.default.into(),
                        max_value: ir_axis.max.into(),
                        axis_name_id: *reverse_names.get(&ir_axis.name).unwrap(),
                        ..Default::default()
                    };
                    if ir_axis.hidden {
                        var.flags |= HIDDEN_AXIS;
                    }
                    var
                })
                .collect(),
            Vec::new(),
        );

        let axis_count = axes_and_instances.axes.len().try_into().unwrap();
        let instance_count = axes_and_instances.instances.len().try_into().unwrap();
        context.set_fvar(Fvar::new(
            MajorMinor::VERSION_1_0,
            axes_and_instances,
            axis_count,
            instance_count,
        ));
        Ok(())
    }
}
