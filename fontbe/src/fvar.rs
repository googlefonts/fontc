//! Generates a [fvar](https://learn.microsoft.com/en-us/typography/opentype/spec/fvar) table.

use std::collections::HashMap;

use fontdrasil::orchestration::Work;
use log::trace;
use read_fonts::types::{Fixed, MajorMinor, Tag};
use write_fonts::tables::fvar::{AxisInstanceArrays, Fvar, VariationAxisRecord};

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

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
                    let axis_name_id: u16 = (*reverse_names.get(&ir_axis.name).unwrap()).into();
                    let mut var = VariationAxisRecord {
                        axis_tag: Tag::new(ir_axis.tag.as_bytes()),
                        min_value: Fixed::from_f64(ir_axis.min.into_inner().into_inner().into()),
                        default_value: Fixed::from_f64(
                            ir_axis.default.into_inner().into_inner().into(),
                        ),
                        max_value: Fixed::from_f64(ir_axis.max.into_inner().into_inner().into()),
                        axis_name_id: axis_name_id.into(),
                        ..Default::default()
                    };
                    if ir_axis.hidden {
                        var.flags |= 0x0001;
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
