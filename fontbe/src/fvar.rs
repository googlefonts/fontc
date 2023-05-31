//! Generates a [fvar](https://learn.microsoft.com/en-us/typography/opentype/spec/fvar) table.

use std::collections::HashMap;

use font_types::Fixed;
use fontdrasil::orchestration::Work;
use fontir::ir::StaticMetadata;
use log::trace;
use read_fonts::types::MajorMinor;
use write_fonts::tables::fvar::{AxisInstanceArrays, Fvar, InstanceRecord, VariationAxisRecord};

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

const HIDDEN_AXIS: u16 = 0x0001;

struct FvarWork {}

pub fn create_fvar_work() -> Box<BeWork> {
    Box::new(FvarWork {})
}

fn generate_fvar(static_metadata: &StaticMetadata) -> Option<Fvar> {
    // Guard clause: don't produce fvar for a static font
    if static_metadata.variable_axes.is_empty() {
        trace!("Skip fvar; this is not a variable font");
        return None;
    }

    let reverse_names: HashMap<_, _> = static_metadata
        .names
        .iter()
        // To match fontmake we should use the font-specific name range and not reuse
        // a well-known name, even if the name matches.
        .filter(|(key, _)| key.name_id.to_u16() > 255)
        .map(|(key, name)| (name, key.name_id))
        .collect();

    let axes_and_instances = AxisInstanceArrays::new(
        static_metadata
            .variable_axes
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
        static_metadata
            .named_instances
            .iter()
            .map(|ni| InstanceRecord {
                subfamily_name_id: *reverse_names.get(&ni.name).unwrap(),
                coordinates: static_metadata
                    .variable_axes
                    .iter()
                    .map(|axis| {
                        let loc = ni
                            .location
                            .get(&axis.name)
                            .unwrap_or(axis.default)
                            .into_inner();
                        Fixed::from_f64(loc.into_inner() as f64)
                    })
                    .collect(),
                ..Default::default()
            })
            .collect(),
    );

    let axis_count = axes_and_instances.axes.len().try_into().unwrap();
    let instance_count = axes_and_instances.instances.len().try_into().unwrap();

    Some(Fvar::new(
        MajorMinor::VERSION_1_0,
        axes_and_instances,
        axis_count,
        instance_count,
    ))
}

impl Work<Context, Error> for FvarWork {
    /// Generate [fvar](https://learn.microsoft.com/en-us/typography/opentype/spec/fvar)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        if let Some(fvar) = generate_fvar(&context.ir.get_init_static_metadata()) {
            context.set_fvar(fvar);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontir::ir::{Axis, StaticMetadata};

    use super::generate_fvar;

    use crate::test_util::axis;

    fn create_static_metadata(axes: &[Axis]) -> StaticMetadata {
        StaticMetadata::new(
            1000,
            Default::default(),
            axes.to_vec(),
            Default::default(),
            Default::default(),
            Default::default(),
        )
        .unwrap()
    }

    #[test]
    fn no_fvar_for_no_axes() {
        let static_metadata = create_static_metadata(&[]);
        let fvar = generate_fvar(&static_metadata);
        assert!(fvar.is_none());
    }

    #[test]
    fn no_fvar_for_point_axes() {
        let static_metadata = create_static_metadata(&[axis(400.0, 400.0, 400.0)]);
        let fvar = generate_fvar(&static_metadata);
        assert!(fvar.is_none());
    }

    #[test]
    fn fvar_includes_only_variable_axes() {
        let static_metadata =
            create_static_metadata(&[axis(400.0, 400.0, 700.0), axis(400.0, 400.0, 400.0)]);
        let fvar = generate_fvar(&static_metadata).unwrap();
        assert_eq!(
            vec![(400.0, 400.0, 700.0),],
            fvar.axis_instance_arrays
                .axes
                .iter()
                .map(|var| (
                    var.min_value.to_f64(),
                    var.default_value.to_f64(),
                    var.max_value.to_f64()
                ))
                .collect::<Vec<_>>()
        );
    }
}
