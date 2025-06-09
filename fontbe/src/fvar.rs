//! Generates a [fvar](https://learn.microsoft.com/en-us/typography/opentype/spec/fvar) table.

use log::trace;

use fontdrasil::coords::UserLocation;
use fontdrasil::orchestration::{Access, Work};
use fontir::{ir::StaticMetadata, orchestration::WorkId as FeWorkId};
use write_fonts::{
    tables::fvar::{AxisInstanceArrays, Fvar, InstanceRecord, VariationAxisRecord},
    types::{Fixed, NameId},
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

const HIDDEN_AXIS: u16 = 0x0001;

// Explicitly specify that an instance has no postscript name.
// https://learn.microsoft.com/en-us/typography/opentype/spec/fvar#instancerecord
const NO_POSTSCRIPT_NAME: NameId = NameId::new(0xFFFF);

#[derive(Debug)]
struct FvarWork {}

pub fn create_fvar_work() -> Box<BeWork> {
    Box::new(FvarWork {})
}

fn generate_fvar(static_metadata: &StaticMetadata) -> Option<Fvar> {
    // Guard clause: don't produce fvar for a static font
    if static_metadata.axes.is_empty() {
        trace!("Skip fvar; this is not a variable font");
        return None;
    }

    // Reuse an existing name record if possible (and allowed by the spec)
    let reverse_names = static_metadata.reverse_names();
    let min_font_specific_name_id = NameId::new(256);
    let reusable_name_id = |name: &str, allow_reserved: bool| {
        reverse_names
            .get(name)
            .unwrap()
            .iter()
            .find(|&&name_id| allow_reserved || name_id >= min_font_specific_name_id)
            .cloned()
            .unwrap()
    };

    // If a single postscript name is present, we must provide one or explicitly
    // indicate absence for every instance. fontations only expects None when NO
    // instances have one.
    // https://github.com/googlefonts/fontations/blob/b4136692/write-fonts/src/tables/fvar.rs#L13-L21
    let has_postscript_names = static_metadata
        .named_instances
        .iter()
        .any(|instance| instance.postscript_name.is_some());

    let default_instance_location: UserLocation = static_metadata
        .axes
        .iter()
        .map(|a| (a.tag, a.default))
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
                    // fonttools sets minNameID=256 here, which disables
                    // reusing spec-reserved nameIDs for axes names:
                    // https://github.com/fonttools/fonttools/blob/0bc8c028/Lib/fontTools/varLib/__init__.py#L106-L108
                    axis_name_id: reusable_name_id(ir_axis.ui_label_name(), false),
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
            .map(|ni| {
                // "The values 2 or 17 should only be used if the named instance corresponds
                // to the font’s default instance."
                // https://learn.microsoft.com/en-us/typography/opentype/spec/fvar#instancerecord
                // https://github.com/fonttools/fonttools/blob/0bc8c028f/Lib/fontTools/varLib/__init__.py#L139-L150
                let subfamily_name_id =
                    reusable_name_id(ni.name.as_str(), ni.location == default_instance_location);

                // fonttools implicitly sets minNameID=256 when adding instance postscript names
                // (even though the default named instance could in theory reuse nameID 6... but the
                // field is optional and unlikely to be explicitly set as == default PostScript name)
                // https://github.com/fonttools/fonttools/blob/0bc8c028f/Lib/fontTools/varLib/__init__.py#L154
                let post_script_name_id = has_postscript_names.then(|| {
                    ni.postscript_name
                        .as_deref()
                        .map(|text| reusable_name_id(text, false))
                        .unwrap_or(NO_POSTSCRIPT_NAME)
                });

                InstanceRecord {
                    subfamily_name_id,
                    post_script_name_id,
                    coordinates: static_metadata
                        .axes
                        .iter()
                        .map(|axis| {
                            let loc = ni
                                .location
                                .get(axis.tag)
                                .unwrap_or(axis.default)
                                .into_inner();
                            Fixed::from_f64(loc.into_inner())
                        })
                        .collect(),
                    ..Default::default()
                }
            })
            .collect(),
    );

    Some(Fvar::new(axes_and_instances))
}

impl Work<Context, AnyWorkId, Error> for FvarWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Fvar.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Variant(AnyWorkId::Fe(FeWorkId::StaticMetadata))
    }

    /// Generate [fvar](https://learn.microsoft.com/en-us/typography/opentype/spec/fvar)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        if let Some(fvar) = generate_fvar(&context.ir.static_metadata.get()) {
            context.fvar.set(fvar);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::types::Axis;
    use fontir::ir::StaticMetadata;

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
            Default::default(),
            Default::default(),
            None,
            false,
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
        let static_metadata = create_static_metadata(&[axis("wght", 400.0, 400.0, 400.0)]);
        let fvar = generate_fvar(&static_metadata);
        assert!(fvar.is_none());
    }

    #[test]
    fn fvar_includes_only_variable_axes() {
        let static_metadata = create_static_metadata(&[
            axis("wght", 400.0, 400.0, 700.0),
            axis("wdth", 400.0, 400.0, 400.0),
        ]);
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
