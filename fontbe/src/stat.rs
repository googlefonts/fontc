//! Generates a [stat](https://learn.microsoft.com/en-us/typography/opentype/spec/stat) table.

use std::collections::HashMap;

use log::trace;

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::{
    ir::{StatLabels, StaticMetadata},
    orchestration::WorkId as FeWorkId,
};
use write_fonts::{
    tables::stat::{AxisRecord, AxisValue, AxisValueTableFlags, Stat},
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
    /// Source labels are emitted as analytic format 1 or format 3 axis values.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let fea_stat = context
            .extra_fea_tables
            .try_get()
            .and_then(|tables| tables.stat.clone());
        let Some(stat) = generate_stat(&static_metadata, fea_stat) else {
            return Ok(());
        };

        context.stat.set(stat);
        Ok(())
    }
}

fn generate_stat(static_metadata: &StaticMetadata, fea_stat: Option<Stat>) -> Option<Stat> {
    match (&static_metadata.stat, fea_stat) {
        (Some(labels), _) => {
            log::info!("Using generated STAT labels from source metadata");
            Some(make_stat_from_labels(static_metadata, labels))
        }
        (None, Some(stat)) => {
            log::info!("Using STAT table from FEA");
            Some(stat)
        }
        (None, None) if static_metadata.axes.is_empty() => {
            trace!("Skip stat; this is not a variable font");
            None
        }
        (None, None) => Some(make_stat_from_axes(static_metadata)),
    }
}

fn make_stat_from_axes(static_metadata: &StaticMetadata) -> Stat {
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

fn make_stat_from_labels(static_metadata: &StaticMetadata, labels: &StatLabels) -> Stat {
    let reusable_names = static_metadata.reverse_names();
    let name_id = |name: &str, min_name_id: NameId| {
        reusable_names
            .get(name)
            .and_then(|ids| ids.iter().find(|&&id| id >= min_name_id))
            .copied()
            .unwrap_or_else(|| panic!("STAT name '{name}' was not registered in font metadata"))
    };

    let design_axes = labels
        .axes
        .iter()
        .enumerate()
        .map(|(axis_index, axis)| AxisRecord {
            axis_tag: axis.tag,
            axis_name_id: name_id(&axis.name, NameId::new(256)),
            axis_ordering: axis_index as u16,
        })
        .collect();

    let axis_values = labels
        .axes
        .iter()
        .enumerate()
        .flat_map(|(axis_index, axis)| {
            let name_id = &name_id;
            axis.labels.iter().map(move |label| {
                let mut flags = AxisValueTableFlags::empty();
                if label.older_sibling {
                    flags |= AxisValueTableFlags::OLDER_SIBLING_FONT_ATTRIBUTE;
                }
                if label.elidable {
                    flags |= AxisValueTableFlags::ELIDABLE_AXIS_VALUE_NAME;
                }

                let value_name_id = name_id(&label.name, NameId::new(0));
                match label.linked_user_value {
                    Some(linked_value) => AxisValue::format_3(
                        axis_index as u16,
                        flags,
                        value_name_id,
                        label.user_value.into(),
                        linked_value.into(),
                    ),
                    None => AxisValue::format_1(
                        axis_index as u16,
                        flags,
                        value_name_id,
                        label.user_value.into(),
                    ),
                }
            })
        })
        .collect();

    Stat::new(
        design_axes,
        axis_values,
        name_id(&labels.elided_fallback_name, NameId::new(0)),
    )
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use fontdrasil::coords::UserCoord;
    use fontir::ir::{AxisLabel, NameKey, StatAxis};
    use write_fonts::{
        dump_table,
        read::{
            FontData, FontRead,
            tables::stat::{self as read_stat, AxisValue as ReadAxisValue},
        },
        types::Tag,
    };

    use super::*;

    fn static_metadata() -> StaticMetadata {
        StaticMetadata::new(
            1000,
            HashMap::from([
                (
                    NameKey::new_bmp_only(NameId::SUBFAMILY_NAME),
                    "Regular".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::TYPOGRAPHIC_SUBFAMILY_NAME),
                    "Italic".to_string(),
                ),
            ]),
            vec![],
            vec![],
            Default::default(),
            None,
            0.0,
            None,
            false,
        )
        .unwrap()
    }

    fn stat_labels() -> StatLabels {
        StatLabels {
            elided_fallback_name: "Regular".to_string(),
            axes: vec![
                StatAxis {
                    tag: Tag::new(b"wght"),
                    name: "Weight".to_string(),
                    labels: vec![AxisLabel {
                        name: "Thin".to_string(),
                        user_value: UserCoord::new(100.0),
                        elidable: true,
                        older_sibling: true,
                        linked_user_value: Some(UserCoord::new(400.0)),
                    }],
                },
                StatAxis {
                    tag: Tag::new(b"ital"),
                    name: "Italic".to_string(),
                    labels: vec![AxisLabel {
                        name: "Italic".to_string(),
                        user_value: UserCoord::new(1.0),
                        elidable: false,
                        older_sibling: false,
                        linked_user_value: None,
                    }],
                },
            ],
        }
    }

    #[test]
    fn source_labels_build_format_1_and_3_values() {
        let mut metadata = static_metadata();
        metadata.set_stat(stat_labels());

        // This models an explicitly variable Glyphs source whose axes are all
        // points. fontmake retains those axes in fvar and emits STAT; fontc
        // currently prunes them from fvar, but the source STAT labels remain.
        assert!(metadata.axes.is_empty());
        let stat = generate_stat(&metadata, None).unwrap();
        let bytes = dump_table(&stat).unwrap();
        let stat = read_stat::Stat::read(FontData::new(&bytes)).unwrap();

        assert_eq!(stat.elided_fallback_name_id(), Some(NameId::SUBFAMILY_NAME));
        let axes = stat.design_axes().unwrap();
        assert_eq!(axes.len(), 2);
        assert_eq!(axes[0].axis_tag(), Tag::new(b"wght"));
        assert_eq!(axes[0].axis_name_id(), NameId::new(256));
        assert_eq!(axes[0].axis_ordering(), 0);
        assert_eq!(axes[1].axis_tag(), Tag::new(b"ital"));
        assert_eq!(axes[1].axis_name_id(), NameId::new(258));
        assert_eq!(axes[1].axis_ordering(), 1);

        let values = stat.offset_to_axis_values().unwrap().unwrap();
        let value = values.axis_values().get(0).unwrap();
        let ReadAxisValue::Format3(value) = value else {
            panic!("expected a format 3 axis value");
        };
        assert_eq!(value.axis_index(), 0);
        assert_eq!(value.value_name_id(), NameId::new(257));
        assert_eq!(value.value().to_f64(), 100.0);
        assert_eq!(value.linked_value().to_f64(), 400.0);
        assert_eq!(
            value.flags(),
            AxisValueTableFlags::OLDER_SIBLING_FONT_ATTRIBUTE
                | AxisValueTableFlags::ELIDABLE_AXIS_VALUE_NAME
        );

        let value = values.axis_values().get(1).unwrap();
        let ReadAxisValue::Format1(value) = value else {
            panic!("expected a format 1 axis value");
        };
        assert_eq!(value.axis_index(), 1);
        assert_eq!(value.value_name_id(), NameId::TYPOGRAPHIC_SUBFAMILY_NAME);
        assert_eq!(value.value().to_f64(), 1.0);
        assert_eq!(value.flags(), AxisValueTableFlags::empty());
    }

    #[test]
    fn source_labels_override_fea_but_fea_precedes_axis_fallback() {
        let fea_stat = Stat::new(
            vec![AxisRecord::new(Tag::new(b"FEA "), NameId::new(300), 7)],
            vec![],
            NameId::new(301),
        );
        let mut metadata = static_metadata();

        assert_eq!(
            generate_stat(&metadata, Some(fea_stat.clone())),
            Some(fea_stat.clone())
        );

        metadata.set_stat(stat_labels());
        let generated = generate_stat(&metadata, Some(fea_stat.clone())).unwrap();
        assert_ne!(generated, fea_stat);
        assert_eq!(generated.design_axes[0].axis_tag, Tag::new(b"wght"));
    }
}
