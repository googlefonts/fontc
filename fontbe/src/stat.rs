//! Generates a [stat](https://learn.microsoft.com/en-us/typography/opentype/spec/stat) table.

use std::collections::HashMap;

use log::trace;

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::{ir::StaticMetadata, orchestration::WorkId as FeWorkId};
use write_fonts::{
    tables::stat::{AxisRecord, AxisValue, AxisValueTableFlags, Stat},
    types::{Fixed, NameId},
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
    #[tracing::instrument(name = "fontbe::StatWork::exec", skip_all)]
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
    // The axis value names and the elided fallback name reuse any name
    // record, like fontTools' _addName with minNameID 0:
    // https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Lib/fontTools/otlLib/builder.py#L3156-L3160
    let any_names: HashMap<&str, NameId> = static_metadata
        .reverse_names()
        .into_iter()
        .filter_map(|(name, ids)| ids.first().map(|id| (name, *id)))
        .collect();

    let axis_values: Vec<AxisValue> = static_metadata
        .axes
        .iter()
        .enumerate()
        .flat_map(|(idx, axis)| {
            static_metadata
                .misc
                .axis_value_labels
                .get(&axis.tag)
                .into_iter()
                .flatten()
                .map(move |label| (idx as u16, label))
        })
        .filter_map(|(axis_index, label)| {
            let Some(name_id) = any_names.get(label.name.as_str()).copied() else {
                log::warn!("no name entry for STAT label {:?}", label.name);
                return None;
            };
            let mut flags = AxisValueTableFlags::empty();
            if label.elidable {
                flags |= AxisValueTableFlags::ELIDABLE_AXIS_VALUE_NAME;
            }
            if label.older_sibling {
                flags |= AxisValueTableFlags::OLDER_SIBLING_FONT_ATTRIBUTE;
            }
            let value = Fixed::from_f64(label.value.0);
            // Format selection follows fontTools' AxisLabelDescriptor.getFormat:
            // https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Lib/fontTools/designspaceLib/__init__.py#L1247-L1262
            Some(if let Some(linked) = label.linked_value {
                AxisValue::format_3(axis_index, flags, name_id, value, Fixed::from_f64(linked.0))
            } else if label.min_value.is_some() || label.max_value.is_some() {
                // A missing bound is infinite, like fontTools' buildStatTable:
                // https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Lib/fontTools/otlLib/builder.py#L3116-L3122
                let min = label
                    .min_value
                    .map(|v| Fixed::from_f64(v.0))
                    .unwrap_or(Fixed::MIN);
                let max = label
                    .max_value
                    .map(|v| Fixed::from_f64(v.0))
                    .unwrap_or(Fixed::MAX);
                AxisValue::format_2(axis_index, flags, name_id, value, min, max)
            } else {
                AxisValue::format_1(axis_index, flags, name_id, value)
            })
        })
        .collect();

    let elided_fallback_name_id = static_metadata
        .misc
        .elided_fallback_name
        .as_deref()
        .and_then(|name| any_names.get(name).copied())
        .unwrap_or(NameId::SUBFAMILY_NAME);

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
        offset_to_axis_values: (!axis_values.is_empty())
            .then(|| axis_values.into_iter().map(Into::into).collect::<Vec<_>>())
            .into(),
        elided_fallback_name_id: Some(elided_fallback_name_id),
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::types::Axis;
    use fontir::ir::{AxisValueLabel, NameKey, StaticMetadata};
    use write_fonts::types::Tag;

    use crate::test_util::axis;

    use super::*;

    fn label(name: &str, value: f64) -> AxisValueLabel {
        AxisValueLabel {
            name: name.to_string(),
            value: value.into(),
            min_value: None,
            max_value: None,
            linked_value: None,
            elidable: false,
            older_sibling: false,
        }
    }

    fn static_metadata(axes: &[Axis], labels: &[(Tag, Vec<AxisValueLabel>)]) -> StaticMetadata {
        let names = labels
            .iter()
            .flat_map(|(_, labels)| labels)
            .map(|label| label.name.as_str())
            .chain(axes.iter().map(|axis| axis.ui_label_name()))
            .enumerate()
            .map(|(idx, name)| {
                (
                    NameKey::new(NameId::new(256 + idx as u16), name),
                    name.to_string(),
                )
            })
            .collect();
        let mut static_metadata = StaticMetadata::new(
            1000,
            names,
            axes.to_vec(),
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
            None,
            false,
        )
        .unwrap();
        static_metadata.misc.axis_value_labels = labels.iter().cloned().collect();
        static_metadata
    }

    #[test]
    fn no_axis_values_leaves_a_null_offset() {
        let stat = make_stat(&static_metadata(&[axis("wght", 400.0, 400.0, 700.0)], &[]));
        assert!(stat.offset_to_axis_values.is_none());
    }

    #[test]
    fn axis_value_lands_on_its_own_axis() {
        let stat = make_stat(&static_metadata(
            &[
                axis("wght", 400.0, 400.0, 700.0),
                axis("wdth", 50.0, 100.0, 200.0),
            ],
            &[(Tag::new(b"wdth"), vec![label("Wide", 200.0)])],
        ));
        let values = stat.offset_to_axis_values.as_ref().unwrap();
        assert_eq!(1, values.len());
        let AxisValue::Format1(value) = values[0].as_ref() else {
            panic!("expected format 1, got {:?}", values[0]);
        };
        assert_eq!(1, value.axis_index);
    }

    #[test]
    fn a_linked_value_wins_over_a_range() {
        let mut linked_and_range = label("Regular", 400.0);
        linked_and_range.linked_value = Some(700.0.into());
        linked_and_range.min_value = Some(300.0.into());
        linked_and_range.max_value = Some(500.0.into());
        let stat = make_stat(&static_metadata(
            &[axis("wght", 400.0, 400.0, 700.0)],
            &[(Tag::new(b"wght"), vec![linked_and_range])],
        ));
        let values = stat.offset_to_axis_values.as_ref().unwrap();
        let AxisValue::Format3(value) = values[0].as_ref() else {
            panic!("expected format 3, got {:?}", values[0]);
        };
        assert_eq!(
            (400.0, 700.0),
            (value.value.to_f64(), value.linked_value.to_f64())
        );
    }

    #[test]
    fn a_one_sided_range_is_open_ended() {
        let mut only_min = label("Heavy", 700.0);
        only_min.min_value = Some(600.0.into());
        only_min.older_sibling = true;
        let stat = make_stat(&static_metadata(
            &[axis("wght", 400.0, 400.0, 700.0)],
            &[(Tag::new(b"wght"), vec![only_min])],
        ));
        let values = stat.offset_to_axis_values.as_ref().unwrap();
        let AxisValue::Format2(value) = values[0].as_ref() else {
            panic!("expected format 2, got {:?}", values[0]);
        };
        assert_eq!(
            (700.0, 600.0, Fixed::MAX.to_f64()),
            (
                value.nominal_value.to_f64(),
                value.range_min_value.to_f64(),
                value.range_max_value.to_f64()
            )
        );
        assert!(
            value
                .flags
                .contains(AxisValueTableFlags::OLDER_SIBLING_FONT_ATTRIBUTE)
        );
    }
}
