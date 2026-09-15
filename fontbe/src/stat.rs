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
    #[tracing::instrument(name = "fontbe::StatWork::exec", skip_all)]
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
    // A STAT from FEA takes precedence over source STAT data, like the other
    // FEA table blocks.
    match fea_stat {
        Some(stat) => {
            log::info!("Using STAT table from FEA");
            Some(stat)
        }
        None if static_metadata.misc.stat_axes.is_empty() => {
            trace!("Skip stat; the source has no STAT axes");
            None
        }
        None => Some(make_stat(static_metadata)),
    }
}

fn make_stat(static_metadata: &StaticMetadata) -> Stat {
    // Axis names only reuse a name record in the font-specific range (nameID >=
    // 256), the axis value names and the elided fallback name reuse any name
    // record, like fontTools:
    // https://github.com/fonttools/fonttools/blob/0bc8c028/Lib/fontTools/otlLib/builder.py#L3048-L3050
    // https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Lib/fontTools/otlLib/builder.py#L3156-L3160
    let names = static_metadata.reverse_names();
    let reusable_names: HashMap<&str, NameId> = names
        .iter()
        .filter_map(|(name, ids)| ids.range(NameId::new(256)..).next().map(|id| (*name, *id)))
        .collect();
    let any_names: HashMap<&str, NameId> = names
        .iter()
        .filter_map(|(name, ids)| ids.first().map(|id| (*name, *id)))
        .collect();

    let stat_axes = &static_metadata.misc.stat_axes;
    let axis_values: Vec<AxisValue> = stat_axes
        .iter()
        .enumerate()
        .flat_map(|(idx, axis)| axis.labels.iter().map(move |label| (idx as u16, label)))
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
        design_axes: stat_axes
            .iter()
            .enumerate()
            .map(|(idx, axis)| AxisRecord {
                axis_tag: axis.tag,
                axis_name_id: *reusable_names
                    .get(axis.name.as_str())
                    .expect("STAT axis names are registered by StaticMetadata::set_stat"),
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
    use std::collections::HashMap;

    use fontdrasil::types::Axis;
    use fontir::ir::{AxisValueLabel, NameKey, StatAxis, StaticMetadata};
    use write_fonts::{
        dump_table,
        read::{
            FontData, FontRead,
            tables::stat::{self as read_stat, AxisValue as ReadAxisValue},
        },
        types::Tag,
    };

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
        let stat_axes = static_metadata
            .axes
            .iter()
            .map(|axis| StatAxis {
                labels: labels
                    .iter()
                    .find(|(tag, _)| *tag == axis.tag)
                    .map(|(_, labels)| labels.clone())
                    .unwrap_or_default(),
                ..StatAxis::from_axis(axis)
            })
            .collect();
        static_metadata.set_stat(stat_axes, None);
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

    /// A source with more STAT axes than fvar axes.
    ///
    /// This models the common Glyphs case: a wdth point axis, which fontc
    /// prunes from fvar but Glyphs still lists in STAT, plus the STAT-only
    /// ital axis glyphsLib synthesizes for an upright family.
    fn stat_beyond_fvar_metadata() -> StaticMetadata {
        let wght = axis("wght", 100.0, 400.0, 700.0);
        let wdth = axis("wdth", 100.0, 100.0, 100.0);
        let mut static_metadata = StaticMetadata::new(
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
            vec![wght.clone(), wdth.clone()],
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
            None,
            false,
        )
        .unwrap();
        assert_eq!(static_metadata.axes.len(), 1);
        let stat_axes = vec![
            StatAxis {
                labels: vec![
                    AxisValueLabel {
                        linked_value: Some(700.0.into()),
                        elidable: true,
                        ..label("Regular", 400.0)
                    },
                    label("Bold", 700.0),
                ],
                ..StatAxis::from_axis(&wght)
            },
            StatAxis {
                labels: vec![AxisValueLabel {
                    elidable: true,
                    ..label("Normal", 100.0)
                }],
                ..StatAxis::from_axis(&wdth)
            },
            StatAxis {
                tag: Tag::new(b"ital"),
                name: "Italic".to_string(),
                labels: vec![label("Italic", 1.0)],
            },
        ];
        static_metadata.set_stat(stat_axes, Some("Regular".to_string()));
        static_metadata
    }

    #[test]
    fn stat_axes_beyond_fvar_axes() {
        let static_metadata = stat_beyond_fvar_metadata();
        let stat = generate_stat(&static_metadata, None).unwrap();
        let bytes = dump_table(&stat).unwrap();
        let stat = read_stat::Stat::read(FontData::new(&bytes)).unwrap();

        assert_eq!(stat.elided_fallback_name_id(), Some(NameId::SUBFAMILY_NAME));
        let axes = stat.design_axes().unwrap();
        assert_eq!(
            axes.iter()
                .map(|axis| (axis.axis_tag(), axis.axis_name_id(), axis.axis_ordering()))
                .collect::<Vec<_>>(),
            [
                // The fvar axis name is shared
                (Tag::new(b"wght"), NameId::new(256), 0),
                (Tag::new(b"wdth"), NameId::new(258), 1),
                // The axis name must not reuse the typographic subfamily name
                (Tag::new(b"ital"), NameId::new(260), 2),
            ]
        );

        let values = stat.offset_to_axis_values().unwrap().unwrap();
        assert_eq!(values.axis_values().len(), 4);
        let ReadAxisValue::Format3(regular) = values.axis_values().get(0).unwrap() else {
            panic!("expected a format 3 axis value");
        };
        assert_eq!(regular.axis_index(), 0);
        // The value name does reuse the subfamily name
        assert_eq!(regular.value_name_id(), NameId::SUBFAMILY_NAME);
        assert_eq!(regular.value().to_f64(), 400.0);
        assert_eq!(regular.linked_value().to_f64(), 700.0);
        assert_eq!(
            regular.flags(),
            AxisValueTableFlags::ELIDABLE_AXIS_VALUE_NAME
        );

        let ReadAxisValue::Format1(bold) = values.axis_values().get(1).unwrap() else {
            panic!("expected a format 1 axis value");
        };
        assert_eq!(bold.axis_index(), 0);
        assert_eq!(bold.value_name_id(), NameId::new(257));
        assert_eq!(bold.value().to_f64(), 700.0);
        assert_eq!(bold.flags(), AxisValueTableFlags::empty());

        let ReadAxisValue::Format1(normal) = values.axis_values().get(2).unwrap() else {
            panic!("expected a format 1 axis value");
        };
        assert_eq!(normal.axis_index(), 1);
        assert_eq!(normal.value_name_id(), NameId::new(259));
        assert_eq!(normal.value().to_f64(), 100.0);
        assert_eq!(
            normal.flags(),
            AxisValueTableFlags::ELIDABLE_AXIS_VALUE_NAME
        );

        let ReadAxisValue::Format1(italic) = values.axis_values().get(3).unwrap() else {
            panic!("expected a format 1 axis value");
        };
        assert_eq!(italic.axis_index(), 2);
        assert_eq!(italic.value_name_id(), NameId::TYPOGRAPHIC_SUBFAMILY_NAME);
        assert_eq!(italic.value().to_f64(), 1.0);
        assert_eq!(italic.flags(), AxisValueTableFlags::empty());
    }

    #[test]
    fn stat_axes_without_fvar_axes() {
        let mut static_metadata = static_metadata(&[], &[]);
        static_metadata.set_stat(
            vec![StatAxis::from_axis(&axis("wght", 400.0, 400.0, 400.0))],
            None,
        );
        assert!(generate_stat(&static_metadata, None).is_some());
    }

    #[test]
    fn fea_stat_overrides_source_stat() {
        let fea_stat = Stat::new(
            vec![AxisRecord::new(Tag::new(b"FEA "), NameId::new(300), 7)],
            vec![],
            NameId::new(301),
        );

        let static_metadata = stat_beyond_fvar_metadata();
        assert_eq!(
            generate_stat(&static_metadata, Some(fea_stat.clone())),
            Some(fea_stat)
        );
        assert_eq!(
            generate_stat(&static_metadata, None).unwrap().design_axes[0].axis_tag,
            Tag::new(b"wght")
        );
    }

    #[test]
    fn no_stat_for_a_static_font() {
        let static_metadata = static_metadata(&[], &[]);
        assert_eq!(generate_stat(&static_metadata, None), None);
    }
}
