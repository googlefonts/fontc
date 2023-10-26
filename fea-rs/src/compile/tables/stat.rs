//! Building the STAT table

use std::collections::HashMap;

use write_fonts::{
    tables::stat as write_stat,
    types::{Fixed, NameId, Tag},
};

use super::{NameBuilder, NameSpec};

#[derive(Clone, Debug)]
pub(crate) struct StatBuilder {
    pub name: StatFallbackName,
    pub records: Vec<AxisRecord>,
    pub values: Vec<AxisValue>,
}

#[derive(Clone, Debug)]
pub(crate) struct AxisRecord {
    pub tag: Tag,
    pub name: Vec<NameSpec>,
    pub ordering: u16,
}

#[derive(Clone, Debug)]
pub(crate) struct AxisValue {
    pub flags: u16,
    pub name: Vec<NameSpec>,
    pub location: AxisLocation,
}

#[derive(Clone, Debug)]
pub enum AxisLocation {
    One {
        tag: Tag,
        value: Fixed,
    },
    Two {
        tag: Tag,
        nominal: Fixed,
        min: Fixed,
        max: Fixed,
    },
    Three {
        tag: Tag,
        value: Fixed,
        linked: Fixed,
    },
    Four(Vec<(Tag, Fixed)>),
}

#[derive(Clone, Debug)]
pub(crate) enum StatFallbackName {
    Id(NameId),
    Record(Vec<NameSpec>),
}

impl StatBuilder {
    pub(crate) fn build(&self, name_builder: &mut NameBuilder) -> write_stat::Stat {
        let elided_fallback_name_id = match &self.name {
            StatFallbackName::Id(id) if name_builder.contains_id(*id) => *id,
            StatFallbackName::Id(id) => {
                panic!("ElidedFallbackNameID '{}' does not exist in font", id)
            }
            StatFallbackName::Record(names) => name_builder.add_anon_group(names),
        };

        //HACK: we jump through a bunch of hoops to ensure our output matches
        //feaLib's; in particular we want to add our name table entries grouped by
        //axis.
        let mut sorted_values = HashMap::<Tag, Vec<_>>::new();
        let mut sorted_records = self.records.iter().collect::<Vec<_>>();
        sorted_records.sort_by_key(|x| x.ordering);

        for axis_value in &self.values {
            match axis_value.location {
                AxisLocation::One { tag, .. }
                | AxisLocation::Two { tag, .. }
                | AxisLocation::Three { tag, .. } => {
                    sorted_values.entry(tag).or_default().push(axis_value)
                }
                AxisLocation::Four(_) => sorted_values
                    .entry(Tag::default())
                    .or_default()
                    .push(axis_value),
            }
        }

        let mut design_axes = Vec::with_capacity(self.records.len());
        let mut axis_values = Vec::with_capacity(self.values.len());

        for (i, record) in self.records.iter().enumerate() {
            let name_id = name_builder.add_anon_group(&record.name);
            let record = write_stat::AxisRecord {
                axis_tag: record.tag,
                axis_name_id: name_id,
                axis_ordering: record.ordering,
            };
            for axis_value in sorted_values
                .get(&record.axis_tag)
                .iter()
                .flat_map(|x| x.iter())
            {
                let flags = write_stat::AxisValueTableFlags::from_bits(axis_value.flags).unwrap();
                let name_id = name_builder.add_anon_group(&axis_value.name);
                let value = match &axis_value.location {
                    AxisLocation::One { value, .. } => write_stat::AxisValue::format_1(
                        //TODO: validate that all referenced tags refer to existing axes
                        i as u16, flags, name_id, *value,
                    ),
                    AxisLocation::Two {
                        nominal, min, max, ..
                    } => write_stat::AxisValue::format_2(
                        i as _, flags, name_id, *nominal, *min, *max,
                    ),
                    AxisLocation::Three { value, linked, .. } => {
                        write_stat::AxisValue::format_3(i as _, flags, name_id, *value, *linked)
                    }

                    AxisLocation::Four(_) => panic!("assigned to separate group"),
                };
                axis_values.push(value);
            }

            design_axes.push(record);
        }

        let format4 = sorted_values
            .remove(&Tag::default())
            .unwrap_or_default()
            .into_iter()
            .map(|format4| {
                let flags = write_stat::AxisValueTableFlags::from_bits(format4.flags).unwrap();
                let name_id = name_builder.add_anon_group(&format4.name);
                let AxisLocation::Four(values) = &format4.location else {
                    panic!("only format 4 in this group")
                };
                let mapping = values
                    .iter()
                    .map(|(tag, value)| {
                        let axis_index = design_axes
                            .iter()
                            .position(|rec| rec.axis_tag == *tag)
                            .expect("validated");
                        write_stat::AxisValueRecord::new(axis_index as _, *value)
                    })
                    .collect();
                write_stat::AxisValue::format_4(flags, name_id, mapping)
            });

        //feaLib puts format4 records first
        let axis_values = format4.chain(axis_values).collect();
        write_stat::Stat::new(design_axes, axis_values, elided_fallback_name_id)
    }
}
