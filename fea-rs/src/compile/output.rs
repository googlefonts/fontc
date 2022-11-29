//! The result of a compilation

use std::collections::{BTreeMap, HashMap};

use write_fonts::{
    dump_table,
    read::{FontRef, TableProvider},
    tables::{
        self,
        layout::{FeatureParams, SizeParams},
        stat::AxisValueRecord,
    },
    types::Tag,
    FontBuilder, OffsetMarker,
};

use super::{
    consts,
    lookups::{AllLookups, FeatureKey, LookupId},
    tables::{NameSpec, Tables},
};
use crate::{
    compile::tables::{AxisLocation, StatFallbackName},
    Diagnostic,
};

/// The output of a compilation operation.
///
/// This losslessly contains all of the information parsed from the FEA necessary
/// for generating the various OpenType tables.
//TODO: ability to generate new errors during this final compilation step
pub struct Compilation {
    pub warnings: Vec<Diagnostic>,
    pub(crate) tables: Tables,
    pub(crate) lookups: AllLookups,
    pub(crate) features: BTreeMap<FeatureKey, Vec<LookupId>>,
    pub(crate) size: Option<SizeFeature>,
}

#[derive(Clone, Debug, Default)]
pub struct SizeFeature {
    pub params: SizeParams,
    pub names: Vec<NameSpec>,
}

impl Compilation {
    /// Attempt to update the provided font with the results of this compilation.
    //TODO: figure out error reporting
    pub fn apply(&self, font: &FontRef) -> Result<Vec<u8>, ()> {
        let mut builder = FontBuilder::default();
        if let Some(head_raw) = &self.tables.head {
            let head = head_raw.build(font);
            builder.add_table(Tag::new(b"head"), dump_table(&head).unwrap());
        }

        //TODO: can this contain some subset of keys? should we preserve
        //existing values in this case?
        if let Some(hhea_raw) = self.tables.hhea.as_ref() {
            let data = dump_table(hhea_raw).unwrap();
            builder.add_table(Tag::new(b"hhea"), data);
        }

        if let Some(vhea_raw) = self.tables.vhea.as_ref() {
            let data = dump_table(vhea_raw).unwrap();
            builder.add_table(Tag::new(b"vhea"), data);
        }

        //TODO: OS/2
        //if let Some(os2) = self.tables.OS2.as_ref() {
        //let data = dump_table(hhea_raw).unwrap();
        //font.add_table(Tag::new(b"hhea"), data);
        //}

        if let Some(gdef) = &self.tables.GDEF {
            builder.add_table(Tag::new(b"GDEF"), gdef.build().unwrap());
        }

        //TODO: reuse existing name table if present
        //let mut name = font
        //.tables
        //.name()
        //.unwrap()
        //.map(|t| t.into_owned())
        //.unwrap_or_else(|| tables::name::Name {
        //records: Vec::new(),
        //});

        let mut name = tables::name::Name::default();
        if let Some(name_raw) = self.tables.name.as_ref() {
            name.name_record.extend(
                name_raw
                    .records
                    .iter()
                    .filter(|record| !(1..=6).contains(&record.name_id))
                    .map(|record| record.spec.to_otf(record.name_id)),
            );
        }

        if let Some(stat_raw) = self.tables.STAT.as_ref() {
            //let mut stat = tables::stat::Stat {
            //design_axes_offset: todo!(),
            //offset_to_axis_value_offsets: todo!(),
            //elided_fallback_name_id: None,
            //};

            let elided_fallback_name_id = match &stat_raw.name {
                StatFallbackName::Id(id) if name.name_record.iter().any(|r| r.name_id == *id) => {
                    *id
                }
                StatFallbackName::Id(id) => {
                    panic!("ElidedFallbackNameID '{}' does not exist in font", id)
                }
                StatFallbackName::Record(names) => {
                    let name_id = find_next_name_id(&name);
                    name.name_record
                        .extend(names.iter().map(|n| n.to_otf(name_id)));
                    name_id
                }
            };

            let mut design_axes = Vec::new();
            for record in &stat_raw.records {
                let name_id = find_next_name_id(&name);
                name.name_record
                    .extend(record.name.iter().map(|n| n.to_otf(name_id)));
                design_axes.push(tables::stat::AxisRecord {
                    axis_tag: record.tag,
                    axis_name_id: name_id,
                    axis_ordering: record.ordering,
                });
            }
            design_axes.sort_unstable_by_key(|x| x.axis_tag);
            let axis_indices = design_axes
                .iter()
                .enumerate()
                .map(|(i, val)| (val.axis_tag, i as u16))
                .collect::<HashMap<_, _>>();

            let axis_values: Vec<OffsetMarker<tables::stat::AxisValue>> = stat_raw
                .values
                .iter()
                .map(|axis_value| {
                    let flags =
                        tables::stat::AxisValueTableFlags::from_bits(axis_value.flags).unwrap();
                    let name_id = find_next_name_id(&name);
                    name.name_record
                        .extend(axis_value.name.iter().map(|n| n.to_otf(name_id)));
                    let to_add = match &axis_value.location {
                        AxisLocation::One { tag, value } => tables::stat::AxisValue::format_1(
                            //TODO: validate that all referenced tags refer to existing axes
                            *axis_indices.get(tag).unwrap(),
                            flags,
                            name_id,
                            *value,
                        ),
                        AxisLocation::Two {
                            tag,
                            nominal,
                            min,
                            max,
                        } => {
                            let axis_index = *axis_indices.get(tag).unwrap();
                            tables::stat::AxisValue::format_2(
                                axis_index, flags, name_id, *nominal, *min, *max,
                            )
                        }
                        AxisLocation::Three { tag, value, linked } => {
                            let axis_index = *axis_indices.get(tag).unwrap();
                            tables::stat::AxisValue::format_3(
                                axis_index, flags, name_id, *value, *linked,
                            )
                        }
                        AxisLocation::Four(values) => {
                            let mapping = values
                                .iter()
                                .map(|(tag, value)| {
                                    AxisValueRecord::new(*axis_indices.get(tag).unwrap(), *value)
                                })
                                .collect();
                            tables::stat::AxisValue::format_4(flags, name_id, mapping)
                        }
                    };
                    OffsetMarker::new(to_add)
                })
                .collect::<Vec<_>>();
            let mut table = tables::stat::Stat::new(design_axes, axis_values.into());
            table.elided_fallback_name_id = Some(elided_fallback_name_id);
            builder.add_table(Tag::new(b"STAT"), dump_table(&table).unwrap());
        }

        let (gsub, mut gpos) = self.lookups.build(&self.features);

        if let Some(size) = self.size.as_ref() {
            let name_id = find_next_name_id(&name);
            name.name_record
                .extend(size.names.iter().map(|n| n.to_otf(name_id)));
            let gpos = gpos.as_mut().unwrap();
            for record in gpos.feature_list.feature_records.iter_mut() {
                if record.feature_tag == consts::SIZE_TAG {
                    record.feature.feature_params = FeatureParams::Size(size.params.clone()).into();
                }
            }
        }

        if let Some(gsub) = gsub {
            builder.add_table(Tag::new(b"GSUB"), dump_table(&gsub).unwrap());
        }

        if let Some(gpos) = gpos {
            builder.add_table(Tag::new(b"GPOS"), dump_table(&gpos).unwrap());
        }

        if !name.name_record.is_empty() {
            builder.add_table(Tag::new(b"name"), dump_table(&name).unwrap());
        }

        for record in font.table_directory.table_records() {
            if !builder.contains(record.tag()) {
                let data = font.data_for_tag(record.tag()).unwrap();
                builder.add_table(record.tag(), data);
            }
        }

        Ok(builder.build())
    }
}

fn find_next_name_id(names: &tables::name::Name) -> u16 {
    const RESERVED_NAMES: u16 = 255;
    names
        .name_record
        .iter()
        .map(|rec| rec.name_id)
        .max()
        .unwrap_or_default()
        .max(RESERVED_NAMES)
        .checked_add(1)
        .expect("ran out of ids in name table")
}
