//! The result of a compilation

use std::collections::{BTreeMap, HashMap};

use fonttools::{
    font::Font,
    layout::common::{sizeFeatureParams, FeatureParams},
    tables,
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
    pub params: (i16, i16, i16, i16),
    pub names: Vec<NameSpec>,
}

impl Compilation {
    /// Attempt to update the provided font with the results of this compilation.
    //TODO: figure out error reporting
    pub fn apply(&self, font: &mut Font) -> Result<(), ()> {
        if let Some(head_raw) = self.tables.head.as_ref() {
            let mut head = font
                .tables
                .head()
                .unwrap()
                .map(|t| t.into_owned())
                .unwrap_or_else(|| head_raw.build());
            head.fontRevision = head_raw.font_revision;
            font.tables.insert(head);
        }

        //TODO: can this contain some subset of keys? should we preserve
        //existing values in this case?
        if let Some(hhea_raw) = self.tables.hhea.as_ref() {
            font.tables.insert(hhea_raw.build());
        }

        if let Some(os2) = self.tables.OS2.as_ref() {
            font.tables.insert(os2.build());
        }

        if let Some(gdef) = self.tables.GDEF.as_ref() {
            font.tables.insert(gdef.build());
        }

        let mut name = font
            .tables
            .name()
            .unwrap()
            .map(|t| t.into_owned())
            .unwrap_or_else(|| tables::name::name {
                records: Vec::new(),
            });

        if let Some(name_raw) = self.tables.name.as_ref() {
            name.records.extend(
                name_raw
                    .records
                    .iter()
                    .filter(|record| !(1..=6).contains(&record.name_id))
                    .map(|record| record.spec.to_otf(record.name_id)),
            );
        }

        if let Some(stat_raw) = self.tables.STAT.as_ref() {
            let mut stat = tables::STAT::STAT {
                elided_fallback_name_id: None,
                design_axes: Vec::new(),
                axis_values: Vec::new(),
            };

            let name_id = match &stat_raw.name {
                StatFallbackName::Id(id) if name.records.iter().any(|r| r.nameID == *id) => *id,
                StatFallbackName::Id(id) => {
                    panic!("ElidedFallbackNameID '{}' does not exist in font", id)
                }
                StatFallbackName::Record(names) => {
                    let name_id = find_next_name_id(&name);
                    name.records.extend(names.iter().map(|n| n.to_otf(name_id)));
                    name_id
                }
            };
            stat.elided_fallback_name_id = Some(name_id);

            for record in &stat_raw.records {
                let name_id = find_next_name_id(&name);
                name.records
                    .extend(record.name.iter().map(|n| n.to_otf(name_id)));
                stat.design_axes.push(tables::STAT::AxisRecord {
                    axisTag: record.tag,
                    axisNameID: name_id,
                    axisOrdering: record.ordering,
                });
            }
            stat.design_axes.sort_unstable_by_key(|x| x.axisTag);
            let axis_indices = stat
                .design_axes
                .iter()
                .map(|x| x.axisTag)
                .enumerate()
                .map(|(i, val)| (val, i as u16))
                .collect::<HashMap<_, _>>();

            for axis_value in &stat_raw.values {
                let flags = tables::STAT::AxisValueFlags::from_bits(axis_value.flags).unwrap();
                let name_id = find_next_name_id(&name);
                name.records
                    .extend(axis_value.name.iter().map(|n| n.to_otf(name_id)));
                let to_add = match &axis_value.location {
                    AxisLocation::One { tag, value } => tables::STAT::AxisValue::new_format1(
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
                        tables::STAT::AxisValue::new_format2(
                            axis_index, flags, name_id, *nominal, *min, *max,
                        )
                    }
                    AxisLocation::Three { tag, value, linked } => {
                        let axis_index = *axis_indices.get(tag).unwrap();
                        tables::STAT::AxisValue::new_format3(
                            axis_index, flags, name_id, *value, *linked,
                        )
                    }
                    AxisLocation::Four(values) => {
                        let mapping = values
                            .iter()
                            .map(|(tag, value)| (*axis_indices.get(tag).unwrap(), *value))
                            .collect();
                        tables::STAT::AxisValue::new_format4(flags, name_id, mapping)
                    }
                };
                stat.axis_values.push(to_add);
            }
            font.tables.insert(stat);
        }

        let (gsub, mut gpos) = self.lookups.build(&self.features);

        if let Some(size) = self.size.as_ref() {
            let name_id = find_next_name_id(&name);
            name.records
                .extend(size.names.iter().map(|n| n.to_otf(name_id)));
            let gpos = gpos.as_mut().unwrap();
            for (tag, _, params) in gpos.features.iter_mut() {
                if tag == &consts::SIZE_TAG {
                    *params = Some(FeatureParams::SizeFeature(sizeFeatureParams {
                        designSize: size.params.0 as u16,
                        subfamilyIdentifier: size.params.1 as u16,
                        subfamilyNameID: name_id,
                        smallest: size.params.2 as u16,
                        largest: size.params.3 as u16,
                    }));
                }
            }
        }

        if let Some(gsub) = gsub {
            font.tables.insert(gsub);
        }

        if let Some(gpos) = gpos {
            font.tables.insert(gpos);
        }

        if !name.records.is_empty() {
            font.tables.insert(name);
        }

        Ok(())
    }
}

fn find_next_name_id(names: &tables::name::name) -> u16 {
    const RESERVED_NAMES: u16 = 255;
    names
        .records
        .iter()
        .map(|rec| rec.nameID)
        .max()
        .unwrap_or_default()
        .max(RESERVED_NAMES)
        .checked_add(1)
        .expect("ran out of ids in name table")
}
