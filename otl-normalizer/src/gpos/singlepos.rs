use std::{collections::HashSet, fmt::Debug};

use write_fonts::read::{
    ReadError,
    tables::gpos::{SinglePos, ValueRecord},
    types::GlyphId,
};

use crate::{glyph_names::NameMap, variations::DeltaComputer};

use super::{PrintNames, ResolvedValueRecord};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct SinglePosRule {
    pub glyph: GlyphId,
    pub record: ResolvedValueRecord,
}

impl SinglePosRule {
    pub fn merge(&mut self, other: &Self) {
        self.record.add_in_place(&other.record);
    }
}

impl PrintNames for SinglePosRule {
    fn fmt_names(&self, f: &mut std::fmt::Formatter<'_>, names: &NameMap) -> std::fmt::Result {
        let glyph = names.get(self.glyph);
        let record = &self.record;
        write!(f, "{glyph} {record}")
    }
}

impl Debug for SinglePosRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SinglePosRule")
            .field("glyph", &self.glyph)
            .finish_non_exhaustive()
    }
}

pub(super) fn get_singlepos_rules(
    subtables: &[SinglePos],
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<SinglePosRule>, ReadError> {
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    let mut add = |glyph: GlyphId, record: ValueRecord| {
        // only the first subtable covering a glyph applies, even for a zero value
        if seen.insert(glyph) {
            let record = ResolvedValueRecord::new(record, delta_computer).unwrap();
            if !record.is_zero() {
                result.push(SinglePosRule { glyph, record });
            }
        }
    };
    for subtable in subtables {
        match subtable {
            SinglePos::Format1(subtable) => {
                for glyph in subtable.coverage().unwrap().iter() {
                    add(glyph, subtable.value_record());
                }
            }
            SinglePos::Format2(subtable) => {
                for (glyph, record) in subtable
                    .coverage()
                    .unwrap()
                    .iter()
                    .zip(subtable.value_records().iter())
                {
                    add(glyph, record.unwrap());
                }
            }
            SinglePos::Format3(_) | SinglePos::Format4(_) => (),
        }
    }
    Ok(result)
}
