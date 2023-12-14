use std::{
    any::Any,
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use indexmap::IndexMap;
use write_fonts::read::{
    tables::{
        gpos::{PairPos, PairPosFormat1, PairPosFormat2, ValueRecord},
        layout::LookupFlag,
    },
    types::GlyphId,
    ReadError,
};

use crate::{common::GlyphSet, glyph_names::NameMap, variations::DeltaComputer};

use super::{AnyRule, LookupRule, LookupType, ResolvedValueRecord};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct PairPosRule {
    first: GlyphId,
    second: GlyphSet,
    record1: ResolvedValueRecord,
    record2: ResolvedValueRecord,
    flags: LookupFlag,
}

impl AnyRule for PairPosRule {
    fn lookup_flags(&self) -> (LookupFlag, Option<u16>) {
        (self.flags, None)
    }

    fn fmt_impl(&self, f: &mut std::fmt::Formatter<'_>, names: &NameMap) -> std::fmt::Result {
        let g1 = names.get(self.first);
        let g2 = self.second.printer(names);
        let v1 = &self.record1;
        let v2 = &self.record2;
        write!(f, "{g1} {v1} {g2}")?;
        if !self.record2.is_zero() {
            write!(f, " {v2}")
        } else {
            Ok(())
        }
    }

    fn lookup_type(&self) -> LookupType {
        LookupType::PairPos
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Debug for PairPosRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PairPosRule")
            .field("first", &self.first)
            .field("second", &self.second)
            .finish_non_exhaustive()
    }
}

// combine any rules where the first glyph and the value records are identical.
fn combine_rules(rules: Vec<PairPosRule>) -> Vec<LookupRule> {
    let mut seen = IndexMap::<_, PairPosRule>::new();
    for rule in rules {
        match seen.entry((rule.first, rule.record1.clone(), rule.record2.clone())) {
            indexmap::map::Entry::Occupied(mut entry) => {
                entry.get_mut().second.combine(rule.second)
            }
            indexmap::map::Entry::Vacant(entry) => {
                entry.insert(rule);
            }
        }
    }
    seen.into_values().map(LookupRule::PairPos).collect()
}

pub(super) fn get_pairpos_rules(
    subtables: &[PairPos],
    flags: LookupFlag,
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<LookupRule>, ReadError> {
    // so we only take the first coverage hit in each subtable, which means
    // we just need track what we've seen.
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for sub in subtables.iter() {
        match sub {
            PairPos::Format1(sub) => {
                append_pairpos_f1_rules(sub, flags, delta_computer, &mut seen, &mut result);
            }
            PairPos::Format2(sub) => {
                append_pairpos_f2_rules(sub, flags, delta_computer, &mut seen, &mut result);
            }
        }
    }
    Ok(combine_rules(result))
}

// okay so we want to return some heterogeneous type here hmhm
fn append_pairpos_f1_rules(
    subtable: &PairPosFormat1,
    flags: LookupFlag,
    delta_computer: Option<&DeltaComputer>,
    seen: &mut HashSet<GlyphId>,
    result: &mut Vec<PairPosRule>,
) {
    let coverage = subtable.coverage().unwrap();
    let pairsets = subtable.pair_sets();
    for (gid1, pairset) in coverage.iter().zip(pairsets.iter()) {
        if !seen.insert(gid1) {
            continue;
        }
        let pairset = pairset.unwrap();
        for pairrec in pairset.pair_value_records().iter() {
            let pairrec = pairrec.unwrap();
            let gid2 = pairrec.second_glyph();
            let data = pairset.offset_data();
            let record1 =
                ResolvedValueRecord::new(pairrec.value_record1, data, delta_computer).unwrap();
            let record2 =
                ResolvedValueRecord::new(pairrec.value_record2, data, delta_computer).unwrap();
            result.push(PairPosRule {
                first: gid1,
                second: gid2.into(),
                record1,
                record2,
                flags,
            })
        }
    }
}

fn is_noop(value_record: &ValueRecord) -> bool {
    value_record.x_placement().unwrap_or(0) == 0
        && value_record.y_placement().unwrap_or(0) == 0
        && value_record.x_advance().unwrap_or(0) == 0
        && value_record.y_advance().unwrap_or(0) == 0
}

fn append_pairpos_f2_rules(
    subtable: &PairPosFormat2,
    flags: LookupFlag,
    delta_computer: Option<&DeltaComputer>,
    seen: &mut HashSet<GlyphId>,
    result: &mut Vec<PairPosRule>,
) {
    let coverage = subtable.coverage().unwrap();
    let class1 = subtable.class_def1().unwrap();
    let class2 = subtable.class_def2().unwrap();
    let mut reverse_class2 = HashMap::new();
    for (gid, class) in class2.iter() {
        reverse_class2.entry(class).or_insert(Vec::new()).push(gid);
    }

    let class1records = subtable.class1_records();
    let data = subtable.offset_data();
    for gid1 in coverage.iter() {
        if !seen.insert(gid1) {
            continue;
        }
        let g1class = class1.get(gid1);
        let class1rec = class1records.get(g1class as _).unwrap();
        for (c2, class2rec) in class1rec.class2_records().iter().enumerate() {
            let class2rec = class2rec.unwrap();
            let record1 = class2rec.value_record1();
            let record2 = class2rec.value_record2();
            if is_noop(record1) && is_noop(record2) {
                continue;
            }
            for gid2 in reverse_class2
                .get(&(c2 as u16))
                .into_iter()
                .flat_map(|c2glyphs| c2glyphs.iter())
                .copied()
            {
                let record1 =
                    ResolvedValueRecord::new(record1.clone(), data, delta_computer).unwrap();
                let record2 =
                    ResolvedValueRecord::new(record2.clone(), data, delta_computer).unwrap();

                result.push(PairPosRule {
                    first: gid1,
                    second: gid2.into(),
                    record1: record1.clone(),
                    record2: record2.clone(),
                    flags,
                })
            }
        }
    }
}
