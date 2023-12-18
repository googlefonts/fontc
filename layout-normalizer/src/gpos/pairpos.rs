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
    seen: &mut HashSet<(GlyphId, GlyphId)>,
    result: &mut Vec<PairPosRule>,
) {
    let coverage = subtable.coverage().unwrap();
    let pairsets = subtable.pair_sets();
    for (gid1, pairset) in coverage.iter().zip(pairsets.iter()) {
        let pairset = pairset.unwrap();
        for pairrec in pairset.pair_value_records().iter() {
            let pairrec = pairrec.unwrap();
            let gid2 = pairrec.second_glyph();
            // if a previous subtable had a kern for this pair, skip it here
            if !seen.insert((gid1, gid2)) {
                continue;
            }
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
    seen: &mut HashSet<(GlyphId, GlyphId)>,
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
                if !seen.insert((gid1, gid2)) {
                    continue;
                }
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

#[cfg(test)]
mod tests {

    use std::collections::BTreeSet;

    use super::*;
    use fea_rs::compile::{Builder, PairPosBuilder, ValueRecord};
    use write_fonts::{
        read::FontRead,
        tables::{gpos::PairPos, variations::ivs_builder::VariationStoreBuilder},
    };

    // a way to bolt a simpler API onto the PairPosBuilder from fea-rs
    trait SimplePairPosBuilder {
        fn add_pair(&mut self, gid1: u16, gid2: u16, x_adv: i16);
        fn add_class(&mut self, class1: &[u16], class2: &[u16], x_adv: i16);
        fn build_exactly_one_subtable(self) -> PairPos;
    }

    impl SimplePairPosBuilder for PairPosBuilder {
        fn add_pair(&mut self, gid1: u16, gid2: u16, x_adv: i16) {
            self.insert_pair(
                GlyphId::new(gid1),
                ValueRecord::new().with_x_advance(x_adv),
                GlyphId::new(gid2),
                ValueRecord::new(),
            )
        }

        fn add_class(&mut self, class1: &[u16], class2: &[u16], x_adv: i16) {
            let class1 = class1.iter().copied().map(GlyphId::new).collect();
            let class2 = class2.iter().copied().map(GlyphId::new).collect();
            let record1 = ValueRecord::new().with_x_advance(x_adv);
            self.insert_classes(class1, record1, class2, ValueRecord::new())
        }

        fn build_exactly_one_subtable(self) -> PairPos {
            let mut varstore = VariationStoreBuilder::new(0);
            let subs = self.build(&mut varstore);
            assert_eq!(subs.len(), 1);
            subs.into_iter().next().unwrap()
        }
    }

    // convert from the enum back to the specific pairpos type.
    //
    // I want to change how these types work, but this is fine for now
    fn extract_rules(rules: Vec<LookupRule>) -> Vec<PairPosRule> {
        rules
            .into_iter()
            .map(|rule| match rule {
                LookupRule::PairPos(rule) => rule,
                _ => panic!("only pairpos rules expected here"),
            })
            .collect()
    }

    // to make our tests easier to read, have a special partialeq impl
    impl PartialEq<(u16, &[u16], i16)> for PairPosRule {
        // (left gid, [right gids], x advance)
        fn eq(&self, other: &(u16, &[u16], i16)) -> bool {
            // bail early because the next comparison allocates -_-
            if self.first.to_u16() != other.0 {
                return false;
            }
            let second_matches = match (other.1, &self.second) {
                ([just_one], GlyphSet::Single(glyph)) => *just_one == glyph.to_u16(),
                (left, GlyphSet::Multiple(right)) => {
                    &left
                        .iter()
                        .copied()
                        .map(GlyphId::new)
                        .collect::<BTreeSet<_>>()
                        == right
                }
                _ => false,
            };
            second_matches
                && self.record1.maybe_just_adv().map(|x| x.default) == Some(other.2)
                && self.record2.is_zero()
        }
    }

    #[test]
    fn first_subtable_wins() {
        let mut sub1 = PairPosBuilder::default();
        sub1.add_pair(5, 6, -7);
        sub1.add_pair(10, 11, 1011);
        let sub1 = sub1.build_exactly_one_subtable();

        let mut sub2 = PairPosBuilder::default();
        // should be ignored:
        sub2.add_pair(5, 6, -30);
        sub2.add_pair(5, 7, -30);
        let sub2 = sub2.build_exactly_one_subtable();

        let mut sub3 = PairPosBuilder::default();
        // adds (4, 7), (4, 8) and (5, 8) but NOT (5, 7)
        sub3.add_class(&[4, 5], &[7, 8], -808);
        let sub3 = sub3.build_exactly_one_subtable();

        // dump tables to bytes
        let sub1 = write_fonts::dump_table(&sub1).unwrap();
        let sub2 = write_fonts::dump_table(&sub2).unwrap();
        let sub3 = write_fonts::dump_table(&sub3).unwrap();

        // read them back as to read-fonts types
        let sub1 = write_fonts::read::tables::gpos::PairPos::read(sub1.as_slice().into()).unwrap();
        let sub2 = write_fonts::read::tables::gpos::PairPos::read(sub2.as_slice().into()).unwrap();
        let sub3 = write_fonts::read::tables::gpos::PairPos::read(sub3.as_slice().into()).unwrap();

        let rules = get_pairpos_rules(&[sub1, sub2, sub3], LookupFlag::empty(), None).unwrap();
        let mut rules = extract_rules(rules);
        // sorted by first glyph
        rules.sort_unstable();

        // (left gid, [right gids], x advance)
        let expected: &[(u16, &[u16], i16)] = &[
            (4, &[7, 8], -808), // from sub3
            (5, &[6], -7),      // sub1
            (5, &[7], -30),     // sub2
            (5, &[8], -808),    // sub3
            (10, &[11], 1011),  //sub 1
        ];

        assert_eq!(rules, expected);
    }
}
