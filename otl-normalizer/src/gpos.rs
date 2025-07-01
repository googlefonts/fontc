use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    io,
};

use write_fonts::{
    read::{
        tables::{
            gdef::{Gdef, MarkGlyphSets},
            gpos::{AnchorTable, Gpos, PositionLookupList, PositionSubtables, ValueRecord},
            layout::DeviceOrVariationIndex,
        },
        FontData, ReadError,
    },
    tables::layout::LookupFlag,
    types::GlyphId16,
};

use crate::{
    common::{self, DeviceOrDeltas, GlyphSet, Lookup, PrintNames, SingleRule},
    error::Error,
    glyph_names::NameMap,
    variations::DeltaComputer,
};

mod marks;
mod pairpos;

#[cfg(test)]
mod test_helpers;

use self::{marks::MarkAttachmentRule, pairpos::PairPosRule};

/// Print normalized GPOS layout rules for the provided font
pub fn print(
    f: &mut dyn io::Write,
    table: &Gpos,
    gdef: Option<&Gdef>,
    names: &NameMap,
) -> Result<(), Error> {
    let var_store = gdef
        .as_ref()
        .and_then(|gdef| gdef.item_var_store())
        .map(|ivs| ivs.and_then(DeltaComputer::new))
        .transpose()
        .unwrap();
    let mark_glyph_sets = gdef
        .and_then(|gdef| gdef.mark_glyph_sets_def())
        .transpose()
        .unwrap();

    let script_list = table.script_list().unwrap();
    let feature_list = table.feature_list().unwrap();
    let lang_systems = common::get_lang_systems(&script_list, &feature_list);
    let lookup_rules = get_lookup_rules(&table.lookup_list().unwrap(), var_store.as_ref());

    // so first we iterate through each feature/language/script set
    for sys in &lang_systems {
        writeln!(f,)?;
        sys.fmt_header(f)?;

        // then for each feature/language/script we iterate through
        // all rules, split by the rule (lookup) type
        let pairpos = lookup_rules.pairpos_rules(&sys.lookups);
        let markmark = lookup_rules.markmark_rules(&sys.lookups);
        let markbase = lookup_rules.markbase_rules(&sys.lookups);
        let markliga = lookup_rules.markliga_rules(&sys.lookups);

        print_rules(f, "PairPos", &pairpos, names, mark_glyph_sets.as_ref())?;
        print_rules(f, "MarkToBase", &markbase, names, mark_glyph_sets.as_ref())?;
        print_rules(f, "MarkToMark", &markmark, names, mark_glyph_sets.as_ref())?;
        print_rules(f, "MarkToLig", &markliga, names, mark_glyph_sets.as_ref())?;
    }

    Ok(())
}

fn print_rules<T: PrintNames + Clone>(
    f: &mut dyn io::Write,
    type_name: &str,
    rules: &[SingleRule<T>],
    names: &NameMap,
    mark_glyph_sets: Option<&MarkGlyphSets>,
) -> Result<(), Error> {
    if rules.is_empty() {
        return Ok(());
    }

    writeln!(f, "# {} {type_name} rules", rules.len(),)?;
    let mut last_flag = None;
    let mut last_filter_set = None;
    for rule in rules {
        let (flags, filter_set_id) = rule.lookup_flags();
        if last_flag != Some(flags) {
            writeln!(f, "# lookupflag {flags:?}")?;
            last_flag = Some(flags);
        }

        if filter_set_id != last_filter_set {
            if let Some(filter_id) = filter_set_id {
                let filter_set = mark_glyph_sets
                    .as_ref()
                    .map(|gsets| gsets.coverages().get(filter_id as usize))
                    .transpose()
                    .unwrap();
                let glyphs = filter_set.map(|cov| cov.iter().collect::<GlyphSet>());
                if let Some(glyphs) = glyphs {
                    writeln!(f, "# filter glyphs: {}", glyphs.printer(names))?;
                }
            }
        }
        last_filter_set = filter_set_id;
        writeln!(f, "{}", rule.printer(names))?;
    }
    Ok(())
}

/// A value plus an optional device table or set of deltas
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ResolvedValue {
    default: i16,
    device_or_deltas: Option<DeviceOrDeltas>,
}

/// A value record where any contained tables have been resolved
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ResolvedValueRecord {
    x_advance: ResolvedValue,
    y_advance: ResolvedValue,
    x_placement: ResolvedValue,
    y_placement: ResolvedValue,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ResolvedAnchor {
    x: ResolvedValue,
    y: ResolvedValue,
}

impl ResolvedValueRecord {
    fn new(
        record: ValueRecord,
        data: FontData,
        computer: Option<&DeltaComputer>,
    ) -> Result<Self, ReadError> {
        let x_advance =
            ResolvedValue::new(record.x_advance(), record.x_advance_device(data), computer)?;
        let y_advance =
            ResolvedValue::new(record.y_advance(), record.y_advance_device(data), computer)?;

        let x_placement = ResolvedValue::new(
            record.x_placement(),
            record.x_placement_device(data),
            computer,
        )?;
        let y_placement = ResolvedValue::new(
            record.y_placement(),
            record.y_placement_device(data),
            computer,
        )?;
        Ok(ResolvedValueRecord {
            x_advance,
            y_advance,
            x_placement,
            y_placement,
        })
    }

    /// If y_adv, y_pos and x_pos are all zero, return x_adv
    fn maybe_just_adv(&self) -> Option<&ResolvedValue> {
        if self.y_advance.is_zero() && self.y_placement.is_zero() && self.x_placement.is_zero() {
            Some(&self.x_advance)
        } else {
            None
        }
    }

    fn is_zero(&self) -> bool {
        self.y_advance.is_zero()
            && self.x_advance.is_zero()
            && self.y_placement.is_zero()
            && self.x_placement.is_zero()
    }

    fn add_in_place(&mut self, other: &Self) {
        self.x_advance.add_in_place(&other.x_advance);
        self.y_advance.add_in_place(&other.y_advance);
        self.x_placement.add_in_place(&other.x_placement);
        self.y_placement.add_in_place(&other.y_placement);
    }
}

impl ResolvedAnchor {
    fn new(anchor: &AnchorTable, computer: Option<&DeltaComputer>) -> Result<Self, ReadError> {
        let (x, y) = match anchor {
            AnchorTable::Format1(_) | AnchorTable::Format2(_) => {
                (anchor.x_coordinate().into(), anchor.y_coordinate().into())
            }
            AnchorTable::Format3(table) => (
                ResolvedValue::new(table.x_coordinate().into(), table.x_device(), computer)?,
                ResolvedValue::new(table.y_coordinate().into(), table.y_device(), computer)?,
            ),
        };
        Ok(ResolvedAnchor { x, y })
    }
}

impl ResolvedValue {
    fn new(
        default: Option<i16>,
        device: Option<Result<DeviceOrVariationIndex, ReadError>>,
        ivs: Option<&DeltaComputer>,
    ) -> Result<Self, ReadError> {
        let default = default.unwrap_or_default();
        let device_or_deltas = device
            .transpose()?
            .map(|device| DeviceOrDeltas::new(default, device, ivs));
        device_or_deltas
            .transpose()
            .map(|device_or_deltas| ResolvedValue {
                default,
                device_or_deltas,
            })
    }

    fn is_zero(&self) -> bool {
        self.default == 0 && self.device_or_deltas.is_none()
    }

    fn add_in_place(&mut self, other: &ResolvedValue) {
        self.default += other.default;
        // note: in theory there could be Device tables here, in which case
        // we are just dropping the second one; in practice Device tables are
        // basically unused, and are completely unused in Google Fonts fonts
        if let (Some(DeviceOrDeltas::Deltas(d1)), Some(DeviceOrDeltas::Deltas(d2))) = (
            self.device_or_deltas.as_mut(),
            other.device_or_deltas.as_ref(),
        ) {
            // these aren't deltas, but rather the resolved value at each
            // defined master location, so they should always be equal.
            assert_eq!(d1.len(), d2.len());
            d1.iter_mut().zip(d2.iter()).for_each(|(d1, d2)| *d1 += d2)
        }
    }
}

#[derive(Clone, Debug, Default)]
struct LookupRules {
    pairpos: Vec<Lookup<PairPosRule>>,
    markbase: Vec<Lookup<MarkAttachmentRule>>,
    markmark: Vec<Lookup<MarkAttachmentRule>>,
    markliga: Vec<Lookup<MarkAttachmentRule>>,
    // decomposed rules for each lookup, in lookup order
}

impl LookupRules {
    fn pairpos_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, PairPosRule>> {
        use std::collections::hash_map;
        // so these rules are currently decomposed, (each rule is for a
        // single pair of glyphs) so we want to normalize them and then combine.
        let mut pairmap = HashMap::<_, _>::new();
        for rule in self
            .pairpos
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
            .flat_map(|lookup| lookup.iter())
        {
            match pairmap.entry((
                rule.rule().first,
                rule.rule().second.clone(),
                rule.lookup_flags(),
            )) {
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(rule);
                }
                hash_map::Entry::Occupied(mut entry) => {
                    let prev_rule = entry.get_mut().rule_mut();
                    prev_rule.merge(rule.rule());
                }
            };
        }

        // now for any given first glyph + adjustment if there are multiple
        // second glyphs we combine these
        let mut seen = HashMap::<_, _>::new();
        for rule in pairmap.into_values() {
            match seen.entry((
                rule.rule().first,
                rule.lookup_flags(),
                rule.rule().record1.clone(),
                rule.rule().record2.clone(),
            )) {
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(rule);
                }
                hash_map::Entry::Occupied(mut entry) => {
                    entry
                        .get_mut()
                        .rule_mut()
                        .second
                        .combine(&rule.rule().second);
                }
            }
        }
        let mut result: Vec<_> = seen.into_values().collect();
        result.sort_unstable();
        result
    }

    fn markbase_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, MarkAttachmentRule>> {
        let lookups = self
            .markbase
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
            .collect::<Vec<_>>();
        normalize_mark_lookups(&lookups)
    }

    fn markmark_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, MarkAttachmentRule>> {
        let lookups = self
            .markmark
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
            .collect::<Vec<_>>();
        normalize_mark_lookups(&lookups)
    }

    fn markliga_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, MarkAttachmentRule>> {
        let lookups = self
            .markliga
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
            .collect::<Vec<_>>();
        normalize_mark_lookups(&lookups)
    }
}

// impl shared between markbase and markmark
fn normalize_mark_lookups<'a>(
    lookups: &[&'a Lookup<MarkAttachmentRule>],
) -> Vec<SingleRule<'a, MarkAttachmentRule>> {
    // normalizing marks is just "last writer wins", e.g. we want to ignore any
    // mark rules that would be superseded in a subsequent lookup.

    // in practice this means:
    // - collect all the lookups
    // - walk them in order, creating a map of (base, mark) -> id of last containing lookup
    // - then do it again, this time removing marks from earlier lookups?
    // - it's a bit gross
    // - but computer not care, computer count fast

    // a little helper used below
    fn prune_shadowed_marks<'a>(
        mut rule: SingleRule<'a, MarkAttachmentRule>,
        last_seen: &HashMap<(GlyphId16, GlyphId16), usize>,
        lookup_id: usize,
    ) -> Option<SingleRule<'a, MarkAttachmentRule>> {
        let to_remove = rule
            .rule()
            .iter_base_mark_pairs()
            .filter_map(|pair| (last_seen.get(&pair) != Some(&lookup_id)).then_some(pair.1))
            .collect::<Vec<_>>();

        if to_remove.is_empty() {
            return Some(rule);
        }

        // if, after removing marks, the rule is empty, we discard it
        if rule.rule_mut().remove_marks(&to_remove) {
            Some(rule)
        } else {
            None
        }
    }

    let mut last_seen = HashMap::new();
    for (i, lookup) in lookups.iter().enumerate() {
        for rule in lookup.iter() {
            for (base, mark) in rule.rule().iter_base_mark_pairs() {
                last_seen.insert((base, mark), i);
            }
        }
    }

    // now we have a map of (base, mark) -> last_containing_lookup.
    let mut result = Vec::new();
    for (i, lookup) in lookups.iter().enumerate() {
        for rule in lookup.iter() {
            let rule = prune_shadowed_marks(rule, &last_seen, i);
            result.extend(rule);
        }
    }

    result.sort_unstable();
    result
}

fn get_lookup_rules(
    lookups: &PositionLookupList,
    delta_computer: Option<&DeltaComputer>,
) -> LookupRules {
    let mut result = LookupRules::default();
    for (id, lookup) in lookups.lookups().iter().enumerate() {
        let lookup = lookup.unwrap();
        let flag = lookup.lookup_flag();
        let mark_filter_id = flag
            .contains(LookupFlag::USE_MARK_FILTERING_SET)
            .then_some(lookup.mark_filtering_set())
            .flatten();
        let subtables = lookup.subtables().unwrap();
        match subtables {
            PositionSubtables::Pair(subs) => {
                let subs = subs.iter().flat_map(|sub| sub.ok()).collect::<Vec<_>>();
                let rules = pairpos::get_pairpos_rules(&subs, delta_computer).unwrap();
                result.pairpos.push(Lookup::new(id, rules, flag, None));
            }
            PositionSubtables::MarkToBase(subs) => {
                let subs = subs.iter().flat_map(|sub| sub.ok()).collect::<Vec<_>>();
                let rules = marks::get_mark_base_rules(&subs, delta_computer).unwrap();
                result
                    .markbase
                    .push(Lookup::new(id, rules, flag, mark_filter_id));
            }
            PositionSubtables::MarkToMark(subs) => {
                let subs = subs.iter().flat_map(|sub| sub.ok()).collect::<Vec<_>>();
                let rules = marks::get_mark_mark_rules(&subs, delta_computer).unwrap();
                result
                    .markmark
                    .push(Lookup::new(id, rules, flag, mark_filter_id));
            }
            PositionSubtables::MarkToLig(subs) => {
                let subs = subs.iter().flat_map(|sub| sub.ok()).collect::<Vec<_>>();
                let rules = marks::get_mark_liga_rules(&subs, delta_computer).unwrap();
                result
                    .markliga
                    .push(Lookup::new(id, rules, flag, mark_filter_id));
            }
            _ => (),
        }
    }
    result
}

impl From<i16> for ResolvedValue {
    fn from(src: i16) -> ResolvedValue {
        ResolvedValue {
            default: src,
            device_or_deltas: None,
        }
    }
}

impl Display for ResolvedValueRecord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(xadv) = self.maybe_just_adv() {
            if !xadv.is_zero() {
                write!(f, "{xadv}")
            } else {
                Ok(())
            }
        } else {
            write!(
                f,
                "<{} {} {} {}>",
                &self.x_placement, &self.y_placement, &self.x_advance, &self.y_advance
            )
        }
    }
}

impl Display for ResolvedValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.default)?;
        if let Some(device_or_deltas) = self.device_or_deltas.as_ref() {
            write!(f, "{device_or_deltas}")?;
        }
        Ok(())
    }
}

impl Display for ResolvedAnchor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@(x: {}, y: {})", self.x, self.y)
    }
}

#[cfg(test)]
mod tests {
    use write_fonts::tables::{
        gpos::builders::{MarkToBaseBuilder, PairPosBuilder},
        layout::LookupFlag,
    };

    use super::test_helpers::{RawAnchor, SimpleMarkBaseBuilder, SimplePairPosBuilder};
    use super::*;
    use write_fonts::{
        read::FontRead,
        tables::{gpos as wgpos, layout as wlayout},
    };

    #[test]
    fn merge_pairpos_lookups() {
        let mut sub1 = PairPosBuilder::default();
        sub1.add_pair(1, 4, 20);
        sub1.add_pair(1, 5, -10);
        let sub1 = sub1.build_exactly_one_subtable();

        let mut sub2 = PairPosBuilder::default();
        // this class overlaps with the previous for the pair (1, 4)
        sub2.add_class(&[1, 2], &[3, 4], 7);
        let sub2 = sub2.build_exactly_one_subtable();

        let lookup1 =
            wgpos::PositionLookup::Pair(wlayout::Lookup::new(LookupFlag::empty(), vec![sub1]));
        let lookup2 =
            wgpos::PositionLookup::Pair(wlayout::Lookup::new(LookupFlag::empty(), vec![sub2]));
        let lookup_list = wlayout::LookupList::new(vec![lookup1, lookup2]);
        let lookup_list = write_fonts::dump_table(&lookup_list).unwrap();
        let lookup_list = write_fonts::read::tables::gpos::PositionLookupList::read(
            lookup_list.as_slice().into(),
        )
        .unwrap();

        let rules = get_lookup_rules(&lookup_list, None);

        let our_rules = rules
            .pairpos_rules(&[0, 1])
            .into_iter()
            .map(|r| r.rule().to_owned())
            .collect::<Vec<_>>();

        // (left gid, [right gids], x advance)
        let expected: &[(u16, &[u16], i16)] = &[
            (1, &[3], 7),
            (1, &[4], 27),
            (1, &[5], -10),  // sub1
            (2, &[3, 4], 7), // sub2
        ];

        assert_eq!(our_rules, expected);
    }

    #[test]
    fn no_merge_different_lookupflags() {
        let mut sub1 = PairPosBuilder::default();
        sub1.add_pair(1, 4, 20);
        sub1.add_pair(1, 5, -10);
        let sub1 = sub1.build_exactly_one_subtable();

        let mut sub2 = PairPosBuilder::default();
        // this class overlaps with the previous for the pair (1, 4)
        sub2.add_class(&[1, 2], &[3, 4], 7);
        let sub2 = sub2.build_exactly_one_subtable();

        let lookup1 =
            wgpos::PositionLookup::Pair(wlayout::Lookup::new(LookupFlag::empty(), vec![sub1]));
        // DIFFERENT LOOKUP FLAG
        let lookup2 =
            wgpos::PositionLookup::Pair(wlayout::Lookup::new(LookupFlag::IGNORE_MARKS, vec![sub2]));
        let lookup_list = wlayout::LookupList::new(vec![lookup1, lookup2]);
        let lookup_list = write_fonts::dump_table(&lookup_list).unwrap();
        let lookup_list = write_fonts::read::tables::gpos::PositionLookupList::read(
            lookup_list.as_slice().into(),
        )
        .unwrap();

        let rules = get_lookup_rules(&lookup_list, None);

        let our_rules = rules
            .pairpos_rules(&[0, 1])
            .into_iter()
            .map(|r| r.rule().to_owned())
            .collect::<Vec<_>>();

        // (left gid, [right gids], x advance)
        let expected: &[(u16, &[u16], i16)] = &[
            (1, &[3, 4], 7),
            (1, &[4], 20),   // is not merged
            (1, &[5], -10),  // sub1
            (2, &[3, 4], 7), // sub2
        ];

        assert_eq!(our_rules, expected);
    }

    #[test]
    fn merge_mark_base_lookups() {
        // overlaps completely with the next lookup, so this should all be discarded
        let mut sub0 = MarkToBaseBuilder::default();
        sub0.add_mark(11, "top", (999, 999));
        sub0.add_mark(12, "top", (998, 998));
        sub0.add_base(1, "top", (777, 777));

        let mut sub1 = MarkToBaseBuilder::default();
        sub1.add_mark(11, "top", (11, 11));
        sub1.add_mark(12, "top", (12, 12));
        sub1.add_base(1, "top", (101, 101));

        let mut sub2 = MarkToBaseBuilder::default();
        sub2.add_mark(10, "top", (-10, -10));
        sub2.add_mark(11, "top", (-11, -11));
        sub2.add_base(1, "top", (-101, -101));

        let sub1 = sub1.build_exactly_one_subtable();
        let sub2 = sub2.build_exactly_one_subtable();

        let lookup1 = wgpos::PositionLookup::MarkToBase(wlayout::Lookup::new(
            LookupFlag::empty(),
            vec![sub1],
        ));
        let lookup2 = wgpos::PositionLookup::MarkToBase(wlayout::Lookup::new(
            LookupFlag::empty(),
            vec![sub2],
        ));

        let lookup_list = wlayout::LookupList::new(vec![lookup1, lookup2]);
        let lookup_list = write_fonts::dump_table(&lookup_list).unwrap();
        let lookup_list = write_fonts::read::tables::gpos::PositionLookupList::read(
            lookup_list.as_slice().into(),
        )
        .unwrap();

        let rules = get_lookup_rules(&lookup_list, None);

        let mark_base_rules = rules
            .markbase_rules(&[0, 1])
            .into_iter()
            .map(|r| r.rule().to_owned())
            .collect::<Vec<_>>();
        let mark_base_rules = test_helpers::SimpleAnchorRule::from_mark_rules(&mark_base_rules);

        // (base gid, base anchor, mark gid, mark anchor)
        // NOTE: ordered by base gid, then base anchor, then mark anchor
        let expected: &[(u16, RawAnchor, u16, RawAnchor)] = &[
            (1, (-101, -101), 11, (-11, -11)),
            (1, (-101, -101), 10, (-10, -10)),
            (1, (101, 101), 12, (12, 12)),
        ];

        assert_eq!(mark_base_rules, expected,)
    }
}
