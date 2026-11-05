use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::{Debug, Display},
    io,
};

use write_fonts::{
    read::{
        ReadError,
        tables::{
            gdef::{Gdef, MarkGlyphSets},
            gpos::{
                AnchorTable, CursivePos, Gpos, MarkBasePos, MarkLigPos, MarkMarkPos,
                PositionLookupList, PositionSubtables, ValueRecord,
            },
            layout::{
                ChainedSequenceContext, DeviceOrVariationIndex, SequenceContext,
                SequenceLookupRecord,
            },
        },
    },
    tables::layout::LookupFlag,
    types::GlyphId,
};

use crate::{
    common::{self, DeviceOrDeltas, GlyphSet, Lookup, PrintNames, SingleRule},
    error::Error,
    glyph_names::NameMap,
    gpos::cursive::CursivePosRule,
    variations::DeltaComputer,
};

mod cursive;
mod marks;
mod pairpos;
mod singlepos;

#[cfg(test)]
mod test_helpers;

use self::{marks::MarkAttachmentRule, pairpos::PairPosRule, singlepos::SinglePosRule};

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
        // TODO: pre-scan GPOS data to collect used subtable indices and filter regions,
        // like gdef::collect_caret_subtables does for ligature carets.
        .map(|ivs| ivs.and_then(|ivs| DeltaComputer::new(ivs, None)))
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

        print_lookup_rules(
            f,
            &lookup_rules,
            &sys.lookups,
            names,
            mark_glyph_sets.as_ref(),
        )?;
    }

    // lookups no feature reaches, plus contextual and FeatureVariations targets:
    // print these on their own since the XML keeps their LookupListIndex references
    for id in standalone_lookup_ids(table, &lang_systems) {
        let mut output = Vec::new();
        print_lookup_rules(
            &mut output,
            &lookup_rules,
            &[id],
            names,
            mark_glyph_sets.as_ref(),
        )?;
        if !output.is_empty() {
            writeln!(f, "\n# lookup {id}")?;
            f.write_all(&output)?;
        }
    }

    Ok(())
}

fn print_lookup_rules(
    f: &mut dyn io::Write,
    rules: &LookupRules,
    lookups: &[u16],
    names: &NameMap,
    mark_glyph_sets: Option<&MarkGlyphSets>,
) -> Result<(), Error> {
    let singlepos = rules.singlepos_rules(lookups);
    let pairpos = rules.pairpos_rules(lookups);
    let markbase = rules.markbase_rules(lookups);
    let markmark = rules.markmark_rules(lookups);
    let markliga = rules.markliga_rules(lookups);
    let cursive = rules.cursive_rules(lookups);

    print_rules(f, "SinglePos", &singlepos, names, mark_glyph_sets)?;
    print_rules(f, "PairPos", &pairpos, names, mark_glyph_sets)?;
    print_rules(f, "MarkToBase", &markbase, names, mark_glyph_sets)?;
    print_rules(f, "MarkToMark", &markmark, names, mark_glyph_sets)?;
    print_rules(f, "MarkToLig", &markliga, names, mark_glyph_sets)?;
    print_rules(f, "CursivePos", &cursive, names, mark_glyph_sets)?;
    Ok(())
}

fn standalone_lookup_ids(table: &Gpos, features: &[common::Feature]) -> BTreeSet<u16> {
    let in_features = features
        .iter()
        .flat_map(|feature| feature.lookups.iter().copied())
        .collect::<BTreeSet<_>>();
    let lookups = table.lookup_list().unwrap();
    let mut ids = (0..lookups.lookups().len())
        .map(|id| u16::try_from(id).unwrap())
        .filter(|id| !in_features.contains(id))
        .collect::<BTreeSet<_>>();
    let mut add_records = |records: &[SequenceLookupRecord]| {
        ids.extend(records.iter().map(|record| record.lookup_list_index()));
    };
    for lookup in lookups.lookups().iter() {
        match lookup.unwrap().subtables().unwrap() {
            PositionSubtables::Contextual(subs) => {
                for sub in subs.iter() {
                    match sub.unwrap() {
                        SequenceContext::Format1(sub) => {
                            for rules in sub.seq_rule_sets().iter().flatten() {
                                for rule in rules.unwrap().seq_rules().iter() {
                                    add_records(rule.unwrap().seq_lookup_records());
                                }
                            }
                        }
                        SequenceContext::Format2(sub) => {
                            for rules in sub.class_seq_rule_sets().iter().flatten() {
                                for rule in rules.unwrap().class_seq_rules().iter() {
                                    add_records(rule.unwrap().seq_lookup_records());
                                }
                            }
                        }
                        SequenceContext::Format3(sub) => add_records(sub.seq_lookup_records()),
                        SequenceContext::Format4(sub) => {
                            for rules in sub.seq_rule_sets().iter().flatten() {
                                for rule in rules.unwrap().seq_rules().iter() {
                                    add_records(rule.unwrap().seq_lookup_records());
                                }
                            }
                        }
                        SequenceContext::Format5(sub) => {
                            for rules in sub.class_seq_rule_sets().iter().flatten() {
                                for rule in rules.unwrap().class_seq_rules().iter() {
                                    add_records(rule.unwrap().seq_lookup_records());
                                }
                            }
                        }
                        SequenceContext::Format6(sub) => add_records(sub.seq_lookup_records()),
                    }
                }
            }
            PositionSubtables::ChainContextual(subs) => {
                for sub in subs.iter() {
                    match sub.unwrap() {
                        ChainedSequenceContext::Format1(sub) => {
                            for rules in sub.chained_seq_rule_sets().iter().flatten() {
                                for rule in rules.unwrap().chained_seq_rules().iter() {
                                    add_records(rule.unwrap().seq_lookup_records());
                                }
                            }
                        }
                        ChainedSequenceContext::Format2(sub) => {
                            for rules in sub.chained_class_seq_rule_sets().iter().flatten() {
                                for rule in rules.unwrap().chained_class_seq_rules().iter() {
                                    add_records(rule.unwrap().seq_lookup_records());
                                }
                            }
                        }
                        ChainedSequenceContext::Format3(sub) => {
                            add_records(sub.seq_lookup_records())
                        }
                        ChainedSequenceContext::Format4(sub) => {
                            for rules in sub.chained_seq_rule_sets().iter().flatten() {
                                for rule in rules.unwrap().chained_seq_rules().iter() {
                                    add_records(rule.unwrap().seq_lookup_records());
                                }
                            }
                        }
                        ChainedSequenceContext::Format5(sub) => {
                            for rules in sub.chained_class_seq_rule_sets().iter().flatten() {
                                for rule in rules.unwrap().chained_class_seq_rules().iter() {
                                    add_records(rule.unwrap().seq_lookup_records());
                                }
                            }
                        }
                    }
                }
            }
            _ => (),
        }
    }
    if let Some(vars) = table.feature_variations().transpose().unwrap() {
        for record in vars.feature_variation_records() {
            let Some(subs) = record
                .feature_table_substitution(vars.offset_data())
                .transpose()
                .unwrap()
            else {
                continue;
            };
            for sub in subs.substitutions() {
                let feature = sub.alternate_feature(subs.offset_data()).unwrap();
                ids.extend(feature.lookup_list_indices().iter().map(|id| id.get()));
            }
        }
    }
    ids
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

        if filter_set_id != last_filter_set
            && let Some(filter_id) = filter_set_id
        {
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
    fn new(record: ValueRecord, computer: Option<&DeltaComputer>) -> Result<Self, ReadError> {
        let x_advance =
            ResolvedValue::new(record.x_advance(), record.x_advance_device(), computer)?;
        let y_advance =
            ResolvedValue::new(record.y_advance(), record.y_advance_device(), computer)?;

        let x_placement =
            ResolvedValue::new(record.x_placement(), record.x_placement_device(), computer)?;
        let y_placement =
            ResolvedValue::new(record.y_placement(), record.y_placement_device(), computer)?;
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
        // note: in theory there could be Device tables here, in which case
        // we are just dropping the second one; in practice Device tables are
        // basically unused, and are completely unused in Google Fonts fonts
        match (
            self.device_or_deltas.as_mut(),
            other.device_or_deltas.as_ref(),
        ) {
            (Some(DeviceOrDeltas::Deltas(d1)), Some(DeviceOrDeltas::Deltas(d2))) => {
                // these aren't deltas, but rather the resolved value at each
                // defined master location, so they should always be equal.
                assert_eq!(d1.len(), d2.len());
                d1.iter_mut().zip(d2.iter()).for_each(|(d1, d2)| *d1 += d2)
            }
            (Some(DeviceOrDeltas::Deltas(d1)), None) => {
                d1.iter_mut().for_each(|d| *d += other.default as i32);
            }
            (None, Some(DeviceOrDeltas::Deltas(d2))) => {
                self.device_or_deltas = Some(DeviceOrDeltas::Deltas(
                    d2.iter().map(|d| d + self.default as i32).collect(),
                ));
            }
            _ => (),
        }
        self.default += other.default;
        if matches!(&self.device_or_deltas, Some(DeviceOrDeltas::Deltas(d)) if d.iter().all(|&v| v == self.default as i32))
        {
            self.device_or_deltas = None;
        }
    }
}

#[derive(Clone, Debug, Default)]
struct LookupRules {
    singlepos: Vec<Lookup<SinglePosRule>>,
    pairpos: Vec<Lookup<PairPosRule>>,
    markbase: Vec<Lookup<MarkAttachmentRule>>,
    markmark: Vec<Lookup<MarkAttachmentRule>>,
    markliga: Vec<Lookup<MarkAttachmentRule>>,
    cursive: Vec<Lookup<CursivePosRule>>,
    // decomposed rules for each lookup, in lookup order
}

impl LookupRules {
    fn singlepos_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, SinglePosRule>> {
        use std::collections::hash_map;
        let mut seen = HashMap::<_, _>::new();
        for rule in self
            .singlepos
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
            .flat_map(|lookup| lookup.iter())
        {
            match seen.entry((rule.rule().glyph, rule.lookup_flags())) {
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(rule);
                }
                hash_map::Entry::Occupied(mut entry) => {
                    entry.get_mut().rule_mut().merge(rule.rule());
                }
            }
        }
        let mut result: Vec<_> = seen
            .into_values()
            .filter(|r| !r.rule().record.is_zero())
            .collect();
        result.sort_unstable();
        result
    }

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

    fn cursive_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, CursivePosRule>> {
        // this seems to just be a "last writer wins" situation (via behdad, in chat)?
        let mut rules = BTreeMap::new();
        for rule in self
            .cursive
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
            .flat_map(Lookup::iter)
        {
            rules.insert((rule.rule().glyph, rule.lookup_flags()), rule);
        }
        rules.into_values().collect()
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
        last_seen: &HashMap<(GlyphId, GlyphId), usize>,
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
            PositionSubtables::Single(subs) => {
                let subs = subs.iter().flat_map(|sub| sub.ok()).collect::<Vec<_>>();
                let rules = singlepos::get_singlepos_rules(&subs, delta_computer).unwrap();
                result
                    .singlepos
                    .push(Lookup::new(id, rules, flag, mark_filter_id));
            }
            PositionSubtables::Pair(subs) => {
                let subs = subs.iter().flat_map(|sub| sub.ok()).collect::<Vec<_>>();
                let rules = pairpos::get_pairpos_rules(&subs, delta_computer).unwrap();
                result.pairpos.push(Lookup::new(id, rules, flag, None));
            }
            PositionSubtables::MarkToBase(subs) => {
                let subs = subs
                    .iter()
                    .flat_map(|sub| sub.ok())
                    .filter_map(|sub| match sub {
                        MarkBasePos::Format1(sub) => Some(sub),
                        MarkBasePos::Format2(_) => None,
                    })
                    .collect::<Vec<_>>();
                let rules = marks::get_mark_base_rules(&subs, delta_computer).unwrap();
                result
                    .markbase
                    .push(Lookup::new(id, rules, flag, mark_filter_id));
            }
            PositionSubtables::MarkToMark(subs) => {
                let subs = subs
                    .iter()
                    .flat_map(|sub| sub.ok())
                    .filter_map(|sub| match sub {
                        MarkMarkPos::Format1(sub) => Some(sub),
                        MarkMarkPos::Format2(_) => None,
                    })
                    .collect::<Vec<_>>();
                let rules = marks::get_mark_mark_rules(&subs, delta_computer).unwrap();
                result
                    .markmark
                    .push(Lookup::new(id, rules, flag, mark_filter_id));
            }
            PositionSubtables::MarkToLig(subs) => {
                let subs = subs
                    .iter()
                    .flat_map(|sub| sub.ok())
                    .filter_map(|sub| match sub {
                        MarkLigPos::Format1(sub) => Some(sub),
                        MarkLigPos::Format2(_) => None,
                    })
                    .collect::<Vec<_>>();
                let rules = marks::get_mark_liga_rules(&subs, delta_computer).unwrap();
                result
                    .markliga
                    .push(Lookup::new(id, rules, flag, mark_filter_id));
            }
            PositionSubtables::Cursive(subs) => {
                let subs = subs
                    .iter()
                    .flat_map(|sub| sub.ok())
                    .filter_map(|sub| match sub {
                        CursivePos::Format1(sub) => Some(sub),
                        CursivePos::Format2(_) => None,
                    })
                    .collect::<Vec<_>>();
                let rules = cursive::get_cursive_rules(&subs, delta_computer).unwrap();
                result
                    .cursive
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
                self.x_placement, self.y_placement, self.x_advance, self.y_advance
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
        gpos::builders::{MarkToBaseBuilder, PairPosBuilder, SinglePosBuilder, ValueRecordBuilder},
        layout::LookupFlag,
        variations::{RegionAxisCoordinates, VariationRegion, ivs_builder::VariationStoreBuilder},
    };
    use write_fonts::types::{F2Dot14, GlyphId16};

    use super::test_helpers::{
        RawAnchor, SimpleMarkBaseBuilder, SimplePairPosBuilder, SimpleSinglePosBuilder,
        build_gpos_and_gdef,
    };
    use super::*;
    use write_fonts::{
        from_obj::ToOwnedTable,
        read::FontRead,
        tables::{gpos as wgpos, layout as wlayout},
    };

    #[test]
    fn merge_static_and_variable_values() {
        let varied = |default, masters| ResolvedValue {
            default,
            device_or_deltas: Some(DeviceOrDeltas::Deltas(masters)),
        };
        let first = ResolvedValueRecord {
            x_advance: 5.into(),
            y_advance: 13.into(),
            x_placement: varied(10, vec![30, 50]),
            y_placement: 0.into(),
        };
        let second = ResolvedValueRecord {
            x_advance: varied(10, vec![30, 50]),
            y_advance: 0.into(),
            x_placement: 7.into(),
            y_placement: varied(-5, vec![-15, -25]),
        };
        for (left, right) in [(&first, &second), (&second, &first)] {
            let mut sum = left.clone();
            sum.add_in_place(right);
            assert_eq!(sum.to_string(), "<17 {37,57} -5 {-15,-25} 15 {35,55} 13>");
        }
    }

    #[test]
    fn merge_variations_to_constant() {
        for (other_default, other_master, expected) in [(0, -20, 10), (-10, -30, 0)] {
            let mut value = ResolvedValue {
                default: 10,
                device_or_deltas: Some(DeviceOrDeltas::Deltas(vec![30])),
            };
            let other = ResolvedValue {
                default: other_default,
                device_or_deltas: Some(DeviceOrDeltas::Deltas(vec![other_master])),
            };
            value.add_in_place(&other);
            assert_eq!(value, expected.into());
        }
    }

    #[test]
    fn singlepos_merge_drops_zero_adjustments() {
        let lookups = [10, -10]
            .into_iter()
            .map(|advance| {
                let mut sub = SinglePosBuilder::default();
                sub.add_pos(1, advance);
                wgpos::PositionLookup::Single(wlayout::Lookup::new(
                    LookupFlag::empty(),
                    vec![sub.build_exactly_one_subtable()],
                ))
            })
            .collect();
        let bytes = write_fonts::dump_table(&wlayout::LookupList::new(lookups)).unwrap();
        let lookups = write_fonts::read::tables::gpos::OriginalPositionLookupList::read(
            bytes.as_slice().into(),
        )
        .unwrap();
        let rules = get_lookup_rules(&lookups.into(), None);
        assert!(rules.singlepos_rules(&[0, 1]).is_empty());
    }

    #[test]
    fn zero_singlepos_in_earlier_subtable_shadows_later_subtable() {
        let names = [".notdef", "A", "B"].map(Into::into).into_iter().collect();
        let sub_zero = wgpos::SinglePos::format_1(
            [GlyphId16::new(1)].into_iter().collect(),
            wgpos::ValueRecord::new(),
        );
        let sub_later = wgpos::SinglePos::format_1(
            [GlyphId16::new(1), GlyphId16::new(2)].into_iter().collect(),
            wgpos::ValueRecord::new().with_x_advance(999),
        );
        let lookup = wgpos::PositionLookup::Single(wlayout::Lookup::new(
            LookupFlag::empty(),
            vec![sub_zero, sub_later],
        ));
        let (gpos, gdef) = build_gpos_and_gdef(lookup, VariationStoreBuilder::new(0));
        let mut output = Vec::new();
        print(&mut output, &gpos, Some(&gdef), &names).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "\n# dist: DFLT/dflt\n# 1 SinglePos rules\n# lookupflag LookupFlag(0)\nB 999\n",
        );
    }

    #[test]
    fn singlepos_resolves_variations_in_both_formats() {
        let names = [".notdef", "A", "B"].map(Into::into).into_iter().collect();
        let region = VariationRegion::new(vec![RegionAxisCoordinates::new(
            F2Dot14::ZERO,
            F2Dot14::ONE,
            F2Dot14::ONE,
        )]);
        for (format, expected) in [
            (
                1,
                "\n# dist: DFLT/dflt\n# 2 SinglePos rules\n# lookupflag LookupFlag(8)\nA <3 {5} -5 {-7} 10 {30} -7 {-4}>\nB <3 {5} -5 {-7} 10 {30} -7 {-4}>\n",
            ),
            (
                2,
                "\n# dist: DFLT/dflt\n# 2 SinglePos rules\n# lookupflag LookupFlag(8)\nA <3 {5} -5 {-7} 10 {30} -7 {-4}>\nB <3 {5} -5 {-7} 11 {31} -7 {-4}>\n",
            ),
        ] {
            let mut store = VariationStoreBuilder::new(1);
            let mut value = |advance| {
                ValueRecordBuilder::new()
                    .with_x_advance(advance)
                    .with_x_advance_device(vec![(region.clone(), 20)])
                    .with_x_placement(3)
                    .with_x_placement_device(vec![(region.clone(), 2)])
                    .with_y_placement(-5)
                    .with_y_placement_device(vec![(region.clone(), -2)])
                    .with_y_advance(-7)
                    .with_y_advance_device(vec![(region.clone(), 3)])
                    .build(&mut store)
            };
            let coverage = [1, 2].map(GlyphId16::new).into_iter().collect();
            let subtable = if format == 1 {
                wgpos::SinglePos::format_1(coverage, value(10))
            } else {
                wgpos::SinglePos::format_2(coverage, vec![value(10), value(11)])
            };
            let shadowed = wgpos::SinglePos::format_1(
                [GlyphId16::new(1)].into_iter().collect(),
                wgpos::ValueRecord::new().with_x_advance(999),
            );
            let lookup = wgpos::PositionLookup::Single(wlayout::Lookup::new(
                LookupFlag::IGNORE_MARKS,
                vec![subtable, shadowed],
            ));
            let (gpos, gdef) = build_gpos_and_gdef(lookup, store);
            for in_feature in [false, true] {
                let mut gpos: wgpos::Gpos = gpos.to_owned_table();
                if !in_feature {
                    gpos.script_list = Some(wlayout::ScriptList::default()).into();
                }
                let bytes = write_fonts::dump_table(&gpos).unwrap();
                let gpos = Gpos::read(bytes.as_slice().into()).unwrap();
                let mut output = Vec::new();
                print(&mut output, &gpos, Some(&gdef), &names).unwrap();
                let expected = if in_feature {
                    expected.to_owned()
                } else {
                    expected.replace("# dist: DFLT/dflt", "# lookup 0")
                };
                assert_eq!(
                    String::from_utf8(output).unwrap(),
                    expected,
                    "format {format}"
                );
            }
        }
    }

    #[test]
    fn contextual_pairpos_lookup_is_printed_separately() {
        let mut pair = PairPosBuilder::default();
        pair.add_pair(1, 2, 50);
        let pair = wgpos::PositionLookup::Pair(wlayout::Lookup::new(
            LookupFlag::empty(),
            vec![pair.build_exactly_one_subtable()],
        ));
        let context = wgpos::PositionLookup::ChainContextual(wlayout::Lookup::new(
            LookupFlag::empty(),
            vec![
                wlayout::ChainedSequenceContext::Format3(
                    wlayout::ChainedSequenceContextFormat3::new(
                        vec![],
                        vec![[GlyphId16::new(1)].into_iter().collect()],
                        vec![[GlyphId16::new(2)].into_iter().collect()],
                        vec![wlayout::SequenceLookupRecord::new(0, 1)],
                    ),
                )
                .into(),
            ],
        ));
        let (gpos, gdef) = build_gpos_and_gdef(context.clone(), VariationStoreBuilder::new(0));
        let names = [".notdef", "A", "B"].map(Into::into).into_iter().collect();
        for shared in [false, true] {
            let mut gpos: wgpos::Gpos = gpos.to_owned_table();
            gpos.lookup_list = Some(wlayout::LookupList::new(vec![
                context.clone(),
                pair.clone(),
            ]))
            .into();
            if shared {
                gpos.feature_list.as_mut().unwrap().feature_records[0]
                    .feature
                    .lookup_list_indices
                    .push(1);
            }
            let bytes = write_fonts::dump_table(&gpos).unwrap();
            let gpos = Gpos::read(bytes.as_slice().into()).unwrap();
            let mut output = Vec::new();
            print(&mut output, &gpos, Some(&gdef), &names).unwrap();
            let output = String::from_utf8(output).unwrap();
            assert!(output.ends_with(
                "\n# lookup 1\n# 1 PairPos rules\n# lookupflag LookupFlag(0)\nA 50 B\n"
            ));
            assert_eq!(output.matches("A 50 B").count(), if shared { 2 } else { 1 });
        }
    }

    #[test]
    fn feature_variation_lookup_is_printed_separately() {
        let lookup = |advance| {
            let mut sub = SinglePosBuilder::default();
            sub.add_pos(1, advance);
            wgpos::PositionLookup::Single(wlayout::Lookup::new(
                LookupFlag::empty(),
                vec![sub.build_exactly_one_subtable()],
            ))
        };
        let (gpos, gdef) = build_gpos_and_gdef(lookup(5), VariationStoreBuilder::new(0));
        let mut gpos: wgpos::Gpos = gpos.to_owned_table();
        gpos.lookup_list
            .as_mut()
            .unwrap()
            .lookups
            .push(lookup(-5).into());
        gpos.feature_list.as_mut().unwrap().feature_records[0]
            .feature
            .lookup_list_indices
            .push(1);
        gpos.feature_variations = Some(wlayout::FeatureVariations::new(vec![
            wlayout::FeatureVariationRecord::new(
                None,
                Some(wlayout::FeatureTableSubstitution::new(vec![
                    wlayout::FeatureTableSubstitutionRecord::new(
                        0,
                        wlayout::Feature::new(None, vec![0]),
                    ),
                ])),
            ),
        ]))
        .into();
        let bytes = write_fonts::dump_table(&gpos).unwrap();
        let gpos = Gpos::read(bytes.as_slice().into()).unwrap();
        let names = [".notdef", "A"].map(Into::into).into_iter().collect();
        let mut output = Vec::new();
        print(&mut output, &gpos, Some(&gdef), &names).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "\n# dist: DFLT/dflt\n\n# lookup 0\n# 1 SinglePos rules\n# lookupflag LookupFlag(0)\nA 5\n",
        );
    }

    #[test]
    fn merge_singlepos_lookups() {
        let mut sub1 = SinglePosBuilder::default();
        sub1.add_pos(1, 10);
        sub1.add_pos(2, -20);
        sub1.add_pos(3, 0);
        let mut sub2 = SinglePosBuilder::default();
        sub2.add_pos(1, 5);
        sub2.add_pos(2, 5);
        let mut sub3 = SinglePosBuilder::default();
        sub3.add_pos(1, 7);

        let lookups = [sub1, sub2, sub3]
            .into_iter()
            .zip([
                LookupFlag::empty(),
                LookupFlag::empty(),
                LookupFlag::IGNORE_MARKS,
            ])
            .map(|(sub, flag)| {
                wgpos::PositionLookup::Single(wlayout::Lookup::new(
                    flag,
                    vec![sub.build_exactly_one_subtable()],
                ))
            })
            .collect();
        let bytes = write_fonts::dump_table(&wlayout::LookupList::new(lookups)).unwrap();
        let lookups = write_fonts::read::tables::gpos::OriginalPositionLookupList::read(
            bytes.as_slice().into(),
        )
        .unwrap();
        let rules = get_lookup_rules(&lookups.into(), None);
        let rules = rules.singlepos_rules(&[0, 1, 2]);
        let names = [".notdef", "A", "B", "C"]
            .map(Into::into)
            .into_iter()
            .collect();
        let mut output = Vec::new();
        print_rules(&mut output, "SinglePos", &rules, &names, None).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "# 3 SinglePos rules\n# lookupflag LookupFlag(8)\nA 7\n# lookupflag LookupFlag(0)\nA 15\nB -15\n"
        );
    }

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
        let lookup_list = write_fonts::read::tables::gpos::OriginalPositionLookupList::read(
            lookup_list.as_slice().into(),
        )
        .unwrap();

        let rules = get_lookup_rules(&lookup_list.into(), None);

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
        let lookup_list = write_fonts::read::tables::gpos::OriginalPositionLookupList::read(
            lookup_list.as_slice().into(),
        )
        .unwrap();

        let rules = get_lookup_rules(&lookup_list.into(), None);

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
            vec![sub1.into()],
        ));
        let lookup2 = wgpos::PositionLookup::MarkToBase(wlayout::Lookup::new(
            LookupFlag::empty(),
            vec![sub2.into()],
        ));

        let lookup_list = wlayout::LookupList::new(vec![lookup1, lookup2]);
        let lookup_list = write_fonts::dump_table(&lookup_list).unwrap();
        let lookup_list = write_fonts::read::tables::gpos::OriginalPositionLookupList::read(
            lookup_list.as_slice().into(),
        )
        .unwrap();

        let rules = get_lookup_rules(&lookup_list.into(), None);

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
