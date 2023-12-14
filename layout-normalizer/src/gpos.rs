use std::{
    any::Any,
    collections::{BTreeMap, HashMap, HashSet},
    fmt::{Debug, Display},
    io,
};

use indexmap::IndexMap;
use write_fonts::read::{
    tables::{
        gpos::{
            AnchorTable, ExtensionSubtable, MarkBasePosFormat1, MarkMarkPosFormat1, PairPos,
            PairPosFormat1, PairPosFormat2, PositionLookup, PositionLookupList, ValueRecord,
        },
        layout::{DeviceOrVariationIndex, LookupFlag},
    },
    types::GlyphId,
    FontData, FontRef, ReadError, TableProvider,
};

use crate::{
    common::{self, GlyphSet},
    error::Error,
    glyph_names::NameMap,
    variations::DeltaComputer,
};

pub(crate) fn print(f: &mut dyn io::Write, font: &FontRef, names: &NameMap) -> Result<(), Error> {
    writeln!(f, "# GPOS #")?;
    let Some(table) = font.gpos().ok() else {
        // no GPOS table, nothing to do
        return Ok(());
    };
    let gdef = font.gdef().ok();
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
        for rule_set in lookup_rules.iter_rule_sets(&sys.lookups) {
            writeln!(
                f,
                "# {} {} rules",
                rule_set.rules.len(),
                rule_set.lookup_type
            )?;
            let mut last_flag = None;
            let mut last_filter_set = None;
            for rule in rule_set.rules {
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
                writeln!(f, "{}", rule_printer(rule, names))?;
            }
        }
    }

    Ok(())
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum DeviceOrDeltas {
    Device {
        start: u16,
        end: u16,
        values: Vec<i8>,
    },
    Deltas(Vec<i32>),
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
        let device_or_deltas = device.transpose()?.map(|device| match device {
            DeviceOrVariationIndex::Device(device) => Ok(DeviceOrDeltas::Device {
                start: device.start_size(),
                end: device.end_size(),
                values: device.iter().collect(),
            }),
            DeviceOrVariationIndex::VariationIndex(idx) => ivs
                .unwrap()
                .master_values(default as _, idx)
                .map(DeviceOrDeltas::Deltas),
        });
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
}

struct LookupRules {
    // decomposed rules for each lookup, in lookup order
    rules: Vec<Vec<LookupRule>>,
}

#[derive(Clone, Debug)]
#[allow(clippy::large_enum_variant)]
enum LookupRule {
    PairPos(PairPosRule),
    MarkBase(MarkAttachmentRule),
    MarkMark(MarkAttachmentRule),
}

// a collection of rules of a single type
struct RuleSet<'a> {
    lookup_type: LookupType,
    rules: Vec<&'a dyn AnyRule>,
}

// a trait we use to type-erase our specific rules while printing
trait AnyRule {
    // The lookup flags plus the mark filter set id
    fn lookup_flags(&self) -> (LookupFlag, Option<u16>);
    fn lookup_type(&self) -> LookupType;
    // write this rule into the provided stream
    //
    // this is passed in a map of gids to names, which are needed for printing
    fn fmt_impl(&self, f: &mut std::fmt::Formatter<'_>, names: &NameMap) -> std::fmt::Result;

    fn as_any(&self) -> &dyn Any;
}

fn rule_printer<'a>(rule: &'a dyn AnyRule, names: &'a NameMap) -> Printer<'a> {
    Printer { rule, names }
}

// this code is annoying.
//
// basically: we have a bunch of different lookups, of different types. But when
// we print the rules, we want to take a bunch of rules from different lookups
// and print them in some canonical way. This means we need to be able to sort
// them after we've collected them, at which point we don't know their actual
// types anymore, so we do this dance. I'm sorry.
impl<'a> Ord for &'a dyn AnyRule {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self.lookup_type(), other.lookup_type()) {
            (LookupType::PairPos, LookupType::PairPos) => self
                .as_any()
                .downcast_ref::<PairPosRule>()
                .unwrap()
                .cmp(other.as_any().downcast_ref::<PairPosRule>().unwrap()),
            (LookupType::MarkToBase, LookupType::MarkToBase)
            | (LookupType::MarkToMark, LookupType::MarkToMark) => self
                .as_any()
                .downcast_ref::<MarkAttachmentRule>()
                .unwrap()
                .cmp(other.as_any().downcast_ref::<MarkAttachmentRule>().unwrap()),
            (self_type, other_type) => {
                assert!(
                    self_type != other_type,
                    "you need to add a new branch in the Ord impl"
                );
                self_type.cmp(&other_type)
            }
        }
    }
}

impl<'a> PartialOrd for &'a dyn AnyRule {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> PartialEq for &'a dyn AnyRule {
    fn eq(&self, other: &Self) -> bool {
        match (self.lookup_type(), other.lookup_type()) {
            (LookupType::PairPos, LookupType::PairPos) => self
                .as_any()
                .downcast_ref::<PairPosRule>()
                .unwrap()
                .eq(other.as_any().downcast_ref::<PairPosRule>().unwrap()),
            (LookupType::MarkToBase, LookupType::MarkToBase)
            | (LookupType::MarkToMark, LookupType::MarkToMark) => self
                .as_any()
                .downcast_ref::<MarkAttachmentRule>()
                .unwrap()
                .eq(other.as_any().downcast_ref::<MarkAttachmentRule>().unwrap()),
            (self_type, other_type) => {
                assert!(
                    self_type != other_type,
                    "you need to add a new branch in the PartialEq impl"
                );
                false
            }
        }
    }
}

impl<'a> Eq for &'a dyn AnyRule {}

struct Printer<'a> {
    rule: &'a dyn AnyRule,
    names: &'a NameMap,
}

impl Display for Printer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.rule.fmt_impl(f, self.names)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum LookupType {
    //SinglePos = 1,
    PairPos = 2,
    MarkToBase,
    MarkToMark,
}

impl LookupRule {
    fn dyn_inner(&self) -> &dyn AnyRule {
        match self {
            LookupRule::PairPos(inner) => inner,
            LookupRule::MarkBase(inner) => inner,
            LookupRule::MarkMark(inner) => inner,
        }
    }
}

impl LookupRules {
    /// returns all the rules in all the referenced lookups
    fn iter_rule_sets<'a>(&'a self, lookups: &[u16]) -> impl Iterator<Item = RuleSet<'a>> {
        let mut by_type = BTreeMap::new();
        for lookup in lookups {
            for rule in &self.rules[*lookup as usize] {
                let as_dyn = rule.dyn_inner();
                let lookup_type = as_dyn.lookup_type();
                by_type
                    .entry(lookup_type)
                    .or_insert_with(|| RuleSet {
                        lookup_type,
                        rules: Vec::new(),
                    })
                    .rules
                    .push(as_dyn);
            }
        }
        by_type.into_values().map(|mut rules| {
            rules.rules.sort_unstable();
            rules
        })
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct PairPosRule {
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

fn get_lookup_rules(
    lookups: &PositionLookupList,
    delta_computer: Option<&DeltaComputer>,
) -> LookupRules {
    let mut result = Vec::new();
    for (_i, lookup) in lookups.lookups().iter().enumerate() {
        let lookup = lookup.unwrap();
        match lookup {
            PositionLookup::Pair(lookup) => {
                let flag = lookup.lookup_flag();
                let subs = lookup
                    .subtables()
                    .iter()
                    .flat_map(|sub| sub.ok())
                    .collect::<Vec<_>>();
                let rules = get_pairpos_rules(&subs, flag, delta_computer).unwrap();
                result.push(rules);
            }
            PositionLookup::MarkToBase(lookup) => {
                let flag = lookup.lookup_flag();
                let mark_filter_id = flag
                    .use_mark_filtering_set()
                    .then(|| lookup.mark_filtering_set());
                let subs: Vec<_> = lookup
                    .subtables()
                    .iter()
                    .flat_map(|subt| subt.ok())
                    .collect();
                let rules =
                    get_mark_base_rules(&subs, flag, mark_filter_id, delta_computer).unwrap();
                result.push(rules);
            }
            PositionLookup::MarkToMark(lookup) => {
                let flag = lookup.lookup_flag();
                let mark_filter_id = flag
                    .use_mark_filtering_set()
                    .then(|| lookup.mark_filtering_set());
                let subs: Vec<_> = lookup
                    .subtables()
                    .iter()
                    .flat_map(|subt| subt.ok())
                    .collect();
                let rules =
                    get_mark_mark_rules(&subs, flag, mark_filter_id, delta_computer).unwrap();
                result.push(rules)
            }
            PositionLookup::Extension(lookup) => {
                let flag = lookup.lookup_flag();
                let mark_filter_id = flag
                    .use_mark_filtering_set()
                    .then(|| lookup.mark_filtering_set());
                let first_sub = lookup.subtables().get(0).ok();
                let rules = match first_sub {
                    Some(ExtensionSubtable::MarkToBase(_)) => {
                        let subs = lookup
                            .subtables()
                            .iter()
                            .flat_map(|sub| match sub {
                                Ok(ExtensionSubtable::MarkToBase(inner)) => inner.extension().ok(),
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        get_mark_base_rules(&subs, flag, mark_filter_id, delta_computer).unwrap()
                    }
                    Some(ExtensionSubtable::MarkToMark(_)) => {
                        let subs = lookup
                            .subtables()
                            .iter()
                            .flat_map(|sub| match sub {
                                Ok(ExtensionSubtable::MarkToMark(inner)) => inner.extension().ok(),
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        get_mark_mark_rules(&subs, flag, mark_filter_id, delta_computer).unwrap()
                    }
                    Some(ExtensionSubtable::Pair(_)) => {
                        let subs = lookup
                            .subtables()
                            .iter()
                            .flat_map(|sub| match sub {
                                Ok(ExtensionSubtable::Pair(inner)) => inner.extension().ok(),
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        get_pairpos_rules(&subs, flag, delta_computer).unwrap()
                    }
                    _ => Vec::new(),
                };
                result.push(rules);
            }
            _other => {
                // we always want to have as many sets as we have lookups
                result.push(Vec::new());
            }
        }
    }
    LookupRules { rules: result }
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

fn get_pairpos_rules(
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct MarkAttachmentRule {
    kind: LookupType,
    flags: LookupFlag,
    base: GlyphId,
    base_anchor: ResolvedAnchor,
    marks: BTreeMap<ResolvedAnchor, GlyphSet>,
    filter_set: Option<u16>,
}

impl AnyRule for MarkAttachmentRule {
    fn lookup_flags(&self) -> (LookupFlag, Option<u16>) {
        (self.flags, self.filter_set)
    }

    fn fmt_impl(&self, f: &mut std::fmt::Formatter<'_>, names: &NameMap) -> std::fmt::Result {
        let base_name = names.get(self.base);
        writeln!(f, "{base_name} {}", self.base_anchor)?;
        for (i, (anchor, glyphs)) in self.marks.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }

            write!(f, "  {anchor} {}", glyphs.printer(names))?;
        }
        Ok(())
    }

    fn lookup_type(&self) -> LookupType {
        self.kind
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

fn get_mark_base_rules(
    subtables: &[MarkBasePosFormat1],
    flags: LookupFlag,
    filter_set: Option<u16>,
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<LookupRule>, ReadError> {
    // so we only take the first coverage hit in each subtable, which means
    // we just need track what we've seen.
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for sub in subtables.iter() {
        append_mark_base_rules(
            sub,
            flags,
            filter_set,
            delta_computer,
            &mut seen,
            &mut result,
        )?;
    }
    Ok(result)
}

// append the rules for a single subtable
fn append_mark_base_rules(
    subtable: &MarkBasePosFormat1,
    flags: LookupFlag,
    filter_set: Option<u16>,
    delta_computer: Option<&DeltaComputer>,
    visited: &mut HashSet<GlyphId>,
    result: &mut Vec<LookupRule>,
) -> Result<(), ReadError> {
    let base_array = subtable.base_array()?;
    let base_records = base_array.base_records();
    let mark_array = subtable.mark_array()?;
    let mark_records = mark_array.mark_records();

    let cov_ix_to_mark_gid: HashMap<_, _> = subtable.mark_coverage()?.iter().enumerate().collect();

    for (base_ix, base_glyph) in subtable.base_coverage()?.iter().enumerate() {
        if !visited.insert(base_glyph) {
            // this was included in a previous subtable, so skip it
            continue;
        }

        let base_record = base_records.get(base_ix)?;
        for (base_anchor_ix, base_anchor) in base_record
            .base_anchors(base_array.offset_data())
            .iter()
            .enumerate()
        {
            let Some(base_anchor) = base_anchor else {
                continue;
            };
            let base_anchor = base_anchor?;
            let base_anchor = ResolvedAnchor::new(&base_anchor, delta_computer)?;
            let mut marks = BTreeMap::default();
            for (mark_ix, mark_record) in mark_records.iter().enumerate() {
                let mark_class = mark_record.mark_class() as usize;
                if mark_class != base_anchor_ix {
                    continue;
                }
                let Some(mark_glyph) = cov_ix_to_mark_gid.get(&mark_ix) else {
                    continue;
                };
                let mark_anchor = mark_record.mark_anchor(mark_array.offset_data())?;
                let mark_anchor = ResolvedAnchor::new(&mark_anchor, delta_computer)?;
                marks
                    .entry(mark_anchor)
                    .or_insert_with(|| GlyphSet::from(*mark_glyph))
                    .add(*mark_glyph);
            }
            let group = MarkAttachmentRule {
                flags,
                base: base_glyph,
                base_anchor,
                marks: marks
                    .into_iter()
                    .map(|(anchor, glyphs)| (anchor, glyphs))
                    .collect(),
                kind: LookupType::MarkToBase,
                filter_set,
            };
            result.push(LookupRule::MarkBase(group));
        }
    }
    Ok(())
}

fn get_mark_mark_rules(
    subtables: &[MarkMarkPosFormat1],
    flags: LookupFlag,
    filter_set: Option<u16>,
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<LookupRule>, ReadError> {
    // so we only take the first coverage hit in each subtable, which means
    // we just need track what we've seen.
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for sub in subtables.iter() {
        append_mark_mark_rules(
            sub,
            flags,
            filter_set,
            delta_computer,
            &mut seen,
            &mut result,
        )?;
    }
    Ok(result)
}

fn append_mark_mark_rules(
    subtable: &MarkMarkPosFormat1,
    flags: LookupFlag,
    filter_set: Option<u16>,
    delta_computer: Option<&DeltaComputer>,
    seen: &mut HashSet<GlyphId>,
    result: &mut Vec<LookupRule>,
) -> Result<(), ReadError> {
    let base_array = subtable.mark2_array()?;
    let base_records = base_array.mark2_records();
    let mark_array = subtable.mark1_array()?;
    let mark_records = mark_array.mark_records();

    let cov_ix_to_mark_gid: HashMap<_, _> = subtable.mark1_coverage()?.iter().enumerate().collect();

    for (base_ix, base_glyph) in subtable.mark2_coverage()?.iter().enumerate() {
        if !seen.insert(base_glyph) {
            continue;
        }
        let base_record = base_records.get(base_ix).unwrap();
        for (base_anchor_ix, base_anchor) in base_record
            .mark2_anchors(base_array.offset_data())
            .iter()
            .enumerate()
        {
            let Some(base_anchor) = base_anchor else {
                continue;
            };
            let base_anchor = base_anchor?;
            let base_anchor = ResolvedAnchor::new(&base_anchor, delta_computer)?;
            let mut marks = BTreeMap::default();
            for (mark_ix, mark_record) in mark_records.iter().enumerate() {
                let mark_class = mark_record.mark_class() as usize;
                if mark_class != base_anchor_ix {
                    continue;
                }
                let Some(mark_glyph) = cov_ix_to_mark_gid.get(&mark_ix) else {
                    continue;
                };
                let mark_anchor = mark_record.mark_anchor(mark_array.offset_data())?;
                let mark_anchor = ResolvedAnchor::new(&mark_anchor, delta_computer)?;
                marks
                    .entry(mark_anchor)
                    .or_insert_with(|| GlyphSet::from(*mark_glyph))
                    .add(*mark_glyph);
            }
            let group = MarkAttachmentRule {
                flags,
                base: base_glyph,
                base_anchor,
                marks: marks
                    .into_iter()
                    .map(|(anchor, glyphs)| (anchor, glyphs))
                    .collect(),
                kind: LookupType::MarkToMark,
                filter_set,
            };
            result.push(LookupRule::MarkMark(group));
        }
    }
    Ok(())
}

impl From<i16> for ResolvedValue {
    fn from(src: i16) -> ResolvedValue {
        ResolvedValue {
            default: src,
            device_or_deltas: None,
        }
    }
}

impl Display for LookupType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            LookupType::PairPos => "PairPos",
            LookupType::MarkToBase => "MarkToBase",
            LookupType::MarkToMark => "MarkToMark",
        };
        f.write_str(name)
    }
}

impl Display for ResolvedValueRecord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(xadv) = self.maybe_just_adv() {
            if !xadv.is_zero() {
                write!(f, "{}", xadv)
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
        match &self.device_or_deltas {
            Some(DeviceOrDeltas::Device { start, end, values }) => {
                write!(f, " [({start})")?;
                for (i, adj) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{adj}")?;
                }
                write!(f, "({end})]")?;
            }
            Some(DeviceOrDeltas::Deltas(deltas)) => {
                write!(f, " {{")?;
                for (i, var) in deltas.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{var}")?;
                }
                write!(f, "}}")?;
            }
            None => (),
        }
        Ok(())
    }
}

impl Display for ResolvedAnchor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@(x: {}, y: {})", self.x, self.y)
    }
}
