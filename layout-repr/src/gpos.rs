use std::{
    any::Any,
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::{Debug, Display},
};

use indexmap::IndexMap;
use read_fonts::{
    tables::{
        gpos::{
            AnchorTable, MarkBasePosFormat1, PairPos, PairPosFormat1, PairPosFormat2,
            PositionLookup, PositionLookupList, ValueRecord,
        },
        layout::{DeviceOrVariationIndex, LookupFlag},
    },
    types::{GlyphId, Tag},
    FontData, FontRef, ReadError, TableProvider,
};

use crate::{
    common::{self, GlyphSet},
    error::Error,
    glyph_names::NameMap,
    variations::DeltaComputer,
};

pub(crate) fn print(font: &FontRef, names: &NameMap) -> Result<(), Error> {
    println!("# GPOS #");
    let table = font
        .gpos()
        .map_err(|_| Error::MissingTable(Tag::new(b"GPOS")))?;
    let var_store = font
        .gdef()
        .ok()
        .and_then(|gdef| gdef.item_var_store())
        .map(|ivs| ivs.and_then(DeltaComputer::new))
        .transpose()
        .unwrap();

    let script_list = table.script_list().unwrap();
    let feature_list = table.feature_list().unwrap();
    let lang_systems = common::get_lang_systems(&script_list, &feature_list);
    let lookup_rules = get_lookup_rules(&table.lookup_list().unwrap(), var_store.as_ref());

    // so we want to iterate the rules sorted by:
    // - rule type
    // - then just... the ordering used by that rule?
    for sys in &lang_systems {
        println!();
        println!("# {}: {}/{} #", sys.feature, sys.script, sys.lang);

        for rule_set in lookup_rules.iter_rule_sets(&sys.lookups) {
            println!("# {} {} rules", rule_set.rules.len(), rule_set.lookup_type);
            let mut last_flag = None;
            for rule in rule_set.rules {
                let flags = rule.lookup_flags();
                if last_flag != Some(flags) {
                    println!("# lookupflag {flags:?}");
                    last_flag = Some(flags);
                }
                println!("{}", rule_printer(rule, names));
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
enum LookupRule {
    PairPos(PairPosRule),
    MarkBase(MarkAttachmentRule),
}

// a collection of rules of a single type
struct RuleSet<'a> {
    lookup_type: LookupType,
    rules: Vec<&'a dyn AnyRule>,
}

// a trait we use to type-erase our specific rules while printing
trait AnyRule {
    fn lookup_flags(&self) -> LookupFlag;
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

impl<'a> Ord for &'a dyn AnyRule {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self.lookup_type(), other.lookup_type()) {
            (LookupType::PairPos, LookupType::PairPos) => self
                .as_any()
                .downcast_ref::<PairPosRule>()
                .unwrap()
                .cmp(other.as_any().downcast_ref::<PairPosRule>().unwrap()),
            (LookupType::MarkToBase, LookupType::MarkToBase) => self
                .as_any()
                .downcast_ref::<MarkAttachmentRule>()
                .unwrap()
                .cmp(other.as_any().downcast_ref::<MarkAttachmentRule>().unwrap()),
            (self_type, other_type) => self_type.cmp(&other_type),
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
            (LookupType::MarkToBase, LookupType::MarkToBase) => self
                .as_any()
                .downcast_ref::<MarkAttachmentRule>()
                .unwrap()
                .eq(other.as_any().downcast_ref::<MarkAttachmentRule>().unwrap()),
            _ => false,
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
    //MarkToMark,
    //MarkToLig,
}

impl LookupRule {
    fn dyn_inner(&self) -> &dyn AnyRule {
        match self {
            LookupRule::PairPos(inner) => inner,
            LookupRule::MarkBase(inner) => inner,
        }
    }
}

impl LookupRules {
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
    fn lookup_flags(&self) -> LookupFlag {
        self.flags
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

fn get_lookup_rules<'a>(
    lookups: &PositionLookupList<'a>,
    delta_computer: Option<&DeltaComputer>,
) -> LookupRules {
    let mut result = Vec::new();
    //let mut pairpos = Vec::new();
    for (_i, lookup) in lookups.lookups().iter().enumerate() {
        let mut pairpos = Vec::new();
        let lookup = lookup.unwrap();
        match lookup {
            PositionLookup::Pair(lookup) => {
                let flag = lookup.lookup_flag();
                for subt in lookup.subtables().iter() {
                    let subt = subt.unwrap();
                    match subt {
                        PairPos::Format1(subt) => get_pairpos_f1_rules(
                            &subt,
                            |rule| pairpos.push(rule),
                            flag,
                            delta_computer,
                        ),
                        PairPos::Format2(subt) => get_pairpos_f2_rules(
                            &subt,
                            |rule| pairpos.push(rule),
                            flag,
                            delta_computer,
                        ),
                    }
                }
                result.push(combine_rules(pairpos));
            }
            PositionLookup::MarkToBase(lookup) => {
                let flag = lookup.lookup_flag();
                for subt in lookup.subtables().iter().flat_map(|subt| subt.ok()) {
                    let rules = get_mark_base_rules(&subt, flag, delta_computer).unwrap();
                    result.push(rules);
                }
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

// okay so we want to return some heterogeneous type here hmhm
fn get_pairpos_f1_rules<'a>(
    subtable: &PairPosFormat1<'a>,
    mut add_fn: impl FnMut(PairPosRule),
    flags: LookupFlag,
    delta_computer: Option<&DeltaComputer>,
) -> () {
    let coverage = subtable.coverage().unwrap();
    let pairsets = subtable.pair_sets();
    for (gid1, pairset) in coverage.iter().zip(pairsets.iter()) {
        let pairset = pairset.unwrap();
        for pairrec in pairset.pair_value_records().iter() {
            let pairrec = pairrec.unwrap();
            let gid2 = pairrec.second_glyph();
            let data = pairset.offset_data();
            let record1 =
                ResolvedValueRecord::new(pairrec.value_record1, data, delta_computer).unwrap();
            let record2 =
                ResolvedValueRecord::new(pairrec.value_record2, data, delta_computer).unwrap();
            add_fn(PairPosRule {
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

fn get_pairpos_f2_rules<'a>(
    subtable: &PairPosFormat2<'a>,
    mut add_fn: impl FnMut(PairPosRule),
    flags: LookupFlag,
    delta_computer: Option<&DeltaComputer>,
) -> () {
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
            if is_noop(&record1) && is_noop(&record2) {
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

                add_fn(PairPosRule {
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
    flags: LookupFlag,
    base: GlyphId,
    base_anchor: ResolvedAnchor,
    marks: BTreeMap<ResolvedAnchor, GlyphSet>,
}

impl AnyRule for MarkAttachmentRule {
    fn lookup_flags(&self) -> LookupFlag {
        self.flags
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
        LookupType::MarkToBase
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

fn get_mark_base_rules(
    subtable: &MarkBasePosFormat1,
    flags: LookupFlag,
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<LookupRule>, ReadError> {
    let base_array = subtable.base_array()?;
    let base_records = base_array.base_records();
    let mark_array = subtable.mark_array()?;
    let mark_records = mark_array.mark_records();

    let cov_ix_to_mark_gid: HashMap<_, _> = subtable.mark_coverage()?.iter().enumerate().collect();
    let mut result = Vec::new();

    for (base_ix, base_glyph) in subtable.base_coverage()?.iter().enumerate() {
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
                    .or_insert(BTreeSet::new())
                    .insert(*mark_glyph);
            }
            let group = MarkAttachmentRule {
                flags,
                base: base_glyph,
                base_anchor,
                marks: marks
                    .into_iter()
                    .map(|(anchor, glyphs)| (anchor, glyphs.into()))
                    .collect(),
            };
            result.push(LookupRule::MarkBase(group));
        }
    }
    Ok(result)
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
            //LookupType::SinglePos => "SinglePos",
            LookupType::PairPos => "PairPos",
            LookupType::MarkToBase => "MarkToBase",
            //LookupType::MarkToMark => "MarkToMark",
            //LookupType::MarkToLig => "MarkToLig",
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
                        write!(f, ", ")?;
                    }
                    write!(f, "{adj}")?;
                }
                write!(f, "({end})]")?;
            }
            Some(DeviceOrDeltas::Deltas(deltas)) => {
                write!(f, " {{")?;
                for (i, var) in deltas.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
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
        write!(f, "@({},{})", self.x, self.y)
    }
}
