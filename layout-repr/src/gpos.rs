use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use indexmap::IndexMap;
use read_fonts::{
    tables::{
        gpos::{
            PairPos, PairPosFormat1, PairPosFormat2, PositionLookup, PositionLookupList,
            ValueRecord,
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
        let gpos_rules = lookup_rules.hmm_temp_fn_to_get_just_pairpos(&sys.lookups);
        if gpos_rules.is_empty() {
            continue;
        }
        println!();
        println!("# {}: {}/{} #", sys.feature, sys.script, sys.lang);
        println!("# {} pair positioning rules", gpos_rules.len());
        let mut last_flag = None;

        // later on we will figure out a better way to do this
        for rule in &gpos_rules {
            if last_flag != Some(rule.flags) {
                println!("# lookupflag {:?}", rule.flags);
                last_flag = Some(rule.flags);
            }
            let g1 = names.get(rule.first);
            let g2 = rule.second.printer(names);
            let v1 = &rule.record1;
            let v2 = &rule.record2;
            print!("{g1} {v1} {g2}");
            if !rule.record2.is_zero() {
                println!(" {v2}");
            } else {
                println!();
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

// only kerning, for now?
// needs to store:
// - the lookup id, so we can find a thing
// - the lookup flag, so we can show it
// - all the rules for each lookup, in some format
//
struct LookupRules {
    // decomposed rules for each lookup, in lookup order
    rules: Vec<Vec<LookupRule>>,
}

#[derive(Clone, Debug)]
enum LookupRule {
    PairPos(PairPosRule),
    SomethingElse,
}

impl LookupRules {
    fn hmm_temp_fn_to_get_just_pairpos<'a>(&'a self, lookups: &[u16]) -> Vec<&'a PairPosRule> {
        let mut result = Vec::new();
        for lookup in lookups {
            for rule in &self.rules[*lookup as usize] {
                if let LookupRule::PairPos(rule) = rule {
                    result.push(rule);
                }
            }
        }
        result.sort_unstable();
        result
    }
}

#[derive(Clone)]
struct PairPosRule {
    first: GlyphId,
    second: GlyphSet,
    record1: ResolvedValueRecord,
    record2: ResolvedValueRecord,
    flags: LookupFlag,
}

impl PartialEq for PairPosRule {
    fn eq(&self, other: &Self) -> bool {
        self.first == other.first
            && self.second == other.second
            && self.record1 == other.record1
            && self.record2 == other.record2
    }
}

impl Eq for PairPosRule {}

impl Ord for PairPosRule {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.first, &self.second).cmp(&(other.first, &other.second))
    }
}

impl PartialOrd for PairPosRule {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
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
    for (_i, lookup) in lookups.lookups().iter().enumerate() {
        let mut rules = Vec::new();
        let lookup = lookup.unwrap();
        match lookup {
            PositionLookup::Pair(lookup) => {
                let flag = lookup.lookup_flag();
                for subt in lookup.subtables().iter() {
                    let subt = subt.unwrap();
                    match subt {
                        PairPos::Format1(subt) => get_pairpos_f1_rules(
                            &subt,
                            |rule| rules.push(rule),
                            flag,
                            delta_computer,
                        ),
                        PairPos::Format2(subt) => get_pairpos_f2_rules(
                            &subt,
                            |rule| rules.push(rule),
                            flag,
                            delta_computer,
                        ),
                    }
                }
            }
            _ => (),
        }
        result.push(combine_rules(rules));
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
