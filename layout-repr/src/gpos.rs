use std::{collections::HashMap, fmt::Display};

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
    common,
    error::Error,
    glyph_names::NameMap,
    variations::{DeltaComputer, Value},
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
    let lookup_rules = get_lookup_rules(&table.lookup_list().unwrap());

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
            let g1 = names.get(rule.gid1);
            let g2 = names.get(rule.gid2);
            let v1 = rule.record1_printer(var_store.as_ref());
            let v2 = rule.record2_printer(var_store.as_ref());
            print!("{g1} {v1} {g2}");
            if !is_noop(&rule.record2) {
                println!(" {v2}");
            } else {
                println!();
            }
        }
    }

    Ok(())
}

// only kerning, for now?
// needs to store:
// - the lookup id, so we can find a thing
// - the lookup flag, so we can show it
// - all the rules for each lookup, in some format
//
struct LookupRules<'a> {
    // decomposed rules for each lookup, in lookup order
    rules: Vec<Vec<LookupRule<'a>>>,
}

struct ValueRecordPrinter<'a, 'b: 'a> {
    record: &'b ValueRecord,
    ivs: Option<&'b DeltaComputer<'a>>,
    data: FontData<'a>,
}

impl ValueRecordPrinter<'_, '_> {
    fn make_value(
        &self,
        default: Option<i16>,
        device: Option<Result<DeviceOrVariationIndex, ReadError>>,
    ) -> Option<Value> {
        let default = default?;
        let variations = match device {
            Some(Ok(DeviceOrVariationIndex::VariationIndex(vi))) => self
                .ivs
                .map(|ivs| ivs.master_values(default as _, vi).unwrap()),
            _ => None,
        };

        Some(Value {
            default,
            variations,
        })
    }

    fn x_placement(&self) -> Option<Value> {
        let default = self.record.x_placement();
        let device = self.record.x_placement_device(self.data);
        self.make_value(default, device)
    }

    fn y_placement(&self) -> Option<Value> {
        let default = self.record.y_placement();
        let device = self.record.y_placement_device(self.data);
        self.make_value(default, device)
    }

    fn x_advance(&self) -> Option<Value> {
        let default = self.record.x_advance();
        let device = self.record.x_advance_device(self.data);
        self.make_value(default, device)
    }

    fn y_advance(&self) -> Option<Value> {
        let default = self.record.y_advance();
        let device = self.record.y_advance_device(self.data);
        self.make_value(default, device)
    }
}

impl Display for ValueRecordPrinter<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let maybe_just_adv = self.record.x_placement.is_none()
            && self.record.y_placement.is_none()
            && self.record.y_advance.is_none();
        if maybe_just_adv {
            if let Some(xadv) = self.x_advance() {
                write!(f, "{}", xadv)
            } else {
                Ok(())
            }
        } else {
            let x_place = self.x_placement().unwrap_or_default();
            let y_place = self.y_placement().unwrap_or_default();
            let x_adv = self.x_advance().unwrap_or_default();
            let y_adv = self.y_advance().unwrap_or_default();
            write!(f, "<{x_place} {y_place} {x_adv} {y_adv}>")
        }
    }
}

impl<'a> LookupRules<'a> {
    fn hmm_temp_fn_to_get_just_pairpos<'b: 'a>(
        &'b self,
        lookups: &[u16],
    ) -> Vec<&'b PairPosRule<'a>> {
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

#[derive(Clone, Debug)]
struct PairPosRule<'a> {
    gid1: GlyphId,
    gid2: GlyphId,
    data: FontData<'a>,
    record1: ValueRecord,
    record2: ValueRecord,
    flags: LookupFlag,
}

impl<'a> PairPosRule<'a> {
    fn record1_printer<'b: 'a>(
        &'b self,
        ivs: Option<&'b DeltaComputer<'a>>,
    ) -> ValueRecordPrinter<'a, 'b> {
        ValueRecordPrinter {
            record: &self.record1,
            ivs,
            data: self.data,
        }
    }

    fn record2_printer<'b: 'a>(
        &'b self,
        ivs: Option<&'b DeltaComputer<'a>>,
    ) -> ValueRecordPrinter<'a, 'b> {
        ValueRecordPrinter {
            record: &self.record2,
            ivs,
            data: self.data,
        }
    }
}

impl PartialEq for PairPosRule<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.gid1 == other.gid1
            && self.gid2 == other.gid2
            && self.record1 == other.record1
            && self.record2 == other.record2
    }
}

impl Eq for PairPosRule<'_> {}

impl Ord for PairPosRule<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.gid1, self.gid2).cmp(&(other.gid1, other.gid2))
    }
}

impl PartialOrd for PairPosRule<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

enum LookupRule<'a> {
    PairPos(PairPosRule<'a>),
    SomethingElse,
}

fn get_lookup_rules<'a>(lookups: &PositionLookupList<'a>) -> LookupRules<'a> {
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
                        PairPos::Format1(subt) => get_pairpos_f1_rules(&subt, &mut rules, flag),
                        PairPos::Format2(subt) => get_pairpos_f2_rules(&subt, &mut rules, flag),
                    }
                }
            }
            _ => (),
        }
        result.push(rules);
    }
    LookupRules { rules: result }
}

// okay so we want to return some heterogeneous type here hmhm
fn get_pairpos_f1_rules<'a>(
    subtable: &PairPosFormat1<'a>,
    rules: &mut Vec<LookupRule<'a>>,
    flags: LookupFlag,
) -> () {
    let coverage = subtable.coverage().unwrap();
    let pairsets = subtable.pair_sets();
    for (gid1, pairset) in coverage.iter().zip(pairsets.iter()) {
        let pairset = pairset.unwrap();
        for pairrec in pairset.pair_value_records().iter() {
            let pairrec = pairrec.unwrap();
            let gid2 = pairrec.second_glyph();
            let data = pairset.offset_data();
            rules.push(LookupRule::PairPos(PairPosRule {
                gid1,
                gid2,
                data,
                record1: pairrec.value_record1,
                record2: pairrec.value_record2,
                flags,
            }))
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
    rules: &mut Vec<LookupRule<'a>>,
    flags: LookupFlag,
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
                rules.push(LookupRule::PairPos(PairPosRule {
                    gid1,
                    gid2,
                    data,
                    record1: record1.clone(),
                    record2: record2.clone(),
                    flags,
                }))
            }
        }
    }
}
