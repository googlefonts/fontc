use std::{
    fmt::{Debug, Display},
    io,
};

use write_fonts::read::{
    tables::{
        gdef::MarkGlyphSets,
        gpos::{AnchorTable, ExtensionSubtable, PositionLookup, PositionLookupList, ValueRecord},
        layout::{DeviceOrVariationIndex, LookupFlag},
    },
    FontData, FontRef, ReadError, TableProvider,
};

use crate::{
    common::{self, GlyphSet, Lookup, PrintNames, SingleRule},
    error::Error,
    glyph_names::NameMap,
    variations::DeltaComputer,
};

mod marks;
mod pairpos;

#[cfg(test)]
mod test_helpers;

use self::{marks::MarkAttachmentRule, pairpos::PairPosRule};

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
        let pairpos = lookup_rules.pairpos_rules(&sys.lookups);
        let markmark = lookup_rules.markmark_rules(&sys.lookups);
        let markbase = lookup_rules.markbase_rules(&sys.lookups);

        print_rules(f, "PairPos", &pairpos, names, mark_glyph_sets.as_ref())?;
        print_rules(f, "MarkToBase", &markbase, names, mark_glyph_sets.as_ref())?;
        print_rules(f, "MarkToMark", &markmark, names, mark_glyph_sets.as_ref())?;
    }

    Ok(())
}

fn print_rules<T: PrintNames>(
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

#[derive(Clone, Debug, Default)]
struct LookupRules {
    pairpos: Vec<Lookup<PairPosRule>>,
    markbase: Vec<Lookup<MarkAttachmentRule>>,
    markmark: Vec<Lookup<MarkAttachmentRule>>,
    // decomposed rules for each lookup, in lookup order
}

impl LookupRules {
    fn pairpos_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, PairPosRule>> {
        //TODO: in here we will do the normalizing of items that appear in multiple lookups
        let mut all_rules = self
            .pairpos
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
            .flat_map(|lookup| lookup.iter())
            .collect::<Vec<_>>();
        all_rules.sort_unstable();
        all_rules
    }

    fn markbase_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, MarkAttachmentRule>> {
        //TODO: in here we will do the normalizing of items that appear in multiple lookups
        let mut all_rules = self
            .markbase
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
            .flat_map(|lookup| lookup.iter())
            .collect::<Vec<_>>();
        all_rules.sort_unstable();
        all_rules
    }

    fn markmark_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, MarkAttachmentRule>> {
        //TODO: in here we will do the normalizing of items that appear in multiple lookups
        let mut all_rules = self
            .markmark
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
            .flat_map(|lookup| lookup.iter())
            .collect::<Vec<_>>();
        all_rules.sort_unstable();
        all_rules
    }
}

fn get_lookup_rules(
    lookups: &PositionLookupList,
    delta_computer: Option<&DeltaComputer>,
) -> LookupRules {
    let mut result = LookupRules::default();
    for (id, lookup) in lookups.lookups().iter().enumerate() {
        let lookup = lookup.unwrap();
        let (flag, mark_filter_id) = get_flags_and_filter_set_id(&lookup);
        match lookup {
            PositionLookup::Pair(lookup) => {
                let subs = lookup
                    .subtables()
                    .iter()
                    .flat_map(|sub| sub.ok())
                    .collect::<Vec<_>>();
                let rules = pairpos::get_pairpos_rules(&subs, delta_computer).unwrap();
                result.pairpos.push(Lookup::new(id, rules, flag, None));
            }
            PositionLookup::MarkToBase(lookup) => {
                let subs: Vec<_> = lookup
                    .subtables()
                    .iter()
                    .flat_map(|subt| subt.ok())
                    .collect();
                let rules = marks::get_mark_base_rules(&subs, delta_computer).unwrap();
                result
                    .markbase
                    .push(Lookup::new(id, rules, flag, mark_filter_id));
            }
            PositionLookup::MarkToMark(lookup) => {
                let subs: Vec<_> = lookup
                    .subtables()
                    .iter()
                    .flat_map(|subt| subt.ok())
                    .collect();
                let rules = marks::get_mark_mark_rules(&subs, delta_computer).unwrap();
                result
                    .markmark
                    .push(Lookup::new(id, rules, flag, mark_filter_id));
            }
            PositionLookup::Extension(lookup) => {
                let first_sub = lookup.subtables().get(0).ok();
                match first_sub {
                    Some(ExtensionSubtable::MarkToBase(_)) => {
                        let subs = lookup
                            .subtables()
                            .iter()
                            .flat_map(|sub| match sub {
                                Ok(ExtensionSubtable::MarkToBase(inner)) => inner.extension().ok(),
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        let rules = marks::get_mark_base_rules(&subs, delta_computer).unwrap();
                        result
                            .markbase
                            .push(Lookup::new(id, rules, flag, mark_filter_id));
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
                        let rules = marks::get_mark_mark_rules(&subs, delta_computer).unwrap();
                        result
                            .markmark
                            .push(Lookup::new(id, rules, flag, mark_filter_id));
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
                        let rules = pairpos::get_pairpos_rules(&subs, delta_computer).unwrap();
                        result
                            .pairpos
                            .push(Lookup::new(id, rules, flag, mark_filter_id));
                    }
                    _ => (),
                };
            }
            _ => (),
        }
    }
    result
}

fn get_flags_and_filter_set_id(lookup: &PositionLookup) -> (LookupFlag, Option<u16>) {
    let (flag, filter_id) = match lookup {
        PositionLookup::Single(lookup) => (lookup.lookup_flag(), lookup.mark_filtering_set()),
        PositionLookup::Pair(lookup) => (lookup.lookup_flag(), lookup.mark_filtering_set()),
        PositionLookup::Cursive(lookup) => (lookup.lookup_flag(), lookup.mark_filtering_set()),
        PositionLookup::MarkToBase(lookup) => (lookup.lookup_flag(), lookup.mark_filtering_set()),
        PositionLookup::MarkToLig(lookup) => (lookup.lookup_flag(), lookup.mark_filtering_set()),
        PositionLookup::MarkToMark(lookup) => (lookup.lookup_flag(), lookup.mark_filtering_set()),
        PositionLookup::Contextual(lookup) => (lookup.lookup_flag(), lookup.mark_filtering_set()),
        PositionLookup::ChainContextual(lookup) => {
            (lookup.lookup_flag(), lookup.mark_filtering_set())
        }
        PositionLookup::Extension(lookup) => (lookup.lookup_flag(), lookup.mark_filtering_set()),
    };
    (flag, flag.use_mark_filtering_set().then_some(filter_id))
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
