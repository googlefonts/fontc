use std::{
    any::Any,
    collections::BTreeMap,
    fmt::{Debug, Display},
    io,
};

use write_fonts::read::{
    tables::{
        gpos::{AnchorTable, ExtensionSubtable, PositionLookup, PositionLookupList, ValueRecord},
        layout::{DeviceOrVariationIndex, LookupFlag},
    },
    FontData, FontRef, ReadError, TableProvider,
};

use crate::{
    common::{self, GlyphSet},
    error::Error,
    glyph_names::NameMap,
    variations::DeltaComputer,
};

use self::{marks::MarkAttachmentRule, pairpos::PairPosRule};

mod marks;
mod pairpos;

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
                let rules = pairpos::get_pairpos_rules(&subs, flag, delta_computer).unwrap();
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
                let rules = marks::get_mark_base_rules(&subs, flag, mark_filter_id, delta_computer)
                    .unwrap();
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
                let rules = marks::get_mark_mark_rules(&subs, flag, mark_filter_id, delta_computer)
                    .unwrap();
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
                        marks::get_mark_base_rules(&subs, flag, mark_filter_id, delta_computer)
                            .unwrap()
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
                        marks::get_mark_mark_rules(&subs, flag, mark_filter_id, delta_computer)
                            .unwrap()
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
                        pairpos::get_pairpos_rules(&subs, flag, delta_computer).unwrap()
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
