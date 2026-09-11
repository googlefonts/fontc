//! Contextual lookup builders

use std::{
    collections::{BTreeMap, HashMap},
    convert::TryInto,
};

use write_fonts::{
    FontWrite,
    tables::{
        gpos::builders::ValueRecordBuilder as ValueRecord,
        gsub::{self as write_gsub, ReverseChainSingleSubstFormat1},
        layout::{
            self as write_layout, LookupFlag,
            builders::{ClassDefBuilder, CoverageTableBuilder},
        },
        variations::ivs_builder::VariationStoreBuilder,
    },
    types::GlyphId16,
    validate::Validate,
};

use crate::common::GlyphOrClass;

use super::{
    Builder, FilterSetId, LookupBuilder, LookupId, PositionLookup, RemapIds, SubstitutionLookup,
};

/// When building a contextual/chaining contextual rule, we also build a
/// bunch of anonymous lookups.
#[derive(Debug, Clone)]
pub(crate) struct ContextualLookupBuilder<T> {
    pub(super) flags: LookupFlag,
    pub(super) mark_set: Option<FilterSetId>,
    subtables: Vec<ContextBuilder>,
    /// anonymous lookups that are not modifiable
    ///
    /// When we have seen an explicit 'subtable' statement we need to ensure that
    /// any inline rules after that statement do not get added to anonymous
    /// lookups that were created before it, so we move them here.
    finished_anon_lookups: Vec<T>,
    /// anonymous lookups that we are allowed to add rules to
    current_anon_lookups: Vec<T>,
    pub(super) root_id: LookupId,
}

// while building we use a common representation, but when compiling we will
// use normal contextual lookups where possible.
pub(crate) enum ChainOrNot {
    Context(LookupBuilder<ContextBuilder>),
    Chain(LookupBuilder<ChainContextBuilder>),
}

// a little helper trait for generating the right kind of LookupId
trait MakeLookupId {
    fn make_id(raw: usize) -> LookupId;
}

impl MakeLookupId for PositionLookup {
    fn make_id(raw: usize) -> LookupId {
        LookupId::Gpos(raw)
    }
}

impl MakeLookupId for SubstitutionLookup {
    fn make_id(raw: usize) -> LookupId {
        LookupId::Gsub(raw)
    }
}

impl<T> ContextualLookupBuilder<T> {
    pub(crate) fn new(flags: LookupFlag, mark_set: Option<FilterSetId>) -> Self {
        ContextualLookupBuilder {
            flags,
            mark_set,
            subtables: vec![Default::default()],
            root_id: LookupId::Empty,
            finished_anon_lookups: Default::default(),
            current_anon_lookups: Default::default(),
        }
    }

    pub(crate) fn into_lookups(self) -> (ChainOrNot, Vec<T>) {
        let ContextualLookupBuilder {
            flags,
            mark_set,
            subtables,
            mut finished_anon_lookups,
            current_anon_lookups,
            ..
        } = self;
        finished_anon_lookups.extend(current_anon_lookups);
        let lookup = if subtables.iter().any(ContextBuilder::is_chain_rule) {
            ChainOrNot::Chain(LookupBuilder::new_with_lookups(
                flags,
                mark_set,
                subtables.into_iter().map(ChainContextBuilder).collect(),
            ))
        } else {
            ChainOrNot::Context(LookupBuilder::new_with_lookups(flags, mark_set, subtables))
        };
        (lookup, finished_anon_lookups)
    }

    /// Returns a mutable reference to the active builder
    pub fn last_mut(&mut self) -> &mut ContextBuilder {
        self.subtables.last_mut().unwrap()
    }

    /// Force a new subtable (caused by the explicit 'subtable' statement)
    ///
    /// For contextual lookups, a subtable break does two things: it ensures
    /// that any subsequent contextual rules go into a new subtable (like with
    /// other tables) but it also forces any new inline rules to go into new
    /// lookups (e.g. lookups cannot be shared across a 'subtable break' boundary.)
    pub fn force_subtable_break(&mut self) {
        self.subtables.push(Default::default());
        self.finished_anon_lookups
            .append(&mut self.current_anon_lookups);
    }

    /// Find or create an anonymous lookup that meets a given condition
    ///
    /// This is a helper called by various methods that add anonymous lookup
    /// rules.
    ///
    /// The `can_use_lookup` argument is a closure that should return `true` if a
    /// given lookup can be used by the caller; if no suitable lookup is found,
    /// then `make_lookup` will be called to create one.
    #[must_use]
    fn find_or_create_anon_lookup(
        &mut self,
        can_use_lookup: impl Fn(&T) -> bool,
        make_lookup: impl FnOnce(LookupFlag, Option<FilterSetId>) -> T,
    ) -> (&mut T, LookupId)
    where
        T: MakeLookupId,
    {
        let idx = match self.current_anon_lookups.iter().position(can_use_lookup) {
            Some(idx) => idx,
            None => {
                let lookup = make_lookup(self.flags, self.mark_set);
                self.current_anon_lookups.push(lookup);
                self.current_anon_lookups.len() - 1
            }
        };
        let raw_id = self.root_id.to_raw() + self.finished_anon_lookups.len() + idx + 1;
        let lookup = self.current_anon_lookups.get_mut(idx).unwrap();
        (lookup, T::make_id(raw_id))
    }
}

impl ContextualLookupBuilder<PositionLookup> {
    pub(crate) fn add_anon_gpos_type_1(
        &mut self,
        glyphs: &GlyphOrClass,
        value: ValueRecord,
    ) -> LookupId {
        let (lookup, id) = self.find_or_create_anon_lookup(
            |existing| match existing {
                PositionLookup::Single(lookup) => lookup
                    .subtables
                    .iter()
                    .all(|subt| glyphs.iter().all(|gid| subt.can_add(gid, &value))),
                _ => false,
            },
            |flags, mark_set| PositionLookup::Single(super::LookupBuilder::new(flags, mark_set)),
        );
        let PositionLookup::Single(lookup) = lookup else {
            panic!("this shouldn't happen");
        };

        let sub = lookup.last_mut().unwrap();
        for id in glyphs.iter() {
            sub.insert(id, value.clone());
        }
        id
    }
}

impl ContextualLookupBuilder<SubstitutionLookup> {
    pub(crate) fn add_anon_gsub_type_1(
        &mut self,
        target: GlyphOrClass,
        replacement: GlyphOrClass,
    ) -> LookupId {
        let (lookup, id) = self.find_or_create_anon_lookup(
            |existing| match existing {
                SubstitutionLookup::Single(subtables) => subtables.subtables.iter().all(|subt| {
                    target
                        .iter()
                        .zip(replacement.iter())
                        .all(|(a, b)| subt.can_add(a, b))
                }),
                _ => false,
            },
            |flags, mark_set| SubstitutionLookup::Single(LookupBuilder::new(flags, mark_set)),
        );

        let SubstitutionLookup::Single(subtables) = lookup else {
            unreachable!("per logic above we only return this variant");
        };
        let sub = subtables.last_mut().unwrap();
        for (target, replacement) in target.iter().zip(replacement.into_iter_for_target()) {
            sub.insert(target, replacement);
        }
        id
    }

    pub(crate) fn add_anon_gsub_type_2(
        &mut self,
        target: GlyphId16,
        replacements: Vec<GlyphId16>,
    ) -> LookupId {
        let (lookup, id) = self.find_or_create_anon_lookup(
            |existing| match existing {
                SubstitutionLookup::Multiple(subtables) => subtables
                    .subtables
                    .iter()
                    .all(|subt| subt.can_add(target, &replacements)),
                _ => false,
            },
            |flags, mark_set| SubstitutionLookup::Multiple(LookupBuilder::new(flags, mark_set)),
        );

        let SubstitutionLookup::Multiple(subtables) = lookup else {
            unreachable!("per logic above we only return this variant");
        };
        let sub = subtables.last_mut().unwrap();
        sub.insert(target, replacements);
        id
    }

    pub(crate) fn add_anon_gsub_type_4(
        &mut self,
        target: Vec<GlyphId16>,
        replacement: GlyphId16,
    ) -> LookupId {
        let (lookup, id) = self.find_or_create_anon_lookup(
            |existing| match existing {
                SubstitutionLookup::Ligature(builder) => builder
                    .subtables
                    .iter()
                    .all(|sub| sub.can_add(&target, replacement)),
                _ => false,
            },
            |flags, mark_set| SubstitutionLookup::Ligature(LookupBuilder::new(flags, mark_set)),
        );

        let SubstitutionLookup::Ligature(subtables) = lookup else {
            unreachable!("per logic above we only return this variant");
        };

        let sub = subtables.last_mut().unwrap();
        sub.insert(target, replacement);
        id
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct ContextBuilder {
    rules: Vec<ContextRule>,
}

// we use separate types here to ensure we don't mix lookups
#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct PosContextBuilder(ContextBuilder);

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct SubContextBuilder(ContextBuilder);

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct ReverseChainBuilder {
    rules: Vec<ReverseSubRule>,
}

#[derive(Clone, Debug, PartialEq)]
struct ReverseSubRule {
    backtrack: Vec<GlyphOrClass>,
    context: BTreeMap<GlyphId16, GlyphId16>,
    lookahead: Vec<GlyphOrClass>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct ChainContextBuilder(ContextBuilder);

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct PosChainContextBuilder(ChainContextBuilder);

#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct SubChainContextBuilder(ChainContextBuilder);

#[derive(Clone, Debug, PartialEq)]
struct ContextRule {
    backtrack: Vec<GlyphOrClass>,
    context: Vec<(GlyphOrClass, Vec<LookupId>)>,
    lookahead: Vec<GlyphOrClass>,
}

impl ContextBuilder {
    pub fn add(
        &mut self,
        backtrack: Vec<GlyphOrClass>,
        context: Vec<(GlyphOrClass, Vec<LookupId>)>,
        lookahead: Vec<GlyphOrClass>,
    ) {
        let rule = ContextRule {
            backtrack,
            context,
            lookahead,
        };
        if self.rules.last_mut().map(|last| last.try_merge(&rule)) != Some(true) {
            self.rules.push(rule);
        }
    }

    // for adjusting ids if we insert aalt at the front
    pub(crate) fn bump_all_lookup_ids(&mut self, from: usize, by: usize) {
        self.rules
            .iter_mut()
            .for_each(|rule| rule.bump_all_lookup_ids(from, by))
    }

    /// Iterate all referenced lookups
    fn iter_lookups(&self) -> impl Iterator<Item = LookupId> + '_ {
        self.rules
            .iter()
            .flat_map(|x| x.context.iter().flat_map(|(_, id)| id.iter()))
            .copied()
    }

    fn is_chain_rule(&self) -> bool {
        self.rules.iter().any(ContextRule::is_chain_rule)
    }

    fn has_non_singleton_glyph_classes(&self) -> bool {
        self.rules
            .iter()
            .any(ContextRule::has_non_singleton_glyph_classes)
    }

    /// If the input sequence can be represented as a class def, return it
    fn input_class_def(&self) -> Option<ClassDefBuilder> {
        let mut builder = ClassDefBuilder::new();
        for class in self
            .rules
            .iter()
            .flat_map(|rule| rule.context.iter().map(|x| &x.0))
        {
            if !builder.checked_add(class.to_class().unwrap().into()) {
                return None;
            }
        }
        Some(builder)
    }

    fn format_1_coverage(&self) -> Option<CoverageTableBuilder> {
        if self.has_non_singleton_glyph_classes() {
            return None;
        }
        Some(
            self.rules
                .iter()
                .map(|rule| rule.first_input_sequence_item().single_glyph().unwrap())
                .collect::<CoverageTableBuilder>(),
        )
    }

    fn build_format_1(&self, in_gpos: bool) -> Option<write_layout::SequenceContext> {
        let coverage = self.format_1_coverage()?.build();
        let mut rule_sets = HashMap::<_, Vec<_>>::new();
        for rule in &self.rules {
            let key = rule.first_input_sequence_item().single_glyph().unwrap();
            let seq_lookups = rule.lookup_records(in_gpos);
            let rule = write_layout::SequenceRule::new(
                rule.context
                    .iter()
                    .skip(1)
                    .map(|(cls, _)| cls.to_glyph().unwrap())
                    .collect(),
                seq_lookups,
            );

            rule_sets.entry(key).or_default().push(rule);
        }
        let rule_sets = coverage
            .iter()
            .map(|gid| {
                Some(write_layout::SequenceRuleSet::new(
                    rule_sets.remove(&gid).unwrap(),
                ))
            })
            .collect();

        Some(write_layout::SequenceContext::format_1(coverage, rule_sets))
    }

    fn format_2_class_def(&self) -> Option<ClassDefBuilder> {
        let mut builder = ClassDefBuilder::new();
        for rule in &self.rules {
            for (glyphs, _) in &rule.context {
                let class = glyphs.iter().collect();
                if !builder.checked_add(class) {
                    return None;
                }
            }
        }
        Some(builder)
    }

    fn build_format_2(&self, in_gpos: bool) -> Option<write_layout::SequenceContext> {
        let classdef = self.format_2_class_def()?;
        let (input_class_def, input_map) = classdef.build_with_mapping();

        let coverage = self
            .rules
            .iter()
            .flat_map(|rule| rule.context.first().unwrap().0.iter())
            .collect::<CoverageTableBuilder>()
            .build();

        let mut rule_sets = vec![Vec::new(); input_map.len() + 1];
        for rule in &self.rules {
            let first_item = rule.first_input_sequence_item().to_class().unwrap();
            let cls_idx = *input_map.get(&first_item.into()).unwrap();
            let input = rule
                .context
                .iter()
                // first class in context is implicit, based on order of rules (class is index)
                .skip(1)
                .map(|(cls, _)| input_map.get(&cls.to_class().unwrap().into()).unwrap())
                .copied()
                .collect();
            rule_sets.get_mut(cls_idx as usize).unwrap().push(
                write_layout::ClassSequenceRule::new(input, rule.lookup_records(in_gpos)),
            );
        }

        let rule_sets = rule_sets
            .into_iter()
            .map(|rules| {
                (!rules.is_empty()).then_some(write_layout::ClassSequenceRuleSet::new(rules))
            })
            .collect();
        Some(write_layout::SequenceContext::format_2(
            coverage,
            input_class_def,
            rule_sets,
        ))
    }
}

impl SubContextBuilder {
    pub(crate) fn iter_lookups(&self) -> impl Iterator<Item = LookupId> + '_ {
        self.0.iter_lookups()
    }
}

impl ContextRule {
    /// If `other` has a single input position and the same backtrack,
    /// lookahead, and lookups as `self`, merge `other`'s input glyphs into
    /// `self` and return `true`.  Otherwise return `false` unchanged.
    ///
    /// This lets the format selector pick the smallest encoding (often
    /// Format 3 with fewer subtables).  Inspired by fonttools PR #4058.
    fn try_merge(&mut self, other: &ContextRule) -> bool {
        if self.context.len() == 1
            && other.context.len() == 1
            && self.backtrack == other.backtrack
            && self.lookahead == other.lookahead
            && self.context[0].1 == other.context[0].1
        {
            self.context[0].0.extend(&other.context[0].0);
            true
        } else {
            false
        }
    }

    pub(crate) fn bump_all_lookup_ids(&mut self, from: usize, by: usize) {
        for (_, lookups) in &mut self.context {
            lookups
                .iter_mut()
                .filter(|x| x.to_raw() >= from)
                .for_each(|x| *x = LookupId::Gsub(x.to_raw() + by))
        }
    }
    fn is_chain_rule(&self) -> bool {
        !self.backtrack.is_empty() || !self.lookahead.is_empty()
    }

    /// `true` unless every item in this rule is exactly one glyph.
    ///
    /// Format 1 requires single glyphs throughout, so an empty class rules it
    /// out just as a larger one does; hence `!= 1` and not `> 1`.
    fn has_non_singleton_glyph_classes(&self) -> bool {
        self.backtrack
            .iter()
            .chain(self.lookahead.iter())
            .chain(self.context.iter().map(|(glyphs, _)| glyphs))
            .any(|x| x.len() != 1)
    }

    fn first_input_sequence_item(&self) -> &GlyphOrClass {
        &self.context.first().unwrap().0
    }

    fn lookup_records(&self, in_gpos: bool) -> Vec<write_layout::SequenceLookupRecord> {
        self.context
            .iter()
            .enumerate()
            .flat_map(|(i, (_, lookups))| {
                lookups.iter().map(move |lookup_id| {
                    let lookup_id = if in_gpos {
                        lookup_id.to_gpos_id_or_die()
                    } else {
                        lookup_id.to_gsub_id_or_die()
                    };

                    write_layout::SequenceLookupRecord::new(i.try_into().unwrap(), lookup_id)
                })
            })
            .collect()
    }
}

impl Builder for PosContextBuilder {
    type Output = Vec<write_layout::SequenceContext>;

    fn build(self, var_store: &mut VariationStoreBuilder) -> Self::Output {
        self.0.build(Some(var_store))
    }
}

impl Builder for SubContextBuilder {
    type Output = Vec<write_layout::SequenceContext>;

    fn build(self, _var_store: &mut VariationStoreBuilder) -> Self::Output {
        self.0.build(None)
    }
}

impl RemapIds for PosContextBuilder {
    fn remap_ids(&mut self, id_map: &super::LookupIdMap) {
        self.0.remap_ids(id_map);
    }
}

impl RemapIds for SubContextBuilder {
    fn remap_ids(&mut self, id_map: &super::LookupIdMap) {
        self.0.remap_ids(id_map);
    }
}

impl RemapIds for ContextBuilder {
    fn remap_ids(&mut self, id_map: &super::LookupIdMap) {
        for rule in &mut self.rules {
            for (_, lookups) in &mut rule.context {
                lookups.iter_mut().for_each(|id| *id = id_map.get(*id));
            }
        }
    }
}

impl ContextBuilder {
    fn build(
        self,
        var_store: Option<&mut VariationStoreBuilder>,
    ) -> Vec<write_layout::SequenceContext> {
        let in_gpos = var_store.is_some();
        self.build_inner(in_gpos)
    }

    fn build_inner(mut self, in_gpos: bool) -> Vec<write_layout::SequenceContext> {
        assert!(self.rules.iter().all(|rule| !rule.is_chain_rule()));
        let format_1 = self.build_format_1(in_gpos).map(|x| vec![x]);
        let format_2 = self.build_format_2(in_gpos).map(|x| vec![x]);
        let format_3 = self
            .rules
            .iter()
            .map(|rule| {
                let cov_tables = rule
                    .context
                    .iter()
                    .map(|(seq, _)| seq.iter().collect::<CoverageTableBuilder>().build())
                    .collect();
                let seq_lookups = rule.lookup_records(in_gpos);

                write_layout::SequenceContext::format_3(cov_tables, seq_lookups)
            })
            .collect();

        match pick_best_format([format_1, format_2, Some(format_3)]) {
            Ok(best) => best,
            Err(overflowed) if self.rules.len() <= 1 => overflowed,
            Err(_) => {
                let second_half = ContextBuilder {
                    rules: self.rules.split_off(self.rules.len() / 2),
                };
                let mut subtables = self.build_inner(in_gpos);
                subtables.extend(second_half.build_inner(in_gpos));
                subtables
            }
        }
    }
}

impl ChainContextBuilder {
    pub(crate) fn iter_lookups(&self) -> impl Iterator<Item = LookupId> + '_ {
        self.0.iter_lookups()
    }

    fn build(mut self, in_gpos: bool) -> Vec<write_layout::ChainedSequenceContext> {
        let maybe_format_1 = self.build_format_1(in_gpos);
        let maybe_format_2 = self.build_format_2(in_gpos);
        let format_3 = self.build_format_3(in_gpos);

        //gross: we try all types we can, and then pick the best one by
        //actually checking the compiled size. There may be heuristic approaches
        //that would approximate this with less work, but they are not obvious.
        match pick_best_format([
            maybe_format_1.map(|x| vec![x]),
            maybe_format_2.map(|x| vec![x]),
            Some(format_3),
        ]) {
            Ok(best) => best,
            Err(overflowed) if self.0.rules.len() <= 1 => overflowed,
            Err(_) => {
                // A lookup's subtables are tried in order. Keep consecutive
                // rules together and preserve their order when retrying them.
                let second_half = ChainContextBuilder(ContextBuilder {
                    rules: self.0.rules.split_off(self.0.rules.len() / 2),
                });
                let mut subtables = self.build(in_gpos);
                subtables.extend(second_half.build(in_gpos));
                subtables
            }
        }
    }

    fn build_format_1(&self, in_gpos: bool) -> Option<write_layout::ChainedSequenceContext> {
        let coverage = self.0.format_1_coverage()?.build();

        let mut rule_sets = HashMap::<_, Vec<_>>::new();
        for rule in &self.0.rules {
            let key = rule.first_input_sequence_item().single_glyph().unwrap();
            let seq_lookups = rule.lookup_records(in_gpos);
            let rule = write_layout::ChainedSequenceRule::new(
                rule.backtrack.iter().flat_map(|cls| cls.iter()).collect(),
                rule.context
                    .iter()
                    .skip(1)
                    .flat_map(|(cls, _)| cls.iter())
                    .collect(),
                rule.lookahead.iter().flat_map(|cls| cls.iter()).collect(),
                seq_lookups,
            );

            rule_sets.entry(key).or_default().push(rule);
        }
        let rule_sets = coverage
            .iter()
            .map(|gid| {
                Some(write_layout::ChainedSequenceRuleSet::new(
                    rule_sets.remove(&gid).unwrap(),
                ))
            })
            .collect();

        Some(write_layout::ChainedSequenceContext::format_1(
            coverage, rule_sets,
        ))
    }

    /// If all of backtrack, input, and lookahead can be represented as classdefs,
    /// make them.
    fn format_2_class_defs(&self) -> Option<(ClassDefBuilder, ClassDefBuilder, ClassDefBuilder)> {
        let input = self.0.input_class_def()?;

        let mut backtrack = ClassDefBuilder::default();
        for class in self.0.rules.iter().flat_map(|rule| rule.backtrack.iter()) {
            if !backtrack.checked_add(class.to_class().unwrap().into()) {
                return None;
            }
        }

        let mut lookahead = ClassDefBuilder::default();
        for class in self.0.rules.iter().flat_map(|rule| rule.lookahead.iter()) {
            if !lookahead.checked_add(class.to_class().unwrap().into()) {
                return None;
            }
        }

        Some((backtrack, input, lookahead))
    }

    /// If this lookup can be expressed as format 2, generate it
    fn build_format_2(&self, in_gpos: bool) -> Option<write_layout::ChainedSequenceContext> {
        let (backtrack, input, lookahead) = self.format_2_class_defs()?;
        let (backtrack_class_def, backtrack_map) = backtrack.build_with_mapping();
        let (input_class_def, input_map) = input.build_with_mapping();
        let (lookahead_class_def, lookahead_map) = lookahead.build_with_mapping();
        let coverage = self
            .0
            .rules
            .iter()
            .flat_map(|rule| rule.context.first().unwrap().0.iter())
            .collect::<CoverageTableBuilder>()
            .build();

        let mut rule_sets = vec![Vec::new(); input_map.len() + 1];

        for rule in &self.0.rules {
            let cls_idx = *input_map
                .get(&rule.first_input_sequence_item().to_class().unwrap().into())
                .unwrap();
            let backtrack = rule
                .backtrack
                .iter()
                .map(|cls| backtrack_map.get(&cls.to_class().unwrap().into()).unwrap())
                .copied()
                .collect();
            let lookahead = rule
                .lookahead
                .iter()
                .map(|cls| lookahead_map.get(&cls.to_class().unwrap().into()).unwrap())
                .copied()
                .collect();
            let input = rule
                .context
                .iter()
                .skip(1)
                .map(|(cls, _)| input_map.get(&cls.to_class().unwrap().into()).unwrap())
                .copied()
                .collect();

            rule_sets.get_mut(cls_idx as usize).unwrap().push(
                write_layout::ChainedClassSequenceRule::new(
                    backtrack,
                    input,
                    lookahead,
                    rule.lookup_records(in_gpos),
                ),
            )
        }
        let rule_sets = rule_sets
            .into_iter()
            .map(|rules| {
                (!rules.is_empty()).then_some(write_layout::ChainedClassSequenceRuleSet::new(rules))
            })
            .collect();

        Some(write_layout::ChainedSequenceContext::format_2(
            coverage,
            backtrack_class_def,
            input_class_def,
            lookahead_class_def,
            rule_sets,
        ))
    }

    /// format 3 is always possible; it also generates a subtable for each rule.
    fn build_format_3(&self, in_gpos: bool) -> Vec<write_layout::ChainedSequenceContext> {
        self.0
            .rules
            .iter()
            .map(|rule| {
                let backtrack = rule
                    .backtrack
                    .iter()
                    .map(|seq| seq.iter().collect::<CoverageTableBuilder>().build())
                    .collect();
                let lookahead = rule
                    .lookahead
                    .iter()
                    .map(|seq| seq.iter().collect::<CoverageTableBuilder>().build())
                    .collect();
                let input = rule
                    .context
                    .iter()
                    .map(|(seq, _)| seq.iter().collect::<CoverageTableBuilder>().build())
                    .collect();
                let seq_lookups = rule.lookup_records(in_gpos);

                write_layout::ChainedSequenceContext::format_3(
                    backtrack,
                    input,
                    lookahead,
                    seq_lookups,
                )
            })
            .collect::<Vec<_>>()
    }
}

impl SubContextBuilder {
    pub(crate) fn bump_all_lookup_ids(&mut self, from: usize, by: usize) {
        self.0.bump_all_lookup_ids(from, by)
    }
}
impl SubChainContextBuilder {
    pub(crate) fn bump_all_lookup_ids(&mut self, from: usize, by: usize) {
        self.0.0.bump_all_lookup_ids(from, by)
    }

    pub(crate) fn iter_lookups(&self) -> impl Iterator<Item = LookupId> + '_ {
        self.0.iter_lookups()
    }
}

impl Builder for PosChainContextBuilder {
    type Output = Vec<write_layout::ChainedSequenceContext>;

    fn build(self, _: &mut VariationStoreBuilder) -> Self::Output {
        self.0.build(true)
    }
}

impl Builder for SubChainContextBuilder {
    type Output = Vec<write_layout::ChainedSequenceContext>;

    fn build(self, _: &mut VariationStoreBuilder) -> Self::Output {
        self.0.build(false)
    }
}

impl RemapIds for PosChainContextBuilder {
    fn remap_ids(&mut self, id_map: &super::LookupIdMap) {
        self.0.0.remap_ids(id_map);
    }
}

impl RemapIds for SubChainContextBuilder {
    fn remap_ids(&mut self, id_map: &super::LookupIdMap) {
        self.0.0.remap_ids(id_map);
    }
}
/// Return the smallest format that can be serialized.
///
/// The error contains a semantically valid candidate for an unsplittable
/// single rule, so the final table writer encounters the same packing failure
/// as it did before contextual splitting was added.
fn pick_best_format<T: FontWrite + Validate>(
    subtables: [Option<Vec<T>>; 3],
) -> Result<Vec<T>, Vec<T>> {
    let mut best = None;
    let mut overflowed = None;

    for (i, table) in subtables.into_iter().enumerate() {
        let Some(table) = table else { continue };
        let size = compute_size(&table);
        let n_subtables = table.len();
        log::trace!("format {} {n_subtables} subtables size {size:?}", i + 1);

        if size == usize::MAX {
            // Format 3 is last and always present, making it the eventual
            // fallback when no candidate fits.
            overflowed = Some(table);
        } else if best
            .as_ref()
            .map(|(best_size, _)| size < *best_size)
            .unwrap_or(true)
        {
            best = Some((size, table));
        }
    }

    best.map(|(_, table)| table)
        .ok_or_else(|| overflowed.expect("at least one contextual format must be available"))
}

fn compute_size<T: FontWrite + Validate>(subtables: &Vec<T>) -> usize {
    write_fonts::dump_table(subtables)
        .ok()
        .map(|x| {
            let subtable_len = x.len();
            // if a format produces more subtables we also account for the fact
            // that each subtable requires an additional offset in the subtable list
            let offset_cost = std::mem::size_of::<u16>() * subtables.len();
            subtable_len + offset_cost
        })
        .unwrap_or(usize::MAX)
}

impl ReverseChainBuilder {
    pub fn add(
        &mut self,
        backtrack: Vec<GlyphOrClass>,
        context: BTreeMap<GlyphId16, GlyphId16>,
        lookahead: Vec<GlyphOrClass>,
    ) {
        self.rules.push(ReverseSubRule {
            backtrack,
            context,
            lookahead,
        })
    }
}

impl Builder for ReverseChainBuilder {
    type Output = Vec<ReverseChainSingleSubstFormat1>;

    fn build(self, _: &mut VariationStoreBuilder) -> Self::Output {
        self.rules
            .into_iter()
            .map(|rule| {
                let backtrack = rule
                    .backtrack
                    .iter()
                    .map(|seq| seq.iter().collect::<CoverageTableBuilder>().build())
                    .collect();
                let lookahead = rule
                    .lookahead
                    .iter()
                    .map(|seq| seq.iter().collect::<CoverageTableBuilder>().build())
                    .collect();
                let input = rule
                    .context
                    .keys()
                    .copied()
                    .collect::<CoverageTableBuilder>();
                let replacements = rule.context.into_values().collect();

                write_gsub::ReverseChainSingleSubstFormat1::new(
                    input.build(),
                    backtrack,
                    lookahead,
                    replacements,
                )
            })
            .collect()
    }
}

impl From<ContextBuilder> for PosContextBuilder {
    fn from(src: ContextBuilder) -> PosContextBuilder {
        PosContextBuilder(src)
    }
}

impl From<ContextBuilder> for SubContextBuilder {
    fn from(src: ContextBuilder) -> SubContextBuilder {
        SubContextBuilder(src)
    }
}

impl From<ChainContextBuilder> for PosChainContextBuilder {
    fn from(src: ChainContextBuilder) -> PosChainContextBuilder {
        PosChainContextBuilder(src)
    }
}

impl From<ContextBuilder> for PosChainContextBuilder {
    fn from(src: ContextBuilder) -> PosChainContextBuilder {
        PosChainContextBuilder(ChainContextBuilder(src))
    }
}

impl From<ChainContextBuilder> for SubChainContextBuilder {
    fn from(src: ChainContextBuilder) -> SubChainContextBuilder {
        SubChainContextBuilder(src)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A rule matching `class` at two consecutive positions, applying
    /// `lookup_id` at the first.
    fn two_position_rule(class: [u16; 2], lookup_id: LookupId) -> ContextRule {
        let class = GlyphOrClass::Class(class.into_iter().map(GlyphId16::new).collect());
        ContextRule {
            backtrack: Vec::new(),
            context: vec![(class.clone(), vec![lookup_id]), (class, Vec::new())],
            lookahead: Vec::new(),
        }
    }

    /// 8000 rules in two halves. The first half's classes are {0,1}, {2,3},
    /// ..., {7998,7999}, the second half's are {1,2}, {3,4}, ..., {7999,8000}.
    ///
    /// Within a half the classes are disjoint, so the half fits a single
    /// ClassDef and can be a format 2 subtable. Across the halves they
    /// interleave, so the whole set can only be 8000 format 3 subtables,
    /// which overflow and force a split.
    fn halves_reproducer_rules(lookup_id: LookupId) -> Vec<ContextRule> {
        let first_half = (0..4000).map(|k| [2 * k, 2 * k + 1]);
        let second_half = (0..4000).map(|k| [2 * k + 1, 2 * k + 2]);
        first_half
            .chain(second_half)
            .map(|class| two_position_rule(class, lookup_id))
            .collect()
    }

    /// A format 2 subtable's coverage is the union of the first-position
    /// glyphs of the rules it holds, so it tells which rules ended up in it.
    fn expected_coverages(
        ranges: impl IntoIterator<Item = (u16, u16)>,
    ) -> Vec<write_layout::CoverageTable> {
        ranges
            .into_iter()
            .map(|(start, end)| {
                (start..=end)
                    .map(GlyphId16::new)
                    .collect::<CoverageTableBuilder>()
                    .build()
            })
            .collect()
    }

    #[test]
    fn oversized_sequence_context_is_split_into_format_2_subtables() {
        let builder = LookupBuilder::new_with_lookups(
            LookupFlag::default(),
            None,
            vec![SubContextBuilder(ContextBuilder {
                rules: halves_reproducer_rules(LookupId::Gsub(0)),
            })],
        );
        let mut var_store = VariationStoreBuilder::new(0);
        let lookup = builder.build(&mut var_store);

        // one format 2 subtable per half, in rule order
        let expected_coverages = expected_coverages([(0, 7999), (1, 8000)]);
        assert_eq!(lookup.subtables.len(), expected_coverages.len());
        for (subtable, expected_coverage) in lookup.subtables.iter().zip(expected_coverages) {
            let write_layout::SequenceContext::Format2(subtable) = subtable.as_ref() else {
                panic!("expected format 2 subtable, got {subtable:?}");
            };
            assert_eq!(subtable.coverage.as_ref(), &expected_coverage);
        }
    }

    #[test]
    fn oversized_chained_sequence_context_is_split_into_format_2_subtables() {
        let builder = LookupBuilder::new_with_lookups(
            LookupFlag::default(),
            None,
            vec![PosChainContextBuilder(ChainContextBuilder(
                ContextBuilder {
                    rules: halves_reproducer_rules(LookupId::Gpos(0)),
                },
            ))],
        );
        let mut var_store = VariationStoreBuilder::new(0);
        let lookup = builder.build(&mut var_store);

        // A chained class rule is 4 bytes bigger than a plain one (backtrack
        // and lookahead counts), so a 4000-rule half still overflows as
        // format 2 and each half is split again: four subtables, in rule order.
        let expected_coverages =
            expected_coverages([(0, 3999), (4000, 7999), (1, 4000), (4001, 8000)]);
        assert_eq!(lookup.subtables.len(), expected_coverages.len());
        for (subtable, expected_coverage) in lookup.subtables.iter().zip(expected_coverages) {
            let write_layout::ChainedSequenceContext::Format2(subtable) = subtable.as_ref() else {
                panic!("expected format 2 subtable, got {subtable:?}");
            };
            assert_eq!(subtable.coverage.as_ref(), &expected_coverage);
        }
    }

    #[test]
    fn unsplittable_single_rule_is_not_split() {
        // One rule with three positions of ~22000 glyphs each. The second
        // class overlaps the first, so no ClassDef partition exists and
        // format 2 is impossible; the only candidate is a single format 3
        // subtable whose three coverages are ~44k each, so the third one is
        // out of Offset16 reach and the subtable cannot be serialized.
        let even_glyphs = (2..44_000)
            .step_by(2)
            .map(GlyphId16::new)
            .collect::<Vec<_>>();
        let even = GlyphOrClass::Class(even_glyphs.clone().into());
        let odd = GlyphOrClass::Class((1..44_000).step_by(2).map(GlyphId16::new).collect());
        let overlapping = GlyphOrClass::Class(
            std::iter::once(GlyphId16::new(1))
                .chain(even_glyphs)
                .collect(),
        );
        let subtables = ContextBuilder {
            rules: vec![ContextRule {
                backtrack: Vec::new(),
                context: vec![
                    (even, vec![LookupId::Gsub(0)]),
                    (overlapping, Vec::new()),
                    (odd, Vec::new()),
                ],
                lookahead: Vec::new(),
            }],
        }
        .build(None);

        // returned as is, for the final packing to report
        assert_eq!(subtables.len(), 1);
        assert!(matches!(
            subtables[0],
            write_layout::SequenceContext::Format3(_)
        ));
        assert_eq!(compute_size(&subtables), usize::MAX);
    }
}
