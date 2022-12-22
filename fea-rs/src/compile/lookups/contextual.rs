//! Contextual lookup builders

use std::{
    collections::{BTreeMap, HashMap},
    convert::TryInto,
};

use write_fonts::{
    tables::{
        gpos::ValueRecord,
        gsub as write_gsub,
        gsub::ReverseChainSingleSubstFormat1,
        layout::{self as write_layout, CoverageTableBuilder, LookupFlag},
    },
    types::GlyphId,
    validate::Validate,
    FontWrite,
};

use crate::types::GlyphOrClass;

use super::{
    Builder, ClassDefBuilder2, FilterSetId, LookupBuilder, LookupId, PositionLookup,
    SubstitutionLookup,
};

/// When building a contextual/chaining contextual rule, we also build a
/// bunch of anonymous lookups.
#[derive(Debug, Clone)]
pub(crate) struct ContextualLookupBuilder<T> {
    flags: LookupFlag,
    mark_set: Option<FilterSetId>,
    subtables: Vec<ContextBuilder>,
    anon_lookups: Vec<T>,
    pub(super) root_id: LookupId,
    force_subtable_break: bool,
}

// while building we use a common representation, but when compiling we will
// use normal contextual lookups where possible.
pub(crate) enum ChainOrNot {
    Context(LookupBuilder<ContextBuilder>),
    Chain(LookupBuilder<ChainContextBuilder>),
}

impl<T> ContextualLookupBuilder<T> {
    pub(crate) fn new(flags: LookupFlag, mark_set: Option<FilterSetId>) -> Self {
        ContextualLookupBuilder {
            flags,
            mark_set,
            anon_lookups: Vec::new(),
            subtables: vec![Default::default()],
            root_id: LookupId::Empty,
            force_subtable_break: false,
        }
    }

    pub(crate) fn into_lookups(self) -> (ChainOrNot, Vec<T>) {
        let ContextualLookupBuilder {
            flags,
            mark_set,
            subtables,
            anon_lookups,
            ..
        } = self;
        let lookup = if subtables.iter().any(ContextBuilder::is_chain_rule) {
            ChainOrNot::Chain(LookupBuilder::new_with_lookups(
                flags,
                mark_set,
                subtables.into_iter().map(ChainContextBuilder).collect(),
            ))
        } else {
            ChainOrNot::Context(LookupBuilder::new_with_lookups(flags, mark_set, subtables))
        };
        (lookup, anon_lookups)
    }

    //TODO: if we keep this, make it unwrap and ensure we always have a subtable
    pub fn last_mut(&mut self) -> &mut ContextBuilder {
        self.subtables.last_mut().unwrap()
    }

    pub fn force_subtable_break(&mut self) {
        self.subtables.push(Default::default());
        self.force_subtable_break = true;
    }

    fn add_new_lookup_if_necessary(
        &mut self,
        check_fn: impl FnOnce(&T) -> bool,
        new_fn: impl FnOnce(LookupFlag, Option<FilterSetId>) -> T,
    ) {
        if self
            .anon_lookups
            .last()
            .map(|lookup| self.force_subtable_break || check_fn(lookup))
            .unwrap_or(true)
        {
            self.force_subtable_break = false;
            let lookup = new_fn(self.flags, self.mark_set);
            self.anon_lookups.push(lookup);
        }
    }
}

impl ContextualLookupBuilder<PositionLookup> {
    fn current_anon_lookup_id(&self) -> LookupId {
        LookupId::Gpos(self.root_id.to_raw() + self.anon_lookups.len())
    }

    pub(crate) fn add_anon_gpos_type_1(
        &mut self,
        glyphs: &GlyphOrClass,
        value: ValueRecord,
    ) -> LookupId {
        self.add_new_lookup_if_necessary(
            |existing| match existing {
                PositionLookup::Single(lookup) => lookup
                    .subtables
                    .iter()
                    .any(|subt| glyphs.iter().any(|gid| !subt.can_add_rule(gid, &value))),
                _ => true,
            },
            |flags, mark_set| PositionLookup::Single(super::LookupBuilder::new(flags, mark_set)),
        );
        let lookup = self.anon_lookups.last_mut().unwrap();
        let PositionLookup::Single(lookup) = lookup else {
            panic!("this shouldn't happen");
        };

        let sub = lookup.last_mut().unwrap();
        for id in glyphs.iter() {
            sub.insert(id, value.clone());
        }
        self.current_anon_lookup_id()
    }
}

impl ContextualLookupBuilder<SubstitutionLookup> {
    fn current_anon_lookup_id(&self) -> LookupId {
        LookupId::Gsub(self.root_id.to_raw() + self.anon_lookups.len())
    }

    pub(crate) fn add_anon_gsub_type_1(
        &mut self,
        target: GlyphOrClass,
        replacement: GlyphOrClass,
    ) -> LookupId {
        // do we need a new lookup or can we use the existing one?
        self.add_new_lookup_if_necessary(
            |existing| match existing {
                SubstitutionLookup::Single(subtables) => subtables
                    .subtables
                    .iter()
                    .any(|subt| target.iter().any(|t| subt.contains_target(t))),
                _ => true,
            },
            |flags, mark_set| SubstitutionLookup::Single(LookupBuilder::new(flags, mark_set)),
        );

        let lookup = self.anon_lookups.last_mut().unwrap();
        let SubstitutionLookup::Single(subtables) = lookup else {
            // we didn't panic here before my refactor but I don't think we
            // want to fall through. let's find out?
            panic!("I don't think this should happen?");
        };
        let sub = subtables.last_mut().unwrap();
        for (target, replacement) in target.iter().zip(replacement.into_iter_for_target()) {
            sub.insert(target, replacement);
        }
        self.current_anon_lookup_id()
    }

    pub(crate) fn add_anon_gsub_type_4(
        &mut self,
        target: Vec<GlyphId>,
        replacement: GlyphId,
    ) -> LookupId {
        // do we need a new lookup or can we use the existing one?
        self.add_new_lookup_if_necessary(
            |existing| match existing {
                SubstitutionLookup::Ligature(builder) => builder
                    .subtables
                    .iter()
                    .any(|sub| sub.contains_target(target.first().copied().unwrap())),
                _ => true,
            },
            |flags, mark_set| SubstitutionLookup::Ligature(LookupBuilder::new(flags, mark_set)),
        );

        let lookup = self.anon_lookups.last_mut().unwrap();
        let SubstitutionLookup::Ligature(subtables) = lookup else {
            panic!("ahhhhhh");
        };

        let sub = subtables.last_mut().unwrap();
        sub.insert(target, replacement);
        self.current_anon_lookup_id()
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct ContextBuilder {
    rules: Vec<ContextRule>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct ReverseChainBuilder {
    rules: Vec<ReverseSubRule>,
}

#[derive(Clone, Debug)]
struct ReverseSubRule {
    backtrack: Vec<GlyphOrClass>,
    context: BTreeMap<GlyphId, GlyphId>,
    lookahead: Vec<GlyphOrClass>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct ChainContextBuilder(ContextBuilder);

#[derive(Clone, Debug)]
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
        self.rules.push(ContextRule {
            backtrack,
            context,
            lookahead,
        })
    }

    // for adjusting ids if we insert aalt at the front
    pub(crate) fn bump_all_lookup_ids(&mut self, by: usize) {
        self.rules
            .iter_mut()
            .for_each(|rule| rule.bump_all_lookup_ids(by))
    }

    fn is_chain_rule(&self) -> bool {
        self.rules.iter().any(ContextRule::is_chain_rule)
    }

    fn has_glyph_classes(&self) -> bool {
        self.rules.iter().any(ContextRule::has_glyph_classes)
    }

    /// If the input sequence can be represented as a class def, return it
    fn input_class_def(&self) -> Option<ClassDefBuilder2> {
        let mut builder = ClassDefBuilder2::new(false);
        for class in self
            .rules
            .iter()
            .flat_map(|rule| rule.context.iter().map(|x| &x.0))
        {
            if !builder.checked_add(class.to_class().unwrap()) {
                return None;
            }
        }
        Some(builder)
    }

    fn format_1_coverage(&self) -> Option<CoverageTableBuilder> {
        if self.has_glyph_classes() {
            return None;
        }
        Some(
            self.rules
                .iter()
                .map(|rule| rule.first_input_sequence_item().to_glyph().unwrap())
                .collect::<CoverageTableBuilder>(),
        )
    }

    fn build_format_1(&self) -> Option<write_layout::SequenceContext> {
        let coverage = self.format_1_coverage()?.build();
        let mut rule_sets = HashMap::<_, Vec<_>>::new();
        for rule in &self.rules {
            let key = rule.first_input_sequence_item().to_glyph().unwrap();
            let seq_lookups = rule.lookup_records();
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
}

impl ContextRule {
    pub(crate) fn bump_all_lookup_ids(&mut self, by: usize) {
        for (_, lookups) in &mut self.context {
            lookups
                .iter_mut()
                .for_each(|x| *x = LookupId::Gsub(x.to_raw() + by))
        }
    }
    fn is_chain_rule(&self) -> bool {
        !self.backtrack.is_empty() || !self.lookahead.is_empty()
    }

    fn has_glyph_classes(&self) -> bool {
        self.backtrack
            .iter()
            .chain(self.lookahead.iter())
            .chain(self.context.iter().map(|(glyphs, _)| glyphs))
            .any(|x| x.is_class())
    }

    fn first_input_sequence_item(&self) -> &GlyphOrClass {
        &self.context.first().unwrap().0
    }

    fn lookup_records(&self) -> Vec<write_layout::SequenceLookupRecord> {
        self.context
            .iter()
            .enumerate()
            .flat_map(|(i, (_, lookups))| {
                lookups.iter().map(move |lookup_id| {
                    write_layout::SequenceLookupRecord::new(
                        i.try_into().unwrap(),
                        lookup_id.to_u16_or_die(),
                    )
                })
            })
            .collect()
    }
}

impl Builder for ContextBuilder {
    type Output = Vec<write_layout::SequenceContext>;

    fn build(self) -> Self::Output {
        assert!(self.rules.iter().all(|rule| !rule.is_chain_rule()));
        let format_1 = self.build_format_1();
        //TODO: I'm skipping format_2 because it seems consistently larger
        // than format 3? but I have no verified this.
        let format_3 = self
            .rules
            .into_iter()
            .map(|rule| {
                let cov_tables = rule
                    .context
                    .iter()
                    .map(|(seq, _)| seq.iter().collect::<CoverageTableBuilder>().build())
                    .collect();
                let seq_lookups = rule.lookup_records();

                write_layout::SequenceContext::format_3(cov_tables, seq_lookups)
            })
            .collect();

        pick_best_format([format_1.map(|x| vec![x]), None, Some(format_3)])
    }
}

impl ChainContextBuilder {
    pub(crate) fn bump_all_lookup_ids(&mut self, by: usize) {
        self.0.bump_all_lookup_ids(by)
    }

    fn build_format_1(&self) -> Option<write_layout::ChainedSequenceContext> {
        let coverage = self.0.format_1_coverage()?.build();

        let mut rule_sets = HashMap::<_, Vec<_>>::new();
        for rule in &self.0.rules {
            let key = rule.first_input_sequence_item().to_glyph().unwrap();
            let seq_lookups = rule.lookup_records();
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
    fn format_2_class_defs(
        &self,
    ) -> Option<(ClassDefBuilder2, ClassDefBuilder2, ClassDefBuilder2)> {
        let input = self.0.input_class_def()?;

        let mut backtrack = ClassDefBuilder2::default();
        for class in self.0.rules.iter().flat_map(|rule| rule.backtrack.iter()) {
            if !backtrack.checked_add(class.to_class().unwrap()) {
                return None;
            }
        }

        let mut lookahead = ClassDefBuilder2::default();
        for class in self.0.rules.iter().flat_map(|rule| rule.lookahead.iter()) {
            if !lookahead.checked_add(class.to_class().unwrap()) {
                return None;
            }
        }

        Some((backtrack, input, lookahead))
    }

    /// If this lookup can be expressed as format 2, generate it
    fn build_format_2(&self) -> Option<write_layout::ChainedSequenceContext> {
        let (backtrack, input, lookahead) = self.format_2_class_defs()?;
        let (backtrack_class_def, backtrack_map) = backtrack.build();
        let (input_class_def, input_map) = input.build();
        let (lookahead_class_def, lookahead_map) = lookahead.build();
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
                .get(&rule.first_input_sequence_item().to_class().unwrap())
                .unwrap();
            let backtrack = rule
                .backtrack
                .iter()
                .map(|cls| backtrack_map.get(&cls.to_class().unwrap()).unwrap())
                .copied()
                .collect();
            let lookahead = rule
                .lookahead
                .iter()
                .map(|cls| lookahead_map.get(&cls.to_class().unwrap()).unwrap())
                .copied()
                .collect();
            let input = rule
                .context
                .iter()
                .skip(1)
                .map(|(cls, _)| input_map.get(&cls.to_class().unwrap()).unwrap())
                .copied()
                .collect();

            rule_sets.get_mut(cls_idx as usize).unwrap().push(
                write_layout::ChainedClassSequenceRule::new(
                    backtrack,
                    input,
                    lookahead,
                    rule.lookup_records(),
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
}

impl Builder for ChainContextBuilder {
    type Output = Vec<write_layout::ChainedSequenceContext>;

    fn build(self) -> Self::Output {
        // do this first, since we take ownership below
        let maybe_format_1 = self.build_format_1();
        let maybe_format_2 = self.build_format_2();

        let format_3 = self
            .0
            .rules
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
                    .iter()
                    .map(|(seq, _)| seq.iter().collect::<CoverageTableBuilder>().build())
                    .collect();
                let seq_lookups = rule.lookup_records();

                write_layout::ChainedSequenceContext::format_3(
                    backtrack,
                    input,
                    lookahead,
                    seq_lookups,
                )
            })
            .collect::<Vec<_>>();

        //gross: we try all types we can, and then pick the best one by
        //actually checking the compiled size
        pick_best_format([
            maybe_format_1.map(|x| vec![x]),
            maybe_format_2.map(|x| vec![x]),
            Some(format_3),
        ])
    }
}

// invariant: at least one item must be Some
fn pick_best_format<T: FontWrite + Validate>(tables: [Option<T>; 3]) -> T {
    // this is written in a sort of funny style so that it's easy to println
    // the computed sizes for debugging
    tables
        .into_iter()
        .enumerate()
        .map(|(i, table)| (i, compute_size(table.as_ref()), table))
        .inspect(|(_i, _size, _table)| {
            //eprintln!("format {} size {_size:?}", _i + 1);
        })
        .min_by_key(|(_, size, _)| size.unwrap_or(usize::MAX))
        .unwrap()
        .2
        .unwrap()
}

fn compute_size<T: FontWrite + Validate>(item: Option<&T>) -> Option<usize> {
    item.map(write_fonts::dump_table)
        .transpose()
        .ok()
        .flatten()
        .map(|x| x.len())
}

impl ReverseChainBuilder {
    pub fn add(
        &mut self,
        backtrack: Vec<GlyphOrClass>,
        context: BTreeMap<GlyphId, GlyphId>,
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

    fn build(self) -> Self::Output {
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
