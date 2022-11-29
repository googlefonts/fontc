//! Contextual lookup builders

use std::{collections::BTreeMap, convert::TryInto};

use write_fonts::{
    tables::{
        gpos::ValueRecord,
        gsub as write_gsub,
        gsub::ReverseChainSingleSubstFormat1,
        layout::{self as write_layout, CoverageTableBuilder, LookupFlag},
    },
    types::GlyphId,
};

use crate::types::GlyphOrClass;

use super::{Builder, FilterSetId, LookupBuilder, LookupId, PositionLookup, SubstitutionLookup};

/// When building a contextual/chaining contextual rule, we also build a
/// bunch of anonymous lookups.
#[derive(Debug, Clone)]
pub(crate) struct ContextualLookupBuilder<T> {
    flags: LookupFlag,
    mark_set: Option<FilterSetId>,
    subtables: Vec<ContextBuilder>,
    anon_lookups: Vec<T>,
    pub(super) root_id: LookupId,
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

    //pub fn force_subtable_break(&mut self) {
    //self.subtables.push(Default::default())
    //}

    fn add_new_lookup_if_necessary(
        &mut self,
        check_fn: impl FnOnce(&T) -> bool,
        new_fn: impl FnOnce(LookupFlag, Option<FilterSetId>) -> T,
    ) {
        if self.anon_lookups.last().map(check_fn).unwrap_or(true) {
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

    fn is_chain_rule(&self) -> bool {
        self.rules.iter().any(ContextRule::is_chain_rule)
    }
}

impl ContextRule {
    fn is_chain_rule(&self) -> bool {
        !self.backtrack.is_empty() || !self.lookahead.is_empty()
    }
}

impl Builder for ContextBuilder {
    type Output = Vec<write_layout::SequenceContext>;

    fn build(self) -> Self::Output {
        assert!(self.rules.iter().all(|rule| !rule.is_chain_rule()));
        // for now we are going to just... always use coverage tables (format 3)
        // TODO: figure out how to decide what tables to use?
        self.rules
            .into_iter()
            .map(|rule| {
                let cov_tables = rule
                    .context
                    .iter()
                    .map(|(seq, _)| seq.iter().collect::<CoverageTableBuilder>().build())
                    .collect();
                let seq_lookups = rule
                    .context
                    .into_iter()
                    .enumerate()
                    .flat_map(|(i, (_, lookups))| {
                        lookups.into_iter().map(move |lookup_id| {
                            write_layout::SequenceLookupRecord::new(
                                i.try_into().unwrap(),
                                lookup_id.to_u16_or_die(),
                            )
                        })
                    })
                    .collect();

                write_layout::SequenceContext::format_3(cov_tables, seq_lookups)
            })
            .collect()
    }
}

impl Builder for ChainContextBuilder {
    type Output = Vec<write_layout::ChainedSequenceContext>;

    fn build(self) -> Self::Output {
        self.0
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
                let seq_lookups = rule
                    .context
                    .into_iter()
                    .enumerate()
                    .flat_map(|(i, (_, lookups))| {
                        lookups.into_iter().map(move |lookup_id| {
                            write_layout::SequenceLookupRecord::new(
                                i.try_into().unwrap(),
                                lookup_id.to_u16_or_die(),
                            )
                        })
                    })
                    .collect();

                write_layout::ChainedSequenceContext::format_3(
                    backtrack,
                    input,
                    lookahead,
                    seq_lookups,
                )
            })
            .collect()
    }
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
