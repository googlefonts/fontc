//! computing the usMaxContext value
//!
//! See <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#usmaxcontext>

use write_fonts::{
    tables::{
        gpos::{
            ExtensionSubtable as GposExt, PositionChainContext, PositionLookup,
            PositionSequenceContext,
        },
        gsub::{
            ExtensionSubtable as GsubExt, LigatureSubstFormat1, ReverseChainSingleSubstFormat1,
            SubstitutionChainContext, SubstitutionLookup, SubstitutionSequenceContext,
        },
        layout::{
            ChainedClassSequenceRule, ChainedClassSequenceRuleSet, ChainedSequenceContext,
            ChainedSequenceContextFormat1, ChainedSequenceContextFormat2, ChainedSequenceRule,
            ChainedSequenceRuleSet, ClassSequenceRule, ClassSequenceRuleSet, Lookup,
            SequenceContext, SequenceContextFormat1, SequenceContextFormat2, SequenceRule,
            SequenceRuleSet,
        },
    },
    NullableOffsetMarker, OffsetMarker,
};

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L89-L96>
fn max_context_of_rule(input_glyph_count: usize, lookahead_glyph_count: usize) -> u16 {
    (input_glyph_count + lookahead_glyph_count) as u16
}

trait Rule {
    fn sequence_len(&self) -> usize;
    fn lookahead_len(&self) -> usize;

    fn context(&self) -> u16 {
        (self.sequence_len() + self.lookahead_len()) as u16
    }
}

trait RuleSet {
    type Rule;
    fn rules(&self) -> &[OffsetMarker<Self::Rule>];
}

/// A subtable with a set of nullable rules that contribute to max context
trait ContextualSubtable {
    type Rule: Rule;
    type RuleSet: RuleSet;
    fn rule_sets(&self) -> &[NullableOffsetMarker<Self::RuleSet>];
}

impl ContextualSubtable for &SequenceContextFormat1 {
    type Rule = SequenceRule;
    type RuleSet = SequenceRuleSet;
    fn rule_sets(&self) -> &[NullableOffsetMarker<Self::RuleSet>] {
        &self.seq_rule_sets
    }
}

impl RuleSet for SequenceRuleSet {
    type Rule = SequenceRule;
    fn rules(&self) -> &[OffsetMarker<Self::Rule>] {
        &self.seq_rules
    }
}

impl Rule for SequenceRule {
    fn sequence_len(&self) -> usize {
        self.input_sequence.len() + 1
    }

    fn lookahead_len(&self) -> usize {
        0
    }
}

impl ContextualSubtable for &SequenceContextFormat2 {
    type Rule = ClassSequenceRule;
    type RuleSet = ClassSequenceRuleSet;
    fn rule_sets(&self) -> &[NullableOffsetMarker<Self::RuleSet>] {
        &self.class_seq_rule_sets
    }
}

impl RuleSet for ClassSequenceRuleSet {
    type Rule = ClassSequenceRule;
    fn rules(&self) -> &[OffsetMarker<Self::Rule>] {
        &self.class_seq_rules
    }
}

impl Rule for ClassSequenceRule {
    fn sequence_len(&self) -> usize {
        self.input_sequence.len() + 1
    }

    fn lookahead_len(&self) -> usize {
        0
    }
}

impl ContextualSubtable for &ChainedSequenceContextFormat1 {
    type Rule = ChainedSequenceRule;
    type RuleSet = ChainedSequenceRuleSet;
    fn rule_sets(&self) -> &[NullableOffsetMarker<Self::RuleSet>] {
        &self.chained_seq_rule_sets
    }
}

impl RuleSet for ChainedSequenceRuleSet {
    type Rule = ChainedSequenceRule;
    fn rules(&self) -> &[OffsetMarker<Self::Rule>] {
        &self.chained_seq_rules
    }
}

impl Rule for ChainedSequenceRule {
    fn sequence_len(&self) -> usize {
        self.input_sequence.len() + 1
    }

    fn lookahead_len(&self) -> usize {
        self.lookahead_sequence.len()
    }
}

impl ContextualSubtable for &ChainedSequenceContextFormat2 {
    type Rule = ChainedClassSequenceRule;
    type RuleSet = ChainedClassSequenceRuleSet;
    fn rule_sets(&self) -> &[NullableOffsetMarker<Self::RuleSet>] {
        &self.chained_class_seq_rule_sets
    }
}

impl RuleSet for ChainedClassSequenceRuleSet {
    type Rule = ChainedClassSequenceRule;
    fn rules(&self) -> &[OffsetMarker<Self::Rule>] {
        &self.chained_class_seq_rules
    }
}

impl Rule for ChainedClassSequenceRule {
    fn sequence_len(&self) -> usize {
        self.input_sequence.len()
    }

    fn lookahead_len(&self) -> usize {
        self.lookahead_sequence.len()
    }
}

fn max_context_of_contextual_subtable<C, RS, R>(subtable: &C) -> u16
where
    C: ContextualSubtable<Rule = R, RuleSet = RS>,
    RS: RuleSet<Rule = R>,
    R: Rule,
{
    subtable
        .rule_sets()
        .iter()
        .filter_map(|nullable| nullable.as_ref())
        .flat_map(|ruleset| ruleset.rules().iter())
        .map(|rule| rule.context())
        .max()
        .unwrap_or_default()
}

pub(super) trait MaxContext {
    fn max_context(&self) -> u16;
}

/// <https://github.com/fonttools/fonttools/blob/bf77873d5a0ea7462664c8335c8cc7ea9e48ca18/Lib/fontTools/otlLib/maxContextCalc.py#L35-L39>
impl MaxContext for LigatureSubstFormat1 {
    fn max_context(&self) -> u16 {
        self.ligature_sets
            .iter()
            .flat_map(|l| l.ligatures.iter())
            // +1: components gives 2..N
            .fold(0, |max_ctx, ligature| {
                max_ctx.max((ligature.component_glyph_ids.len() + 1) as u16)
            })
    }
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L41-L43>
impl MaxContext for SequenceContext {
    fn max_context(&self) -> u16 {
        match self as &SequenceContext {
            SequenceContext::Format1(format1) => max_context_of_contextual_subtable(&format1),
            SequenceContext::Format2(format2) => max_context_of_contextual_subtable(&format2),
            SequenceContext::Format3(format3) => max_context_of_rule(format3.coverages.len(), 0),
        }
    }
}

impl MaxContext for SubstitutionSequenceContext {
    fn max_context(&self) -> u16 {
        self.as_inner().max_context()
    }
}

impl MaxContext for PositionSequenceContext {
    fn max_context(&self) -> u16 {
        self.as_inner().max_context()
    }
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L45-L49>
impl MaxContext for ChainedSequenceContext {
    fn max_context(&self) -> u16 {
        match self {
            ChainedSequenceContext::Format1(format1) => {
                max_context_of_contextual_subtable(&format1)
            }
            ChainedSequenceContext::Format2(format2) => {
                max_context_of_contextual_subtable(&format2)
            }
            ChainedSequenceContext::Format3(format3) => max_context_of_rule(
                format3.input_coverages.len(),
                format3.lookahead_coverages.len(),
            ),
        }
    }
}

impl MaxContext for SubstitutionChainContext {
    fn max_context(&self) -> u16 {
        self.as_inner().max_context()
    }
}

impl MaxContext for PositionChainContext {
    fn max_context(&self) -> u16 {
        self.as_inner().max_context()
    }
}

impl MaxContext for GsubExt {
    fn max_context(&self) -> u16 {
        match self {
            // <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L27>
            GsubExt::Single(_) | GsubExt::Multiple(_) | GsubExt::Alternate(_) => 1,
            GsubExt::Ligature(inner) => inner.extension.max_context(),
            GsubExt::Contextual(inner) => inner.extension.max_context(),
            GsubExt::ChainContextual(inner) => inner.extension.max_context(),
            GsubExt::Reverse(inner) => inner.extension.max_context(),
        }
    }
}

// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L55-L57>
impl MaxContext for ReverseChainSingleSubstFormat1 {
    fn max_context(&self) -> u16 {
        max_context_of_rule(
            self.backtrack_coverages.len(),
            self.lookahead_coverages.len(),
        )
    }
}

impl MaxContext for SubstitutionLookup {
    fn max_context(&self) -> u16 {
        match self {
            // <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L27>
            SubstitutionLookup::Single(_)
            | SubstitutionLookup::Multiple(_)
            | SubstitutionLookup::Alternate(_) => 1u16,
            SubstitutionLookup::Ligature(sub) => sub.max_context(),
            SubstitutionLookup::Contextual(sub) => sub.max_context(),
            SubstitutionLookup::ChainContextual(sub) => sub.max_context(),
            SubstitutionLookup::Extension(sub) => sub.max_context(),
            SubstitutionLookup::Reverse(sub) => sub.max_context(),
        }
    }
}

impl MaxContext for PositionLookup {
    fn max_context(&self) -> u16 {
        match self {
            PositionLookup::Single(_) => 1,
            PositionLookup::Pair(_) => 2,
            // these four types are ignored in fonttools:
            // https://github.com/fonttools/fonttools/blob/11343ed64c/Lib/fontTools/otlLib/maxContextCalc.py#L20
            PositionLookup::Cursive(_)
            | PositionLookup::MarkToBase(_)
            | PositionLookup::MarkToLig(_)
            | PositionLookup::MarkToMark(_) => 0,
            PositionLookup::Contextual(sub) => sub.max_context(),
            PositionLookup::ChainContextual(sub) => sub.max_context(),
            PositionLookup::Extension(sub) => sub.max_context(),
        }
    }
}

impl MaxContext for GposExt {
    fn max_context(&self) -> u16 {
        match self {
            GposExt::Single(_) => 1,
            GposExt::Pair(_) => 2,
            GposExt::Cursive(_)
            | GposExt::MarkToBase(_)
            | GposExt::MarkToLig(_)
            | GposExt::MarkToMark(_) => 0,
            GposExt::Contextual(inner) => inner.extension.max_context(),
            GposExt::ChainContextual(inner) => inner.extension.max_context(),
        }
    }
}
impl<T: MaxContext> MaxContext for Lookup<T> {
    fn max_context(&self) -> u16 {
        self.subtables
            .iter()
            .map(|sub| sub.max_context())
            .max()
            .unwrap_or_default()
    }
}
