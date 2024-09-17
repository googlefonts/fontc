//! computing the usMaxContext value
//!
//! See <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#usmaxcontext>

use write_fonts::{
    tables::{
        gsub::{
            AlternateSubstFormat1, ExtensionSubtable, LigatureSubstFormat1, MultipleSubstFormat1,
            ReverseChainSingleSubstFormat1, SingleSubst, SubstitutionChainContext,
            SubstitutionLookup, SubstitutionSequenceContext,
        },
        layout::{
            ChainedClassSequenceRule, ChainedClassSequenceRuleSet, ChainedSequenceContext,
            ChainedSequenceContextFormat1, ChainedSequenceContextFormat2, ChainedSequenceRule,
            ChainedSequenceRuleSet, ClassSequenceRule, ClassSequenceRuleSet, SequenceContext,
            SequenceContextFormat1, SequenceContextFormat2, SequenceRule, SequenceRuleSet,
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
        self.input_sequence.len()
    }

    fn lookahead_len(&self) -> usize {
        self.seq_lookup_records.len()
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
        self.input_sequence.len()
    }

    fn lookahead_len(&self) -> usize {
        self.seq_lookup_records.len()
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
        self.input_sequence.len()
    }

    fn lookahead_len(&self) -> usize {
        self.seq_lookup_records.len()
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
        self.seq_lookup_records.len()
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

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L27>
impl MaxContext for SingleSubst {
    fn max_context(&self) -> u16 {
        1
    }
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L27>
impl MaxContext for MultipleSubstFormat1 {
    fn max_context(&self) -> u16 {
        1
    }
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L27>
impl MaxContext for AlternateSubstFormat1 {
    fn max_context(&self) -> u16 {
        1
    }
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
impl MaxContext for SubstitutionSequenceContext {
    fn max_context(&self) -> u16 {
        match self as &SequenceContext {
            SequenceContext::Format1(format1) => max_context_of_contextual_subtable(&format1),
            SequenceContext::Format2(format2) => max_context_of_contextual_subtable(&format2),
            SequenceContext::Format3(format3) => {
                max_context_of_rule(0, format3.seq_lookup_records.len())
            }
        }
    }
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L45-L49>
impl MaxContext for SubstitutionChainContext {
    fn max_context(&self) -> u16 {
        match self as &ChainedSequenceContext {
            ChainedSequenceContext::Format1(format1) => {
                max_context_of_contextual_subtable(&format1)
            }
            ChainedSequenceContext::Format2(format2) => {
                max_context_of_contextual_subtable(&format2)
            }
            ChainedSequenceContext::Format3(format3) => {
                max_context_of_rule(0, format3.seq_lookup_records.len())
            }
        }
    }
}

impl MaxContext for ExtensionSubtable {
    fn max_context(&self) -> u16 {
        match self {
            ExtensionSubtable::Single(inner) => inner.extension.max_context(),
            ExtensionSubtable::Multiple(inner) => inner.extension.max_context(),
            ExtensionSubtable::Alternate(inner) => inner.extension.max_context(),
            ExtensionSubtable::Ligature(inner) => inner.extension.max_context(),
            ExtensionSubtable::Contextual(inner) => inner.extension.max_context(),
            ExtensionSubtable::ChainContextual(inner) => inner.extension.max_context(),
            ExtensionSubtable::Reverse(inner) => inner.extension.max_context(),
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

impl MaxContext for &SubstitutionLookup {
    fn max_context(&self) -> u16 {
        match self {
            SubstitutionLookup::Single(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Multiple(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Alternate(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Ligature(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Contextual(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::ChainContextual(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Extension(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Reverse(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
        }
        .unwrap_or_default()
    }
}
