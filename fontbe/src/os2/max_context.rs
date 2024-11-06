//! computing the usMaxContext value
//!
//! See <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#usmaxcontext>

use write_fonts::{
    tables::{
        gpos::{
            ExtensionSubtable as GposExt, Gpos, PositionChainContext, PositionLookup,
            PositionSequenceContext,
        },
        gsub::{
            ExtensionSubtable as GsubExt, Gsub, LigatureSubstFormat1,
            ReverseChainSingleSubstFormat1, SubstitutionChainContext, SubstitutionLookup,
            SubstitutionSequenceContext,
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

/// main entry point for computing max context.
///
/// corresponds to
/// <https://github.com/fonttools/fonttools/blob/63611d44/Lib/fontTools/otlLib/maxContextCalc.py#L4>
pub(super) fn compute_max_context_value(gpos: Option<&Gpos>, gsub: Option<&Gsub>) -> u16 {
    let max_gpos = gpos.map(|gpos| {
        gpos.lookup_list
            .lookups
            .iter()
            .fold(0, |max_ctx, lookup| max_ctx.max(lookup.max_context()))
    });
    let max_gsub = gsub.map(|gsub| {
        gsub.lookup_list
            .lookups
            .iter()
            .fold(0, |max_ctx, lookup| max_ctx.max(lookup.max_context()))
    });

    max_gpos
        .map(|max_gpos| max_gpos.max(max_gsub.unwrap_or_default()))
        .or(max_gsub)
        .unwrap_or_default()
}

/// The "chain" in Python
#[derive(Debug, Copy, Clone)]
enum ContextualRuleType {
    Contextual,
    Chained,
    ReverseChained,
}

pub trait MaxContext {
    fn max_context(&self) -> u16;
}

/// <https://github.com/fonttools/fonttools/blob/e04dfaab53c54b83096222993b914dc62e483156/Lib/fontTools/otlLib/maxContextCalc.py#L89-L96>
fn max_context_of_rule(
    input_glyph_count: usize,
    lookahead_glyph_count: usize,
    rule_type: ContextualRuleType,
) -> u16 {
    (match rule_type {
        ContextualRuleType::Contextual => input_glyph_count,
        ContextualRuleType::Chained => input_glyph_count + lookahead_glyph_count,
        ContextualRuleType::ReverseChained => 1 + lookahead_glyph_count,
    }) as u16
}

trait Rule {
    fn sequence_len(&self) -> usize;
    fn lookahead_len(&self) -> usize;

    fn context(&self, rule_type: ContextualRuleType) -> u16 {
        max_context_of_rule(self.sequence_len(), self.lookahead_len(), rule_type)
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
        // input sequence starts with the second glyph and has count - 1 entries
        // <https://learn.microsoft.com/en-us/typography/opentype/spec/chapter2#sequence-context-format-1-simple-glyph-contexts>
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
        // input sequence starts with the second glyph and has count - 1 entries
        // <https://learn.microsoft.com/en-us/typography/opentype/spec/chapter2#sequence-context-format-2-class-based-glyph-contexts>
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
        // input sequence starts with the second glyph and has count - 1 entries
        // <https://learn.microsoft.com/en-us/typography/opentype/spec/chapter2#chained-sequence-context-format-1-simple-glyph-contexts>
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
        // input sequence starts with the second glyph and has count - 1 entries
        // <https://learn.microsoft.com/en-us/typography/opentype/spec/chapter2#chained-sequence-context-format-2-class-based-glyph-contexts>
        self.input_sequence.len() + 1
    }

    fn lookahead_len(&self) -> usize {
        self.lookahead_sequence.len()
    }
}

fn max_context_of_contextual_subtable<C, RS, R>(subtable: &C, rule_type: ContextualRuleType) -> u16
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
        .map(|rule| rule.context(rule_type))
        .max()
        .unwrap_or_default()
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
            SequenceContext::Format1(format1) => {
                max_context_of_contextual_subtable(&format1, ContextualRuleType::Contextual)
            }
            SequenceContext::Format2(format2) => {
                max_context_of_contextual_subtable(&format2, ContextualRuleType::Contextual)
            }
            SequenceContext::Format3(format3) => {
                max_context_of_rule(format3.coverages.len(), 0, ContextualRuleType::Contextual)
            }
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

/// <https://github.com/fonttools/fonttools/blob/e04dfaab53c54b83096222993b914dc62e483156/Lib/fontTools/otlLib/maxContextCalc.py#L45-L49>
impl MaxContext for ChainedSequenceContext {
    fn max_context(&self) -> u16 {
        match self {
            ChainedSequenceContext::Format1(format1) => {
                max_context_of_contextual_subtable(&format1, ContextualRuleType::Chained)
            }
            ChainedSequenceContext::Format2(format2) => {
                max_context_of_contextual_subtable(&format2, ContextualRuleType::Chained)
            }
            ChainedSequenceContext::Format3(format3) => max_context_of_rule(
                format3.input_coverages.len(),
                format3.lookahead_coverages.len(),
                ContextualRuleType::Chained,
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
            ContextualRuleType::ReverseChained,
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

#[cfg(test)]
mod tests {
    use super::*;

    use write_fonts::{
        tables::{
            gpos::{
                PairPosFormat1, PairSet, PairValueRecord, PositionLookupList, SinglePosFormat1,
                ValueRecord,
            },
            gsub::{SingleSubstFormat1, SubstitutionLookupList},
            layout::{ClassDef, ClassDefFormat1, LookupFlag},
        },
        types::GlyphId16,
    };

    fn make_gids<const N: usize, T: FromIterator<GlyphId16>>(gids: [u16; N]) -> T {
        gids.into_iter().map(GlyphId16::new).collect()
    }

    fn make_lookup<T: Default>(subtable: T) -> Lookup<T> {
        Lookup::new(LookupFlag::empty(), vec![subtable])
    }

    fn make_singlepos() -> PositionLookup {
        PositionLookup::Single(make_lookup(
            SinglePosFormat1::new(make_gids([5u16, 6]), ValueRecord::new().with_x_advance(12))
                .into(),
        ))
    }

    fn make_pairpos() -> PositionLookup {
        PositionLookup::Pair(make_lookup(
            PairPosFormat1::new(
                make_gids([1u16, 2]),
                vec![
                    PairSet::new(vec![PairValueRecord::new(
                        GlyphId16::new(3),
                        ValueRecord::new().with_x_advance(1),
                        ValueRecord::default(),
                    )]),
                    PairSet::new(vec![PairValueRecord::new(
                        GlyphId16::new(4),
                        ValueRecord::new().with_x_advance(5),
                        ValueRecord::default(),
                    )]),
                ],
            )
            .into(),
        ))
    }

    fn make_singlesub() -> SubstitutionLookup {
        SubstitutionLookup::Single(make_lookup(
            SingleSubstFormat1::new(make_gids([12u16, 13]), 4).into(),
        ))
    }

    fn make_class_def(start_glyph_id: GlyphId16, class_value_array: Vec<u16>) -> ClassDef {
        ClassDef::Format1(ClassDefFormat1 {
            start_glyph_id,
            class_value_array,
        })
    }

    fn make_chain_context_format1<const N: usize, const M: usize>(
        input: [u16; N],
        lookahead: [u16; M],
    ) -> SubstitutionLookup {
        SubstitutionLookup::ChainContextual(make_lookup(
            ChainedSequenceContext::Format1(ChainedSequenceContextFormat1::new(
                make_gids([31]),
                vec![Some(ChainedSequenceRuleSet::new(vec![
                    ChainedSequenceRule::new(
                        // backtrack doesn't count, this shouldn't show up
                        make_gids([5, 111, 112, 114, 115, 117]),
                        make_gids(input),
                        make_gids(lookahead),
                        vec![],
                    ),
                ]))],
            ))
            .into(),
        ))
    }

    fn make_chain_context_format2<const N: usize, const M: usize>(
        input: [u16; N],
        lookahead: [u16; M],
    ) -> SubstitutionLookup {
        SubstitutionLookup::ChainContextual(make_lookup(
            ChainedSequenceContext::Format2(ChainedSequenceContextFormat2::new(
                make_gids([31]),
                make_class_def(1.into(), vec![2, 3]),
                make_class_def(1.into(), vec![2, 3]),
                make_class_def(1.into(), vec![2, 3]),
                vec![Some(ChainedClassSequenceRuleSet::new(vec![
                    ChainedClassSequenceRule::new(
                        // backtrack doesn't count, this shouldn't show up
                        vec![5, 111, 112, 114, 115, 117],
                        input.to_vec(),
                        lookahead.to_vec(),
                        vec![],
                    ),
                ]))],
            ))
            .into(),
        ))
    }

    #[test]
    fn max_context_nothin() {
        assert_eq!(compute_max_context_value(None, None), 0);
        assert_eq!(
            compute_max_context_value(Some(&Default::default()), None),
            0
        );
        assert_eq!(
            compute_max_context_value(None, Some(&Default::default())),
            0
        );
    }

    #[test]
    fn max_context_simple() {
        let gpos = Gpos::new(
            Default::default(),
            Default::default(),
            PositionLookupList::new(vec![make_singlepos()]),
        );

        assert_eq!(compute_max_context_value(Some(&gpos), None), 1);
    }

    #[test]
    fn max_context_both_tables() {
        let gpos = Gpos::new(
            Default::default(),
            Default::default(),
            PositionLookupList::new(vec![make_pairpos()]),
        );

        let gsub = Gsub::new(
            Default::default(),
            Default::default(),
            SubstitutionLookupList::new(vec![make_singlesub()]),
        );

        assert_eq!(compute_max_context_value(Some(&gpos), Some(&gsub)), 2);
    }

    #[test]
    fn max_context_kitchen_sink() {
        let gpos = Gpos::new(
            Default::default(),
            Default::default(),
            PositionLookupList::new(vec![make_singlepos(), make_pairpos()]),
        );

        let gsub = Gsub::new(
            Default::default(),
            Default::default(),
            SubstitutionLookupList::new(vec![
                make_singlesub(),
                make_chain_context_format1([1], [2, 3]),
            ]),
        );

        assert_eq!(compute_max_context_value(Some(&gpos), Some(&gsub)), 4);
    }

    // Ref <https://github.com/googlefonts/fontc/pull/1096>, exposes a missing +1 basically
    #[test]
    fn max_context_chaining_contextual_sub() {
        let gpos = Gpos::new(
            Default::default(),
            Default::default(),
            PositionLookupList::new(vec![make_pairpos()]),
        );

        let gsub = Gsub::new(
            Default::default(),
            Default::default(),
            SubstitutionLookupList::new(vec![make_chain_context_format2([1], [2, 3])]),
        );

        assert_eq!(compute_max_context_value(Some(&gpos), Some(&gsub)), 4);
    }
}
