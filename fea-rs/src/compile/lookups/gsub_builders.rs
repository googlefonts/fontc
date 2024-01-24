//! GSUB lookup builders

use std::{collections::BTreeMap, convert::TryFrom};

use write_fonts::{
    tables::{gsub as write_gsub, variations::ivs_builder::VariationStoreBuilder},
    types::{FixedSize, GlyphId},
};

use super::Builder;

#[derive(Clone, Debug, Default)]
pub struct SingleSubBuilder {
    items: BTreeMap<GlyphId, (GlyphId, PossibleSingleSubFormat)>,
}

/// Used to divide pairs into subtables as needed.
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum PossibleSingleSubFormat {
    // this pair can be format1
    Delta(i16),
    // this pair must be format 2 (delta not in i16 range)
    Format2,
}

impl SingleSubBuilder {
    pub fn insert(&mut self, target: GlyphId, replacement: GlyphId) {
        let delta = replacement.to_u16() as i32 - target.to_u16() as i32;
        let delta = i16::try_from(delta)
            .map(PossibleSingleSubFormat::Delta)
            .unwrap_or(PossibleSingleSubFormat::Format2);
        self.items.insert(target, (replacement, delta));
    }

    pub fn contains_target(&self, target: GlyphId) -> bool {
        self.items.contains_key(&target)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    // used when compiling aalt
    pub(crate) fn iter_pairs(&self) -> impl Iterator<Item = (GlyphId, GlyphId)> + '_ {
        self.items.iter().map(|(target, (alt, _))| (*target, *alt))
    }

    pub(crate) fn promote_to_multi_sub(self) -> MultipleSubBuilder {
        MultipleSubBuilder {
            items: self
                .items
                .into_iter()
                .map(|(key, (gid, _))| (key, vec![gid]))
                .collect(),
        }
    }
}

impl Builder for SingleSubBuilder {
    type Output = Vec<write_gsub::SingleSubst>;

    fn build(self, _: &mut VariationStoreBuilder) -> Self::Output {
        const COST_OF_EXTRA_SUB1F1_SUBTABLE: usize = 2 + // extra offset
            2 + 2 + 2 + // format1 table itself
            2 + 2; // extra coverage table

        const N_GLYPHS_TO_JUSTIFY_EXTRA_SUB1F1: usize =
            COST_OF_EXTRA_SUB1F1_SUBTABLE / GlyphId::RAW_BYTE_LEN;

        #[derive(Default)]
        struct SubtableMap {
            // key is the delta between the two gylphs.
            format1: BTreeMap<i16, Vec<(GlyphId, GlyphId)>>,
            format2: Vec<(GlyphId, GlyphId)>,
        }

        impl SubtableMap {
            fn from_builder(builder: SingleSubBuilder) -> Self {
                let mut this = SubtableMap::default();
                for (g1, (g2, delta)) in builder.items {
                    match delta {
                        PossibleSingleSubFormat::Delta(delta) => {
                            this.format1.entry(delta).or_default().push((g1, g2))
                        }
                        PossibleSingleSubFormat::Format2 => this.format2.push((g1, g2)),
                    }
                }
                this
            }

            fn len(&self) -> usize {
                self.format1.len() + usize::from(!self.format2.is_empty())
            }

            fn reduce(&mut self) {
                if self.len() <= 1 {
                    return;
                }

                //TODO: there is an optimization here where we preserve two
                //(and possibly three?) format1 tables if format2 does not already exist

                let SubtableMap { format1, format2 } = self;
                format1.retain(|_delta, pairs| {
                    if pairs.len() < N_GLYPHS_TO_JUSTIFY_EXTRA_SUB1F1 {
                        format2.extend(pairs.iter().copied());
                        false
                    } else {
                        true
                    }
                })
            }

            fn build(mut self) -> Vec<write_gsub::SingleSubst> {
                let mut result = Vec::with_capacity(self.len());
                if !self.format2.is_empty() {
                    self.format2.sort_unstable();
                    let coverage = self.format2.iter().copied().map(|(g1, _)| g1).collect();
                    let subs = self.format2.into_iter().map(|(_, g2)| g2).collect();
                    result.push(write_gsub::SingleSubst::format_2(coverage, subs));
                }

                for (delta, pairs) in self.format1 {
                    let coverage = pairs.into_iter().map(|(g1, _)| g1).collect();
                    result.push(write_gsub::SingleSubst::format_1(coverage, delta));
                }
                result
            }
        }

        // optimal subtable generation:
        // - sort all pairs into their 'preferred' subtables (everything that
        // can be in a format 1 table is)
        // - go through the format1 tables and move small ones into the format 2 table

        let mut map = SubtableMap::from_builder(self);
        map.reduce();
        map.build()
    }
}

#[derive(Clone, Debug, Default)]
pub struct MultipleSubBuilder {
    items: BTreeMap<GlyphId, Vec<GlyphId>>,
}

impl Builder for MultipleSubBuilder {
    type Output = Vec<write_gsub::MultipleSubstFormat1>;

    fn build(self, _: &mut VariationStoreBuilder) -> Self::Output {
        let coverage = self.items.keys().copied().collect();
        let seq_tables = self
            .items
            .into_values()
            .map(write_gsub::Sequence::new)
            .collect();
        vec![write_gsub::MultipleSubstFormat1::new(coverage, seq_tables)]
    }
}

impl MultipleSubBuilder {
    pub fn insert(&mut self, target: GlyphId, replacement: Vec<GlyphId>) {
        self.items.insert(target, replacement);
    }

    pub fn contains_target(&self, target: GlyphId) -> bool {
        self.items.contains_key(&target)
    }
}

#[derive(Clone, Debug, Default)]
pub struct AlternateSubBuilder {
    items: BTreeMap<GlyphId, Vec<GlyphId>>,
}

impl AlternateSubBuilder {
    pub fn insert(&mut self, target: GlyphId, replacement: Vec<GlyphId>) {
        self.items.insert(target, replacement);
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    // used when compiling aalt
    pub(crate) fn iter_pairs(&self) -> impl Iterator<Item = (GlyphId, GlyphId)> + '_ {
        self.items
            .iter()
            .flat_map(|(target, alt)| alt.iter().map(|alt| (*target, *alt)))
    }
}

impl Builder for AlternateSubBuilder {
    type Output = Vec<write_gsub::AlternateSubstFormat1>;

    fn build(self, _: &mut VariationStoreBuilder) -> Self::Output {
        let coverage = self.items.keys().copied().collect();
        let seq_tables = self
            .items
            .into_values()
            .map(write_gsub::AlternateSet::new)
            .collect();
        vec![write_gsub::AlternateSubstFormat1::new(coverage, seq_tables)]
    }
}

#[derive(Clone, Debug, Default)]
pub struct LigatureSubBuilder {
    items: BTreeMap<GlyphId, Vec<(Vec<GlyphId>, GlyphId)>>,
}

impl LigatureSubBuilder {
    pub fn insert(&mut self, target: Vec<GlyphId>, replacement: GlyphId) {
        let mut iter = target.into_iter();
        let first = iter.next().unwrap();
        let rest = iter.collect::<Vec<_>>();
        self.items
            .entry(first)
            .or_default()
            .push((rest, replacement));
    }

    pub fn contains_target(&self, target: GlyphId) -> bool {
        //FIXME: we could be more aggressive here, but for now we will force a new
        //lookup anytime the target exists? idk
        self.items.contains_key(&target)
    }
}

impl Builder for LigatureSubBuilder {
    type Output = Vec<write_gsub::LigatureSubstFormat1>;

    fn build(self, _: &mut VariationStoreBuilder) -> Self::Output {
        let coverage = self.items.keys().copied().collect();
        let lig_sets = self
            .items
            .into_values()
            .map(|mut ligs| {
                // we want to sort longer items first, but otherwise preserve
                // the order provided by the user.
                ligs.sort_by_key(|(lig, _)| std::cmp::Reverse(lig.len()));
                write_gsub::LigatureSet::new(
                    ligs.into_iter()
                        .map(|(components, lig_glyph)| {
                            write_gsub::Ligature::new(lig_glyph, components)
                        })
                        .collect(),
                )
            })
            .collect();

        vec![write_gsub::LigatureSubstFormat1::new(coverage, lig_sets)]
    }
}
