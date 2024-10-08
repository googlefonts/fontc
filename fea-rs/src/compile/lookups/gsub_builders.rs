//! GSUB lookup builders

use std::{collections::BTreeMap, convert::TryFrom};

use write_fonts::{
    tables::{gsub as write_gsub, variations::ivs_builder::VariationStoreBuilder},
    types::GlyphId16,
};

use crate::common::GlyphOrClass;

use super::Builder;

#[derive(Clone, Debug, Default)]
pub struct SingleSubBuilder {
    items: BTreeMap<GlyphId16, GlyphId16>,
}

impl SingleSubBuilder {
    pub fn insert(&mut self, target: GlyphId16, replacement: GlyphId16) {
        self.items.insert(target, replacement);
    }

    pub(crate) fn can_add(&self, target: &GlyphOrClass, replacement: &GlyphOrClass) -> bool {
        for (target, replacement) in target.iter().zip(replacement.iter()) {
            if matches!(self.items.get(&target), Some(x) if *x != replacement) {
                return false;
            }
        }
        true
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    // used when compiling aalt
    pub(crate) fn iter_pairs(&self) -> impl Iterator<Item = (GlyphId16, GlyphId16)> + '_ {
        self.items.iter().map(|(target, alt)| (*target, *alt))
    }

    pub(crate) fn promote_to_multi_sub(self) -> MultipleSubBuilder {
        MultipleSubBuilder {
            items: self
                .items
                .into_iter()
                .map(|(key, gid)| (key, vec![gid]))
                .collect(),
        }
    }
}

impl Builder for SingleSubBuilder {
    type Output = Vec<write_gsub::SingleSubst>;

    fn build(self, _: &mut VariationStoreBuilder) -> Self::Output {
        if self.items.is_empty() {
            return Default::default();
        }
        // if all pairs are equidistant and within the i16 range, find the
        // common delta
        let delta = self
            .items
            .iter()
            .map(|(k, v)| v.to_u16() as i32 - k.to_u16() as i32)
            .reduce(|acc, val| if acc == val { acc } else { i32::MAX })
            .and_then(|delta| i16::try_from(delta).ok());

        let coverage = self.items.keys().copied().collect();
        if let Some(delta) = delta {
            vec![write_gsub::SingleSubst::format_1(coverage, delta)]
        } else {
            let replacements = self.items.values().copied().collect();
            vec![write_gsub::SingleSubst::format_2(coverage, replacements)]
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct MultipleSubBuilder {
    items: BTreeMap<GlyphId16, Vec<GlyphId16>>,
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
    pub fn insert(&mut self, target: GlyphId16, replacement: Vec<GlyphId16>) {
        self.items.insert(target, replacement);
    }

    pub fn can_add(&self, target: GlyphId16, replacement: &[GlyphId16]) -> bool {
        match self.items.get(&target) {
            None => true,
            Some(thing) => thing == replacement,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct AlternateSubBuilder {
    items: BTreeMap<GlyphId16, Vec<GlyphId16>>,
}

impl AlternateSubBuilder {
    pub fn insert(&mut self, target: GlyphId16, replacement: Vec<GlyphId16>) {
        self.items.insert(target, replacement);
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    // used when compiling aalt
    pub(crate) fn iter_pairs(&self) -> impl Iterator<Item = (GlyphId16, GlyphId16)> + '_ {
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
    items: BTreeMap<GlyphId16, Vec<(Vec<GlyphId16>, GlyphId16)>>,
}

impl LigatureSubBuilder {
    pub fn insert(&mut self, target: Vec<GlyphId16>, replacement: GlyphId16) {
        let mut iter = target.into_iter();
        let first = iter.next().unwrap();
        let rest = iter.collect::<Vec<_>>();
        self.items
            .entry(first)
            .or_default()
            .push((rest, replacement));
    }

    pub fn contains_target(&self, target: GlyphId16) -> bool {
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
