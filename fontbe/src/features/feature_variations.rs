//! OpenType [Feature Variations][]
//!
//! [Feature Variations]: https://learn.microsoft.com/en-us/typography/opentype/spec/chapter2#feature-variations

use std::collections::{BTreeMap, HashMap};

use fea_rs::compile::{FeatureBuilder, FeatureProvider, PendingLookup};
use fontdrasil::types::GlyphName;
use fontir::{
    feature_variations::{NBox, Region},
    ir::{GlyphOrder, StaticMetadata, VariableFeature},
};
use write_fonts::{
    tables::{gsub::builders::SingleSubBuilder, layout::ConditionSet},
    types::Tag,
};

use crate::error::Error;

const RVRN: Tag = Tag::new(b"rvrn");

pub(super) struct FeatureVariationsProvider {
    tags: Vec<Tag>,
    lookups: Vec<PendingLookup<SingleSubBuilder>>,
    // the vec is indices into `lookups` for that conditionset
    conditions: Vec<(ConditionSet, Vec<usize>)>,
}

// https://github.com/fonttools/fonttools/blob/a1c189bda7c8d970143f92b542f5e26759945a37/Lib/fontTools/varLib/__init__.py#L799
pub(super) fn make_gsub_feature_variations(
    ir_variations: &VariableFeature,
    static_metadata: &StaticMetadata,
    glyph_order: &GlyphOrder,
) -> Result<FeatureVariationsProvider, Error> {
    let mut conditional_subs = Vec::new();

    for rule in &ir_variations.rules {
        let mut region = Region::default();
        for conditions in &rule.conditions {
            let mut space = NBox::default();
            for condition in conditions {
                let axis = static_metadata
                    .axis(&condition.axis)
                    .expect("checked already");
                let min = condition.min.map(|min| min.to_normalized(&axis.converter));
                let max = condition.max.map(|max| max.to_normalized(&axis.converter));
                space.insert(condition.axis, min, max);
            }
            region.push(std::mem::take(&mut space));
        }
        let substitutions = rule
            .substitutions
            .iter()
            .map(|ir_sub| (ir_sub.replace.clone(), ir_sub.with.clone()))
            .collect::<BTreeMap<_, _>>();

        conditional_subs.push((region, substitutions));
    }
    let substitutions = fontir::feature_variations::overlay_feature_variations(conditional_subs);
    let (lookups, lookup_map) = make_substitution_lookups(&substitutions, glyph_order);

    let conditions = substitutions
        .iter()
        .map(|(cond_set, subs)| {
            let mut indices = subs
                .iter()
                .map(|subs| lookup_map.get(&subs).copied().unwrap())
                .collect::<Vec<_>>();
            indices.sort();
            let condition_set = cond_set.to_condition_set(static_metadata);
            (condition_set, indices)
        })
        .collect::<Vec<_>>();

    Ok(FeatureVariationsProvider {
        tags: ir_variations.features.clone(),
        lookups,
        conditions,
    })
}

/// returns a vec of lookups, and a map from the raw lookups to the indices in the vec.
#[allow(clippy::type_complexity)] // ugly but only used in one place
fn make_substitution_lookups<'a>(
    subs: &'a [(NBox, Vec<BTreeMap<GlyphName, GlyphName>>)],
    glyph_order: &GlyphOrder, // used to match fonttools sort order for generated lookups
) -> (
    Vec<PendingLookup<SingleSubBuilder>>,
    HashMap<&'a BTreeMap<GlyphName, GlyphName>, usize>,
) {
    fn make_single_sub_lookup(
        subs: &BTreeMap<GlyphName, GlyphName>,
        glyph_order: &GlyphOrder,
    ) -> PendingLookup<SingleSubBuilder> {
        let mut builder = SingleSubBuilder::default();
        for (target, replacement) in subs.iter() {
            let target = glyph_order.glyph_id(target).unwrap();
            let replacement = glyph_order.glyph_id(replacement).unwrap();
            builder.insert(target, replacement);
        }
        PendingLookup::new(vec![builder], Default::default(), None)
    }

    let mut lookups = Vec::new();
    let mut lookup_map = HashMap::new();

    // we create an intermediate vec here so we can match fontmake's sorting for
    // the lookups, which is based on glyph name.
    let mut sub_rules = subs.iter().flat_map(|x| x.1.iter()).collect::<Vec<_>>();
    sub_rules.sort();
    sub_rules.dedup();
    for sub_rules in sub_rules {
        if lookup_map.contains_key(sub_rules) {
            continue;
        }

        lookup_map.insert(sub_rules, lookups.len());
        let lookup = make_single_sub_lookup(sub_rules, glyph_order);
        lookups.push(lookup)
    }
    (lookups, lookup_map)
}

impl FeatureProvider for FeatureVariationsProvider {
    fn add_features(&self, builder: &mut FeatureBuilder) {
        // if only one feature and it is rvrn, add lookups to front of lookup list
        let add_lookups_at_front = matches!(self.tags.as_slice(), &[RVRN]);
        let lookup_ids = self
            .lookups
            .iter()
            .map(|lk| builder.add_lookup(lk.to_owned().at_front_of_list(add_lookups_at_front)))
            .collect::<Vec<_>>();

        let conditions = self
            .conditions
            .iter()
            .map(|(condset, lookups)| {
                (
                    condset.to_owned(),
                    lookups.iter().map(|id| lookup_ids[*id]).collect::<Vec<_>>(),
                )
            })
            .collect::<Vec<_>>();
        builder.add_feature_variations(self.tags.clone(), conditions);
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::types::Axes;
    use fontir::ir::Rule;
    use write_fonts::tables::{gsub::Gsub, layout::FeatureVariations};

    use crate::features::test_helpers::{LayoutOutput, LayoutOutputBuilder};

    use super::*;

    const RCLT: Tag = Tag::new(b"rclt");

    trait FeatureVariationsOutput {
        fn compile_feature_variations(&self, variations: VariableFeature) -> Gsub;
    }

    impl FeatureVariationsOutput for LayoutOutput {
        fn compile_feature_variations(&self, variations: VariableFeature) -> Gsub {
            let thingie =
                make_gsub_feature_variations(&variations, &self.static_metadata, &self.glyph_order)
                    .unwrap();
            self.compile(&thingie).gsub.unwrap()
        }
    }

    fn simple_feature_variations(feature: Tag) -> FeatureVariations {
        let variations = VariableFeature {
            features: vec![feature],
            rules: vec![Rule::for_test(
                &[&[("wght", (600.0, 700.0))]],
                &[("a", "a.bracket600")],
            )],
        };

        let gsub = LayoutOutputBuilder::new()
            .with_user_fea(
                r#"
    feature test {
    sub one by two;
    } test;
    "#,
            )
            .with_axes(Axes::for_test(&["wght"]))
            .with_glyph_order(
                ["one", "two", "a", "a.bracket600"]
                    .into_iter()
                    .map(GlyphName::new)
                    .collect(),
            )
            .build()
            .compile_feature_variations(variations);
        gsub.feature_variations.into_inner().unwrap()
    }

    #[test]
    fn insert_at_front() {
        let featvar = simple_feature_variations(RVRN);
        assert_eq!(featvar.feature_variation_records.len(), 1);
        let feat_sub = featvar.feature_variation_records[0]
            .feature_table_substitution
            .as_ref()
            .unwrap();
        let alt_feature = feat_sub.substitutions[0].alternate_feature.as_ref();
        assert_eq!(alt_feature.lookup_list_indices, [0]);
    }

    #[test]
    fn insert_at_back() {
        let featvar = simple_feature_variations(RCLT);
        assert_eq!(featvar.feature_variation_records.len(), 1);
        let feat_sub = featvar.feature_variation_records[0]
            .feature_table_substitution
            .as_ref()
            .unwrap();
        let alt_feature = feat_sub.substitutions[0].alternate_feature.as_ref();
        assert_eq!(alt_feature.lookup_list_indices, [1]);
    }
}
