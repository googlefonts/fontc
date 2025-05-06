//! OpenType [Feature Variations][]
//!
//! [Feature Variations]: https://learn.microsoft.com/en-us/typography/opentype/spec/chapter2#feature-variations

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Debug,
};

use fea_rs::compile::{FeatureBuilder, FeatureProvider, PendingLookup};
use fontdrasil::{coords::NormalizedCoord, types::GlyphName};
use fontir::ir::{GlyphOrder, StaticMetadata, VariableFeature};
use indexmap::IndexMap;
use write_fonts::{
    tables::{
        gsub::builders::SingleSubBuilder,
        layout::{ConditionFormat1, ConditionSet},
    },
    types::{F2Dot14, GlyphId16, Tag},
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
    let get_gid_or_err = |name: &GlyphName| -> Result<GlyphId16, Error> {
        glyph_order.glyph_id(name).ok_or_else(|| {
            Error::GlyphError(name.clone(), crate::error::GlyphProblem::NotInGlyphOrder)
        })
    };

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
            .map(|ir_sub| {
                get_gid_or_err(&ir_sub.replace)
                    .and_then(|a| get_gid_or_err(&ir_sub.with).map(|b| (a, b)))
            })
            .collect::<Result<BTreeMap<_, _>, _>>()?;

        conditional_subs.push((region, substitutions));
    }
    let substitutions = overlay_feature_variations(conditional_subs);
    let (lookups, lookup_map) = make_substitution_lookups(&substitutions);

    let conditions = substitutions
        .iter()
        .map(|(cond_set, subs)| {
            let indices = subs
                .iter()
                .map(|subs| lookup_map.get(&subs).copied().unwrap())
                .collect::<Vec<_>>();
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
fn make_substitution_lookups(
    subs: &[(NBox, Vec<BTreeMap<GlyphId16, GlyphId16>>)],
) -> (
    Vec<PendingLookup<SingleSubBuilder>>,
    HashMap<&BTreeMap<GlyphId16, GlyphId16>, usize>,
) {
    fn make_single_sub_lookup(
        subs: &BTreeMap<GlyphId16, GlyphId16>,
    ) -> PendingLookup<SingleSubBuilder> {
        let mut builder = SingleSubBuilder::default();
        for (target, replacement) in subs.iter() {
            builder.insert(*target, *replacement);
        }
        PendingLookup::new(vec![builder], Default::default(), None)
    }

    let mut lookups = Vec::new();
    let mut lookup_map = HashMap::new();

    for sub_rules in subs.iter().flat_map(|(_, subs)| subs.iter()) {
        if lookup_map.contains_key(sub_rules) {
            continue;
        }

        lookup_map.insert(sub_rules, lookups.len());
        let lookup = make_single_sub_lookup(sub_rules);
        lookups.push(lookup)
    }
    (lookups, lookup_map)
}

/// A rectilinear region of an n-dimensional designspace.
///
/// Omitted axes are interpretted as being fully included in the box.
#[derive(Clone, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct NBox(BTreeMap<Tag, (NormalizedCoord, NormalizedCoord)>);

/// A subset of a designspace.
///
/// A region is the union of all the member boxes.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub(super) struct Region(Vec<NBox>);

impl NBox {
    pub(super) fn insert(
        &mut self,
        axis: Tag,
        min: Option<NormalizedCoord>,
        max: Option<NormalizedCoord>,
    ) {
        let min = min.unwrap_or(NormalizedCoord::new(-1.0));
        let max = max.unwrap_or(NormalizedCoord::new(1.0));
        self.0.insert(axis, (min, max));
    }

    pub(super) fn to_condition_set(&self, static_meta: &StaticMetadata) -> ConditionSet {
        let conditions = self
            .0
            .iter()
            .map(|(tag, (min, max))| {
                let axis_index = static_meta
                    .axes
                    .iter()
                    .position(|a| a.tag == *tag)
                    .expect("should be checked before now");
                let min = F2Dot14::from_f32(min.to_f64() as _);
                let max = F2Dot14::from_f32(max.to_f64() as _);
                ConditionFormat1::new(axis_index.try_into().unwrap(), min, max).into()
            })
            .collect();
        ConditionSet::new(conditions)
    }

    /// Remove any axes that have default values.
    ///
    /// <https://github.com/fonttools/fonttools/blob/1c2704dfc79d7/Lib/fontTools/varLib/featureVars.py#L332>
    fn cleanup(&mut self) {
        self.0
            .retain(|_, (min, max)| (min.to_f64(), max.to_f64()) != (-1.0, 1.0));
    }

    fn get(&self, axis: Tag) -> (NormalizedCoord, NormalizedCoord) {
        self.0
            .get(&axis)
            .copied()
            .unwrap_or((NormalizedCoord::MIN, NormalizedCoord::MAX))
    }

    fn iter(&self) -> impl Iterator<Item = (Tag, (NormalizedCoord, NormalizedCoord))> + use<'_> {
        self.0.iter().map(|(k, (min, max))| (*k, (*min, *max)))
    }

    /// Overlay `self` onto `other`.
    ///
    /// Returns two new boxes: the intersection (or None, if boxes don't intersect)
    /// and the 'remainder' (none, if `other` is fully withing `self`).
    ///
    /// <https://github.com/fonttools/fonttools/blob/1c2704dfc79d7/Lib/fontTools/varLib/featureVars.py#L247>
    fn overlay_onto(&self, other: &NBox) -> (Option<NBox>, Option<NBox>) {
        let mut intersection: NBox = self.iter().chain(other.iter()).collect();

        let intersection_axes = self
            .0
            .keys()
            .filter(|k| other.0.contains_key(*k))
            .collect::<HashSet<_>>();
        for axis in intersection_axes {
            let (min1, max1) = self.get(*axis);
            let (min2, max2) = other.get(*axis);
            let min = min1.max(min2);
            let max = max1.min(max2);

            if min >= max {
                // no intersection
                return (None, Some(other.clone()));
            }
            intersection.insert(*axis, Some(min), Some(max));
        }

        // remainder:
        let mut remainder = other.clone();

        let mut extruding = false;
        let mut fully_inside = true;

        // https://github.com/fonttools/fonttools/blob/1c2704dfc79d753f/Lib/fontTools/varLib/featureVars.py#L285
        if self.0.keys().any(|k| !other.0.contains_key(k)) {
            extruding = true;
            fully_inside = false;
        }

        for axis in other.0.keys() {
            if !self.0.contains_key(axis) {
                continue;
            }

            let (min1, max1) = intersection.get(*axis);
            let (min2, max2) = other.get(*axis);

            if min1 <= min2 && max2 <= max1 {
                continue;
            }

            // this is an overlap.

            if extruding {
                // if there was a previous overlap then remainder is not representable
                // as a box; just return the original.
                return (Some(intersection), Some(other.clone()));
            }

            extruding = true;
            fully_inside = false;

            // otherwise cut the remainder on this axis
            let (min, max) = if min1 <= min2 {
                // cut left side
                (max1.max(min2), max2)
            } else if max2 <= max1 {
                // cut right side
                (min2, min1.min(max2))
            } else {
                // remainder extends on both sides: give up
                return (Some(intersection), Some(other.clone()));
            };

            remainder.insert(*axis, Some(min), Some(max));
        }
        if fully_inside || remainder.0.is_empty() {
            // other is fully inside self
            (Some(intersection), None)
        } else {
            (Some(intersection), Some(remainder))
        }
    }
}

impl Region {
    pub(super) fn push(&mut self, box_: NBox) {
        self.0.push(box_);
    }

    /// cleanup and sort boxes
    fn cleanup_and_normalize(&mut self) {
        for box_ in &mut self.0 {
            box_.cleanup();
        }
        self.0.sort();
    }
}

/// <https://github.com/fonttools/fonttools/blob/08962727756/Lib/fontTools/varLib/featureVars.py#L122>
pub(super) fn overlay_feature_variations(
    conditional_subs: Vec<(Region, BTreeMap<GlyphId16, GlyphId16>)>,
) -> Vec<(NBox, Vec<BTreeMap<GlyphId16, GlyphId16>>)> {
    // preflight stuff:
    let conditional_subs = merge_same_sub_rules(conditional_subs);
    let conditional_subs = merge_same_region_rules(conditional_subs);

    // overlay logic
    // Rank is the bit-set of the index of all contributing layers:
    // https://github.com/fonttools/fonttools/blob/1c2704dfc7/Lib/fontTools/varLib/featureVars.py#L201
    fn init_map() -> IndexMap<NBox, u64> {
        IndexMap::from([(NBox::default(), 0)])
    }

    let mut boxmap = init_map();
    for (i, (cur_region, _)) in conditional_subs.iter().enumerate() {
        let cur_rank = 1u64 << i;
        for (box_, rank) in std::mem::replace(&mut boxmap, init_map()) {
            for cur_box in &cur_region.0 {
                let (intersection, remainder) = cur_box.overlay_onto(&box_);
                if let Some(intersection) = intersection {
                    *boxmap.entry(intersection).or_insert(0) |= rank | cur_rank;
                }
                if let Some(remainder) = remainder {
                    *boxmap.entry(remainder).or_insert(0) |= rank;
                }
            }
        }
    }

    let mut items = Vec::new();
    let mut sorted = boxmap.into_iter().collect::<Vec<_>>();
    sorted.sort_by_key(|(_, rank)| rank.count_zeros());
    for (box_, mut rank) in sorted {
        if rank == 0 {
            continue;
        }

        let mut substs_list = Vec::new();
        let mut i = 0;
        while rank != 0 {
            if (rank & 1) != 0 {
                substs_list.push(conditional_subs[i].1.clone())
            }
            rank >>= 1;
            i += 1;
        }
        items.push((box_, substs_list))
    }
    items
}

//https://github.com/fonttools/fonttools/blob/1c2704dfc/Lib/fontTools/varLib/featureVars.py#L168
fn merge_same_sub_rules(
    conditional_substitutions: Vec<(Region, BTreeMap<GlyphId16, GlyphId16>)>,
) -> Vec<(Region, BTreeMap<GlyphId16, GlyphId16>)> {
    let mut merged = IndexMap::new();
    for (region, subs) in conditional_substitutions {
        match merged.entry(subs) {
            indexmap::map::Entry::Vacant(entry) => {
                entry.insert(region);
            }
            indexmap::map::Entry::Occupied(mut entry) => {
                entry.get_mut().0.extend_from_slice(&region.0)
            }
        }
    }
    merged.into_iter().map(|(k, v)| (v, k)).collect()
}

fn merge_same_region_rules(
    conditional_substitutions: Vec<(Region, BTreeMap<GlyphId16, GlyphId16>)>,
) -> Vec<(Region, BTreeMap<GlyphId16, GlyphId16>)> {
    let mut merged = IndexMap::new();
    // we add in reverse order here because we want earlier rules to overwrite
    // later rules
    for (mut region, subs) in conditional_substitutions.into_iter().rev() {
        region.cleanup_and_normalize();

        match merged.entry(region) {
            indexmap::map::Entry::Vacant(entry) => {
                entry.insert(subs);
            }
            indexmap::map::Entry::Occupied(mut entry) => {
                entry.get_mut().extend(subs);
            }
        }
    }
    merged.into_iter().rev().collect()
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

impl FromIterator<(Tag, (NormalizedCoord, NormalizedCoord))> for NBox {
    fn from_iter<T: IntoIterator<Item = (Tag, (NormalizedCoord, NormalizedCoord))>>(
        iter: T,
    ) -> Self {
        NBox(iter.into_iter().collect())
    }
}

impl Debug for NBox {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("NBox(")?;
        f.debug_map()
            .entries(
                self.0
                    .iter()
                    .map(|(k, v)| (k, (v.0.to_f64(), v.1.to_f64()))),
            )
            .finish()?;
        f.write_str(")")
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use fontdrasil::{coords::DesignCoord, types::Axis};
    use fontir::ir::{Condition, Rule, Substitution};
    use write_fonts::tables::{gsub::Gsub, layout::FeatureVariations};

    use crate::features::test_helpers::{LayoutOutput, LayoutOutputBuilder};

    use super::*;

    impl NBox {
        fn for_test(things: &[(&str, (f64, f64))]) -> Self {
            NBox(
                things
                    .iter()
                    .map(|(tag, (min, max))| {
                        (
                            Tag::from_str(tag).unwrap(),
                            (NormalizedCoord::new(*min), NormalizedCoord::new(*max)),
                        )
                    })
                    .collect(),
            )
        }
    }

    fn make_overlay_input(
        region: &[&[(&str, (f64, f64))]],
        subs: &[(u16, u16)],
    ) -> (Region, BTreeMap<GlyphId16, GlyphId16>) {
        (
            Region(region.iter().map(|nbox| NBox::for_test(nbox)).collect()),
            subs.iter()
                .map(|(a, b)| (GlyphId16::new(*a), GlyphId16::new(*b)))
                .collect(),
        )
    }

    fn match_condition(
        location: &[(&str, f64)],
        overlaps: &[(NBox, Vec<BTreeMap<GlyphId16, GlyphId16>>)],
    ) -> Vec<(u16, u16)> {
        for (nbox, substitutions) in overlaps {
            for (tag, coord) in location {
                let tag = Tag::from_str(tag).unwrap();
                let (start, end) = nbox.get(tag);
                if start.to_f64() <= *coord && end.to_f64() >= *coord {
                    let mut merged = substitutions
                        .iter()
                        .flat_map(|sub_set| sub_set.iter())
                        .map(|(a, b)| (a.to_u16(), b.to_u16()))
                        .collect::<Vec<_>>();
                    merged.sort();
                    return merged;
                }
            }
        }
        Vec::new()
    }

    // https://github.com/fonttools/fonttools/blob/1c2704dfc79d/Tests/varLib/featureVars_test.py#L216
    #[test]
    fn overlaps_1() {
        let conds = vec![
            make_overlay_input(&[&[("abcd", (0.4, 0.9))]], &[(0, 0)]),
            make_overlay_input(&[&[("abcd", (0.5, 1.))]], &[(1, 1)]),
            make_overlay_input(&[&[("abcd", (0., 0.8))]], &[(2, 2)]),
            make_overlay_input(&[&[("abcd", (0.3, 0.7))]], &[(3, 3)]),
        ];

        let overlaps = overlay_feature_variations(conds);
        //dbg!(&overlaps);
        assert_eq!(match_condition(&[("abcd", 0.)], &overlaps), [(2, 2)]);
        assert_eq!(match_condition(&[("abcd", 0.1)], &overlaps), [(2, 2)]);
        assert_eq!(
            match_condition(&[("abcd", 0.3)], &overlaps),
            [(2, 2), (3, 3)]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.4)], &overlaps),
            [(0, 0), (2, 2), (3, 3)]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.5)], &overlaps),
            [(0, 0), (1, 1), (2, 2), (3, 3)]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.7)], &overlaps),
            [(0, 0), (1, 1), (2, 2), (3, 3)]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.8)], &overlaps),
            [(0, 0), (1, 1), (2, 2),]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.9)], &overlaps),
            [(0, 0), (1, 1)]
        );
        assert_eq!(match_condition(&[("abcd", 1.0)], &overlaps), [(1, 1)]);
    }

    //https://github.com/fonttools/fonttools/blob/1c2704dfc79d/Tests/varLib/featureVars_test.py#L245
    #[test]
    fn overlaps_2() {
        let conds = vec![
            make_overlay_input(&[&[("abcd", (0.1, 0.9))]], &[(0, 0)]),
            make_overlay_input(&[&[("abcd", (0.8, 1.))]], &[(1, 1)]),
            make_overlay_input(&[&[("abcd", (0.3, 0.4))]], &[(2, 2)]),
            make_overlay_input(&[&[("abcd", (0.1, 1.0))]], &[(3, 3)]),
        ];
        let overlaps = overlay_feature_variations(conds);
        assert_eq!(match_condition(&[("abcd", 0.0)], &overlaps), []);
        assert_eq!(
            match_condition(&[("abcd", 0.1)], &overlaps),
            [(0, 0), (3, 3)]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.2)], &overlaps),
            [(0, 0), (3, 3)]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.3)], &overlaps),
            [(0, 0), (2, 2), (3, 3)]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.5)], &overlaps),
            [(0, 0), (3, 3)]
        );
        assert_eq!(
            match_condition(&[("abcd", 1.0)], &overlaps),
            [(1, 1), (3, 3)]
        );
    }

    //https://github.com/fonttools/fonttools/blob/1c2704dfc79d753fe822dc7699b067495c0edede/Tests/varLib/featureVars_test.py#L269
    #[test]
    fn overlay_box() {
        let top = NBox::for_test(&[("opsz", (0.75, 1.0)), ("wght", (0.5, 1.0))]);
        let bottom = NBox::for_test(&[("wght", (0.25, 1.0))]);

        let (intersection, remainder) = top.overlay_onto(&bottom);
        assert_eq!(intersection, Some(top));
        assert_eq!(remainder, Some(bottom));
    }

    #[test]
    fn overlay_onto_empty() {
        let top = NBox::for_test(&[("wght", (0.5, 0.6))]);
        let bottom = NBox::default();
        let (intersection, remainder) = top.overlay_onto(&bottom);
        assert_eq!(intersection, Some(top));
        assert!(remainder.is_none(), "{remainder:?}");
    }

    #[test]
    fn overlay_overlap_both() {
        let top = NBox::for_test(&[("wght", (0.5, 0.6))]);
        let bottom = NBox::for_test(&[("wght", (0.1, 0.8))]);

        let (intersection, remainder) = top.overlay_onto(&bottom);
        assert_eq!(intersection, Some(top));
        assert_eq!(remainder, Some(bottom));
    }

    #[test]
    fn overlay_overlap_left() {
        let top = NBox::for_test(&[("wght", (0.5, 0.6))]);
        let bottom = NBox::for_test(&[("wght", (0.1, 0.55))]);

        let (intersection, remainder) = top.overlay_onto(&bottom);
        assert_eq!(intersection, Some(NBox::for_test(&[("wght", (0.5, 0.55))])));
        assert_eq!(remainder, Some(NBox::for_test(&[("wght", (0.1, 0.5))])));
    }

    #[test]
    fn overlay_overlap_right() {
        let top = NBox::for_test(&[("wght", (0.5, 0.6))]);
        let bottom = NBox::for_test(&[("wght", (0.55, 0.8))]);

        let (intersection, remainder) = top.overlay_onto(&bottom);
        assert_eq!(intersection, Some(NBox::for_test(&[("wght", (0.55, 0.6))])));
        assert_eq!(remainder, Some(NBox::for_test(&[("wght", (0.6, 0.8))])));
    }

    const RCLT: Tag = Tag::new(b"rclt");

    fn make_rule(condition_sets: &[&[(&str, (f64, f64))]], subs: &[(&str, &str)]) -> Rule {
        Rule {
            conditions: condition_sets
                .iter()
                .map(|cond_set| {
                    cond_set
                        .iter()
                        .map(|(tag, (min, max))| Condition {
                            axis: Tag::from_str(tag).unwrap(),
                            min: Some(DesignCoord::new(*min)),
                            max: Some(DesignCoord::new(*max)),
                        })
                        .collect()
                })
                .collect(),
            substitutions: subs
                .iter()
                .map(|(a, b)| Substitution {
                    replace: GlyphName::new(a),
                    with: GlyphName::new(b),
                })
                .collect(),
        }
    }

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
            rules: vec![make_rule(
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
            .with_axes(vec![Axis::for_test("wght")])
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
