//! OpenType [Feature Variations][]
//!
//! [Feature Variations]: https://learn.microsoft.com/en-us/typography/opentype/spec/chapter2#feature-variations

use std::{
    collections::{BTreeMap, HashSet},
    fmt::Debug,
};

use crate::ir::StaticMetadata;
use fontdrasil::{coords::NormalizedCoord, types::GlyphName};
use indexmap::IndexMap;
use write_fonts::{
    tables::layout::{ConditionFormat1, ConditionSet},
    types::{F2Dot14, Tag},
};

/// A rectilinear region of an n-dimensional designspace.
///
/// Omitted axes are interpretted as being fully included in the box.
#[derive(Clone, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NBox(BTreeMap<Tag, (NormalizedCoord, NormalizedCoord)>);

/// A subset of a designspace.
///
/// A region is the union of all the member boxes.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Region(Vec<NBox>);

impl NBox {
    pub fn insert(
        &mut self,
        axis: Tag,
        min: Option<NormalizedCoord>,
        max: Option<NormalizedCoord>,
    ) {
        let min = min
            .unwrap_or(NormalizedCoord::MIN)
            .max(NormalizedCoord::MIN);
        let max = max
            .unwrap_or(NormalizedCoord::MAX)
            .min(NormalizedCoord::MAX);
        self.0.insert(axis, (min, max));
    }

    pub fn to_condition_set(&self, static_meta: &StaticMetadata) -> ConditionSet {
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

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (Tag, (NormalizedCoord, NormalizedCoord))> + use<'_> {
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
        if fully_inside {
            // other is fully inside self
            (Some(intersection), None)
        } else {
            (Some(intersection), Some(remainder))
        }
    }
}

impl Region {
    pub fn push(&mut self, box_: NBox) {
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
pub fn overlay_feature_variations(
    conditional_subs: Vec<(Region, BTreeMap<GlyphName, GlyphName>)>,
) -> Vec<(NBox, Vec<BTreeMap<GlyphName, GlyphName>>)> {
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
    conditional_substitutions: Vec<(Region, BTreeMap<GlyphName, GlyphName>)>,
) -> Vec<(Region, BTreeMap<GlyphName, GlyphName>)> {
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
    conditional_substitutions: Vec<(Region, BTreeMap<GlyphName, GlyphName>)>,
) -> Vec<(Region, BTreeMap<GlyphName, GlyphName>)> {
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

impl From<Vec<NBox>> for Region {
    fn from(src: Vec<NBox>) -> Region {
        Region(src)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

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
        subs: &[(&str, &str)],
    ) -> (Region, BTreeMap<GlyphName, GlyphName>) {
        (
            Region(region.iter().map(|nbox| NBox::for_test(nbox)).collect()),
            subs.iter()
                .map(|(a, b)| ((*a).into(), (*b).into()))
                .collect(),
        )
    }

    fn match_condition<'a>(
        location: &[(&str, f64)],
        overlaps: &'a [(NBox, Vec<BTreeMap<GlyphName, GlyphName>>)],
    ) -> Vec<(&'a str, &'a str)> {
        for (nbox, substitutions) in overlaps {
            for (tag, coord) in location {
                let tag = Tag::from_str(tag).unwrap();
                let (start, end) = nbox.get(tag);
                if start.to_f64() <= *coord && end.to_f64() >= *coord {
                    let mut merged = substitutions
                        .iter()
                        .flat_map(|sub_set| sub_set.iter())
                        .map(|(a, b)| (a.as_str(), b.as_str()))
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
            make_overlay_input(&[&[("abcd", (0.4, 0.9))]], &[("0", "0")]),
            make_overlay_input(&[&[("abcd", (0.5, 1.))]], &[("1", "1")]),
            make_overlay_input(&[&[("abcd", (0., 0.8))]], &[("2", "2")]),
            make_overlay_input(&[&[("abcd", (0.3, 0.7))]], &[("3", "3")]),
        ];

        let overlaps = overlay_feature_variations(conds);
        //dbg!(&overlaps);
        assert_eq!(match_condition(&[("abcd", 0.)], &overlaps), [("2", "2")]);
        assert_eq!(match_condition(&[("abcd", 0.1)], &overlaps), [("2", "2")]);
        assert_eq!(
            match_condition(&[("abcd", 0.3)], &overlaps),
            [("2", "2"), ("3", "3")]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.4)], &overlaps),
            [("0", "0"), ("2", "2"), ("3", "3")]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.5)], &overlaps),
            [("0", "0"), ("1", "1"), ("2", "2"), ("3", "3")]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.7)], &overlaps),
            [("0", "0"), ("1", "1"), ("2", "2"), ("3", "3")]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.8)], &overlaps),
            [("0", "0"), ("1", "1"), ("2", "2"),]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.9)], &overlaps),
            [("0", "0"), ("1", "1")]
        );
        assert_eq!(match_condition(&[("abcd", 1.0)], &overlaps), [("1", "1")]);
    }

    //https://github.com/fonttools/fonttools/blob/1c2704dfc79d/Tests/varLib/featureVars_test.py#L245
    #[test]
    fn overlaps_2() {
        let conds = vec![
            make_overlay_input(&[&[("abcd", (0.1, 0.9))]], &[("0", "0")]),
            make_overlay_input(&[&[("abcd", (0.8, 1.))]], &[("1", "1")]),
            make_overlay_input(&[&[("abcd", (0.3, 0.4))]], &[("2", "2")]),
            make_overlay_input(&[&[("abcd", (0.1, 1.0))]], &[("3", "3")]),
        ];
        let overlaps = overlay_feature_variations(conds);
        assert_eq!(match_condition(&[("abcd", 0.0)], &overlaps), []);
        assert_eq!(
            match_condition(&[("abcd", 0.1)], &overlaps),
            [("0", "0"), ("3", "3")]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.2)], &overlaps),
            [("0", "0"), ("3", "3")]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.3)], &overlaps),
            [("0", "0"), ("2", "2"), ("3", "3")]
        );
        assert_eq!(
            match_condition(&[("abcd", 0.5)], &overlaps),
            [("0", "0"), ("3", "3")]
        );
        assert_eq!(
            match_condition(&[("abcd", 1.0)], &overlaps),
            [("1", "1"), ("3", "3")]
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
        assert_eq!(remainder, Some(NBox::default()));
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

    // it's possible that design coordinates are out of bounds, which leads to
    // out of bounds normalized coordinates:
    #[test]
    fn box_clamps() {
        let mut nbox = NBox::default();
        nbox.insert(
            Tag::new(b"derp"),
            Some(NormalizedCoord::new(-45.0)),
            Some(NormalizedCoord::new(101.)),
        );

        assert_eq!(nbox, NBox::for_test(&[("derp", (-1.0, 1.0))]))
    }
}
