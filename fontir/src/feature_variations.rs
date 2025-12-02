//! OpenType [Feature Variations][]
//!
//! [Feature Variations]: https://learn.microsoft.com/en-us/typography/opentype/spec/chapter2#feature-variations

use std::{
    collections::{BTreeMap, HashSet},
    fmt::Debug,
    ops::{BitOr, BitOrAssign},
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
            .filter_map(|(tag, (min, max))| {
                let axis_index = static_meta
                    .axes
                    .iter()
                    .position(|a| a.tag == *tag)
                    .expect("should be checked before now");
                let axis = static_meta.axes.iter().nth(axis_index).unwrap();
                let min = F2Dot14::from_f32(min.to_f64() as _);
                let max = F2Dot14::from_f32(max.to_f64() as _);
                let axis_min = axis.min.to_normalized(&axis.converter).to_f2dot14();
                let axis_max = axis.max.to_normalized(&axis.converter).to_f2dot14();

                ((min, max) != (axis_min, axis_max))
                    .then(|| ConditionFormat1::new(axis_index.try_into().unwrap(), min, max).into())
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

/// For emulating the behaviour of a python int, which has arbitrary precision.
///
/// This is necessary because the python code we are emulating does some bit
/// fiddling, and the required number of bits may be larger than a standard word.
///
/// This solution doesn't handle all eventualities; it can count 511 items,
/// and so will fail if there are more than 511 unique positions assigned
/// to bracket layers.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
struct Rank([u128; 4]);

impl Rank {
    const ZERO: Self = Self([0, 0, 0, 0]);
    const MAX: usize = 128 * 4 - 1;

    fn new(val: usize) -> Self {
        let val = val.min(Self::MAX);
        let mut this = Self::default();
        let column = val / 128;
        let bit = val % 128;
        this.0[3 - column] = 1 << bit;
        this
    }

    fn count_zeros(&self) -> u32 {
        self.0.into_iter().map(u128::count_zeros).sum()
    }

    fn first_bit_is_set(&self) -> bool {
        (self.0[3] & 1) > 0
    }

    // we don't need the general case, so just handle this.
    fn right_shift_one(&mut self) {
        let mut carry_bit = 0;
        for val in &mut self.0 {
            let next_carry = *val & 1;
            *val >>= 1;
            *val |= carry_bit << 127;
            carry_bit = next_carry;
        }
    }
}

impl BitOr for Rank {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        let [a1, b1, c1, d1] = self.0;
        let [a2, b2, c2, d2] = rhs.0;
        Self([a1 | a2, b1 | b2, c1 | c2, d1 | d2])
    }
}

impl BitOrAssign for Rank {
    fn bitor_assign(&mut self, rhs: Self) {
        let [a2, b2, c2, d2] = rhs.0;
        self.0[0] |= a2;
        self.0[1] |= b2;
        self.0[2] |= c2;
        self.0[3] |= d2;
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
    fn init_map() -> IndexMap<NBox, Rank> {
        IndexMap::from([(NBox::default(), Default::default())])
    }

    if conditional_subs.len() > Rank::MAX {
        log::warn!(
            "number of unique feature variation regions exceeds what can\n\
            be represented, output may be incorrect"
        );
    }
    // still worth crashing in debug I think?
    debug_assert!(
        conditional_subs.len() <= Rank::MAX,
        "Maximum number of unique substitution regions exceeded"
    );

    let mut boxmap = init_map();
    for (i, (cur_region, _)) in conditional_subs.iter().enumerate() {
        let cur_rank = Rank::new(i);
        for (box_, rank) in std::mem::replace(&mut boxmap, init_map()) {
            for cur_box in &cur_region.0 {
                let (intersection, remainder) = cur_box.overlay_onto(&box_);
                if let Some(intersection) = intersection {
                    *boxmap.entry(intersection).or_default() |= rank | cur_rank;
                }
                if let Some(remainder) = remainder {
                    *boxmap.entry(remainder).or_default() |= rank;
                }
            }
        }
    }

    let mut items = Vec::new();
    let mut sorted = boxmap.into_iter().collect::<Vec<_>>();
    sorted.sort_by_key(|(_, rank)| rank.count_zeros());
    for (box_, mut rank) in sorted {
        if rank == Rank::ZERO {
            continue;
        }

        let mut substs_list = Vec::new();
        let mut i = 0;
        while rank != Rank::ZERO {
            if rank.first_bit_is_set() {
                substs_list.push(conditional_subs[i].1.clone())
            }
            rank.right_shift_one();
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

    #[test]
    fn overlaps_many() {
        // we had an issue where we were using a u64 to store the 'rank' in
        // this computation, and that meant we would overflow when handling more
        // than 63 distinct regions.
        static DUMB_OLE_STRING: &str = "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_+=-<>?:\";'/.,";

        let conds = (0..70usize)
            .map(|i| {
                let f = (i as f64) / 100.0;
                let c = &DUMB_OLE_STRING[i..i + 1];
                make_overlay_input(&[&[("derp", (f, f + 0.5))]], &[(c, c)])
            })
            .collect::<Vec<_>>();

        let _yay_i_dont_panic = overlay_feature_variations(conds);
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

    #[test]
    fn rank_new() {
        assert_eq!(Rank::new(0).0, [0, 0, 0, 1]);
        assert_eq!(Rank::new(1).0, [0, 0, 0, 2]);
        assert_eq!(Rank::new(2).0, [0, 0, 0, 4]);
        assert_eq!(Rank::new(5).0, [0, 0, 0, 1 << 5]);
        assert_eq!(Rank::new(127).0, [0, 0, 0, 1 << 127]);
        assert_eq!(Rank::new(128).0, [0, 0, 1, 0]);
        assert_eq!(Rank::new(129).0, [0, 0, 2, 0]);
        assert_eq!(Rank::new(255).0, [0, 0, 1 << 127, 0]);
        assert_eq!(Rank::new(256).0, [0, 1, 0, 0]);
        assert_eq!(Rank::new(257).0, [0, 2, 0, 0]);
        assert_eq!(Rank::new(510).0, [1 << 126, 0, 0, 0]);
        assert_eq!(Rank::new(511).0, [1 << 127, 0, 0, 0]);
        assert_eq!(Rank::new(512).0, [1 << 127, 0, 0, 0]); // saturating
        assert_eq!(Rank::new(3000).0, [1 << 127, 0, 0, 0]);
    }

    #[test]
    fn rank_shr_one() {
        fn shr_one(init: usize) -> [u128; 4] {
            let mut r = Rank::new(init);
            r.right_shift_one();
            r.0
        }

        assert_eq!(shr_one(0), [0, 0, 0, 0]);
        assert_eq!(shr_one(1), [0, 0, 0, 1]);
        assert_eq!(shr_one(2), [0, 0, 0, 2]);
        assert_eq!(shr_one(5), [0, 0, 0, 1 << 4]);
        assert_eq!(shr_one(127), [0, 0, 0, 1 << 126]);
        assert_eq!(shr_one(128), [0, 0, 0, 1 << 127]);
        assert_eq!(shr_one(129), [0, 0, 1, 0]);
        assert_eq!(shr_one(256), [0, 0, 1 << 127, 0]);
        assert_eq!(shr_one(257), [0, 1, 0, 0]);
        assert_eq!(shr_one(258), [0, 2, 0, 0]);
        assert_eq!(shr_one(511), [1 << 126, 0, 0, 0]);
        assert_eq!(shr_one(512), [1 << 126, 0, 0, 0]);
        assert_eq!(shr_one(513), [1 << 126, 0, 0, 0]); // saturating
        assert_eq!(shr_one(3000), [1 << 126, 0, 0, 0]);
    }

    #[test]
    fn rank_bitor() {
        let mut thing = Rank::new(1);
        thing |= Rank::new(5);
        assert_eq!(thing.0, [0, 0, 0, 0b100010]);
    }
}
