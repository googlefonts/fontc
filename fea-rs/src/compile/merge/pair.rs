//! Merging pair positioning lookups.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use write_fonts::{
    read::collections::IntSet,
    tables::{
        gpos::builders::{PairPosBuilder, ValueRecordBuilder},
        layout::builders::LookupBuilder,
    },
};

use super::{MergeCtx, MergeError, lookups::AlignedLookup};
use crate::{common::GlyphId16, compile::VariationInfo};

type ClassPair<'a> = (
    &'a IntSet<GlyphId16>,
    &'a IntSet<GlyphId16>,
    &'a ValueRecordBuilder,
    &'a ValueRecordBuilder,
);

/// One master's pair lookup, in the order its subtables will be tried.
///
/// Each builder becomes a glyph-pair subtable followed by its class
/// subtables, so this is the order needed to find the value a master
/// actually applies to a pair.
struct PairLookup<'a> {
    subtables: Vec<PairSubtable<'a>>,
}

struct PairSubtable<'a> {
    pairs: HashMap<(GlyphId16, GlyphId16), (&'a ValueRecordBuilder, &'a ValueRecordBuilder)>,
    classes: Vec<Vec<ClassPair<'a>>>,
}

impl<'a> PairLookup<'a> {
    fn new(subtables: &'a [PairPosBuilder]) -> Self {
        let subtables = subtables
            .iter()
            .map(|builder| PairSubtable {
                pairs: builder
                    .iter_pairs()
                    .map(|(g1, g2, r1, r2)| ((g1, g2), (r1, r2)))
                    .collect(),
                classes: builder
                    .iter_class_subtables()
                    .map(|rules| rules.collect())
                    .collect(),
            })
            .collect();
        PairLookup { subtables }
    }

    fn glyph_pairs(&self) -> impl Iterator<Item = (GlyphId16, GlyphId16)> + '_ {
        self.subtables
            .iter()
            .flat_map(|subtable| subtable.pairs.keys().copied())
    }

    /// The value this lookup applies to a pair, if it applies at all.
    ///
    /// Subtables are tried in order and the first that covers the pair is
    /// used. A class subtable covers every pair whose first glyph is in one
    /// of its first classes, and applies an empty record to pairs whose
    /// second glyph is in none of its second classes.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L281-L305>
    fn effective_value(
        &self,
        first: GlyphId16,
        second: GlyphId16,
    ) -> Option<(&'a ValueRecordBuilder, &'a ValueRecordBuilder)> {
        static EMPTY: ValueRecordBuilder = ValueRecordBuilder {
            x_advance: None,
            y_advance: None,
            x_placement: None,
            y_placement: None,
        };
        for subtable in &self.subtables {
            if let Some(records) = subtable.pairs.get(&(first, second)) {
                return Some(*records);
            }
            for class_subtable in &subtable.classes {
                let mut covers_first = false;
                for (class1, class2, r1, r2) in class_subtable {
                    if !class1.contains(first) {
                        continue;
                    }
                    covers_first = true;
                    if class2.contains(second) {
                        return Some((r1, r2));
                    }
                }
                if covers_first {
                    return Some((&EMPTY, &EMPTY));
                }
            }
        }
        None
    }

    /// `true` if any rule adjusts the second glyph of the pair.
    fn positions_second_glyph(&self) -> bool {
        self.subtables.iter().any(|subtable| {
            subtable
                .pairs
                .values()
                .map(|(_, r2)| *r2)
                .chain(subtable.classes.iter().flatten().map(|(_, _, _, r2)| *r2))
                .any(|record| !record.format().is_empty())
        })
    }

    fn class_subtables(&self) -> Vec<Vec<ClassPair<'a>>> {
        self.subtables
            .iter()
            .flat_map(|subtable| subtable.classes.iter().cloned())
            .collect()
    }
}

impl<V: VariationInfo> MergeCtx<'_, V> {
    /// Merge a pair positioning lookup across masters.
    ///
    /// Glyph pairs are unioned. A master's value for a pair is whatever its
    /// lookup applies to that pair, which may come from a class rule or from
    /// a later subtable, or be nothing at all, in which case it contributes
    /// zero. The merged glyph pairs go in the first subtable, ahead of the
    /// class subtables, which keeps the effective values right.
    ///
    /// Whether the second glyph of pairs is positioned decides how a
    /// subtable consumes glyphs, so the masters must agree on it; varLib
    /// asserts the same.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L370-L455>
    pub(super) fn merge_pair_pos(
        &self,
        aligned: AlignedLookup<'_, PairPosBuilder>,
        index: usize,
    ) -> Result<LookupBuilder<PairPosBuilder>, MergeError> {
        let per_master: Vec<PairLookup> = aligned
            .per_master
            .iter()
            .map(|subtables| PairLookup::new(subtables))
            .collect();

        let positions_second = per_master[0].positions_second_glyph();
        if let Some(master) = per_master
            .iter()
            .position(|lookup| lookup.positions_second_glyph() != positions_second)
        {
            return Err(MergeError::SecondGlyphPositioning {
                master,
                lookup: self.lookup_ref(index),
            });
        }

        //TODO: merge class pairs instead of requiring equality
        let class_subtables = per_master[0].class_subtables();
        if per_master
            .iter()
            .any(|lookup| lookup.class_subtables() != class_subtables)
        {
            return Err(self.unsupported(index, crate::Kind::GposType2));
        }

        let pairs: BTreeSet<(GlyphId16, GlyphId16)> = per_master
            .iter()
            .flat_map(PairLookup::glyph_pairs)
            .collect();
        let mut merged_pairs = BTreeMap::new();
        for (first, second) in pairs {
            let values: Vec<_> = per_master
                .iter()
                .map(|lookup| lookup.effective_value(first, second))
                .collect();
            let firsts: Vec<_> = values.iter().map(|v| v.map(|(r1, _)| r1)).collect();
            let seconds: Vec<_> = values.iter().map(|v| v.map(|(_, r2)| r2)).collect();
            merged_pairs.insert(
                (first, second),
                (
                    self.merge_value_record(&firsts, index)?,
                    self.merge_value_record(&seconds, index)?,
                ),
            );
        }

        let mut subtables = Vec::new();
        let mut first_subtable = PairPosBuilder::default();
        for ((first, second), (r1, r2)) in merged_pairs {
            first_subtable.insert_pair(first, r1, second, r2);
        }
        let mut class_subtables = class_subtables.into_iter();
        if let Some(classes) = class_subtables.next() {
            insert_classes(&mut first_subtable, &classes);
        }
        subtables.push(first_subtable);
        for classes in class_subtables {
            let mut subtable = PairPosBuilder::default();
            insert_classes(&mut subtable, &classes);
            subtables.push(subtable);
        }
        Ok(aligned.build(subtables))
    }
}

/// Add one class subtable's rules to a builder.
///
/// Within a subtable the classes on each side are disjoint, so they all fit
/// in one class subtable of the builder whatever the insertion order.
fn insert_classes(builder: &mut PairPosBuilder, classes: &[ClassPair]) {
    for (class1, class2, r1, r2) in classes {
        builder.insert_classes(
            (*class1).clone(),
            (*r1).clone(),
            (*class2).clone(),
            (*r2).clone(),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::super::{MergeError, test_helpers::*};

    #[test]
    fn values_vary() {
        assert_eq!(
            merged_binary(&[
                "feature kern { pos a b -20; } kern;",
                "feature kern { pos a b -40; } kern;",
            ]),
            one_shot_binary("feature kern { pos a b (wght=400:-20 wght=900:-40); } kern;")
        );
    }

    #[test]
    fn pair_missing_in_a_master_is_zero_there() {
        assert_eq!(
            merged_binary(&[
                "feature kern { pos a b -20; } kern;",
                "feature kern { pos a b -20; pos a c -10; } kern;",
            ]),
            one_shot_binary(
                "feature kern { pos a b -20; pos a c (wght=400:0 wght=900:-10); } kern;"
            )
        );
    }

    #[test]
    fn pair_missing_in_a_master_takes_its_class_value() {
        assert_eq!(
            merged_binary(&[
                "feature kern { pos a b -50; pos [a c] [b f_i] -40; } kern;",
                "feature kern { pos [a c] [b f_i] -40; } kern;",
            ]),
            one_shot_binary(
                "feature kern { pos a b (wght=400:-50 wght=900:-40); pos [a c] [b f_i] -40; } kern;"
            )
        );
    }

    #[test]
    fn pair_shadowed_by_an_earlier_class_subtable_takes_the_class_value() {
        // the glyph pairs sit behind the class subtable in both masters, so
        // the class value is what applies; the merged pair moves in front of
        // the class subtable and must keep it
        assert_eq!(
            merged_binary(&[
                "feature kern { pos [a c] [b f_i] -40; subtable; pos a b -50; } kern;",
                "feature kern { pos [a c] [b f_i] -40; subtable; pos a b -60; } kern;",
            ]),
            one_shot_binary("feature kern { pos a b -40; pos [a c] [b f_i] -40; } kern;")
        );
    }

    #[test]
    fn masters_must_agree_on_positioning_the_second_glyph() {
        assert!(matches!(
            merge_masters(&[
                "feature kern { pos a b -20; } kern;",
                "feature kern { pos a <-20 0 -20 0> b <0 10 0 0>; } kern;",
            ]),
            Err(MergeError::SecondGlyphPositioning { master: 1, .. })
        ));
    }
}
