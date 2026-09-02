//! Merging cursive attachment lookups.

use std::collections::{BTreeMap, BTreeSet};

use write_fonts::tables::{gpos::builders::CursivePosBuilder, layout::builders::LookupBuilder};

use super::{MergeCtx, MergeError, lookups::AlignedLookup};
use crate::{common::GlyphId16, compile::VariationInfo};

impl<V: VariationInfo> MergeCtx<'_, V> {
    /// Merge a cursive attachment lookup across masters, subtable by subtable.
    ///
    /// Coverage is the union of the masters'; a glyph's entry and exit anchors
    /// are each merged over the masters that have them.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L916-L946>
    pub(super) fn merge_cursive(
        &self,
        aligned: AlignedLookup<'_, CursivePosBuilder>,
        index: usize,
    ) -> Result<LookupBuilder<CursivePosBuilder>, MergeError> {
        let subtables = aligned.indexwise(index, self, |row| {
            let per_master: Vec<BTreeMap<_, _>> = row
                .iter()
                .map(|subtable| {
                    subtable
                        .iter()
                        .map(|(glyph, entry, exit)| (glyph, (entry, exit)))
                        .collect()
                })
                .collect();
            let glyphs: BTreeSet<GlyphId16> = per_master
                .iter()
                .flat_map(|map| map.keys().copied())
                .collect();

            let mut builder = CursivePosBuilder::default();
            for glyph in glyphs {
                let anchors = |pick: fn(&(Option<_>, Option<_>)) -> Option<_>| -> Vec<_> {
                    per_master
                        .iter()
                        .map(|map| map.get(&glyph).and_then(pick))
                        .collect()
                };
                let entry = self.merge_anchor(&anchors(|(entry, _)| *entry), index)?;
                let exit = self.merge_anchor(&anchors(|(_, exit)| *exit), index)?;
                builder.insert(glyph, entry, exit);
            }
            Ok(builder)
        })?;
        Ok(aligned.build(subtables))
    }
}

#[cfg(test)]
mod tests {
    use super::super::{MergeError, test_helpers::*};

    #[test]
    fn anchors_vary() {
        assert_eq!(
            merged_binary(&[
                "feature curs { pos cursive a <anchor 100 200> <anchor NULL>; } curs;",
                "feature curs { pos cursive a <anchor 100 250> <anchor NULL>; } curs;",
            ]),
            one_shot_binary(
                "feature curs { pos cursive a <anchor 100 (wght=400:200 wght=900:250)> <anchor NULL>; } curs;"
            )
        );
    }

    #[test]
    fn glyph_missing_in_a_middle_master_is_sparse() {
        assert_eq!(
            merged_binary(&[
                "feature curs { pos cursive a <anchor 1 2> <anchor NULL>; pos cursive b <anchor NULL> <anchor 10 10>; } curs;",
                "feature curs { pos cursive a <anchor 1 2> <anchor NULL>; } curs;",
                "feature curs { pos cursive a <anchor 1 2> <anchor NULL>; pos cursive b <anchor NULL> <anchor 10 20>; } curs;",
            ]),
            one_shot_binary(
                "feature curs { pos cursive a <anchor 1 2> <anchor NULL>; pos cursive b <anchor NULL> <anchor 10 (wght=400:10 wght=900:20)>; } curs;"
            )
        );
    }

    #[test]
    fn glyph_missing_at_the_default_is_an_error() {
        assert!(matches!(
            merge_masters(&[
                "feature curs { pos cursive a <anchor 1 2> <anchor NULL>; } curs;",
                "feature curs { pos cursive a <anchor 1 2> <anchor NULL>; pos cursive b <anchor NULL> <anchor 10 20>; } curs;",
            ]),
            Err(MergeError::MissingAtDefault { .. })
        ));
    }
}
