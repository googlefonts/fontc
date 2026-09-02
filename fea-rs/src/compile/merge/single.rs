//! Merging single positioning lookups.

use std::collections::{BTreeMap, BTreeSet};

use write_fonts::tables::{
    gpos::builders::{SinglePosBuilder, ValueRecordBuilder},
    layout::builders::LookupBuilder,
};

use super::{MergeCtx, MergeError, lookups::AlignedLookup};
use crate::{common::GlyphId16, compile::VariationInfo};

impl<V: VariationInfo> MergeCtx<'_, V> {
    /// Merge a single positioning lookup across masters.
    ///
    /// A lookup applies at most one subtable to a glyph, the first that
    /// covers it, so each master's subtables flatten to one map without
    /// changing behaviour; the builder splits the merged map back into
    /// subtables when the lookup is built.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L263-L278>
    pub(super) fn merge_single_pos(
        &self,
        aligned: AlignedLookup<'_, SinglePosBuilder>,
        index: usize,
    ) -> Result<LookupBuilder<SinglePosBuilder>, MergeError> {
        let per_master: Vec<BTreeMap<GlyphId16, &ValueRecordBuilder>> = aligned
            .per_master
            .iter()
            .map(|subtables| {
                let mut map = BTreeMap::new();
                for subtable in *subtables {
                    for (glyph, record) in subtable.iter() {
                        map.entry(glyph).or_insert(record);
                    }
                }
                map
            })
            .collect();
        let glyphs: BTreeSet<GlyphId16> = per_master
            .iter()
            .flat_map(|map| map.keys().copied())
            .collect();

        let mut builder = SinglePosBuilder::default();
        for glyph in glyphs {
            let records: Vec<_> = per_master
                .iter()
                .map(|map| map.get(&glyph).copied())
                .collect();
            builder.insert(glyph, self.merge_value_record(&records, index)?);
        }
        Ok(aligned.build(vec![builder]))
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_helpers::*;

    #[test]
    fn values_vary() {
        assert_eq!(
            merged_binary(&[
                "feature kern { pos a 10; pos b 5; } kern;",
                "feature kern { pos a 30; pos b 5; } kern;",
            ]),
            one_shot_binary("feature kern { pos a (wght=400:10 wght=900:30); pos b 5; } kern;")
        );
    }

    #[test]
    fn rule_missing_in_a_master_is_zero_there() {
        assert_eq!(
            merged_binary(&[
                "feature kern { pos a 10; } kern;",
                "feature kern { pos a 10; pos b 20; } kern;",
            ]),
            one_shot_binary("feature kern { pos a 10; pos b (wght=400:0 wght=900:20); } kern;")
        );
    }

    #[test]
    fn subtables_flatten_first_wins() {
        assert_eq!(
            merged_binary(&[
                "feature kern { pos a 10; subtable; pos a 99; pos b 20; } kern;",
                "feature kern { pos a 10; pos b 25; } kern;",
            ]),
            one_shot_binary("feature kern { pos a 10; pos b (wght=400:20 wght=900:25); } kern;")
        );
    }
}
