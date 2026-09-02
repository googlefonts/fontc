//! Merging mark-to-base and mark-to-mark lookups.

use std::collections::{BTreeMap, BTreeSet};

use write_fonts::tables::{
    gpos::builders::{AnchorBuilder, MarkToBaseBuilder, MarkToMarkBuilder},
    layout::builders::LookupBuilder,
};

use super::{MergeCtx, MergeError, lookups::AlignedLookup};
use crate::{common::GlyphId16, compile::VariationInfo};

/// One master's mark attachment subtable: marks with their class and anchor,
/// and bases (or the marks attached to) with an anchor per class.
#[derive(Default)]
struct MarkAttach<'a> {
    marks: BTreeMap<GlyphId16, (&'a str, &'a AnchorBuilder)>,
    bases: BTreeMap<GlyphId16, BTreeMap<&'a str, &'a AnchorBuilder>>,
}

impl<'a> MarkAttach<'a> {
    fn new(
        marks: impl Iterator<Item = (GlyphId16, &'a str, &'a AnchorBuilder)>,
        bases: impl Iterator<Item = (GlyphId16, &'a str, &'a AnchorBuilder)>,
    ) -> Self {
        let mut result = Self::default();
        for (glyph, class, anchor) in marks {
            result.marks.insert(glyph, (class, anchor));
        }
        for (glyph, class, anchor) in bases {
            result.bases.entry(glyph).or_default().insert(class, anchor);
        }
        result
    }
}

/// The merged marks and bases, in the order to insert them.
#[derive(Default)]
struct MarkAttachMerged<'a> {
    marks: Vec<(GlyphId16, &'a str, AnchorBuilder)>,
    bases: Vec<(GlyphId16, &'a str, AnchorBuilder)>,
}

impl<V: VariationInfo> MergeCtx<'_, V> {
    /// Merge mark-to-base lookups subtable by subtable.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L761-L773>
    pub(super) fn merge_mark_to_base(
        &self,
        aligned: AlignedLookup<'_, MarkToBaseBuilder>,
        index: usize,
    ) -> Result<LookupBuilder<MarkToBaseBuilder>, MergeError> {
        let subtables = aligned.indexwise(index, self, |row| {
            let per_master: Vec<_> = row
                .iter()
                .map(|subtable| MarkAttach::new(subtable.iter_marks(), subtable.iter_bases()))
                .collect();
            let merged = self.merge_mark_attach(&per_master, index)?;
            let mut builder = MarkToBaseBuilder::default();
            for (glyph, class, anchor) in merged.marks {
                builder
                    .insert_mark(glyph, class, anchor)
                    .expect("each mark is inserted once");
            }
            for (glyph, class, anchor) in merged.bases {
                builder.insert_base(glyph, class, anchor);
            }
            Ok(builder)
        })?;
        Ok(aligned.build(subtables))
    }

    /// Merge mark-to-mark lookups subtable by subtable.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L776-L788>
    pub(super) fn merge_mark_to_mark(
        &self,
        aligned: AlignedLookup<'_, MarkToMarkBuilder>,
        index: usize,
    ) -> Result<LookupBuilder<MarkToMarkBuilder>, MergeError> {
        let subtables = aligned.indexwise(index, self, |row| {
            let per_master: Vec<_> = row
                .iter()
                .map(|subtable| MarkAttach::new(subtable.iter_mark1s(), subtable.iter_mark2s()))
                .collect();
            let merged = self.merge_mark_attach(&per_master, index)?;
            let mut builder = MarkToMarkBuilder::default();
            for (glyph, class, anchor) in merged.marks {
                builder
                    .insert_mark1(glyph, class, anchor)
                    .expect("each mark is inserted once");
            }
            for (glyph, class, anchor) in merged.bases {
                builder.insert_mark2(glyph, class, anchor);
            }
            Ok(builder)
        })?;
        Ok(aligned.build(subtables))
    }

    /// Merge one mark attachment subtable across masters.
    ///
    /// Marks and bases are unioned and their anchors merged over the masters
    /// that have them. Marks are matched by class *name*, since the builder
    /// numbers classes in insertion order and two masters can number the same
    /// classes differently; a mark in different classes in different masters
    /// is an error, as in varLib. Marks are inserted in glyph order, so the
    /// merged class numbering depends only on the input.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L675-L758>
    fn merge_mark_attach<'a>(
        &self,
        per_master: &[MarkAttach<'a>],
        index: usize,
    ) -> Result<MarkAttachMerged<'a>, MergeError> {
        let mut merged = MarkAttachMerged::default();
        let mark_glyphs: BTreeSet<GlyphId16> = per_master
            .iter()
            .flat_map(|master| master.marks.keys().copied())
            .collect();
        for glyph in mark_glyphs {
            let mut class = None;
            let mut anchors = Vec::with_capacity(per_master.len());
            for (i, master) in per_master.iter().enumerate() {
                let Some((master_class, anchor)) = master.marks.get(&glyph) else {
                    anchors.push(None);
                    continue;
                };
                match class {
                    None => class = Some(*master_class),
                    Some(expected) if expected != *master_class => {
                        return Err(MergeError::MarkClassConflict {
                            master: i,
                            glyph,
                            expected: expected.into(),
                            found: (*master_class).into(),
                        });
                    }
                    Some(_) => (),
                }
                anchors.push(Some(*anchor));
            }
            let anchor = self
                .merge_anchor(&anchors, index)?
                .expect("some master has this mark");
            merged
                .marks
                .push((glyph, class.expect("some master has this mark"), anchor));
        }

        let base_glyphs: BTreeSet<GlyphId16> = per_master
            .iter()
            .flat_map(|master| master.bases.keys().copied())
            .collect();
        for glyph in base_glyphs {
            let classes: BTreeSet<&str> = per_master
                .iter()
                .filter_map(|master| master.bases.get(&glyph))
                .flat_map(|anchors| anchors.keys().copied())
                .collect();
            for class in classes {
                let anchors: Vec<_> = per_master
                    .iter()
                    .map(|master| {
                        master
                            .bases
                            .get(&glyph)
                            .and_then(|anchors| anchors.get(class))
                            .copied()
                    })
                    .collect();
                let anchor = self
                    .merge_anchor(&anchors, index)?
                    .expect("some master has this anchor");
                merged.bases.push((glyph, class, anchor));
            }
        }
        Ok(merged)
    }
}

#[cfg(test)]
mod tests {
    use super::super::{MergeError, test_helpers::*};

    #[test]
    fn base_anchors_vary() {
        assert_eq!(
            merged_binary(&[
                "markClass acute <anchor 100 200> @TOP; feature mark { pos base a <anchor 150 500> mark @TOP; } mark;",
                "markClass acute <anchor 100 200> @TOP; feature mark { pos base a <anchor 150 520> mark @TOP; } mark;",
            ]),
            one_shot_binary(
                "markClass acute <anchor 100 200> @TOP; feature mark { pos base a <anchor 150 (wght=400:500 wght=900:520)> mark @TOP; } mark;"
            )
        );
    }

    #[test]
    fn mark_anchors_vary() {
        assert_eq!(
            merged_binary(&[
                "markClass acute <anchor 100 200> @TOP; feature mark { pos base a <anchor 150 500> mark @TOP; } mark;",
                "markClass acute <anchor 100 210> @TOP; feature mark { pos base a <anchor 150 500> mark @TOP; } mark;",
            ]),
            one_shot_binary(
                "markClass acute <anchor 100 (wght=400:200 wght=900:210)> @TOP; feature mark { pos base a <anchor 150 500> mark @TOP; } mark;"
            )
        );
    }

    #[test]
    fn base_missing_in_a_master_is_sparse() {
        assert_eq!(
            merged_binary(&[
                "markClass acute <anchor 100 200> @TOP; feature mark { pos base a <anchor 150 500> mark @TOP; pos base b <anchor 150 600> mark @TOP; } mark;",
                "markClass acute <anchor 100 200> @TOP; feature mark { pos base a <anchor 150 520> mark @TOP; } mark;",
            ]),
            one_shot_binary(
                "markClass acute <anchor 100 200> @TOP; feature mark { pos base a <anchor 150 (wght=400:500 wght=900:520)> mark @TOP; pos base b <anchor 150 600> mark @TOP; } mark;"
            )
        );
    }

    #[test]
    fn base_missing_at_the_default_is_an_error() {
        assert!(matches!(
            merge_masters(&[
                "markClass acute <anchor 100 200> @TOP; feature mark { pos base a <anchor 150 500> mark @TOP; } mark;",
                "markClass acute <anchor 100 200> @TOP; feature mark { pos base a <anchor 150 500> mark @TOP; pos base b <anchor 150 600> mark @TOP; } mark;",
            ]),
            Err(MergeError::MissingAtDefault { .. })
        ));
    }

    #[test]
    fn two_classes_and_a_class_missing_on_a_base_in_one_master() {
        // b has a @BOTTOM anchor only in the default master; it stays static
        assert_eq!(
            merged_binary(&[
                "markClass acute <anchor 100 200> @TOP; markClass grave <anchor 100 -10> @BOTTOM; \
                 feature mark { pos base a <anchor 150 500> mark @TOP <anchor 150 -20> mark @BOTTOM; \
                 pos base b <anchor 160 500> mark @TOP <anchor 160 -20> mark @BOTTOM; } mark;",
                "markClass acute <anchor 100 200> @TOP; markClass grave <anchor 100 -10> @BOTTOM; \
                 feature mark { pos base a <anchor 150 520> mark @TOP <anchor 150 -20> mark @BOTTOM; \
                 pos base b <anchor 160 520> mark @TOP; } mark;",
            ]),
            one_shot_binary(
                "markClass acute <anchor 100 200> @TOP; markClass grave <anchor 100 -10> @BOTTOM; \
                 feature mark { pos base a <anchor 150 (wght=400:500 wght=900:520)> mark @TOP <anchor 150 -20> mark @BOTTOM; \
                 pos base b <anchor 160 (wght=400:500 wght=900:520)> mark @TOP <anchor 160 -20> mark @BOTTOM; } mark;"
            )
        );
    }

    #[test]
    fn mark_to_mark_anchors_vary() {
        assert_eq!(
            merged_binary(&[
                "markClass acute <anchor 100 200> @TOP; feature mkmk { pos mark grave <anchor 100 300> mark @TOP; } mkmk;",
                "markClass acute <anchor 100 200> @TOP; feature mkmk { pos mark grave <anchor 100 320> mark @TOP; } mkmk;",
            ]),
            one_shot_binary(
                "markClass acute <anchor 100 200> @TOP; feature mkmk { pos mark grave <anchor 100 (wght=400:300 wght=900:320)> mark @TOP; } mkmk;"
            )
        );
    }
}
