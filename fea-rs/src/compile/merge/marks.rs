//! Merging mark-to-base, mark-to-mark and mark-to-ligature lookups.

use std::collections::{BTreeMap, BTreeSet};

use write_fonts::tables::{
    gpos::builders::{AnchorBuilder, MarkToBaseBuilder, MarkToLigBuilder, MarkToMarkBuilder},
    layout::builders::LookupBuilder,
};

use super::{MergeCtx, MergeError, lookups::AlignedLookup};
use crate::{common::GlyphId16, compile::VariationInfo};

/// One master's mark attachment subtable: marks with their class and anchor,
/// and bases (or the marks attached to) with an anchor per class.
#[derive(Default)]
struct MarkAttach<'a> {
    marks: Marks<'a>,
    bases: BTreeMap<GlyphId16, BTreeMap<&'a str, &'a AnchorBuilder>>,
}

type Marks<'a> = BTreeMap<GlyphId16, (&'a str, &'a AnchorBuilder)>;

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
    /// that have them.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L675-L758>
    fn merge_mark_attach<'a>(
        &self,
        per_master: &[MarkAttach<'a>],
        index: usize,
    ) -> Result<MarkAttachMerged<'a>, MergeError> {
        let marks: Vec<_> = per_master.iter().map(|master| &master.marks).collect();
        let mut merged = MarkAttachMerged {
            marks: self.merge_marks(&marks, index)?,
            bases: Vec::new(),
        };

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

    /// Merge the marks of one subtable across masters.
    ///
    /// Marks are matched by class *name*, since the builder numbers classes
    /// in insertion order and two masters can number the same classes
    /// differently; a mark in different classes in different masters is an
    /// error, as in varLib. The result is in glyph order, so the merged
    /// class numbering depends only on the input.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L693-L730>
    fn merge_marks<'a>(
        &self,
        per_master: &[&Marks<'a>],
        index: usize,
    ) -> Result<Vec<(GlyphId16, &'a str, AnchorBuilder)>, MergeError> {
        let glyphs: BTreeSet<GlyphId16> = per_master
            .iter()
            .flat_map(|marks| marks.keys().copied())
            .collect();
        let mut merged = Vec::with_capacity(glyphs.len());
        for glyph in glyphs {
            let mut class = None;
            let mut anchors = Vec::with_capacity(per_master.len());
            for (i, marks) in per_master.iter().enumerate() {
                let Some((master_class, anchor)) = marks.get(&glyph) else {
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
            merged.push((glyph, class.expect("some master has this mark"), anchor));
        }
        Ok(merged)
    }

    /// Merge mark-to-ligature lookups subtable by subtable.
    ///
    /// Ligatures are unioned; the masters that have a ligature must agree on
    /// its number of components, and each component's anchors are merged
    /// per class over the masters that have them. varLib has no special
    /// handling for these lookups and so requires the ligatures and their
    /// component counts to be identical; this is a superset of that.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L95-L128>
    pub(super) fn merge_mark_to_lig(
        &self,
        aligned: AlignedLookup<'_, MarkToLigBuilder>,
        index: usize,
    ) -> Result<LookupBuilder<MarkToLigBuilder>, MergeError> {
        let subtables = aligned.indexwise(index, self, |row| {
            let marks: Vec<Marks> = row
                .iter()
                .map(|subtable| {
                    subtable
                        .iter_marks()
                        .map(|(glyph, class, anchor)| (glyph, (class, anchor)))
                        .collect()
                })
                .collect();
            let ligatures: Vec<BTreeMap<GlyphId16, &[BTreeMap<String, AnchorBuilder>]>> = row
                .iter()
                .map(|subtable| subtable.iter_ligatures().collect())
                .collect();

            let mut builder = MarkToLigBuilder::default();
            for (glyph, class, anchor) in
                self.merge_marks(&marks.iter().collect::<Vec<_>>(), index)?
            {
                builder
                    .insert_mark(glyph, class, anchor)
                    .expect("each mark is inserted once");
            }

            let glyphs: BTreeSet<GlyphId16> = ligatures
                .iter()
                .flat_map(|master| master.keys().copied())
                .collect();
            for glyph in glyphs {
                let per_master: Vec<Option<&[BTreeMap<String, AnchorBuilder>]>> = ligatures
                    .iter()
                    .map(|master| master.get(&glyph).copied())
                    .collect();
                let component_count = per_master
                    .iter()
                    .flatten()
                    .next()
                    .expect("some master has this ligature")
                    .len();
                if let Some(master) = per_master
                    .iter()
                    .position(|components| components.is_some_and(|c| c.len() != component_count))
                {
                    return Err(MergeError::LigatureComponents {
                        master,
                        lookup: self.lookup_ref(index),
                        glyph,
                    });
                }
                let mut components = Vec::with_capacity(component_count);
                for i in 0..component_count {
                    let classes: BTreeSet<&str> = per_master
                        .iter()
                        .flatten()
                        .flat_map(|master| master[i].keys().map(String::as_str))
                        .collect();
                    let mut merged = BTreeMap::new();
                    for class in classes {
                        let anchors: Vec<_> = per_master
                            .iter()
                            .map(|master| master.and_then(|m| m[i].get(class)))
                            .collect();
                        if let Some(anchor) = self.merge_anchor(&anchors, index)? {
                            merged.insert(class.to_owned(), anchor);
                        }
                    }
                    components.push(merged);
                }
                builder.add_ligature_components_directly(glyph, components);
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
    fn ligature_anchors_vary() {
        assert_eq!(
            merged_binary(&[
                "markClass acute <anchor 100 200> @TOP; feature mark { pos ligature f_i <anchor 100 500> mark @TOP ligComponent <anchor 300 500> mark @TOP; } mark;",
                "markClass acute <anchor 100 200> @TOP; feature mark { pos ligature f_i <anchor 100 500> mark @TOP ligComponent <anchor 320 500> mark @TOP; } mark;",
            ]),
            one_shot_binary(
                "markClass acute <anchor 100 200> @TOP; feature mark { pos ligature f_i <anchor 100 500> mark @TOP ligComponent <anchor (wght=400:300 wght=900:320) 500> mark @TOP; } mark;"
            )
        );
    }

    #[test]
    fn ligature_component_counts_must_agree() {
        assert!(matches!(
            merge_masters(&[
                "markClass acute <anchor 100 200> @TOP; feature mark { pos ligature f_i <anchor 100 500> mark @TOP ligComponent <anchor 300 500> mark @TOP; } mark;",
                "markClass acute <anchor 100 200> @TOP; feature mark { pos ligature f_i <anchor 100 500> mark @TOP; } mark;",
            ]),
            Err(MergeError::LigatureComponents { master: 1, .. })
        ));
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
