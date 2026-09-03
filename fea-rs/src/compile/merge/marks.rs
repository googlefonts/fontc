//! Merging mark-to-base, mark-to-mark and mark-to-ligature lookups.

use std::collections::{BTreeMap, BTreeSet};

use write_fonts::tables::{
    gpos::builders::{AnchorBuilder, MarkToBaseBuilder, MarkToLigBuilder, MarkToMarkBuilder},
    layout::builders::LookupBuilder,
};

use super::{MergeCtx, MergeError, lookups::AlignedLookup};
use crate::{common::GlyphId16, compile::VariationInfo};

/// One master's marks in a subtable, each with its class (as that master
/// names it) and anchor.
type Marks<'a> = BTreeMap<GlyphId16, (&'a str, &'a AnchorBuilder)>;

/// One master's bases (or the marks attached to) in a subtable: an anchor
/// per shared class name.
type Bases<'a> = BTreeMap<GlyphId16, BTreeMap<String, &'a AnchorBuilder>>;

/// For each master, its own mark class names mapped to the shared ones.
///
/// See [`MergeCtx::align_mark_classes`].
type ClassMaps<'a> = Vec<BTreeMap<&'a str, String>>;

/// The merged marks and bases, in the order to insert them.
#[derive(Default)]
struct MarkAttachMerged {
    marks: Vec<(GlyphId16, String, AnchorBuilder)>,
    bases: Vec<(GlyphId16, String, AnchorBuilder)>,
}

fn marks<'a>(iter: impl Iterator<Item = (GlyphId16, &'a str, &'a AnchorBuilder)>) -> Marks<'a> {
    iter.map(|(glyph, class, anchor)| (glyph, (class, anchor)))
        .collect()
}

fn bases<'a>(
    iter: impl Iterator<Item = (GlyphId16, &'a str, &'a AnchorBuilder)>,
    classes: &BTreeMap<&str, String>,
) -> Bases<'a> {
    let mut result = Bases::default();
    for (glyph, class, anchor) in iter {
        result
            .entry(glyph)
            .or_default()
            .insert(shared_name(classes, class), anchor);
    }
    result
}

fn shared_name(classes: &BTreeMap<&str, String>, class: &str) -> String {
    classes
        .get(class)
        .cloned()
        .unwrap_or_else(|| class.to_owned())
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
            let marks: Vec<_> = row
                .iter()
                .map(|subtable| marks(subtable.iter_marks()))
                .collect();
            let classes = self.align_mark_classes(&marks, index)?;
            let bases: Vec<_> = row
                .iter()
                .zip(&classes)
                .map(|(subtable, classes)| bases(subtable.iter_bases(), classes))
                .collect();
            let merged = self.merge_mark_attach(&marks, &classes, &bases, index)?;
            let mut builder = MarkToBaseBuilder::default();
            for (glyph, class, anchor) in merged.marks {
                builder
                    .insert_mark(glyph, &class, anchor)
                    .expect("each mark is inserted once");
            }
            for (glyph, class, anchor) in merged.bases {
                builder.insert_base(glyph, &class, anchor);
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
            let marks: Vec<_> = row
                .iter()
                .map(|subtable| marks(subtable.iter_mark1s()))
                .collect();
            let classes = self.align_mark_classes(&marks, index)?;
            let bases: Vec<_> = row
                .iter()
                .zip(&classes)
                .map(|(subtable, classes)| bases(subtable.iter_mark2s(), classes))
                .collect();
            let merged = self.merge_mark_attach(&marks, &classes, &bases, index)?;
            let mut builder = MarkToMarkBuilder::default();
            for (glyph, class, anchor) in merged.marks {
                builder
                    .insert_mark1(glyph, &class, anchor)
                    .expect("each mark is inserted once");
            }
            for (glyph, class, anchor) in merged.bases {
                builder.insert_mark2(glyph, &class, anchor);
            }
            Ok(builder)
        })?;
        Ok(aligned.build(subtables))
    }

    /// Match up the mark classes of one subtable across masters.
    ///
    /// Class names are a source-level convenience: the binary has class
    /// indices, and two masters can name the same class differently, or use
    /// one name for different classes. What must agree is how the marks are
    /// partitioned into classes: marks that share a class in one master
    /// share it in every master that has them both. Each class gets one
    /// shared name, the default master's where it has the class, and the
    /// result maps each master's own names to those.
    ///
    /// varLib compares class indices instead, which amounts to the same
    /// check when the masters number their classes alike:
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L693-L730>
    fn align_mark_classes<'a>(
        &self,
        per_master: &[Marks<'a>],
        index: usize,
    ) -> Result<ClassMaps<'a>, MergeError> {
        // the shared class of every mark seen in an earlier master
        let mut shared_of: BTreeMap<GlyphId16, String> = BTreeMap::new();
        let mut used: BTreeSet<String> = BTreeSet::new();
        let mut maps = Vec::with_capacity(per_master.len());
        for (master, marks) in per_master.iter().enumerate() {
            let mut map: BTreeMap<&'a str, String> = BTreeMap::new();
            // the mark that put each own or shared class in the map
            let mut by_own: BTreeMap<&'a str, GlyphId16> = BTreeMap::new();
            let mut by_shared: BTreeMap<String, GlyphId16> = BTreeMap::new();
            for (&glyph, &(class, _)) in marks {
                let Some(shared) = shared_of.get(&glyph) else {
                    continue;
                };
                let other = match map.get(class) {
                    Some(existing) if existing != shared => Some(by_own[class]),
                    Some(_) => None,
                    None => match by_shared.get(shared) {
                        Some(other) => Some(*other),
                        None => {
                            map.insert(class, shared.clone());
                            by_own.insert(class, glyph);
                            by_shared.insert(shared.clone(), glyph);
                            None
                        }
                    },
                };
                if let Some(other) = other {
                    return Err(MergeError::MarkClassMismatch {
                        master,
                        lookup: self.lookup_ref(index),
                        glyph,
                        other,
                    });
                }
            }
            for (class, _) in marks.values() {
                if map.contains_key(class) {
                    continue;
                }
                let mut name = (*class).to_owned();
                for i in 1.. {
                    if used.insert(name.clone()) {
                        break;
                    }
                    name = format!("{class}.{i}");
                }
                map.insert(class, name);
            }
            for (&glyph, &(class, _)) in marks {
                shared_of.entry(glyph).or_insert_with(|| map[class].clone());
            }
            maps.push(map);
        }
        Ok(maps)
    }

    /// Merge one mark attachment subtable across masters.
    ///
    /// Marks and bases are unioned and their anchors merged over the masters
    /// that have them.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L675-L758>
    fn merge_mark_attach(
        &self,
        marks: &[Marks],
        classes: &ClassMaps,
        bases: &[Bases],
        index: usize,
    ) -> Result<MarkAttachMerged, MergeError> {
        let mut merged = MarkAttachMerged {
            marks: self.merge_marks(marks, classes, index)?,
            bases: Vec::new(),
        };

        let base_glyphs: BTreeSet<GlyphId16> = bases
            .iter()
            .flat_map(|master| master.keys().copied())
            .collect();
        for glyph in base_glyphs {
            let class_names: BTreeSet<&str> = bases
                .iter()
                .filter_map(|master| master.get(&glyph))
                .flat_map(|anchors| anchors.keys().map(String::as_str))
                .collect();
            for class in class_names {
                let anchors: Vec<_> = bases
                    .iter()
                    .map(|master| {
                        master
                            .get(&glyph)
                            .and_then(|anchors| anchors.get(class))
                            .copied()
                    })
                    .collect();
                let anchor = self
                    .merge_anchor(&anchors, index)?
                    .expect("some master has this anchor");
                merged.bases.push((glyph, class.to_owned(), anchor));
            }
        }
        Ok(merged)
    }

    /// Merge the marks of one subtable across masters.
    ///
    /// The result is in glyph order, so the merged class numbering depends
    /// only on the input.
    fn merge_marks(
        &self,
        per_master: &[Marks],
        classes: &ClassMaps,
        index: usize,
    ) -> Result<Vec<(GlyphId16, String, AnchorBuilder)>, MergeError> {
        let glyphs: BTreeSet<GlyphId16> = per_master
            .iter()
            .flat_map(|marks| marks.keys().copied())
            .collect();
        let mut merged = Vec::with_capacity(glyphs.len());
        for glyph in glyphs {
            let mut class = None;
            let mut anchors = Vec::with_capacity(per_master.len());
            for (marks, classes) in per_master.iter().zip(classes) {
                let Some((own, anchor)) = marks.get(&glyph) else {
                    anchors.push(None);
                    continue;
                };
                class.get_or_insert_with(|| classes[*own].clone());
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
            let marks: Vec<_> = row
                .iter()
                .map(|subtable| marks(subtable.iter_marks()))
                .collect();
            let classes = self.align_mark_classes(&marks, index)?;
            let ligatures: Vec<BTreeMap<GlyphId16, Vec<BTreeMap<String, &AnchorBuilder>>>> = row
                .iter()
                .zip(&classes)
                .map(|(subtable, classes)| {
                    subtable
                        .iter_ligatures()
                        .map(|(glyph, components)| {
                            let components = components
                                .iter()
                                .map(|anchors| {
                                    anchors
                                        .iter()
                                        .map(|(class, anchor)| {
                                            (shared_name(classes, class), anchor)
                                        })
                                        .collect()
                                })
                                .collect();
                            (glyph, components)
                        })
                        .collect()
                })
                .collect();

            let mut builder = MarkToLigBuilder::default();
            for (glyph, class, anchor) in self.merge_marks(&marks, &classes, index)? {
                builder
                    .insert_mark(glyph, &class, anchor)
                    .expect("each mark is inserted once");
            }

            let glyphs: BTreeSet<GlyphId16> = ligatures
                .iter()
                .flat_map(|master| master.keys().copied())
                .collect();
            for glyph in glyphs {
                let per_master: Vec<Option<&Vec<BTreeMap<String, &AnchorBuilder>>>> =
                    ligatures.iter().map(|master| master.get(&glyph)).collect();
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
                            .map(|master| master.and_then(|m| m[i].get(class)).copied())
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
    fn mark_class_names_need_not_match() {
        // ufo2ft names mark classes after the lookup they were generated
        // for, so two masters can call the same class different things
        assert_eq!(
            merged_binary(&[
                "markClass acute <anchor 100 200> @A; feature mark { pos base a <anchor 150 500> mark @A; } mark;",
                "markClass acute <anchor 100 200> @B; feature mark { pos base a <anchor 150 520> mark @B; } mark;",
            ]),
            one_shot_binary(
                "markClass acute <anchor 100 200> @A; feature mark { pos base a <anchor 150 (wght=400:500 wght=900:520)> mark @A; } mark;"
            )
        );
    }

    #[test]
    fn mark_class_names_swapped_between_masters() {
        assert_eq!(
            merged_binary(&[
                "markClass acute <anchor 100 200> @A; markClass grave <anchor 100 -10> @B; \
                 feature mark { pos base a <anchor 150 500> mark @A <anchor 150 -20> mark @B; } mark;",
                "markClass acute <anchor 100 200> @B; markClass grave <anchor 100 -10> @A; \
                 feature mark { pos base a <anchor 150 520> mark @B <anchor 150 -30> mark @A; } mark;",
            ]),
            one_shot_binary(
                "markClass acute <anchor 100 200> @A; markClass grave <anchor 100 -10> @B; \
                 feature mark { pos base a <anchor 150 (wght=400:500 wght=900:520)> mark @A \
                 <anchor 150 (wght=400:-20 wght=900:-30)> mark @B; } mark;"
            )
        );
    }

    #[test]
    fn a_mark_in_a_different_class_per_lookup() {
        let fea = |y: &str| {
            format!(
                "markClass acute <anchor 100 200> @TOP; markClass acute <anchor 100 0> @BOTTOM; \
                 feature mark {{ lookup L1 {{ pos base a <anchor 150 {y}> mark @TOP; }} L1; \
                 lookup L2 {{ pos base b <anchor 150 -20> mark @BOTTOM; }} L2; }} mark;"
            )
        };
        assert_eq!(
            merged_binary(&[&fea("500"), &fea("520")]),
            one_shot_binary(&fea("(wght=400:500 wght=900:520)"))
        );
    }

    #[test]
    fn marks_partitioned_differently_is_an_error() {
        let one_class = "markClass [acute grave] <anchor 100 200> @TOP; \
            feature mark { pos base a <anchor 150 500> mark @TOP; } mark;";
        let two_classes = "markClass acute <anchor 100 200> @TOP; markClass grave <anchor 100 200> @BOTTOM; \
            feature mark { pos base a <anchor 150 500> mark @TOP <anchor 150 0> mark @BOTTOM; } mark;";
        assert!(matches!(
            merge_masters(&[one_class, two_classes]),
            Err(MergeError::MarkClassMismatch { master: 1, .. })
        ));
        assert!(matches!(
            merge_masters(&[two_classes, one_class]),
            Err(MergeError::MarkClassMismatch { master: 1, .. })
        ));
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
    fn ligature_mark_class_names_need_not_match() {
        assert_eq!(
            merged_binary(&[
                "markClass acute <anchor 100 200> @A; feature mark { pos ligature f_i <anchor 100 500> mark @A ligComponent <anchor 300 500> mark @A; } mark;",
                "markClass acute <anchor 100 200> @B; feature mark { pos ligature f_i <anchor 100 500> mark @B ligComponent <anchor 320 500> mark @B; } mark;",
            ]),
            one_shot_binary(
                "markClass acute <anchor 100 200> @A; feature mark { pos ligature f_i <anchor 100 500> mark @A ligComponent <anchor (wght=400:300 wght=900:320) 500> mark @A; } mark;"
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
