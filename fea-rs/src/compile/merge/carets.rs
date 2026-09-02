//! Merging GDEF ligature carets.

use std::collections::{BTreeMap, BTreeSet};

use write_fonts::tables::layout::builders::{CaretValueBuilder, DeviceOrDeltas, Metric};

use super::{MergeCtx, MergeError};
use crate::{common::GlyphId16, compile::VariationInfo};

impl<V: VariationInfo> MergeCtx<'_, V> {
    /// Merge the `LigatureCaretByPos`/`ByIndex` statements across masters.
    ///
    /// Ligature glyphs are unioned; the masters that have one must agree on
    /// its number of carets and on which are points. Positions are merged
    /// over the masters that have the glyph, and so must be present at the
    /// default master. varLib requires the caret list itself to be identical
    /// and merges only the positions; this is a superset of that.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L1273-L1282>
    pub(super) fn merge_ligature_carets(&mut self) -> Result<(), MergeError> {
        let per_master: Vec<Option<&BTreeMap<GlyphId16, Vec<CaretValueBuilder>>>> =
            std::iter::once(&self.merged)
                .chain(&self.others)
                .map(|master| master.tables.gdef.as_ref().map(|gdef| &gdef.ligature_pos))
                .collect();
        let glyphs: BTreeSet<GlyphId16> = per_master
            .iter()
            .flatten()
            .flat_map(|carets| carets.keys().copied())
            .collect();
        if glyphs.is_empty() {
            return Ok(());
        }

        let mut merged = BTreeMap::new();
        for glyph in glyphs {
            let carets: Vec<Option<&[CaretValueBuilder]>> = per_master
                .iter()
                .map(|master| master.and_then(|c| c.get(&glyph)).map(Vec::as_slice))
                .collect();
            let Some(default) = carets[0] else {
                return Err(MergeError::LigatureCaretsMissingAtDefault { glyph });
            };
            let mismatch = |caret: &CaretValueBuilder, other: &CaretValueBuilder| {
                matches!(
                    (caret, other),
                    (
                        CaretValueBuilder::Coordinate { .. },
                        CaretValueBuilder::PointIndex(_)
                    ) | (
                        CaretValueBuilder::PointIndex(_),
                        CaretValueBuilder::Coordinate { .. }
                    )
                )
            };
            if let Some(master) = carets.iter().position(|master| {
                master.is_some_and(|other| {
                    other.len() != default.len()
                        || default.iter().zip(other).any(|(a, b)| mismatch(a, b))
                })
            }) {
                return Err(MergeError::LigatureCarets { master, glyph });
            }

            let mut values = Vec::with_capacity(default.len());
            for (i, caret) in default.iter().enumerate() {
                let CaretValueBuilder::Coordinate { .. } = caret else {
                    // a point index is not a position; the masters that have
                    // the glyph must agree on it
                    if carets.iter().flatten().any(|other| other[i] != *caret) {
                        return Err(MergeError::LigatureCarets { master: 0, glyph });
                    }
                    values.push(caret.clone());
                    continue;
                };
                let per_master: Vec<Option<i16>> = carets
                    .iter()
                    .map(|master| {
                        master.map(|carets| match &carets[i] {
                            CaretValueBuilder::Coordinate { default, deltas } => {
                                assert!(
                                    matches!(deltas, DeviceOrDeltas::None),
                                    "compile_for_merge cannot produce varying carets"
                                );
                                *default
                            }
                            CaretValueBuilder::PointIndex(_) => unreachable!("checked above"),
                        })
                    })
                    .collect();
                let Metric {
                    default,
                    device_or_deltas,
                } = self
                    .merge_scalars(&per_master)
                    .map_err(|message| MergeError::LigatureCaretDeltas { glyph, message })?;
                values.push(CaretValueBuilder::Coordinate {
                    default,
                    deltas: device_or_deltas,
                });
            }
            merged.insert(glyph, values);
        }
        self.merged
            .tables
            .gdef
            .get_or_insert_with(Default::default)
            .ligature_pos = merged;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use write_fonts::tables::layout::builders::{CaretValueBuilder, DeviceOrDeltas};

    use super::super::{MergeError, test_helpers::*};

    fn carets(fea_carets: &str) -> String {
        format!("table GDEF {{ {fea_carets} }} GDEF;")
    }

    #[test]
    fn caret_positions_vary() {
        let merged = merge_masters(&[
            &carets("LigatureCaretByPos f_i 300;"),
            &carets("LigatureCaretByPos f_i 320;"),
        ])
        .unwrap();
        let gdef = merged.tables.gdef.as_ref().unwrap();
        let f_i = glyph_map().get("f_i").unwrap();
        let [CaretValueBuilder::Coordinate { default, deltas }] =
            gdef.ligature_pos[&f_i].as_slice()
        else {
            panic!("expected one coordinate caret");
        };
        assert_eq!(*default, 300);
        let DeviceOrDeltas::Deltas(deltas) = deltas else {
            panic!("expected deltas");
        };
        assert_eq!(deltas.iter().map(|(_, d)| *d).collect::<Vec<_>>(), vec![20]);
    }

    #[test]
    fn equal_carets_stay_static_and_glyphs_are_unioned() {
        let merged = merge_masters(&[
            &carets("LigatureCaretByPos f_i 300 600; LigatureCaretByIndex c 4;"),
            &carets("LigatureCaretByPos f_i 300 600;"),
        ])
        .unwrap();
        let gdef = merged.tables.gdef.as_ref().unwrap();
        let map = glyph_map();
        assert_eq!(
            gdef.ligature_pos[&map.get("f_i").unwrap()],
            vec![
                CaretValueBuilder::Coordinate {
                    default: 300,
                    deltas: DeviceOrDeltas::None
                },
                CaretValueBuilder::Coordinate {
                    default: 600,
                    deltas: DeviceOrDeltas::None
                },
            ]
        );
        assert_eq!(
            gdef.ligature_pos[&map.get("c").unwrap()],
            vec![CaretValueBuilder::PointIndex(4)]
        );
    }

    #[test]
    fn caret_count_and_kind_must_agree() {
        assert!(matches!(
            merge_masters(&[
                &carets("LigatureCaretByPos f_i 300 600;"),
                &carets("LigatureCaretByPos f_i 300;"),
            ]),
            Err(MergeError::LigatureCarets { master: 1, .. })
        ));
        assert!(matches!(
            merge_masters(&[
                &carets("LigatureCaretByPos f_i 300;"),
                &carets("LigatureCaretByIndex f_i 3;"),
            ]),
            Err(MergeError::LigatureCarets { master: 1, .. })
        ));
    }

    #[test]
    fn carets_missing_at_the_default_are_an_error() {
        assert!(matches!(
            merge_masters(&[
                &carets("GlyphClassDef [a], , , ;"),
                &carets("LigatureCaretByPos f_i 300;")
            ]),
            Err(MergeError::LigatureCaretsMissingAtDefault { .. })
        ));
    }
}
