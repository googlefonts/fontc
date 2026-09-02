//! Merging per-master compilations into a single variable compilation.
//!
//! This is the fea-rs counterpart of fontTools' `varLib.merger`, applied one
//! stage earlier: instead of merging built GPOS/GDEF tables we merge the
//! compiler's intermediate state, before the feature writer has run and before
//! any `ItemVariationStore` exists.

use std::collections::HashMap;

use fontdrasil::coords::NormalizedLocation;
use smol_str::SmolStr;
use write_fonts::types::GlyphId16;

use super::{PendingCompilation, VariationInfo};

mod cursive;
mod error;
mod lookups;
mod metric;
mod single;
#[cfg(test)]
mod test_helpers;

pub use error::{LookupRef, MergeError};

/// Merge per-master compilations of non-variable FEA into one variable compilation.
///
/// Each input is the output of [`compile_for_merge`] for one master, paired
/// with that master's location. The caller must ensure that:
///
/// - the master at index 0 is the default master, i.e. its location is the
///   default location of `var_info`'s variation model;
/// - locations are normalized, and every master's location is one the
///   variation model knows about;
/// - only masters that actually have FEA are included. A master without a
///   feature file is not "an empty feature file"; leave it out.
///
/// Everything that cannot vary must be identical across masters: language
/// systems, features and the lookups they reference, GSUB, conditionsets,
/// mark filtering sets, and so on. GDEF glyph classes and mark class
/// membership are unioned, with an error if a glyph is classified
/// differently in two masters. Tables other than GPOS and GDEF, and any
/// warnings, are taken from the default master; the caller should report
/// each master's own diagnostics before merging.
///
/// [`compile_for_merge`]: super::compile_for_merge
pub fn merge<V: VariationInfo>(
    masters: Vec<(NormalizedLocation, PendingCompilation)>,
    var_info: &V,
) -> Result<PendingCompilation, MergeError> {
    let mut ctx = MergeCtx::new(masters, var_info)?;
    ctx.check_structure()?;
    ctx.merge_mark_classes()?;
    ctx.merge_gdef()?;
    ctx.merge_gpos()?;
    ctx.finish()
}

/// The state of a merge in progress.
///
/// `merged` starts as the default master and is updated in place; `others`
/// are the remaining masters, so master `i + 1` is `others[i]`. `locations`
/// has one entry per master in that same order, the default first.
struct MergeCtx<'a, V> {
    merged: PendingCompilation,
    others: Vec<PendingCompilation>,
    locations: Vec<NormalizedLocation>,
    var_info: &'a V,
}

impl<'a, V: VariationInfo> MergeCtx<'a, V> {
    fn new(
        masters: Vec<(NormalizedLocation, PendingCompilation)>,
        var_info: &'a V,
    ) -> Result<Self, MergeError> {
        let mut seen = HashMap::new();
        for (i, (location, _)) in masters.iter().enumerate() {
            if let Some(first) = seen.insert(location.clone(), i) {
                return Err(MergeError::DuplicateLocation { first, second: i });
            }
        }
        let (locations, compilations): (Vec<_>, Vec<_>) = masters.into_iter().unzip();
        let mut compilations = compilations.into_iter();
        let Some(mut merged) = compilations.next() else {
            return Err(MergeError::NoMasters);
        };
        assert!(merged.lig_carets_from_feature_writer.is_empty());
        merged.axis_count = var_info.axis_count();
        Ok(MergeCtx {
            merged,
            others: compilations.collect(),
            locations,
            var_info,
        })
    }

    /// Run any checks that need the fully merged result, and return it.
    fn finish(self) -> Result<PendingCompilation, MergeError> {
        Ok(self.merged)
    }

    /// Check that everything which cannot vary is the same in every master.
    fn check_structure(&self) -> Result<(), MergeError> {
        let default = &self.merged;
        for (i, other) in self.others.iter().enumerate() {
            let master = i + 1;
            if default.default_lang_systems != other.default_lang_systems {
                return Err(MergeError::LanguageSystems { master });
            }
            if default.insert_markers != other.insert_markers {
                return Err(MergeError::InsertMarkers { master });
            }
            if default.conditionset_defs != other.conditionset_defs {
                return Err(MergeError::ConditionSets { master });
            }
            if default.opts.compile_gsub != other.opts.compile_gsub
                || default.opts.compile_gpos != other.opts.compile_gpos
                || default.opts.compile_debg != other.opts.compile_debg
            {
                return Err(MergeError::Options { master });
            }
            if default.mark_filter_sets != other.mark_filter_sets {
                return Err(MergeError::MarkFilterSets { master });
            }
            if default.mark_attach_class_id != other.mark_attach_class_id {
                return Err(MergeError::MarkAttachClasses { master });
            }
            if default.lookups.named() != other.lookups.named() {
                return Err(MergeError::NamedLookups { master });
            }
            if default.lookups.gsub() != other.lookups.gsub() {
                return Err(MergeError::Gsub { master });
            }
            if default.features != other.features {
                return Err(MergeError::Features { master });
            }
        }
        Ok(())
    }

    /// Union mark class membership.
    ///
    /// Only membership matters here: the anchors are merged where they are
    /// used, in the mark lookups, and this map only feeds GDEF glyph class
    /// inference.
    fn merge_mark_classes(&mut self) -> Result<(), MergeError> {
        let merged = &mut self.merged.mark_classes;
        let mut membership: HashMap<GlyphId16, SmolStr> = merged
            .iter()
            .flat_map(|(name, class)| {
                class
                    .members
                    .iter()
                    .flat_map(|(glyphs, _)| glyphs.iter())
                    .map(move |glyph| (glyph, name.clone()))
            })
            .collect();

        for (i, other) in self.others.iter().enumerate() {
            for (name, class) in &other.mark_classes {
                for (glyphs, anchor) in &class.members {
                    let mut is_new = false;
                    for glyph in glyphs.iter() {
                        match membership.get(&glyph) {
                            Some(existing) if existing != name => {
                                return Err(MergeError::MarkClassConflict {
                                    master: i + 1,
                                    glyph,
                                    expected: existing.clone(),
                                    found: name.clone(),
                                });
                            }
                            Some(_) => (),
                            None => {
                                membership.insert(glyph, name.clone());
                                is_new = true;
                            }
                        }
                    }
                    if is_new {
                        merged
                            .entry(name.clone())
                            .or_default()
                            .members
                            .push((glyphs.clone(), anchor.clone()));
                    }
                }
            }
        }
        Ok(())
    }

    fn merge_gdef(&mut self) -> Result<(), MergeError> {
        for (i, other) in self.others.iter().enumerate() {
            let master = i + 1;
            let Some(other) = other.tables.gdef.as_ref() else {
                continue;
            };
            let merged = self.merged.tables.gdef.get_or_insert_with(Default::default);
            if merged.attach != other.attach {
                return Err(MergeError::GdefAttach { master });
            }
            //TODO: merge these instead of requiring equality
            if merged.ligature_pos != other.ligature_pos {
                return Err(MergeError::LigatureCarets { master });
            }
            // sorted so that the reported conflict is deterministic
            let mut classes: Vec<_> = other.glyph_classes.iter().collect();
            classes.sort();
            for (glyph, class) in classes {
                match merged.glyph_classes.insert(*glyph, *class) {
                    Some(expected) if expected != *class => {
                        return Err(MergeError::GlyphClassConflict {
                            master,
                            glyph: *glyph,
                            expected,
                            found: *class,
                        });
                    }
                    _ => (),
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use write_fonts::{tables::gdef::GlyphClassDef, types::Tag};

    use super::{test_helpers::*, *};
    use crate::{Kind, compile::NopFeatureProvider};

    #[test]
    fn identical_masters_round_trip() {
        assert_eq!(
            merged_binary(&[KITCHEN_SINK, KITCHEN_SINK, KITCHEN_SINK]),
            one_shot_binary(KITCHEN_SINK)
        );
    }

    #[test]
    fn single_master_is_identity() {
        assert_eq!(
            merged_binary(&[KITCHEN_SINK]),
            one_shot_binary(KITCHEN_SINK)
        );
    }

    #[test]
    fn no_masters() {
        assert_eq!(merge_masters(&[]).err(), Some(MergeError::NoMasters));
    }

    #[test]
    fn duplicate_locations() {
        let masters = vec![
            (location(0.0), pending(KITCHEN_SINK)),
            (location(1.0), pending(KITCHEN_SINK)),
            (location(0.0), pending(KITCHEN_SINK)),
        ];
        assert_eq!(
            merge(masters, &var_info()).err(),
            Some(MergeError::DuplicateLocation {
                first: 0,
                second: 2
            })
        );
    }

    #[test]
    fn gsub_must_match() {
        let a = "feature liga { sub a b by f_i; } liga;";
        let b = "feature liga { sub a c by f_i; } liga;";
        assert_eq!(
            merge_masters(&[a, a, b]).err(),
            Some(MergeError::Gsub { master: 2 })
        );
    }

    #[test]
    fn language_systems_must_match() {
        let a = "languagesystem DFLT dflt; feature kern { pos a b -20; } kern;";
        let b = "languagesystem latn dflt; feature kern { pos a b -20; } kern;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::LanguageSystems { master: 1 })
        );
    }

    #[test]
    fn features_must_match() {
        let a = "lookup K { pos a b -20; } K; feature kern { lookup K; } kern;";
        let b = "lookup K { pos a b -20; } K; feature kern { lookup K; } kern; feature dist { lookup K; } dist;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::Features { master: 1 })
        );
    }

    #[test]
    fn insert_markers_must_match() {
        let a = "feature kern { pos a b -20; } kern;";
        let b = "feature kern { # Automatic Code\n pos a b -20; } kern;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::InsertMarkers { master: 1 })
        );
    }

    #[test]
    fn gdef_glyph_classes_are_unioned() {
        let a = "table GDEF { GlyphClassDef [a], , [acute], ; } GDEF;";
        let b = "table GDEF { GlyphClassDef [b], , [acute], ; } GDEF;";
        let (compilation, _) = merge_masters(&[a, b])
            .unwrap()
            .finish::<NopFeatureProvider>(None)
            .unwrap();
        let classes = compilation.gdef_classes.unwrap();
        let map = glyph_map();
        let class_of = |name: &str| classes.get(&map.get(name).unwrap()).copied();
        assert_eq!(class_of("a"), Some(GlyphClassDef::Base));
        assert_eq!(class_of("b"), Some(GlyphClassDef::Base));
        assert_eq!(class_of("acute"), Some(GlyphClassDef::Mark));
        assert_eq!(class_of("c"), None);
    }

    #[test]
    fn gdef_glyph_class_conflict() {
        let a = "table GDEF { GlyphClassDef [a], , [acute], ; } GDEF;";
        let b = "table GDEF { GlyphClassDef [acute], , [a], ; } GDEF;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::GlyphClassConflict {
                master: 1,
                glyph: glyph_map().get("a").unwrap(),
                expected: GlyphClassDef::Base,
                found: GlyphClassDef::Mark,
            })
        );
    }

    #[test]
    fn mark_class_conflict() {
        let a = "markClass acute <anchor 0 0> @TOP; feature mark { pos base a <anchor 0 0> mark @TOP; } mark;";
        let b = "markClass acute <anchor 0 0> @BOTTOM; feature mark { pos base a <anchor 0 0> mark @BOTTOM; } mark;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::MarkClassConflict {
                master: 1,
                glyph: glyph_map().get("acute").unwrap(),
                expected: "@TOP".into(),
                found: "@BOTTOM".into(),
            })
        );
    }

    #[test]
    fn mark_classes_are_unioned() {
        let a = "markClass acute <anchor 0 0> @TOP; feature mark { pos base a <anchor 0 0> mark @TOP; } mark;";
        let b = "markClass [acute grave] <anchor 0 0> @TOP; feature mark { pos base a <anchor 0 0> mark @TOP; } mark;";
        // the mark lookups differ, so this stops at the lookup merge;
        // what matters is that the mark class union itself is accepted.
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::Unsupported {
                lookup: lookup_ref(0, None, Some(Tag::new(b"mark"))),
                kind: Kind::GposType4,
            })
        );
    }
}
