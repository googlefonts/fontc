//! Logic for tracking features during compilation

use std::collections::{BTreeMap, HashMap, HashSet};

use write_fonts::{
    tables::layout::SizeParams,
    types::{GlyphId, Tag},
};

use super::{
    language_system::{DefaultLanguageSystems, LanguageSystem},
    lookups::{FeatureKey, LookupId},
    tables::{NameBuilder, NameSpec},
    tags,
};

/// Tracking lookups in a feature block
pub(crate) struct ActiveFeature {
    tag: Tag,
    default_systems: DefaultLanguageSystems,
    current_lang_sys: Option<LanguageSystem>,
    lookups: HashMap<LanguageSystem, Vec<LookupId>>,
    script_default_lookups: HashMap<Tag, Vec<LookupId>>,
}

/// State required to generate the aalt feature.
///
/// This is a special and annoying case. We create this object when we encounter
/// the aalt feature block, and then we use this to generate the aalt lookups
/// once we've finished processing the input.
#[derive(Clone, Debug, Default)]
pub(crate) struct AaltFeature {
    aalt_features: Vec<Tag>,
    pub(crate) all_alts: HashMap<GlyphId, Vec<GlyphId>>,
    // to avoid duplicates
    all_pairs: HashSet<(GlyphId, GlyphId)>,
}

/// Helper for compiling the `size` feature
#[derive(Clone, Debug, Default)]
pub(crate) struct SizeFeature {
    pub design_size: u16,
    pub identifier: u16,
    pub range_start: u16,
    pub range_end: u16,
    pub names: Vec<NameSpec>,
}

/// If we are at the root of one of four magic features, we have special behaviour.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) enum SpecialVerticalFeatureState {
    /// we are not in a special vertical feature
    #[default]
    Ready,
    /// we are at the root of a special vertical feature (and so should behave specially)
    Root,
    /// we are inside a lookup in a special vertical feature (and so should not
    /// behave specially)
    InnerLookup,
}

impl ActiveFeature {
    pub(crate) fn new(tag: Tag, default_systems: DefaultLanguageSystems) -> Self {
        ActiveFeature {
            tag,
            script_default_lookups: Default::default(),
            lookups: Default::default(),
            current_lang_sys: Default::default(),
            default_systems,
        }
    }

    /// Change the active language system.
    ///
    /// This method is called when encountering 'script' and 'language' statements
    /// in a feature block. These statements have strange semantics, best documented
    /// in issues like <https://github.com/fonttools/fonttools/pull/1307>.
    ///
    /// This method handles figuring out what previously declared lookups should
    /// be included with the newly assigned language system.
    pub(crate) fn set_system(&mut self, system: LanguageSystem, exclude_dflt: bool) -> FeatureKey {
        // if the language is default, this is either the DFLT dflt system
        // or a script default (like latn dflt). In this second case, we keep
        // the script dflt lookups separate from the DFLT dflt lookups, because
        // it is possible to add them to a language that is not one of the
        // default language systems.

        // but if this is a fully-resolved language system, we add the default
        // lookups now, when we have access to the 'exclude_dflt' flag.
        if system.language != tags::LANG_DFLT {
            let mut lookups = Vec::new();
            if !exclude_dflt {
                let script_dflt = LanguageSystem {
                    script: system.script,
                    language: tags::LANG_DFLT,
                };
                // if *either* this is an explicit default, or this is part of
                // a script where script/dflt is an explicit default, and we have
                // seen a script keyword, add the default lookups
                if self.default_systems.contains(&system)
                    || (self.default_systems.contains(&script_dflt)
                        && self.script_default_lookups.contains_key(&system.script))
                {
                    lookups.extend(
                        self.lookups
                            .get(&LanguageSystem::default())
                            .into_iter()
                            .flat_map(|v| v.iter().copied()),
                    );
                }
                lookups.extend(
                    self.script_default_lookups
                        .get(&system.script)
                        .into_iter()
                        .flat_map(|v| v.iter().copied()),
                );
            }
            self.lookups.entry(system).or_insert_with(|| lookups);
        }

        self.current_lang_sys = Some(system);
        system.to_feature_key(self.tag)
    }

    pub(crate) fn add_lookup(&mut self, lookup: LookupId) {
        // there is a distinction between "implicit DFLT/dflt" and having
        // an explicit 'DFLT' script in the lookup block.
        let is_script_default = match self.current_lang_sys {
            None => false,
            Some(sys) => sys.language == tags::LANG_DFLT,
        };

        if is_script_default {
            self.script_default_lookups
                .entry(self.current_lang_sys.unwrap().script)
                .or_default()
                .push(lookup);
        } else {
            self.lookups
                .entry(self.current_lang_sys.unwrap_or_default())
                .or_default()
                .push(lookup);
        }
    }

    /// take the lookups for this feature, and add them to the Big List Of Features
    pub(crate) fn add_to_features(mut self, features: &mut BTreeMap<FeatureKey, Vec<LookupId>>) {
        // remove the default lookups; we will add them back later if DFLT dflt
        // is registered
        let defaults = self
            .lookups
            .remove(&LanguageSystem::default())
            .unwrap_or_default();

        // first, update our internal list of lookups to include all script defaults
        for (script, mut lookups) in self.script_default_lookups {
            let system = LanguageSystem {
                script,
                ..Default::default()
            };

            // only add root defaults if this is a registered default system
            if self.default_systems.contains(&system) {
                lookups = defaults.iter().copied().chain(lookups).collect();
            };
            assert!(
                self.lookups.insert(system, lookups).is_none(),
                "script defaults never added before now"
            );
        }

        // then, add our default lookups to any registered default system that
        // we haven't explicity handled before now
        for system in self.default_systems.iter() {
            self.lookups
                .entry(system)
                .or_insert_with(|| defaults.clone());
        }

        // Now our internal lookups map is up to date, and we can use it to update
        // the main map. Since there can be multiple blocks for the same feature,
        // we are always appending, not just setting
        for (system, lookups) in self.lookups {
            let key = system.to_feature_key(self.tag);
            match features.entry(key) {
                std::collections::btree_map::Entry::Occupied(slot) => {
                    slot.into_mut().extend(lookups)
                }
                std::collections::btree_map::Entry::Vacant(slot) => {
                    slot.insert(lookups);
                }
            }
        }
    }

    #[cfg(test)]
    fn build_features(self) -> BTreeMap<FeatureKey, Vec<LookupId>> {
        let mut out = Default::default();
        self.add_to_features(&mut out);
        out
    }
}

impl SizeFeature {
    pub(crate) fn build(&self, names: &mut NameBuilder) -> SizeParams {
        let name_entry = if self.identifier == 0 {
            assert!(self.names.is_empty());
            0
        } else {
            assert!(!self.names.is_empty());
            names.add_anon_group(&self.names).to_u16()
        };
        SizeParams {
            design_size: self.design_size,
            identifier: self.identifier,
            name_entry,
            range_start: self.range_start,
            range_end: self.range_end,
        }
    }
}

impl AaltFeature {
    pub(crate) fn add_feature_reference(&mut self, feature: Tag) {
        self.aalt_features.push(feature);
    }

    pub(crate) fn features(&self) -> &[Tag] {
        &self.aalt_features
    }

    pub(crate) fn add(&mut self, target: GlyphId, alt: GlyphId) {
        if self.all_pairs.insert((target, alt)) {
            self.all_alts.entry(target).or_default().push(alt);
        }
    }
}

impl Extend<(GlyphId, GlyphId)> for AaltFeature {
    fn extend<T: IntoIterator<Item = (GlyphId, GlyphId)>>(&mut self, iter: T) {
        for (target, alt) in iter.into_iter() {
            self.add(target, alt)
        }
    }
}

impl SpecialVerticalFeatureState {
    const VERTICAL_FEATURES: &[Tag] = &[
        Tag::new(b"valt"),
        Tag::new(b"vhal"),
        Tag::new(b"vkrn"),
        Tag::new(b"vpal"),
    ];

    pub(crate) fn begin_feature(&mut self, tag: Tag) {
        if Self::VERTICAL_FEATURES.contains(&tag) {
            *self = Self::Root;
        }
    }

    pub(crate) fn end_feature(&mut self) {
        *self = Self::Ready;
    }

    pub(crate) fn begin_lookup_block(&mut self) {
        if *self == Self::Root {
            *self = Self::InnerLookup;
        }
    }

    pub(crate) fn end_lookup_block(&mut self) {
        if *self == Self::InnerLookup {
            *self = Self::Root;
        }
    }

    pub(crate) fn in_eligible_vertical_feature(&self) -> bool {
        *self == Self::Root
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const fn langsys(script: &str, lang: &str) -> LanguageSystem {
        LanguageSystem {
            script: Tag::new(script.as_bytes()),
            language: Tag::new(lang.as_bytes()),
        }
    }

    fn make_ids<const N: usize>() -> [LookupId; N] {
        let mut out = [LookupId::Empty; N];
        #[allow(clippy::needless_range_loop)]
        for i in 0..N {
            out[i] = LookupId::Gpos(i);
        }
        out
    }

    fn default_systems<const N: usize>(inp: [LanguageSystem; N]) -> DefaultLanguageSystems {
        let mut out = DefaultLanguageSystems::default();
        inp.into_iter().for_each(|sys| out.insert(sys));
        out
    }

    const DFLT_DFLT: LanguageSystem = langsys("DFLT", "dflt");
    const LATN_DFLT: LanguageSystem = langsys("latn", "dflt");
    const LATN_DEU: LanguageSystem = langsys("latn", "DEU");
    const LATN_TRK: LanguageSystem = langsys("latn", "TRK");
    const LATN_POL: LanguageSystem = langsys("latn", "POL");
    const TAG_TEST: Tag = Tag::new(b"test");

    #[test]
    fn non_default_script_default() {
        let default_systems = default_systems([DFLT_DFLT, LATN_DEU, LATN_POL]);
        let [id_1, id_2] = make_ids();

        let mut feature = ActiveFeature::new(TAG_TEST, default_systems);
        feature.add_lookup(id_1); // added to default lookups
        feature.set_system(LATN_DFLT, false);
        feature.add_lookup(id_2); // added to script-default lookups

        feature.set_system(LATN_TRK, false);
        feature.set_system(LATN_POL, false);

        let built = feature.build_features();

        // should have script default, but not root default (as is not a registered default)
        let key = LATN_TRK.to_feature_key(TAG_TEST);
        assert_eq!(built.get(&key), Some(&vec![id_2]));

        // should have root default, but not default (as was not explicitly set
        // as a language)
        let key = LATN_DEU.to_feature_key(TAG_TEST);
        assert_eq!(built.get(&key), Some(&vec![id_1]));

        // should have both:
        let key = LATN_POL.to_feature_key(TAG_TEST);
        assert_eq!(built.get(&key), Some(&vec![id_1, id_2]));
    }

    const DFLT_FRE: LanguageSystem = langsys("DFLT", "FRE");
    const DFLT_ABC: LanguageSystem = langsys("DFLT", "ABC");
    const LATN_ABC: LanguageSystem = langsys("latn", "ABC");
    const LATN_FRE: LanguageSystem = langsys("latn", "FRE");
    const LATN_DEF: LanguageSystem = langsys("latn", "DEF");

    // <https://github.com/fonttools/fonttools/pull/1307>
    #[test]
    fn fonttools_1307() {
        let defaults = default_systems([DFLT_DFLT, DFLT_FRE, DFLT_ABC, LATN_DFLT, LATN_ABC]);
        let [id1, id2, id3, id4, id5, id6, id7, id8] = make_ids();

        let mut feature = ActiveFeature::new(TAG_TEST, defaults);
        feature.add_lookup(id1);
        feature.set_system(DFLT_DFLT, false);
        feature.add_lookup(id2);
        feature.set_system(DFLT_DFLT, false);
        feature.add_lookup(id3);
        feature.set_system(DFLT_FRE, false);
        feature.add_lookup(id4);
        feature.set_system(LATN_DFLT, false);
        feature.add_lookup(id5);
        feature.set_system(LATN_DFLT, false);
        feature.add_lookup(id6);
        feature.set_system(LATN_FRE, false);
        feature.add_lookup(id7);
        feature.set_system(LATN_DEF, true);
        feature.add_lookup(id8);

        let built = feature.build_features();
        // the root defaults, as well as explicit DFLT script defaults
        let key = DFLT_DFLT.to_feature_key(TAG_TEST);
        assert_eq!(built.get(&key), Some(&vec![id1, id2, id3]));
        // only the root defaults, not defaults afer first script statement
        let key = DFLT_ABC.to_feature_key(TAG_TEST);
        assert_eq!(built.get(&key), Some(&vec![id1]));
        // DFLT_DFLT + the explicit lookup added for this system
        let key = DFLT_FRE.to_feature_key(TAG_TEST);
        assert_eq!(built.get(&key), Some(&vec![id1, id2, id3, id4]));
        // root defaults, plus explicit f/g
        let key = LATN_DFLT.to_feature_key(TAG_TEST);
        assert_eq!(built.get(&key), Some(&vec![id1, id5, id6]));
        // LATN_DFLT + explicit h
        let key = LATN_FRE.to_feature_key(TAG_TEST);
        assert_eq!(built.get(&key), Some(&vec![id1, id5, id6, id7]));
        // only the root defaults
        let key = LATN_ABC.to_feature_key(TAG_TEST);
        assert_eq!(built.get(&key), Some(&vec![id1]));
        // only the explicit 'i' (excludes defaults)
        let key = LATN_DEF.to_feature_key(TAG_TEST);
        assert_eq!(built.get(&key), Some(&vec![id8]));
    }
}
