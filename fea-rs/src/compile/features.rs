//! Logic for tracking features during compilation

use std::collections::{hash_map, BTreeMap, HashMap, HashSet};

use smol_str::SmolStr;
use write_fonts::{
    tables::layout::{ConditionSet, FeatureParams, SizeParams, StylisticSetParams},
    types::{GlyphId16, Tag, Uint24},
};

use super::{
    language_system::{DefaultLanguageSystems, LanguageSystem},
    lookups::{AllLookups, FeatureKey, IterAaltPairs, LookupId, LookupIdMap},
    tables::{NameBuilder, NameSpec},
    tags,
};

#[derive(Clone, Debug, Default)]
pub(crate) struct FeatureLookups {
    /// the base (not variation specific) lookups
    pub(crate) base: Vec<LookupId>,
    pub(crate) variations: HashMap<ConditionSet, Vec<LookupId>>,
}

/// A type to store accumulated features during compilation
///
/// We update this type as we encounter feature blocks in the source FEA.
#[derive(Clone, Debug, Default)]
pub(crate) struct AllFeatures {
    pub(crate) features: BTreeMap<FeatureKey, FeatureLookups>,
    required_features: HashSet<FeatureKey>,
    pub(crate) size: Option<SizeFeature>,
    pub(crate) aalt: Option<AaltFeature>,
    pub(crate) stylistic_sets: BTreeMap<Tag, Vec<NameSpec>>,
    pub(crate) character_variants: HashMap<Tag, CvParams>,
}

/// Tracking state within a feature block
///
/// When we're inside a 'feature' block we need to track the current language
/// and script, and add any lookups as appropriate; this struct handles that
/// logic.
pub(crate) struct ActiveFeature {
    pub(crate) tag: Tag,
    condition_set: Option<ConditionSet>,
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
    pub(crate) all_alts: HashMap<GlyphId16, Vec<GlyphId16>>,
    // to avoid duplicates
    all_pairs: HashSet<(GlyphId16, GlyphId16)>,
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

#[derive(Clone, Debug, Default)]
pub(crate) struct CvParams {
    pub feat_ui_label_name: Vec<NameSpec>,
    pub feat_ui_tooltip_text_name: Vec<NameSpec>,
    pub sample_text_name: Vec<NameSpec>,
    pub param_ui_label_names: Vec<Vec<NameSpec>>,
    pub characters: Vec<char>,
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

/// maps names to conditionsets, also tracking declaration order (which
/// is maintained in the final output table)
#[derive(Clone, Debug, Default)]
pub(crate) struct ConditionSetMap {
    named_conditionsets: HashMap<SmolStr, ConditionSet>,
    // used for sorting
    reference_order: HashMap<ConditionSet, usize>,
}

impl AllFeatures {
    pub(crate) fn get_or_insert(&mut self, key: FeatureKey) -> &mut FeatureLookups {
        self.features.entry(key).or_default()
    }

    pub(crate) fn insert(&mut self, key: FeatureKey, base_lookups: Vec<LookupId>) {
        self.get_or_insert(key).base = base_lookups;
    }

    pub(crate) fn add_required(&mut self, key: FeatureKey) {
        self.required_features.insert(key);
    }

    pub(crate) fn is_required(&self, key: &FeatureKey) -> bool {
        self.required_features.contains(key)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&FeatureKey, &FeatureLookups)> {
        self.features.iter()
    }

    pub(crate) fn remap_ids(&mut self, id_map: &LookupIdMap) {
        self.features
            .values_mut()
            .for_each(|lookups| lookups.remap_ids(id_map));
    }

    pub(crate) fn finalize_aalt(
        &mut self,
        all_lookups: &mut AllLookups,
        default_lang_systems: &DefaultLanguageSystems,
    ) {
        let Some(mut aalt) = self.aalt.take() else {
            return;
        };
        // add all the relevant lookups from the referenced features
        let mut relevant_lookups = vec![vec![]; aalt.features().len()];
        // first sort all lookups by the order of the tags in the aalt table:
        for (key, feat_lookups) in self.features.iter() {
            let Some(feat_idx) = aalt.features().iter().position(|tag| *tag == key.feature) else {
                continue;
            };
            relevant_lookups[feat_idx].extend(feat_lookups.base.iter().copied().flat_map(|idx| {
                all_lookups
                    .aalt_lookups(idx)
                    .into_iter()
                    .map(move |lookup| (idx, lookup))
            }))
        }

        // then within a given tag we want to sort the lookups by lookup id;
        // this isn't exactly described in the spec but it matches the (implicit)
        // behaviour of fonttools.
        for lookups in &mut relevant_lookups {
            lookups.sort_by_key(|(idx, _)| *idx);
        }
        // now go through the lookups, ordered by appearance of feature in aalt
        for (_, lookup) in relevant_lookups.iter().flat_map(|x| x.iter()) {
            match lookup {
                super::lookups::SubstitutionLookup::Single(lookup) => aalt.extend(
                    lookup
                        .iter_subtables()
                        .flat_map(|sub| sub.iter_aalt_pairs()),
                ),
                super::lookups::SubstitutionLookup::Alternate(lookup) => aalt.extend(
                    lookup
                        .iter_subtables()
                        .flat_map(|sub| sub.iter_aalt_pairs()),
                ),
                super::lookups::SubstitutionLookup::Multiple(lookup) => {
                    aalt.extend(
                        lookup
                            .iter_subtables()
                            .flat_map(|sub| sub.iter_aalt_pairs()),
                    );
                }

                super::lookups::SubstitutionLookup::Ligature(lookup) => {
                    aalt.extend(
                        lookup
                            .iter_subtables()
                            .flat_map(|sub| sub.iter_aalt_pairs()),
                    );
                }

                _ => (),
            }
        }

        // aalt lookups get inserted at the front, but behind rvrn lookups:
        let insert_point = self
            .features
            .iter()
            .filter(|(k, _)| k.feature == "rvrn")
            .flat_map(|(_, feat)| feat.iter_ids())
            .collect::<HashSet<_>>()
            .len();

        // now we have all of our referenced lookups, and so we want to use that
        // to construct the aalt lookups:
        let aalt_lookup_indices =
            all_lookups.insert_aalt_lookups(insert_point, std::mem::take(&mut aalt.all_alts));

        // now adjust our previously set lookupids, which are now invalid,
        // since we're going to insert the aalt lookups in front of the lookup
        // list:
        self.adjust_gsub_ids(insert_point, aalt_lookup_indices.len());
        // finally add the aalt feature to all the default language systems
        for sys in default_lang_systems.iter() {
            self.insert(sys.to_feature_key(tags::AALT), aalt_lookup_indices.clone());
        }

        self.aalt = Some(aalt);
    }

    pub(crate) fn dedupe_lookups(&mut self) {
        // if any duplicate lookups have made their way into our features, remove them;
        // they will be ignored by the shaper anyway.
        let mut seen = HashSet::new();
        for lookup in self.features.values_mut() {
            seen.clear();
            lookup.base.retain(|id| seen.insert(*id));
            lookup.base.sort();
        }
    }

    // when adding the aalt feature we insert a bunch of lookups at the front
    // of the lookup list, which requires us to adjust any previously-computed
    // lookup ids
    fn adjust_gsub_ids(&mut self, from: usize, delta: usize) {
        for feat in self.features.values_mut() {
            feat.adjust_gsub_ids(from, delta)
        }
    }

    pub(crate) fn build_feature_params(
        &self,
        name_builder: &mut NameBuilder,
    ) -> HashMap<Tag, FeatureParams> {
        let mut result = HashMap::new();
        if let Some(size) = &self.size {
            result.insert(tags::SIZE, FeatureParams::Size(size.build(name_builder)));
        }

        for (tag, names) in self.stylistic_sets.iter() {
            let id = name_builder.add_anon_group(names);
            let params = FeatureParams::StylisticSet(StylisticSetParams::new(id));
            result.insert(*tag, params);
        }

        for (tag, cv_params) in self.character_variants.iter() {
            let params = cv_params.build(name_builder);
            result.insert(*tag, FeatureParams::CharacterVariant(params));
        }
        result
    }

    //FIXME: what do we do with conflicts?
    pub(crate) fn merge_external_features(
        &mut self,
        features: BTreeMap<FeatureKey, FeatureLookups>,
    ) {
        for (key, lookups) in features {
            let thingie = self.get_or_insert(key);
            thingie.base.extend(lookups.base);
            for (condset, lookups) in lookups.variations {
                match thingie.variations.entry(condset) {
                    // this is very unlikely; it would require feature variations
                    // specified in FEA as well as from a designspace or similar.
                    // In this case we will just append, but maybe we should log too?
                    hash_map::Entry::Occupied(entry) => {
                        let var_lookups = entry.into_mut();
                        var_lookups.extend_from_slice(&lookups);
                        log::warn!("feature variations exist for {} in FEA and also in feature writer; this is weird!", key.feature);
                    }
                    hash_map::Entry::Vacant(entry) => {
                        entry.insert(lookups);
                    }
                }
            }
        }
    }

    #[cfg(test)]
    fn get_base(&self, key: &FeatureKey) -> Option<&[LookupId]> {
        self.features.get(key).map(|x| x.base.as_slice())
    }
}

impl FeatureLookups {
    fn adjust_gsub_ids(&mut self, from: usize, delta: usize) {
        self.base
            .iter_mut()
            .chain(self.variations.values_mut().flat_map(|x| x.iter_mut()))
            .filter(|id| id.to_raw() >= from)
            .for_each(|id| id.adjust_if_gsub(delta));
    }

    pub(crate) fn remap_ids(&mut self, id_map: &LookupIdMap) {
        self.base
            .iter_mut()
            .chain(self.variations.values_mut().flat_map(|x| x.iter_mut()))
            .for_each(|id| *id = id_map.get(*id));
    }

    pub(crate) fn iter_ids(&self) -> impl Iterator<Item = LookupId> + use<'_> {
        self.base
            .iter()
            .chain(self.variations.values().flat_map(|v| v.iter()))
            .copied()
    }

    // split lookups into gpos/gsub
    pub(crate) fn split_base_lookups(&self) -> (Vec<u16>, Vec<u16>) {
        split_lookups(&self.base)
    }

    pub(crate) fn split_variations(&self) -> Vec<(&ConditionSet, Vec<u16>, Vec<u16>)> {
        self.variations
            .iter()
            .map(|(cond, lookups)| {
                let (gpos, gsub) = split_lookups(lookups);
                (cond, gpos, gsub)
            })
            .collect()
    }
}

impl ActiveFeature {
    pub(crate) fn new(
        tag: Tag,
        default_systems: DefaultLanguageSystems,
        condition_set: Option<ConditionSet>,
    ) -> Self {
        ActiveFeature {
            tag,
            condition_set,
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
    pub(crate) fn add_to_features(mut self, features: &mut AllFeatures) {
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
            let feature = features.get_or_insert(key);
            let to_add = match self.condition_set.clone() {
                Some(condset) => feature.variations.entry(condset).or_default(),
                None => &mut feature.base,
            };
            to_add.extend(lookups);
            to_add.sort();
        }
    }

    #[cfg(test)]
    fn build_features(self) -> AllFeatures {
        let mut out = Default::default();
        self.add_to_features(&mut out);
        out
    }
}

/// Given a slice of lookupids, split them into (GPOS, GSUB)
///
/// In general, a feature only has either GSUB or GPOS lookups, but this is not
/// a requirement, and in the wild we will encounter features that contain mixed
/// lookups.
fn split_lookups(lookups: &[LookupId]) -> (Vec<u16>, Vec<u16>) {
    let mut gpos = Vec::new();
    let mut gsub = Vec::new();
    for lookup in lookups {
        match lookup {
            LookupId::Gpos(_) => gpos.push(lookup.to_gpos_id_or_die()),
            LookupId::Gsub(_) => gsub.push(lookup.to_gsub_id_or_die()),
            LookupId::Empty => (),
            LookupId::ExternalGpos(_)
            | LookupId::ExternalGsub(_)
            | LookupId::ExternalFrontOfList(_) => {
                panic!("external lookups should not be present at split time")
            }
        }
    }

    (gpos, gsub)
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

    pub(crate) fn add(&mut self, target: GlyphId16, alt: GlyphId16) {
        if self.all_pairs.insert((target, alt)) {
            self.all_alts.entry(target).or_default().push(alt);
        }
    }
}

impl Extend<(GlyphId16, GlyphId16)> for AaltFeature {
    fn extend<T: IntoIterator<Item = (GlyphId16, GlyphId16)>>(&mut self, iter: T) {
        for (target, alt) in iter.into_iter() {
            self.add(target, alt)
        }
    }
}

impl CvParams {
    pub(crate) fn build(
        &self,
        names: &mut NameBuilder,
    ) -> write_fonts::tables::layout::CharacterVariantParams {
        let mut out = write_fonts::tables::layout::CharacterVariantParams::default();
        if !self.feat_ui_label_name.is_empty() {
            out.feat_ui_label_name_id = names.add_anon_group(&self.feat_ui_label_name);
        }
        if !self.feat_ui_tooltip_text_name.is_empty() {
            out.feat_ui_tooltip_text_name_id =
                names.add_anon_group(&self.feat_ui_tooltip_text_name);
        }

        if !self.sample_text_name.is_empty() {
            out.sample_text_name_id = names.add_anon_group(&self.sample_text_name);
        }

        if let Some((first, rest)) = self.param_ui_label_names.split_first() {
            out.first_param_ui_label_name_id = names.add_anon_group(first);
            for item in rest {
                names.add_anon_group(item);
            }
        }
        out.num_named_parameters = self.param_ui_label_names.len().try_into().unwrap();
        for c in &self.characters {
            out.character.push(Uint24::checked_new(*c as _).unwrap());
        }

        out
    }
}

impl SpecialVerticalFeatureState {
    const VERTICAL_FEATURES: &'static [Tag] = &[
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

impl ConditionSetMap {
    pub(crate) fn insert(&mut self, label: SmolStr, conditionset: ConditionSet) {
        self.named_conditionsets.insert(label, conditionset);
    }

    pub(crate) fn get(&self, label: &SmolStr) -> Option<&ConditionSet> {
        self.named_conditionsets.get(label)
    }

    // track the order in which conditionsets are used; this is used for sorting
    // the output.
    pub(crate) fn register_use(&mut self, conditionset: &ConditionSet) {
        if !self.reference_order.contains_key(conditionset) {
            self.reference_order
                .insert(conditionset.to_owned(), self.reference_order.len());
        }
    }

    pub(crate) fn sort_order(&self, conditionset: &ConditionSet) -> usize {
        // only used after all conditionsets in the file have been added
        *self.reference_order.get(conditionset).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const fn langsys(script: &[u8; 4], lang: &[u8; 4]) -> LanguageSystem {
        LanguageSystem {
            script: Tag::new(script),
            language: Tag::new(lang),
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

    const DFLT_DFLT: LanguageSystem = langsys(b"DFLT", b"dflt");
    const LATN_DFLT: LanguageSystem = langsys(b"latn", b"dflt");
    const LATN_DEU: LanguageSystem = langsys(b"latn", b"DEU ");
    const LATN_TRK: LanguageSystem = langsys(b"latn", b"TRK ");
    const LATN_POL: LanguageSystem = langsys(b"latn", b"POL ");
    const TAG_TEST: Tag = Tag::new(b"test");

    #[test]
    fn non_default_script_default() {
        let default_systems = default_systems([DFLT_DFLT, LATN_DEU, LATN_POL]);
        let [id_1, id_2] = make_ids();

        let mut feature = ActiveFeature::new(TAG_TEST, default_systems, None);
        feature.add_lookup(id_1); // added to default lookups
        feature.set_system(LATN_DFLT, false);
        feature.add_lookup(id_2); // added to script-default lookups

        feature.set_system(LATN_TRK, false);
        feature.set_system(LATN_POL, false);

        let built = feature.build_features();

        // should have script default, but not root default (as is not a registered default)
        let key = LATN_TRK.to_feature_key(TAG_TEST);
        assert_eq!(built.get_base(&key), Some([id_2].as_slice()));

        // should have root default, but not default (as was not explicitly set
        // as a language)
        let key = LATN_DEU.to_feature_key(TAG_TEST);
        assert_eq!(built.get_base(&key), Some([id_1].as_slice()));

        // should have both:
        let key = LATN_POL.to_feature_key(TAG_TEST);
        assert_eq!(built.get_base(&key), Some([id_1, id_2].as_slice()));
    }

    const DFLT_FRE: LanguageSystem = langsys(b"DFLT", b"FRE ");
    const DFLT_ABC: LanguageSystem = langsys(b"DFLT", b"ABC ");
    const LATN_ABC: LanguageSystem = langsys(b"latn", b"ABC ");
    const LATN_FRE: LanguageSystem = langsys(b"latn", b"FRE ");
    const LATN_DEF: LanguageSystem = langsys(b"latn", b"DEF ");

    // <https://github.com/fonttools/fonttools/pull/1307>
    #[test]
    fn fonttools_1307() {
        let defaults = default_systems([DFLT_DFLT, DFLT_FRE, DFLT_ABC, LATN_DFLT, LATN_ABC]);
        let [id1, id2, id3, id4, id5, id6, id7, id8] = make_ids();

        let mut feature = ActiveFeature::new(TAG_TEST, defaults, None);
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
        assert_eq!(built.get_base(&key), Some([id1, id2, id3].as_slice()));
        // only the root defaults, not defaults afer first script statement
        let key = DFLT_ABC.to_feature_key(TAG_TEST);
        assert_eq!(built.get_base(&key), Some([id1].as_slice()));
        // DFLT_DFLT + the explicit lookup added for this system
        let key = DFLT_FRE.to_feature_key(TAG_TEST);
        assert_eq!(built.get_base(&key), Some([id1, id2, id3, id4].as_slice()));
        // root defaults, plus explicit f/g
        let key = LATN_DFLT.to_feature_key(TAG_TEST);
        assert_eq!(built.get_base(&key), Some([id1, id5, id6].as_slice()));
        // LATN_DFLT + explicit h
        let key = LATN_FRE.to_feature_key(TAG_TEST);
        assert_eq!(built.get_base(&key), Some([id1, id5, id6, id7].as_slice()));
        // only the root defaults
        let key = LATN_ABC.to_feature_key(TAG_TEST);
        assert_eq!(built.get_base(&key), Some([id1].as_slice()));
        // only the explicit 'i' (excludes defaults)
        let key = LATN_DEF.to_feature_key(TAG_TEST);
        assert_eq!(built.get_base(&key), Some([id8].as_slice()));
    }
}
