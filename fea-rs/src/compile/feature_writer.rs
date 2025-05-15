//! API for the client to manually add additional features

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use write_fonts::{
    tables::{
        gpos::builders::{
            CursivePosBuilder, MarkToBaseBuilder, MarkToLigBuilder, MarkToMarkBuilder,
            PairPosBuilder,
        },
        gsub::builders::SingleSubBuilder,
        layout::{builders::LookupBuilder, ConditionSet, LookupFlag},
    },
    types::{GlyphId16, Tag},
};

use crate::{
    compile::tags::{LANG_DFLT, SCRIPT_DFLT},
    GlyphSet,
};

use super::{
    features::{AllFeatures, FeatureLookups},
    language_system::{DefaultLanguageSystems, LanguageSystem},
    lookups::{
        AllLookups, FeatureKey, FilterSetId, LookupId, LookupIdMap, PositionLookup,
        SubstitutionLookup,
    },
    tables::{GdefBuilder, Tables},
    CaretValue,
};

/// A trait that can be implemented by the client to do custom feature writing.
pub trait FeatureProvider {
    /// The client can write additional features into the provided builder
    fn add_features(&self, builder: &mut FeatureBuilder);
}

/// A nop implementation of [FeatureProvider]
pub struct NopFeatureProvider;

impl FeatureProvider for NopFeatureProvider {
    fn add_features(&self, _: &mut FeatureBuilder) {}
}

#[derive(Clone, Debug)]
pub(crate) struct RawFeatureVariations {
    pub(crate) features: Vec<Tag>,
    pub(crate) conditions: Vec<(ConditionSet, Vec<LookupId>)>,
}

/// A structure that allows client code to add additional features to the compilation.
pub struct FeatureBuilder<'a> {
    pub(crate) language_systems: &'a DefaultLanguageSystems,
    pub(crate) tables: &'a mut Tables,
    pub(crate) pos_lookups: Vec<(LookupId, PositionLookup)>,
    pub(crate) sub_lookups: Vec<(LookupId, SubstitutionLookup)>,
    pub(crate) features: BTreeMap<FeatureKey, FeatureLookups>,
    pub(crate) lig_carets: BTreeMap<GlyphId16, Vec<CaretValue>>,
    mark_filter_sets: &'a mut HashMap<GlyphSet, FilterSetId>,
    feature_variations: Option<RawFeatureVariations>,
}

pub trait LookupSubtableBuilder: Sized {
    #[doc(hidden)]
    fn to_pos_lookup(
        flags: LookupFlag,
        filter_set: Option<FilterSetId>,
        subtables: Vec<Self>,
    ) -> ExternalLookup;
}

/// A lookup generated outside of user FEA
///
/// This will be merged into any user-provided features during compilation.
#[derive(Debug, Default, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PendingLookup<T> {
    subtables: Vec<T>,
    flags: LookupFlag,
    mark_filter_set: Option<GlyphSet>,
    /// If `true`, this lookup will be inserted at the front of the lookup list.
    ///
    /// This is required for lookups referenced by the `rvrn` feature.
    goes_to_front_of_list: bool,
}

impl<T> PendingLookup<T> {
    /// Create a new lookup.
    ///
    /// This can later be added to the feature builder via [`FeatureBuilder::add_lookup`]
    pub fn new(subtables: Vec<T>, flags: LookupFlag, mark_filter_set: Option<GlyphSet>) -> Self {
        Self {
            subtables,
            flags,
            mark_filter_set,
            goes_to_front_of_list: false,
        }
    }

    /// Builder-style method to mark this lookup as needing to be at the front
    /// of the lookup list.
    ///
    /// Lookups with this flag set will be ordered first, and will otherwise
    /// maintain the order in which they were added.
    pub fn at_front_of_list(mut self, flag: bool) -> Self {
        self.goes_to_front_of_list = flag;
        self
    }

    /// Return a reference to the subtables in this lookup.
    pub fn subtables(&self) -> &[T] {
        &self.subtables
    }

    /// Return the `LookupFlag` for this lookup.
    pub fn flags(&self) -> LookupFlag {
        self.flags
    }
}

/// An externally created lookup.
///
/// This exists so that we can avoid making our internal types `pub`.
pub struct ExternalLookup(Inner);

// private so as not to expose the payloads
enum Inner {
    Gpos(PositionLookup),
    Gsub(SubstitutionLookup),
}

impl<'a> FeatureBuilder<'a> {
    pub(crate) fn new(
        language_systems: &'a DefaultLanguageSystems,
        tables: &'a mut Tables,
        mark_filter_sets: &'a mut HashMap<GlyphSet, u16>,
    ) -> Self {
        Self {
            language_systems,
            tables,
            pos_lookups: Default::default(),
            sub_lookups: Default::default(),
            features: Default::default(),
            mark_filter_sets,
            feature_variations: Default::default(),
            lig_carets: Default::default(),
        }
    }

    /// An iterator over the default language systems registered in the FEA
    pub fn language_systems(&self) -> impl Iterator<Item = LanguageSystem> + 'a {
        self.language_systems.iter()
    }

    /// If the FEA text contained an explicit GDEF table block, return its contents
    pub fn gdef(&self) -> Option<&GdefBuilder> {
        self.tables.gdef.as_ref()
    }

    /// Add caret positions for the GDEF `LigCaretList` table
    pub fn add_lig_carets(&mut self, lig_carets: BTreeMap<GlyphId16, Vec<CaretValue>>) {
        self.lig_carets = lig_carets;
    }

    /// Add a lookup to the lookup list.
    ///
    /// The `LookupId` that is returned can then be included in features (i.e,
    /// passed to [`add_feature`](Self::add_feature).)
    pub fn add_lookup<T: LookupSubtableBuilder>(&mut self, lookup: PendingLookup<T>) -> LookupId {
        let PendingLookup {
            subtables,
            flags,
            mark_filter_set,
            goes_to_front_of_list,
        } = lookup;
        let filter_set_id = mark_filter_set.map(|cls| self.get_filter_set_id(cls));
        let lookup = T::to_pos_lookup(flags, filter_set_id, subtables);
        let next_id = match &lookup.0 {
            Inner::Gpos(_) => LookupId::ExternalGpos(self.pos_lookups.len()),
            Inner::Gsub(_) if goes_to_front_of_list => {
                LookupId::ExternalFrontOfList(self.sub_lookups.len())
            }
            Inner::Gsub(_) => LookupId::ExternalGsub(self.sub_lookups.len()),
        };
        match lookup.0 {
            Inner::Gpos(lookup) => self.pos_lookups.push((next_id, lookup)),
            Inner::Gsub(lookup) => self.sub_lookups.push((next_id, lookup)),
        }
        next_id
    }

    /// Add lookups to every default language system.
    ///
    /// Convenience method for recurring pattern.
    pub fn add_to_default_language_systems(&mut self, feature_tag: Tag, lookups: &[LookupId]) {
        for langsys in self.language_systems() {
            let feature_key = langsys.to_feature_key(feature_tag);
            self.add_feature(feature_key, lookups.to_vec());
        }
    }

    /// Create a new feature, registered for a particular language system.
    ///
    /// The caller must call this method once for each language system under
    /// which a feature is to be registered.
    pub fn add_feature(&mut self, key: FeatureKey, lookups: Vec<LookupId>) {
        self.features.entry(key).or_default().base = lookups;
    }

    /// Add feature variations.
    ///
    /// The variations will be added to each feature in `tags`.
    ///
    /// For each feature tag:
    /// - if any `FeatureRecord`s exist with that tag, the variations will be
    ///   added to them.
    /// - if none exist, a new feature will be registered for the default
    ///   language systems, and the variations will be added to these features.
    pub fn add_feature_variations(
        &mut self,
        features: Vec<Tag>,
        conditions: Vec<(ConditionSet, Vec<LookupId>)>,
    ) {
        assert!(self.feature_variations.is_none(), "can only be added once");
        self.feature_variations = Some(RawFeatureVariations {
            features,
            conditions,
        })
    }

    fn get_filter_set_id(&mut self, cls: GlyphSet) -> FilterSetId {
        let next_id = self.mark_filter_sets.len();
        *self.mark_filter_sets.entry(cls).or_insert_with(|| {
            next_id
                .try_into()
                // is this in any way an expected error condition?
                .expect("too many filter sets?")
        })
    }

    pub(crate) fn finish(self) -> ExternalFeatures {
        let FeatureBuilder {
            pos_lookups,
            sub_lookups,
            features,
            lig_carets,
            feature_variations,
            ..
        } = self;
        ExternalFeatures {
            features,
            pos_lookups,
            sub_lookups,
            feature_variations,
            lig_carets,
        }
    }
}

macro_rules! impl_builder_trait {
    ($builder:ty, $var:ident) => {
        impl LookupSubtableBuilder for $builder {
            fn to_pos_lookup(
                flags: LookupFlag,
                filter_set: Option<FilterSetId>,
                subtables: Vec<Self>,
            ) -> ExternalLookup {
                ExternalLookup(Inner::$var(
                    LookupBuilder::new_with_lookups(flags, filter_set, subtables).into(),
                ))
            }
        }
    };
}

impl_builder_trait!(SingleSubBuilder, Gsub);
impl_builder_trait!(PairPosBuilder, Gpos);
impl_builder_trait!(MarkToBaseBuilder, Gpos);
impl_builder_trait!(MarkToMarkBuilder, Gpos);
impl_builder_trait!(MarkToLigBuilder, Gpos);
impl_builder_trait!(CursivePosBuilder, Gpos);

// features that can be added by a feature writer
const CURS: Tag = Tag::new(b"curs");
const MARK: Tag = Tag::new(b"mark");
const MKMK: Tag = Tag::new(b"mkmk");
const ABVM: Tag = Tag::new(b"abvm");
const BLWM: Tag = Tag::new(b"blwm");
const KERN: Tag = Tag::new(b"kern");
const DIST: Tag = Tag::new(b"dist");

/// All of the state that is generated by the external provider
pub(crate) struct ExternalFeatures {
    pub(crate) pos_lookups: Vec<(LookupId, PositionLookup)>,
    pub(crate) sub_lookups: Vec<(LookupId, SubstitutionLookup)>,
    pub(crate) features: BTreeMap<FeatureKey, FeatureLookups>,
    pub(crate) lig_carets: BTreeMap<GlyphId16, Vec<CaretValue>>,
    pub(crate) feature_variations: Option<RawFeatureVariations>,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub(crate) struct InsertionPoint {
    /// The position in the lookuplist to insert a set of new lookups
    pub(crate) lookup_id: LookupId,
    /// if multiple sets are inserted at the same lookup index, this breaks ties
    ///
    /// This is common! for instance if you have FEA with two feature blocks
    /// that contain `# Automatic Code` comments and nothing else, this is used
    /// to order them.
    pub(crate) priority: usize,
}

struct MergeCtx<'a> {
    all_lookups: &'a mut AllLookups,
    all_feats: &'a mut AllFeatures,
    // that were explicit
    insert_markers: &'a HashMap<Tag, InsertionPoint>,
    ext_pos_lookups: BTreeMap<LookupId, PositionLookup>,
    ext_sub_lookups: BTreeMap<LookupId, SubstitutionLookup>,
    ext_features: BTreeMap<FeatureKey, FeatureLookups>,
    feature_variations: Option<RawFeatureVariations>,
    // ready for insertion
    processed_lookups: Vec<(InsertionPoint, Vec<(LookupId, PositionLookup)>)>,
    // track how many groups of lookups have been appended on the end,
    // so we can give them the right priority
    append_priority: usize,
}

impl MergeCtx<'_> {
    fn merge(mut self) {
        // This is complicated.
        //
        // We are trying to match the behaviour provided by 'feature writers'
        // in ufo2ft.
        //
        // These work by actually generating FEA, and inserting it into the AST
        // before compilation.
        //
        // We are not modifying the AST; the AST has already been compiled, and
        // we are now inserting generated lookups into those that were compiled
        // from the AST.
        //
        // This means we need to figure out where in the AST these external
        // lookups _would have been inserted_, and what IDs would have been
        // assigned to them if they had been there.

        // To make it easier to follow our logic, we will process the external
        // lookups and features in groups, replicating how they would be
        // handled by the various feature writers.

        self.do_curs();
        self.do_kern_and_dist();
        self.do_marks();

        // okay so now 'processed_lookups' should contain insertion points for
        // all of our lookups

        if !self.ext_pos_lookups.is_empty() {
            log::warn!("feature merging left unhandled features!");
        }
        let mut id_map = self.finalize_gpos();
        self.finalize_gsub(&mut id_map);

        self.all_feats.merge_external_features(self.ext_features);
        self.all_feats.remap_ids(&id_map);
        self.all_lookups.remap_ids(&id_map);
    }

    fn finalize_gpos(&mut self) -> LookupIdMap {
        self.processed_lookups.sort_by_key(|(key, _)| *key);

        // this is the actual logic for inserting the lookups into the main
        // lookup list, keeping track of how the ids change.

        let mut map = LookupIdMap::default();
        let mut inserted_so_far = 0;

        // 'adjustments' stores the state we need to remap existing ids, if needed.
        let mut adjustments = Vec::new();

        for (insert_point, lookups) in &mut self.processed_lookups {
            let first_id = insert_point.lookup_id.to_raw();
            // within a feature, the lookups should honor the ordering they were
            // assigned by the user
            lookups.sort_by_key(|(key, _)| *key);
            // first update the ids
            for (i, (temp_id, _)) in lookups.iter().enumerate() {
                let final_id = LookupId::Gpos(first_id + inserted_so_far + i);
                map.insert(*temp_id, final_id);
            }
            // then insert the lookups into the correct position
            let insert_at = first_id + inserted_so_far;
            inserted_so_far += lookups.len();
            self.all_lookups
                .splice_gpos(insert_at, lookups.iter().map(|v| v.1.clone()));
            adjustments.push((first_id, inserted_so_far));
        }

        // now based on our recorded adjustments, figure out the remapping
        // for the existing ids. each entry in adjustment is an (index, delta)
        // pair, where the delta applies from adjustment[n] to adjustment[n +1]
        if !adjustments.is_empty() {
            // add the end of the last range
            adjustments.push((self.all_lookups.next_gpos_id().to_raw(), inserted_so_far));
        }
        let (mut range_start, mut adjust) = (0, 0);
        let mut adjustments = adjustments.as_slice();
        while let Some(((next_start, next_adjust), remaining)) = adjustments.split_first() {
            if adjust > 0 {
                for old_id in range_start..*next_start {
                    map.insert(LookupId::Gpos(old_id), LookupId::Gpos(old_id + adjust));
                }
            }
            (range_start, adjust, adjustments) = (*next_start, *next_adjust, remaining);
        }
        map
    }

    fn finalize_gsub(&mut self, id_map: &mut LookupIdMap) {
        let (start, end): (Vec<_>, Vec<_>) = self
            .ext_sub_lookups
            .iter()
            .partition(|(id, _)| matches!(id, LookupId::ExternalFrontOfList(_)));

        if !start.is_empty() {
            // if we're adding items at the start, we need to go and remap all the existing ids
            for old_id in 0..self.all_lookups.next_gsub_id().to_raw() {
                id_map.insert(LookupId::Gsub(old_id), LookupId::Gsub(old_id + start.len()));
            }
        }

        self.all_lookups
            .splice_gsub(0, start.iter().map(|(_, lk)| (*lk).clone()));

        let last_id = self.all_lookups.next_gsub_id().to_raw();
        self.all_lookups
            .splice_gsub(last_id, end.iter().map(|(_, lk)| (*lk).clone()));

        for (i, (temp_id, _)) in start.iter().enumerate() {
            id_map.insert(**temp_id, LookupId::Gsub(i));
        }

        for (i, (temp_id, _)) in end.iter().enumerate() {
            id_map.insert(**temp_id, LookupId::Gsub(last_id + i));
        }
        self.add_feature_variations();
    }

    fn add_feature_variations(&mut self) {
        let Some(RawFeatureVariations {
            features,
            conditions,
        }) = self.feature_variations.take()
        else {
            return;
        };

        let feat_tags = features.into_iter().collect::<HashSet<_>>();
        log::info!("adding feature variations for {feat_tags:?}");

        // first check if any of the added features already exist; if they do,
        // the replacement feature will include the old lookups.
        let mut done_tags = HashSet::new();
        for (key, feat) in self.all_feats.features.iter_mut() {
            if !feat_tags.contains(&key.feature) {
                continue;
            }
            done_tags.insert(key.feature);
            for (cond, lookups) in &conditions {
                let combined_lookups = feat.base.iter().chain(lookups.iter()).copied().collect();
                log::debug!(
                    "adding feature variations to existing feature '{}'",
                    key.feature
                );
                if let Some(_prev) = feat.variations.insert(cond.to_owned(), combined_lookups) {
                    log::warn!("feature writer replaced existing variations for {key:?}");
                }
            }
        }

        // then create new features for any remaining, and add to all known language systems
        let mut all_lang_systems = self
            .all_feats
            .features
            .keys()
            .map(|key| (key.script, key.language))
            .collect::<HashSet<_>>();

        if all_lang_systems.is_empty() {
            all_lang_systems.insert((SCRIPT_DFLT, LANG_DFLT));
        }

        for feature_tag in feat_tags.difference(&done_tags) {
            for (script, lang) in &all_lang_systems {
                let key = FeatureKey::new(*feature_tag, *lang, *script);
                self.all_feats
                    .get_or_insert(key)
                    .variations
                    .extend(conditions.iter().cloned());
            }
        }
    }

    fn do_curs(&mut self) {
        let curs_pos = self
            .insert_markers
            .get(&CURS)
            .copied()
            .unwrap_or_else(|| self.insertion_point_for_append());
        self.finalize_lookups_for_feature(CURS, curs_pos);
    }

    fn do_kern_and_dist(&mut self) {
        // the lookups for these two features are grouped together,
        // and are inserted before whichever of them occurs first.

        let marker = self
            .insert_markers
            .get(&DIST)
            .or_else(|| self.insert_markers.get(&KERN))
            .copied()
            .unwrap_or_else(|| self.insertion_point_for_append());

        let lookups = self.take_lookups_for_features(&[KERN, DIST]);
        if !lookups.is_empty() {
            self.processed_lookups.push((marker, lookups));
        }
    }

    fn do_marks(&mut self) {
        // This one is complicated!
        // there is logic in the base feature writer that says, when multiple
        // features are added, features that are earlier in the input list will
        // be inserted before features that come later.
        //
        // the markFeatureWriter passes these features in in alphabetical order:
        // https://github.com/googlefonts/ufo2ft/blob/16ed156bd6a/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L1174
        //
        // BUT! the lookups for abvm & blwm are included _in_ the feature blocks
        // for those features, but the lookups for mark and mkmk are not
        // inlined, and so they are grouped together, but inserted before any
        // other lookups in this group. My head hurts :) :)

        const ORDER: [Tag; 4] = [ABVM, BLWM, MARK, MKMK];
        let mut inserts = [None; 4];
        for (i, tag) in ORDER.iter().enumerate() {
            inserts[i] = self.insert_markers.get(tag).copied();
        }

        for i in 0..ORDER.len() {
            if let Some(insert) = inserts[i] {
                // if a later feature has an explicit position, put the earlier
                // features in front of it
                for j in 0..i {
                    let j = i - j - 1; // we want to go in reverse order,
                                       // prepending each time
                    if inserts[j].is_none() {
                        inserts[j] = Some(InsertionPoint {
                            lookup_id: insert.lookup_id,
                            priority: insert.priority - 1,
                        })
                    }
                }
            }
        }

        // so now `inserts` has explicit positions for features that need them.
        // lets fill in any features that were not assigned positions this way:
        for insert in inserts.iter_mut() {
            if insert.is_none() {
                *insert = Some(self.insertion_point_for_append());
            }
        }

        // and finally lets put the lookups in our processed list:
        self.finalize_lookups_for_feature(ABVM, inserts[0].unwrap());
        self.finalize_lookups_for_feature(BLWM, inserts[1].unwrap());
        self.finalize_lookups_for_feature(MARK, inserts[2].unwrap());
        self.finalize_lookups_for_feature(MKMK, inserts[3].unwrap());
    }

    fn finalize_lookups_for_feature(&mut self, feature: Tag, pos: InsertionPoint) {
        let lookups = self.take_lookups_for_features(&[feature]);
        if !lookups.is_empty() {
            self.processed_lookups.push((pos, lookups));
        }
    }

    fn lookup_ids_for_features(&self, features: &[Tag]) -> BTreeSet<LookupId> {
        self.ext_features
            .iter()
            .filter(|(feat, _)| features.contains(&feat.feature))
            .flat_map(|(_, lookups)| lookups.iter_ids())
            .collect()
    }

    fn take_lookups_for_features(&mut self, features: &[Tag]) -> Vec<(LookupId, PositionLookup)> {
        self.lookup_ids_for_features(features)
            .into_iter()
            .map(|id| (id, self.ext_pos_lookups.remove(&id).unwrap()))
            .collect()
    }

    fn insertion_point_for_append(&mut self) -> InsertionPoint {
        let lookup_id = self.all_lookups.next_gpos_id();
        self.append_priority += 1;
        InsertionPoint {
            lookup_id,
            priority: self.append_priority,
        }
    }
}

impl ExternalFeatures {
    /// Merge the external features into the already compiled features.
    pub(crate) fn merge_into(
        &mut self,
        all_lookups: &mut AllLookups,
        all_feats: &mut AllFeatures,
        markers: &HashMap<Tag, InsertionPoint>,
    ) {
        let ctx = MergeCtx {
            all_lookups,
            all_feats,
            ext_pos_lookups: self.pos_lookups.iter().cloned().collect(),
            ext_sub_lookups: self.sub_lookups.iter().cloned().collect(),
            ext_features: self.features.clone(),
            feature_variations: self.feature_variations.clone(),
            insert_markers: markers,
            processed_lookups: Default::default(),
            append_priority: 1_000_000_000,
        };
        ctx.merge();
    }
}

#[cfg(test)]
mod tests {
    use write_fonts::tables::layout::ConditionSet;

    use super::*;

    use crate::compile::tags::{LANG_DFLT, SCRIPT_DFLT};

    impl AllFeatures {
        // after remapping, return the order of features, based on the final
        // ordering of lookups.
        //
        // this works because for these tests we add one unique lookup to each
        // feature, so lookup order is a proxy for feature order.
        // - one lookup per feature
        // - no duplicates
        fn feature_order_for_test(&self) -> Vec<Tag> {
            let mut id_and_tag = self
                .features
                .iter()
                .map(|(key, val)| (val.iter_ids().next().unwrap(), key.feature))
                .collect::<Vec<_>>();
            id_and_tag.sort();
            id_and_tag.into_iter().map(|(_, tag)| tag).collect()
        }
    }

    #[test]
    fn merge_external_lookups_before() {
        let mut all = AllLookups::default();
        all.splice_gpos(
            0,
            (0..8).map(|_| PositionLookup::Single(Default::default())),
        );

        let pos_lookups = (0..6)
            .map(|id| {
                (
                    LookupId::ExternalGpos(id),
                    PositionLookup::Pair(Default::default()),
                )
            })
            .collect();
        let features: BTreeMap<_, _> =
            [(MARK, [0].as_slice()), (MKMK, &[1, 2]), (KERN, &[3, 4, 5])]
                .iter()
                .map(|(tag, ids)| {
                    let features = FeatureLookups {
                        base: ids.iter().copied().map(LookupId::ExternalGpos).collect(),
                        variations: Default::default(),
                    };
                    (FeatureKey::new(*tag, LANG_DFLT, SCRIPT_DFLT), features)
                })
                .collect();

        // mark is 'after', kern is 'before', and mkmk has no marker (goes at the end)
        let markers = HashMap::from([
            (
                MARK,
                InsertionPoint {
                    lookup_id: LookupId::Gpos(3),
                    priority: 100,
                },
            ),
            (
                KERN,
                InsertionPoint {
                    lookup_id: LookupId::Gpos(5),
                    priority: 200,
                },
            ),
        ]);

        let mut external_features = ExternalFeatures {
            pos_lookups,
            sub_lookups: Default::default(),
            features,
            lig_carets: Default::default(),
            feature_variations: Default::default(),
        };

        let mut all_features = AllFeatures::default();
        external_features.merge_into(&mut all, &mut all_features, &markers);
        let expected_ids: [(Tag, &[usize]); 3] =
            [(MARK, &[3]), (MKMK, &[12, 13]), (KERN, &[6, 7, 8])];

        for (tag, ids) in expected_ids {
            let key = FeatureKey::new(tag, LANG_DFLT, SCRIPT_DFLT);
            let result = all_features
                .get_or_insert(key)
                .iter_ids()
                .map(|id| id.to_raw())
                .collect::<Vec<_>>();
            assert_eq!(ids, result)
        }
    }

    #[test]
    fn merge_rvrn_features_before_other_one() {
        const RVRN: Tag = Tag::new(b"rvrn");
        const RCLT: Tag = Tag::new(b"rclt");

        let mut sub_lookups = Vec::new();
        let mut features = BTreeMap::new();
        for (id, tag) in [(0, RCLT), (1, RCLT), (2, RVRN), (3, RVRN)] {
            let id = if tag == RCLT {
                LookupId::ExternalGsub(id)
            } else {
                LookupId::ExternalFrontOfList(id)
            };
            let lookup = SubstitutionLookup::Single(Default::default());
            let key = FeatureKey::new(tag, LANG_DFLT, SCRIPT_DFLT);
            features
                .entry(key)
                .or_insert(FeatureLookups::default())
                .variations
                .entry(ConditionSet::default())
                .or_default()
                .push(id);
            sub_lookups.push((id, lookup));
        }

        let mut external_features = ExternalFeatures {
            pos_lookups: Default::default(),
            sub_lookups,
            features,
            lig_carets: Default::default(),
            feature_variations: Default::default(),
        };

        let markers = make_markers_with_order([]);
        let mut all = AllLookups::default();
        all.splice_gsub(
            0,
            (0..3).map(|_| SubstitutionLookup::Multiple(Default::default())),
        );

        let mut all_feats = AllFeatures::default();
        external_features.merge_into(&mut all, &mut all_feats, &markers);

        let rvrn = all_feats
            .features
            .get(&FeatureKey::new(RVRN, LANG_DFLT, SCRIPT_DFLT))
            .unwrap();

        // rvrn ids go at the front
        assert_eq!(
            rvrn.iter_ids().map(LookupId::to_raw).collect::<Vec<_>>(),
            [0, 1]
        );
        let rclt = all_feats
            .features
            .get(&FeatureKey::new(RCLT, LANG_DFLT, SCRIPT_DFLT))
            .unwrap();
        // other ids go at the back
        assert_eq!(
            rclt.iter_ids().map(LookupId::to_raw).collect::<Vec<_>>(),
            [5, 6]
        );
    }

    fn mock_external_features(tags: &[Tag]) -> ExternalFeatures {
        let mut pos_lookups = Vec::new();
        let mut features = BTreeMap::new();

        for (i, feature) in tags.iter().enumerate() {
            let id = LookupId::ExternalGpos(i);
            let lookup = PositionLookup::Single(Default::default());
            let key = FeatureKey::new(*feature, LANG_DFLT, SCRIPT_DFLT);

            let feature_lookups = FeatureLookups {
                base: vec![id],
                variations: Default::default(),
            };
            pos_lookups.push((id, lookup));
            features.insert(key, feature_lookups);
        }
        ExternalFeatures {
            pos_lookups,
            sub_lookups: Default::default(),
            features,
            lig_carets: Default::default(),
            feature_variations: Default::default(),
        }
    }

    fn make_markers_with_order<const N: usize>(order: [Tag; N]) -> HashMap<Tag, InsertionPoint> {
        order
            .into_iter()
            .enumerate()
            .map(|(i, tag)| {
                (
                    tag,
                    InsertionPoint {
                        lookup_id: LookupId::Gpos(0),
                        priority: i + 10,
                    },
                )
            })
            .collect()
    }

    // respect dependencies: kern before dist, abvm/blwm/mark before mkmk.
    #[test]
    fn feature_ordering_without_markers() {
        let mut external = mock_external_features(&[KERN, DIST, MKMK, ABVM, BLWM, MARK, CURS]);
        let markers = make_markers_with_order([]);
        let mut all = AllLookups::default();
        let mut all_feats = AllFeatures::default();
        external.merge_into(&mut all, &mut all_feats, &markers);

        assert_eq!(
            all_feats.feature_order_for_test(),
            [CURS, KERN, DIST, ABVM, BLWM, MARK, MKMK]
        );
    }

    #[test]
    fn kern_and_dist_respect_input_order() {
        // for kern/dist lookups are inserted together, and respect the lookup
        // order assigned when they were passed in.

        let mut external = mock_external_features(&[DIST, KERN, CURS]);

        let markers = make_markers_with_order([]);
        let mut all = AllLookups::default();
        let mut all_feats = AllFeatures::default();
        external.merge_into(&mut all, &mut all_feats, &markers);
        assert_eq!(all_feats.feature_order_for_test(), [CURS, DIST, KERN]);
    }

    #[test]
    fn kern_and_dist_respect_input_order_with_marker() {
        // - dist is earlier in input order
        // - kern has an explicit marker
        // = the marker is used for both kern/dist, and they keep input order

        let mut external = mock_external_features(&[CURS, DIST, KERN]);

        let markers = make_markers_with_order([KERN]);
        let mut all = AllLookups::default();
        let mut all_feats = AllFeatures::default();
        external.merge_into(&mut all, &mut all_feats, &markers);
        assert_eq!(all_feats.feature_order_for_test(), [DIST, KERN, CURS]);
    }

    #[test]
    fn blwm_with_marker_takes_abvm_with_it() {
        let mut external = mock_external_features(&[BLWM, ABVM, DIST]);
        let markers = make_markers_with_order([BLWM]);
        let mut all = AllLookups::default();
        let mut all_feats = AllFeatures::default();
        external.merge_into(&mut all, &mut all_feats, &markers);
        // because BLWM has a marker and ABVM depends on it, insert ABVM first
        assert_eq!(all_feats.feature_order_for_test(), [ABVM, BLWM, DIST]);
    }

    #[test]
    fn marks_with_marker_goes_before_kern() {
        let mut external = mock_external_features(&[MARK, KERN]);
        // 'mark' gets an explicit location
        let markers = make_markers_with_order([MARK]);
        let mut all = AllLookups::default();
        let mut all_feats = AllFeatures::default();
        external.merge_into(&mut all, &mut all_feats, &markers);
        assert_eq!(all_feats.feature_order_for_test(), [MARK, KERN]);
    }

    #[test]
    fn mkmk_brings_along_the_whole_family() {
        let mut external = mock_external_features(&[BLWM, KERN, MKMK, DIST, MARK, ABVM]);
        let markers = make_markers_with_order([MKMK]);
        let mut all = AllLookups::default();
        let mut all_feats = AllFeatures::default();
        external.merge_into(&mut all, &mut all_feats, &markers);
        assert_eq!(
            all_feats.feature_order_for_test(),
            // abvm/blwm/mark all have to go before mkmk
            [ABVM, BLWM, MARK, MKMK, KERN, DIST]
        );
    }
}
