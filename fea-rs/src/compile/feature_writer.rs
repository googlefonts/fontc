//! API for the client to manually add additional features

use std::collections::{BTreeMap, HashMap};

use write_fonts::{
    tables::layout::LookupFlag,
    types::{GlyphId16, Tag},
};

use crate::GlyphSet;

use super::{
    features::{AllFeatures, FeatureLookups},
    language_system::{DefaultLanguageSystems, LanguageSystem},
    lookups::{
        AllLookups, FeatureKey, FilterSetId, LookupBuilder, LookupId, LookupIdMap, PositionLookup,
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

/// A structure that allows client code to add additional features to the compilation.
pub struct FeatureBuilder<'a> {
    pub(crate) language_systems: &'a DefaultLanguageSystems,
    pub(crate) tables: &'a mut Tables,
    pub(crate) lookups: Vec<(LookupId, PositionLookup)>,
    pub(crate) features: BTreeMap<FeatureKey, FeatureLookups>,
    pub(crate) lig_carets: BTreeMap<GlyphId16, Vec<CaretValue>>,
    mark_filter_sets: &'a mut HashMap<GlyphSet, FilterSetId>,
}

pub trait GposSubtableBuilder: Sized {
    #[doc(hidden)]
    fn to_pos_lookup(
        flags: LookupFlag,
        filter_set: Option<FilterSetId>,
        subtables: Vec<Self>,
    ) -> ExternalGposLookup;
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
        }
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

/// An externally created GPOS lookup.
///
/// This only exists so that we can avoid making our internal types `pub`.
pub struct ExternalGposLookup(PositionLookup);

impl<'a> FeatureBuilder<'a> {
    pub(crate) fn new(
        language_systems: &'a DefaultLanguageSystems,
        tables: &'a mut Tables,
        mark_filter_sets: &'a mut HashMap<GlyphSet, u16>,
    ) -> Self {
        Self {
            language_systems,
            tables,
            lookups: Default::default(),
            features: Default::default(),
            mark_filter_sets,
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
    pub fn add_lookup<T: GposSubtableBuilder>(&mut self, lookup: PendingLookup<T>) -> LookupId {
        let PendingLookup {
            subtables,
            flags,
            mark_filter_set,
        } = lookup;
        let filter_set_id = mark_filter_set.map(|cls| self.get_filter_set_id(cls));
        let lookup = T::to_pos_lookup(flags, filter_set_id, subtables);
        let next_id = LookupId::External(self.lookups.len());
        self.lookups.push((next_id, lookup.0));
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
            lookups,
            features,
            lig_carets,
            ..
        } = self;
        ExternalFeatures {
            features,
            lookups,
            lig_carets,
        }
    }
}

impl<T> GposSubtableBuilder for T
where
    T: Default,
    LookupBuilder<T>: Into<PositionLookup>,
{
    fn to_pos_lookup(
        flags: LookupFlag,
        filter_set: Option<FilterSetId>,
        subtables: Vec<Self>,
    ) -> ExternalGposLookup {
        ExternalGposLookup(LookupBuilder::new_with_lookups(flags, filter_set, subtables).into())
    }
}

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
    pub(crate) lookups: Vec<(LookupId, PositionLookup)>,
    pub(crate) features: BTreeMap<FeatureKey, FeatureLookups>,
    pub(crate) lig_carets: BTreeMap<GlyphId16, Vec<CaretValue>>,
}

impl ExternalFeatures {
    /// Merge the external features into the already compiled features.
    pub(crate) fn merge_into(
        &self,
        all_lookups: &mut AllLookups,
        features: &mut AllFeatures,
        markers: &HashMap<Tag, (LookupId, usize)>,
    ) {
        let id_map = self.merge_lookups(all_lookups, markers);
        features.merge_external_features(self.features.clone());
        features.remap_ids(&id_map);
        all_lookups.remap_ids(&id_map);
    }

    fn merge_lookups(
        &self,
        all: &mut AllLookups,
        insert_markers: &HashMap<Tag, (LookupId, usize)>,
    ) -> LookupIdMap {
        // okay so this is a bit complicated, so a brief outline might be useful:
        // - 'lookups' and 'features' are being passed in from the outside
        // - `insert_markers` are optional locations at which the lookups
        //    for a given feature should be inserted
        // - if a feature has no marker, its lookups are appended at the end.
        //
        // NOTE: inserting lookups into the middle of the lookup list means that
        // all subsequent lookup ids become invalid. As a consequence, we need
        // to figure out the transform from old->new for any affected lookups,
        // and remap those ids at the end.

        // preflight: we have a list of the manual insert markers, but we also
        // need to figure out where the marker-less features should go.
        // fonttools some something fancy here, where certain features are
        // always inserted ahead of certain other features, if they exist;
        // otherwise features are appended.
        //
        // see https://github.com/googlefonts/ufo2ft/blob/16ed156bd6a8b9bc035/Lib/ufo2ft/featureWriters/baseFeatureWriter.py#L235
        // and https://github.com/googlefonts/ufo2ft/issues/506

        let mut insert_markers = insert_markers.to_owned();

        // Also: in fonttools, dist/kern are added before marks, so let's do
        // that first; and kern has to go before dist, if both is present
        // https://github.com/googlefonts/ufo2ft/blob/16ed156bd6a8b9bc035d0/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L311
        if let Some((kern_id, kern_pos)) = insert_markers.get(&DIST).copied() {
            // if kern has a marker but dist doesn't, insert dist first
            insert_markers
                .entry(KERN)
                .or_insert((kern_id, kern_pos - 1));
        }

        // then handle the mark features, in the same order as ufo2ft:
        // see https://github.com/googlefonts/ufo2ft/blob/16ed156bd6/Lib/ufo2ft/featureWriters/baseFeatureWriter.py#L235
        let order = [ABVM, BLWM, MARK, MKMK];
        for (i, tag) in order.iter().enumerate() {
            if let Some((id, mut pos)) = insert_markers.get(tag).copied() {
                // if a later feature has an explicit position, put the earlier
                // features in front of it; rev() because we're prepending each
                // time
                for prev_tag in order[..i].iter().rev() {
                    if !insert_markers.contains_key(prev_tag) {
                        pos -= 1;
                        insert_markers.insert(*prev_tag, (id, pos));
                    }
                }
            }
        }

        // finally insert dummy markers for any other feature that doesn't exist
        // when appending, we use the last id and increment the position
        let last_id = all.next_gpos_id();
        // quite likely to be after EOF!
        let mut next_pos = 1_000_000_000;
        // for adding extras, mimic the order ufo2ft uses, which is alphabetical
        // but grouped by feature writer. We add markers for all features,
        // even if they dont exist in the font; we'll ignore those later
        for feature in [CURS, KERN, DIST, ABVM, BLWM, MARK, MKMK] {
            insert_markers.entry(feature).or_insert_with(|| {
                next_pos += 1;
                (last_id, next_pos)
            });
        }

        let lookup_to_feature = self
            .features
            .iter()
            .filter(|(feat, _)| insert_markers.contains_key(&feat.feature))
            .flat_map(|(feat, lookups)| lookups.iter_ids().map(|id| (id, feat.feature)))
            .collect::<HashMap<_, _>>();

        // now we want to process the lookups that had an insert marker,
        // and we want to do it by grouping them by feature tag.
        let mut lookups_by_feature = HashMap::new();
        for lk in &self.lookups {
            let feature = lookup_to_feature.get(&lk.0).unwrap();
            lookups_by_feature
                .entry(*feature)
                .or_insert(Vec::new())
                .push(lk);
        }

        // except we want to assign the ids respecting the order of the markers
        // in the source file, so convert to a vec and sort.
        let mut lookups_by_feature = lookups_by_feature.into_iter().collect::<Vec<_>>();
        lookups_by_feature.sort_by_key(|(tag, _)| insert_markers.get(tag).unwrap());

        let mut map = LookupIdMap::default();
        let mut inserted_so_far = 0;

        // 'adjustments' stores the state we need to remap existing ids, if needed.
        let mut adjustments = Vec::new();

        for (tag, lookups) in lookups_by_feature {
            let first_id = insert_markers.get(&tag).unwrap().0.to_raw();
            // first update the ids
            for (i, (temp_id, _)) in lookups.iter().enumerate() {
                let final_id = LookupId::Gpos(first_id + inserted_so_far + i);
                map.insert(*temp_id, final_id);
            }
            // then insert the lookups into the correct position
            let insert_at = first_id + inserted_so_far;
            inserted_so_far += lookups.len();
            all.splice_gpos(insert_at, lookups.into_iter().map(|v| v.1.clone()));
            adjustments.push((first_id, inserted_so_far));
        }

        // now based on our recorded adjustments, figure out the remapping
        // for the existing ids. each entry in adjustment is an (index, delta)
        // pair, where the delta applies from adjustment[n] to adjustment[n +1]
        if !adjustments.is_empty() {
            // add the end of the last range
            adjustments.push((all.next_gpos_id().to_raw(), inserted_so_far));
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
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::compile::tags::{LANG_DFLT, SCRIPT_DFLT};

    impl AllFeatures {
        // after remapping, return the order of features, based on the final
        // ordering of lookups.
        //
        // this works because for these tests we add one unique lookup to each feature,
        // so lookup order is a proxy for feature order.
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

        let lookups = (0..6)
            .map(|id| {
                (
                    LookupId::External(id),
                    PositionLookup::Pair(Default::default()),
                )
            })
            .collect();
        let features: BTreeMap<_, _> =
            [(MARK, [0].as_slice()), (MKMK, &[1, 2]), (KERN, &[3, 4, 5])]
                .iter()
                .map(|(tag, ids)| {
                    let mut features = FeatureLookups::default();
                    features.base = ids.iter().copied().map(LookupId::External).collect();
                    (FeatureKey::new(*tag, LANG_DFLT, SCRIPT_DFLT), features)
                })
                .collect();

        // mark is 'after', kern is 'before', and mkmk has no marker (goes at the end)
        let markers = HashMap::from([
            (MARK, (LookupId::Gpos(3), 100)),
            (KERN, (LookupId::Gpos(5), 200)),
        ]);

        let external_features = ExternalFeatures {
            lookups,
            features,
            lig_carets: Default::default(),
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

    fn mock_external_features(tags: &[Tag]) -> ExternalFeatures {
        let mut lookups = Vec::new();
        let mut features = BTreeMap::new();

        for (i, feature) in tags.iter().enumerate() {
            let id = LookupId::External(i);
            let lookup = PositionLookup::Single(Default::default());
            let key = FeatureKey::new(*feature, LANG_DFLT, SCRIPT_DFLT);

            let mut feature_lookups = FeatureLookups::default();
            feature_lookups.base = vec![id];
            lookups.push((id, lookup));
            features.insert(key, feature_lookups);
        }
        ExternalFeatures {
            lookups,
            features,
            lig_carets: Default::default(),
        }
    }

    fn make_markers_with_order<const N: usize>(order: [Tag; N]) -> HashMap<Tag, (LookupId, usize)> {
        order
            .into_iter()
            .enumerate()
            .map(|(i, tag)| (tag, (LookupId::Gpos(0), i + 10)))
            .collect()
    }

    #[test]
    fn feature_ordering_without_markers() {
        let external = mock_external_features(&[CURS, DIST, KERN, ABVM, BLWM, MARK, MKMK]);
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
    fn feature_ordering_without_markers_input_order_doesnt_matter_either() {
        // just to demonstrate that the order we passed in is irrelevant
        let external = mock_external_features(&[BLWM, KERN, CURS, MKMK, DIST, MARK, ABVM]);
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
    fn blwm_with_marker_takes_abvm_with_it() {
        let external = mock_external_features(&[BLWM, ABVM, DIST]);
        let markers = make_markers_with_order([BLWM]);
        let mut all = AllLookups::default();
        let mut all_feats = AllFeatures::default();
        external.merge_into(&mut all, &mut all_feats, &markers);
        // because BLWM has a marker and ABVM depends on it, we insert ABVM first
        assert_eq!(all_feats.feature_order_for_test(), [ABVM, BLWM, DIST]);
    }

    #[test]
    fn marks_with_marker_goes_before_kern() {
        let external = mock_external_features(&[MARK, KERN]);
        // 'mark' gets an explicit location
        let markers = make_markers_with_order([MARK]);
        let mut all = AllLookups::default();
        let mut all_feats = AllFeatures::default();
        external.merge_into(&mut all, &mut all_feats, &markers);
        assert_eq!(all_feats.feature_order_for_test(), [MARK, KERN]);
    }

    #[test]
    fn mkmk_brings_along_the_whole_family() {
        let external = mock_external_features(&[BLWM, KERN, MKMK, DIST, MARK, ABVM]);
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
