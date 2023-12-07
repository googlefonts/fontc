//! API for the client to manually add additional features

use std::collections::{BTreeMap, HashMap};

use write_fonts::{tables::layout::LookupFlag, types::Tag};

use crate::GlyphSet;

use super::{
    features::FeatureLookups,
    language_system::{DefaultLanguageSystems, LanguageSystem},
    lookups::{FeatureKey, FilterSetId, LookupBuilder, LookupId, PositionLookup},
    tables::{GdefBuilder, Tables},
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
    mark_filter_sets: HashMap<GlyphSet, FilterSetId>,
    // because there may already be defined filter sets from the root fea
    filter_set_id_start: usize,
}

pub trait GposSubtableBuilder: Sized {
    #[doc(hidden)]
    fn to_pos_lookup(
        flags: LookupFlag,
        filter_set: Option<FilterSetId>,
        subtables: Vec<Self>,
    ) -> ExternalGposLookup;
}

/// An externally created GPOS lookup.
///
/// This only exists so that we can avoid making our internal types `pub`.
pub struct ExternalGposLookup(PositionLookup);

impl<'a> FeatureBuilder<'a> {
    pub(crate) fn new(
        language_systems: &'a DefaultLanguageSystems,
        tables: &'a mut Tables,
        filter_set_id_start: usize,
    ) -> Self {
        Self {
            language_systems,
            tables,
            lookups: Default::default(),
            features: Default::default(),
            mark_filter_sets: Default::default(),
            filter_set_id_start,
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

    /// Create a new lookup.
    ///
    /// The `LookupId` that is returned can then be included in features
    pub fn add_lookup<T: GposSubtableBuilder>(
        &mut self,
        flags: LookupFlag,
        filter_set: Option<GlyphSet>,
        subtables: Vec<T>,
    ) -> LookupId {
        let filter_set_id = filter_set.map(|cls| self.get_filter_set_id(cls));
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
        let next_id = self.filter_set_id_start + self.mark_filter_sets.len();
        //.expect("too many filter sets");
        *self.mark_filter_sets.entry(cls).or_insert_with(|| {
            next_id
                .try_into()
                // is this in any way an expected error condition?
                .expect("too many filter sets?")
        })
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
