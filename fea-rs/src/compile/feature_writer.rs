//! API for the client to manually add additional features

use std::collections::{BTreeMap, HashMap};

use smol_str::SmolStr;
use write_fonts::tables::{
    gpos::AnchorTable,
    layout::{LookupFlag, PendingVariationIndex},
    variations::VariationRegion,
};

use crate::common::{GlyphClass, MarkClass};

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

/// A structure that allows client code to add additional features to the compilation.
pub struct FeatureBuilder<'a> {
    pub(crate) language_systems: &'a DefaultLanguageSystems,
    pub(crate) tables: &'a mut Tables,
    pub(crate) lookups: Vec<(LookupId, PositionLookup)>,
    pub(crate) features: BTreeMap<FeatureKey, FeatureLookups>,
    pub(crate) mark_classes: HashMap<SmolStr, MarkClass>,
    mark_filter_sets: HashMap<GlyphClass, FilterSetId>,
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
            mark_classes: Default::default(),
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

    /// Define a new mark class, for use in mark-base and mark-mark rules.
    ///
    /// TODO: do we want to ensure there are no redefinitions? do we want
    /// return an error? do we want to uphold any other invariants?
    ///
    /// ALSO: mark class IDs depend on definition order in the source.
    /// I have no idea how best to approximate that in this API :/
    pub fn define_mark_class(
        &mut self,
        class_name: impl Into<SmolStr>,
        members: Vec<(GlyphClass, Option<AnchorTable>)>,
    ) {
        self.mark_classes
            .insert(class_name.into(), MarkClass { members });
    }

    /// Create a new lookup.
    ///
    /// The `LookupId` that is returned can then be included in features via
    /// the [`add_feature`] method.
    pub fn add_lookup<T: GposSubtableBuilder>(
        &mut self,
        flags: LookupFlag,
        filter_set: Option<GlyphClass>,
        subtables: Vec<T>,
    ) -> LookupId {
        let filter_set_id = filter_set.map(|cls| self.get_filter_set_id(cls));
        let lookup = T::to_pos_lookup(flags, filter_set_id, subtables);
        let next_id = LookupId::External(self.lookups.len());
        self.lookups.push((next_id, lookup.0));
        next_id
    }

    /// Add a set of deltas to the `ItemVariationStore`.
    ///
    /// Returns a `PendingVariationIndex` which should be stored whereever a
    /// `VariationIndex` table would be expected (it will be remapped during
    /// compilation).
    pub fn add_deltas<T: Into<i32>>(
        &mut self,
        deltas: Vec<(VariationRegion, T)>,
    ) -> PendingVariationIndex {
        let delta_set_id = self.tables.var_store().add_deltas(deltas);
        PendingVariationIndex { delta_set_id }
    }

    /// Create a new feature, registered for a particular language system.
    ///
    /// The caller must call this method once for each language system under
    /// which a feature is to be registered.
    pub fn add_feature(&mut self, key: FeatureKey, lookups: Vec<LookupId>) {
        self.features.entry(key).or_default().base = lookups;
    }

    fn get_filter_set_id(&mut self, cls: GlyphClass) -> FilterSetId {
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
