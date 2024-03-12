//! gsub/gpos lookup table stuff

mod contextual;
mod gpos_builders;
mod gsub_builders;
mod helpers;

use std::{
    collections::{BTreeMap, HashMap},
    convert::TryInto,
    fmt::Debug,
};

use smol_str::SmolStr;

use write_fonts::{
    tables::{
        gdef::GlyphClassDef,
        gpos::{self as write_gpos},
        gsub as write_gsub,
        layout::{
            ConditionSet as RawConditionSet, Feature, FeatureList, FeatureRecord,
            FeatureTableSubstitution, FeatureTableSubstitutionRecord, FeatureVariationRecord,
            FeatureVariations, LangSys, LangSysRecord, Lookup as RawLookup, LookupFlag, LookupList,
            Script, ScriptList, ScriptRecord,
        },
        variations::ivs_builder::VariationStoreBuilder,
    },
    types::Tag,
};

use crate::{
    common::{GlyphId, GlyphOrClass, GlyphSet},
    compile::{lookups::contextual::ChainOrNot, metrics::ValueRecord},
    Kind, Opts,
};

use super::{features::AllFeatures, metrics::Anchor, tags};

use contextual::{
    ContextualLookupBuilder, PosChainContextBuilder, PosContextBuilder, ReverseChainBuilder,
    SubChainContextBuilder, SubContextBuilder,
};

use gpos_builders::{CursivePosBuilder, MarkToLigBuilder, SinglePosBuilder};
pub use gpos_builders::{
    MarkToBaseBuilder, MarkToMarkBuilder, PairPosBuilder, PreviouslyAssignedClass,
};
use gsub_builders::{
    AlternateSubBuilder, LigatureSubBuilder, MultipleSubBuilder, SingleSubBuilder,
};
pub(crate) use helpers::ClassDefBuilder2;

/// A simple trait for building lookups
// This exists because we use it to implement `LookupBuilder<T>`
pub trait Builder {
    /// The type produced by this builder.
    ///
    /// In the case of lookups, this is always a `Vec<Subtable>`, because a single
    /// builder may produce multiple subtables in some instances.
    type Output;
    /// Finalize the builder, producing the output.
    ///
    /// # Note:
    ///
    /// The var_store is only used in GPOS, but we pass it everywhere.
    /// This is annoying but feels like the lesser of two evils. It's easy to
    /// ignore this argument where it isn't used, and this makes the logic
    /// in LookupBuilder simpler, since it is identical for GPOS/GSUB.
    ///
    /// It would be nice if this could then be Option<&mut T>, but that type is
    /// annoying to work with, as Option<&mut _> doesn't impl Copy, so you need
    /// to do a dance anytime you use it.
    fn build(self, var_store: &mut VariationStoreBuilder) -> Self::Output;
}

pub(crate) type FilterSetId = u16;

#[derive(Clone, Debug, Default)]
pub(crate) struct AllLookups {
    current: Option<SomeLookup>,
    current_name: Option<SmolStr>,
    gpos: Vec<PositionLookup>,
    gsub: Vec<SubstitutionLookup>,
    named: HashMap<SmolStr, LookupId>,
}

#[derive(Clone, Debug)]
pub(crate) struct LookupBuilder<T> {
    flags: LookupFlag,
    mark_set: Option<FilterSetId>,
    subtables: Vec<T>,
}

#[derive(Clone, Debug)]
pub(crate) enum PositionLookup {
    Single(LookupBuilder<SinglePosBuilder>),
    Pair(LookupBuilder<PairPosBuilder>),
    Cursive(LookupBuilder<CursivePosBuilder>),
    MarkToBase(LookupBuilder<MarkToBaseBuilder>),
    MarkToLig(LookupBuilder<MarkToLigBuilder>),
    MarkToMark(LookupBuilder<MarkToMarkBuilder>),
    // currently unused, matching feaLib: <https://github.com/fonttools/fonttools/issues/2539>
    #[allow(dead_code)]
    Contextual(LookupBuilder<PosContextBuilder>),
    ChainedContextual(LookupBuilder<PosChainContextBuilder>),
}

// a litle helper to implement this conversion trait.
//
// Note: this is only used in the API for adding external features ( aka feature
// writers) and so we only implement the conversion for the specific lookup types
// that we want to allow the client to add externally.
macro_rules! impl_into_pos_lookup {
    ($builder:ty, $variant:ident) => {
        impl From<LookupBuilder<$builder>> for PositionLookup {
            fn from(src: LookupBuilder<$builder>) -> PositionLookup {
                PositionLookup::$variant(src)
            }
        }
    };
}

impl_into_pos_lookup!(PairPosBuilder, Pair);
impl_into_pos_lookup!(MarkToBaseBuilder, MarkToBase);
impl_into_pos_lookup!(MarkToMarkBuilder, MarkToMark);

#[derive(Clone, Debug)]
pub(crate) enum SubstitutionLookup {
    Single(LookupBuilder<SingleSubBuilder>),
    Multiple(LookupBuilder<MultipleSubBuilder>),
    Alternate(LookupBuilder<AlternateSubBuilder>),
    Ligature(LookupBuilder<LigatureSubBuilder>),
    Contextual(LookupBuilder<SubContextBuilder>),
    ChainedContextual(LookupBuilder<SubChainContextBuilder>),
    Reverse(LookupBuilder<ReverseChainBuilder>),
}

#[derive(Clone, Debug)]
pub(crate) enum SomeLookup {
    GsubLookup(SubstitutionLookup),
    GposLookup(PositionLookup),
    GposContextual(ContextualLookupBuilder<PositionLookup>),
    GsubContextual(ContextualLookupBuilder<SubstitutionLookup>),
}

/// IDs assigned to lookups during compilation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum LookupId {
    /// An id for a GPOS lookup
    Gpos(usize),
    /// An id for a GSUB lookup
    Gsub(usize),
    /// A temporary ID assigned to a lookup constructed by the client.
    ///
    /// This id will be remapped when the external features are merged into
    /// the features generated from the FEA.
    External(usize),
    /// Used when a named lookup block has no rules.
    ///
    /// We parse this, but then discard it immediately whenever it is referenced.
    Empty,
}

/// A struct that remaps initial lookup ids to their final values.
///
/// LookupIds can need adjusting in a number of cases:
/// - if the 'aalt' feature is present it causes additional lookups to be
///   inserted at the start of the GSUB lookup list
/// - FEA code can indicate with inline comments where additional lookups
///   should be inserted
#[derive(Clone, Debug, Default)]
pub(crate) struct LookupIdMap {
    // we could consider having this store the final values as u16?
    mapping: HashMap<LookupId, LookupId>,
}

/// Tracks the current lookupflags state
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct LookupFlagInfo {
    pub(crate) flags: LookupFlag,
    pub(crate) mark_filter_set: Option<FilterSetId>,
}

/// A feature associated with a particular script and language.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FeatureKey {
    pub(crate) feature: Tag,
    pub(crate) language: Tag,
    pub(crate) script: Tag,
}

type FeatureIdx = u16;
type LookupIdx = u16;

/// A helper for building GSUB/GPOS tables
pub(crate) struct PosSubBuilder<T> {
    lookups: Vec<T>,
    scripts: BTreeMap<Tag, BTreeMap<Tag, LangSys>>,
    // map a feature tag + set of lookups to an index
    features: BTreeMap<(Tag, Vec<LookupIdx>), FeatureIdx>,
    // map a conditionset to a map of target features and the lookups to substitute
    variations: HashMap<RawConditionSet, HashMap<FeatureIdx, Vec<LookupIdx>>>,
}

impl<T: Default> LookupBuilder<T> {
    fn new(flags: LookupFlag, mark_set: Option<FilterSetId>) -> Self {
        LookupBuilder {
            flags,
            mark_set,
            subtables: vec![Default::default()],
        }
    }

    pub(crate) fn new_with_lookups(
        flags: LookupFlag,
        mark_set: Option<FilterSetId>,
        subtables: Vec<T>,
    ) -> Self {
        Self {
            flags,
            mark_set,
            subtables,
        }
    }

    //TODO: if we keep this, make it unwrap and ensure we always have a subtable
    pub fn last_mut(&mut self) -> Option<&mut T> {
        self.subtables.last_mut()
    }

    pub fn force_subtable_break(&mut self) {
        self.subtables.push(Default::default())
    }

    pub(crate) fn iter_subtables(&self) -> impl Iterator<Item = &T> + '_ {
        self.subtables.iter()
    }
}

impl<U> LookupBuilder<U> {
    /// A helper method for converting from (say) ContextBuilder to PosContextBuilder
    fn convert<T: From<U>>(self) -> LookupBuilder<T> {
        let LookupBuilder {
            flags,
            mark_set,
            subtables,
        } = self;
        LookupBuilder {
            flags,
            mark_set,
            subtables: subtables.into_iter().map(Into::into).collect(),
        }
    }
}

impl PositionLookup {
    fn force_subtable_break(&mut self) {
        match self {
            PositionLookup::Single(lookup) => lookup.force_subtable_break(),
            PositionLookup::Pair(lookup) => lookup.force_subtable_break(),
            PositionLookup::Cursive(lookup) => lookup.force_subtable_break(),
            PositionLookup::MarkToBase(lookup) => lookup.force_subtable_break(),
            PositionLookup::MarkToLig(lookup) => lookup.force_subtable_break(),
            PositionLookup::MarkToMark(lookup) => lookup.force_subtable_break(),
            PositionLookup::Contextual(lookup) => lookup.force_subtable_break(),
            PositionLookup::ChainedContextual(lookup) => lookup.force_subtable_break(),
        }
    }
}

impl SubstitutionLookup {
    fn force_subtable_break(&mut self) {
        match self {
            SubstitutionLookup::Single(lookup) => lookup.force_subtable_break(),
            SubstitutionLookup::Multiple(lookup) => lookup.force_subtable_break(),
            SubstitutionLookup::Alternate(lookup) => lookup.force_subtable_break(),
            SubstitutionLookup::Ligature(lookup) => lookup.force_subtable_break(),
            SubstitutionLookup::Contextual(lookup) => lookup.force_subtable_break(),
            SubstitutionLookup::Reverse(lookup) => lookup.force_subtable_break(),
            SubstitutionLookup::ChainedContextual(lookup) => lookup.force_subtable_break(),
        }
    }
}

impl<U, T> Builder for LookupBuilder<T>
where
    T: Builder<Output = Vec<U>>,
    U: Default,
{
    type Output = RawLookup<U>;

    fn build(self, var_store: &mut VariationStoreBuilder) -> Self::Output {
        let subtables = self
            .subtables
            .into_iter()
            .flat_map(|b| b.build(var_store).into_iter())
            .collect();
        RawLookup::new(self.flags, subtables, self.mark_set.unwrap_or_default())
    }
}

impl Builder for PositionLookup {
    type Output = write_gpos::PositionLookup;

    fn build(self, var_store: &mut VariationStoreBuilder) -> Self::Output {
        match self {
            PositionLookup::Single(lookup) => {
                write_gpos::PositionLookup::Single(lookup.build(var_store))
            }
            PositionLookup::Pair(lookup) => {
                write_gpos::PositionLookup::Pair(lookup.build(var_store))
            }
            PositionLookup::Cursive(lookup) => {
                write_gpos::PositionLookup::Cursive(lookup.build(var_store))
            }
            PositionLookup::MarkToBase(lookup) => {
                write_gpos::PositionLookup::MarkToBase(lookup.build(var_store))
            }
            PositionLookup::MarkToLig(lookup) => {
                write_gpos::PositionLookup::MarkToLig(lookup.build(var_store))
            }
            PositionLookup::MarkToMark(lookup) => {
                write_gpos::PositionLookup::MarkToMark(lookup.build(var_store))
            }
            PositionLookup::Contextual(lookup) => {
                write_gpos::PositionLookup::Contextual(lookup.build(var_store).into_concrete())
            }
            PositionLookup::ChainedContextual(lookup) => {
                write_gpos::PositionLookup::ChainContextual(lookup.build(var_store).into_concrete())
            }
        }
    }
}

impl Builder for SubstitutionLookup {
    type Output = write_gsub::SubstitutionLookup;

    fn build(self, _var_store: &mut VariationStoreBuilder) -> Self::Output {
        match self {
            SubstitutionLookup::Single(lookup) => {
                write_gsub::SubstitutionLookup::Single(lookup.build(_var_store))
            }
            SubstitutionLookup::Multiple(lookup) => {
                write_gsub::SubstitutionLookup::Multiple(lookup.build(_var_store))
            }
            SubstitutionLookup::Alternate(lookup) => {
                write_gsub::SubstitutionLookup::Alternate(lookup.build(_var_store))
            }
            SubstitutionLookup::Ligature(lookup) => {
                write_gsub::SubstitutionLookup::Ligature(lookup.build(_var_store))
            }
            SubstitutionLookup::Contextual(lookup) => {
                write_gsub::SubstitutionLookup::Contextual(lookup.build(_var_store).into_concrete())
            }
            SubstitutionLookup::ChainedContextual(lookup) => {
                write_gsub::SubstitutionLookup::ChainContextual(
                    lookup.build(_var_store).into_concrete(),
                )
            }
            SubstitutionLookup::Reverse(lookup) => {
                write_gsub::SubstitutionLookup::Reverse(lookup.build(_var_store))
            }
        }
    }
}

impl AllLookups {
    fn push(&mut self, lookup: SomeLookup) -> LookupId {
        match lookup {
            SomeLookup::GsubLookup(sub) => {
                self.gsub.push(sub);
                LookupId::Gsub(self.gsub.len() - 1)
            }
            SomeLookup::GposLookup(pos) => {
                self.gpos.push(pos);
                LookupId::Gpos(self.gpos.len() - 1)
            }
            SomeLookup::GposContextual(lookup) => {
                let id = LookupId::Gpos(self.gpos.len());
                assert_eq!(id, lookup.root_id); // sanity check
                let (lookup, anon_lookups) = lookup.into_lookups();
                match lookup {
                    ChainOrNot::Context(lookup) => self
                        .gpos
                        //NOTE: we currently force all GPOS7 into GPOS8, to match
                        //the behaviour of fonttools.
                        .push(PositionLookup::ChainedContextual(lookup.convert())),
                    ChainOrNot::Chain(lookup) => self
                        .gpos
                        .push(PositionLookup::ChainedContextual(lookup.convert())),
                }
                self.gpos.extend(anon_lookups);
                id
            }
            SomeLookup::GsubContextual(lookup) => {
                let id = LookupId::Gsub(self.gsub.len());
                assert_eq!(id, lookup.root_id); // sanity check
                let (lookup, anon_lookups) = lookup.into_lookups();
                match lookup {
                    ChainOrNot::Context(lookup) => self
                        .gsub
                        .push(SubstitutionLookup::Contextual(lookup.convert())),
                    ChainOrNot::Chain(lookup) => self
                        .gsub
                        .push(SubstitutionLookup::ChainedContextual(lookup.convert())),
                }
                self.gsub.extend(anon_lookups);
                id
            }
        }
    }

    pub(crate) fn get_named(&self, name: &str) -> Option<LookupId> {
        self.named.get(name).copied()
    }

    pub(crate) fn current_mut(&mut self) -> Option<&mut SomeLookup> {
        self.current.as_mut()
    }

    pub(crate) fn has_current(&self) -> bool {
        self.current.is_some()
    }

    /// Returns `true` if there is an active lookup of this kind
    pub(crate) fn has_current_kind(&self, kind: Kind) -> bool {
        self.current.as_ref().map(SomeLookup::kind) == Some(kind)
    }

    // `false` if we didn't have an active lookup
    pub(crate) fn add_subtable_break(&mut self) -> bool {
        if let Some(current) = self.current.as_mut() {
            match current {
                SomeLookup::GsubLookup(lookup) => lookup.force_subtable_break(),
                SomeLookup::GposLookup(lookup) => lookup.force_subtable_break(),
                SomeLookup::GposContextual(lookup) => lookup.force_subtable_break(),
                SomeLookup::GsubContextual(lookup) => lookup.force_subtable_break(),
            }
            true
        } else {
            false
        }
    }

    // doesn't start it, just stashes the name
    pub(crate) fn start_named(&mut self, name: SmolStr) {
        self.current_name = Some(name);
    }

    pub(crate) fn start_lookup(&mut self, kind: Kind, flags: LookupFlagInfo) -> Option<LookupId> {
        let finished_id = self.current.take().map(|lookup| self.push(lookup));
        let mut new_one = SomeLookup::new(kind, flags.flags, flags.mark_filter_set);

        let new_id = if is_gpos_rule(kind) {
            LookupId::Gpos(self.gpos.len())
        } else {
            LookupId::Gsub(self.gsub.len())
        };

        match &mut new_one {
            SomeLookup::GsubContextual(lookup) => lookup.root_id = new_id,
            SomeLookup::GposContextual(lookup) => lookup.root_id = new_id,
            //SomeLookup::GsubReverse(_) => (),
            SomeLookup::GsubLookup(_) | SomeLookup::GposLookup(_) => (),
        }
        self.current = Some(new_one);
        finished_id
    }

    pub(crate) fn finish_current(&mut self) -> Option<(LookupId, Option<SmolStr>)> {
        if let Some(lookup) = self.current.take() {
            let id = self.push(lookup);
            if let Some(name) = self.current_name.take() {
                self.named.insert(name.clone(), id);
                Some((id, Some(name)))
            } else {
                Some((id, None))
            }
        } else if let Some(name) = self.current_name.take() {
            self.named.insert(name.clone(), LookupId::Empty);
            // there was a named block with no rules, return the empty lookup
            Some((LookupId::Empty, Some(name)))
        } else {
            None
        }
    }

    pub(crate) fn promote_single_sub_to_multi_if_necessary(&mut self) {
        if !self.has_current_kind(Kind::GsubType1) {
            return;
        }
        let Some(SomeLookup::GsubLookup(SubstitutionLookup::Single(lookup))) = self.current.take()
        else {
            unreachable!()
        };
        let promoted = LookupBuilder {
            flags: lookup.flags,
            mark_set: lookup.mark_set,
            subtables: lookup
                .subtables
                .into_iter()
                .map(SingleSubBuilder::promote_to_multi_sub)
                .collect(),
        };
        self.current = Some(SomeLookup::GsubLookup(SubstitutionLookup::Multiple(
            promoted,
        )));
    }

    pub(crate) fn infer_glyph_classes(&self, mut f: impl FnMut(GlyphId, GlyphClassDef)) {
        for lookup in &self.gpos {
            match lookup {
                PositionLookup::MarkToBase(lookup) => {
                    for subtable in &lookup.subtables {
                        subtable
                            .base_glyphs()
                            .for_each(|k| f(k, GlyphClassDef::Base));
                        subtable
                            .mark_glyphs()
                            .for_each(|k| f(k, GlyphClassDef::Mark));
                    }
                }
                PositionLookup::MarkToLig(lookup) => {
                    for subtable in &lookup.subtables {
                        subtable
                            .lig_glyphs()
                            .for_each(|k| f(k, GlyphClassDef::Ligature));
                        subtable
                            .mark_glyphs()
                            .for_each(|k| f(k, GlyphClassDef::Mark));
                    }
                }
                PositionLookup::MarkToMark(lookup) => {
                    for subtable in &lookup.subtables {
                        subtable
                            .mark1_glyphs()
                            .chain(subtable.mark2_glyphs())
                            .for_each(|k| f(k, GlyphClassDef::Mark));
                    }
                }
                _ => (),
            }
        }
        //TODO: the spec says to do gsub too, but fonttools doesn't?
    }

    /// Return the aalt-relevant lookups for this lookup Id.
    ///
    /// If lookup is GSUB type 1 or 3, return a single lookup.
    /// If contextual, returns any referenced single-sub lookups.
    pub(crate) fn aalt_lookups(&self, id: LookupId) -> Vec<&SubstitutionLookup> {
        let lookup = self.get_gsub_lookup(&id);

        match lookup {
            Some(sub @ SubstitutionLookup::Single(_) | sub @ SubstitutionLookup::Alternate(_)) => {
                vec![sub]
            }
            Some(SubstitutionLookup::Contextual(lookup)) => lookup
                .subtables
                .iter()
                .flat_map(|sub| sub.iter_lookups())
                .filter_map(|id| match self.get_gsub_lookup(&id) {
                    Some(sub @ SubstitutionLookup::Single(_)) => Some(sub),
                    _ => None,
                })
                .collect(),
            Some(SubstitutionLookup::ChainedContextual(lookup)) => lookup
                .subtables
                .iter()
                .flat_map(|sub| sub.iter_lookups())
                .filter_map(|id| match self.get_gsub_lookup(&id) {
                    Some(sub @ SubstitutionLookup::Single(_)) => Some(sub),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    fn get_gsub_lookup(&self, id: &LookupId) -> Option<&SubstitutionLookup> {
        match id {
            LookupId::Gsub(idx) => self.gsub.get(*idx),
            _ => None,
        }
    }

    pub(crate) fn insert_aalt_lookups(
        &mut self,
        all_alts: HashMap<GlyphId, Vec<GlyphId>>,
    ) -> Vec<LookupId> {
        let mut single = SingleSubBuilder::default();
        let mut alt = AlternateSubBuilder::default();

        for (target, alts) in all_alts {
            if alts.len() == 1 {
                single.insert(target, alts[0]);
            } else {
                alt.insert(target, alts);
            }
        }
        let one = (!single.is_empty()).then(|| {
            SubstitutionLookup::Single(LookupBuilder::new_with_lookups(
                LookupFlag::empty(),
                None,
                vec![single],
            ))
        });
        let two = (!alt.is_empty()).then(|| {
            SubstitutionLookup::Alternate(LookupBuilder::new_with_lookups(
                LookupFlag::empty(),
                None,
                vec![alt],
            ))
        });

        let lookups = one.into_iter().chain(two).collect::<Vec<_>>();
        let lookup_ids = (0..lookups.len()).map(LookupId::Gsub).collect();

        // now we need to insert these lookups at the front of our gsub lookups,
        // and bump all of their ids:

        self.gsub.iter_mut().for_each(|lookup| match lookup {
            SubstitutionLookup::Contextual(lookup) => lookup
                .subtables
                .iter_mut()
                .for_each(|sub| sub.bump_all_lookup_ids(lookups.len())),
            SubstitutionLookup::ChainedContextual(lookup) => lookup
                .subtables
                .iter_mut()
                .for_each(|sub| sub.bump_all_lookup_ids(lookups.len())),
            _ => (),
        });

        let prev_lookups = std::mem::replace(&mut self.gsub, lookups);
        self.gsub.extend(prev_lookups);

        lookup_ids
    }

    /// Returns a map that must be used to remap the ids in any features where
    /// they were used.
    pub(crate) fn merge_external_lookups(
        &mut self,
        lookups: Vec<(LookupId, PositionLookup)>,
    ) -> LookupIdMap {
        let mut map = LookupIdMap::default();
        for (temp_id, lookup) in lookups {
            let final_id = self.push(SomeLookup::GposLookup(lookup));
            map.insert(temp_id, final_id);
        }
        map
    }

    pub(crate) fn build(
        &self,
        features: &AllFeatures,
        var_store: &mut VariationStoreBuilder,
        opts: &Opts,
    ) -> (Option<write_gsub::Gsub>, Option<write_gpos::Gpos>) {
        let mut gpos_builder = PosSubBuilder::new(self.gpos.clone());
        let mut gsub_builder = PosSubBuilder::new(self.gsub.clone());

        for (key, feature_lookups) in features.iter() {
            let required = features.is_required(key);

            if key.feature == tags::SIZE {
                gpos_builder.add(*key, Vec::new(), required);
                continue;
            }

            let (gpos_idxes, gsub_idxes) = feature_lookups.split_base_lookups();
            let mut gpos_feat_id = None;
            let mut gsub_feat_id = None;
            if opts.compile_gpos && !gpos_idxes.is_empty() {
                gpos_feat_id = Some(gpos_builder.add(*key, gpos_idxes.clone(), required));
            }

            if opts.compile_gsub && !gsub_idxes.is_empty() {
                gsub_feat_id = Some(gsub_builder.add(*key, gsub_idxes.clone(), required));
            }

            let variations = feature_lookups.split_variations();
            for (cond, gpos_var_idxes, gsub_var_idxes) in variations {
                if opts.compile_gpos && !gpos_var_idxes.is_empty() {
                    // add the lookups for the base feature
                    let mut all_ids = gpos_idxes.clone();
                    all_ids.extend(gpos_var_idxes);

                    // if this feature only has variations, we insert an empty
                    // base feature
                    let feat_id = gpos_feat_id
                        .get_or_insert_with(|| gpos_builder.add(*key, Vec::new(), false));
                    gpos_builder.add_variation(*feat_id, cond, all_ids);
                }
                if opts.compile_gsub && !gsub_var_idxes.is_empty() {
                    // add the lookups for the base feature
                    let mut all_ids = gsub_idxes.clone();
                    all_ids.extend(gsub_var_idxes);

                    let feat_id = gsub_feat_id
                        .get_or_insert_with(|| gsub_builder.add(*key, Vec::new(), false));

                    gsub_builder.add_variation(*feat_id, cond, all_ids);
                }
            }
        }

        (gsub_builder.build(var_store), gpos_builder.build(var_store))
    }
}

impl LookupId {
    fn to_raw(self) -> usize {
        match self {
            LookupId::Gpos(idx) => idx,
            LookupId::Gsub(idx) => idx,
            LookupId::Empty => usize::MAX,
            LookupId::External(idx) => idx,
        }
    }

    pub(crate) fn adjust_if_gsub(&mut self, value: usize) {
        if let LookupId::Gsub(idx) = self {
            *idx += value;
        }
    }

    pub(crate) fn to_gpos_id_or_die(self) -> u16 {
        let LookupId::Gpos(x) = self else {
            panic!("this *really* shouldn't happen")
        };
        x.try_into().unwrap()
    }

    pub(crate) fn to_gsub_id_or_die(self) -> u16 {
        let LookupId::Gsub(x) = self else {
            panic!("this *really* shouldn't happen")
        };
        x.try_into().unwrap()
    }
}

impl LookupIdMap {
    fn insert(&mut self, from: LookupId, to: LookupId) {
        self.mapping.insert(from, to);
    }

    pub(crate) fn get(&self, id: LookupId) -> LookupId {
        self.mapping.get(&id).copied().unwrap_or(id)
    }
}

impl LookupFlagInfo {
    pub(crate) fn new(flags: LookupFlag, mark_filter_set: Option<FilterSetId>) -> Self {
        LookupFlagInfo {
            flags,
            mark_filter_set,
        }
    }

    pub(crate) fn clear(&mut self) {
        self.flags = LookupFlag::empty();
        self.mark_filter_set = None;
    }
}

impl SomeLookup {
    fn new(kind: Kind, flags: LookupFlag, filter: Option<FilterSetId>) -> Self {
        // special kinds:
        match kind {
            Kind::GposType7 | Kind::GposType8 => {
                return SomeLookup::GposContextual(ContextualLookupBuilder::new(flags, filter))
            }
            Kind::GsubType5 | Kind::GsubType6 => {
                return SomeLookup::GsubContextual(ContextualLookupBuilder::new(flags, filter))
            }
            _ => (),
        }

        if is_gpos_rule(kind) {
            let lookup = match kind {
                Kind::GposType1 => PositionLookup::Single(LookupBuilder::new(flags, filter)),
                Kind::GposType2 => PositionLookup::Pair(LookupBuilder::new(flags, filter)),
                Kind::GposType3 => PositionLookup::Cursive(LookupBuilder::new(flags, filter)),
                Kind::GposType4 => PositionLookup::MarkToBase(LookupBuilder::new(flags, filter)),
                Kind::GposType5 => PositionLookup::MarkToLig(LookupBuilder::new(flags, filter)),
                Kind::GposType6 => PositionLookup::MarkToMark(LookupBuilder::new(flags, filter)),
                Kind::GposNode => unimplemented!("other gpos type?"),
                other => panic!("illegal kind for lookup: '{}'", other),
            };
            SomeLookup::GposLookup(lookup)
        } else {
            let lookup = match kind {
                Kind::GsubType1 => SubstitutionLookup::Single(LookupBuilder::new(flags, filter)),
                Kind::GsubType2 => SubstitutionLookup::Multiple(LookupBuilder::new(flags, filter)),
                Kind::GsubType3 => SubstitutionLookup::Alternate(LookupBuilder::new(flags, filter)),
                Kind::GsubType4 => SubstitutionLookup::Ligature(LookupBuilder::new(flags, filter)),
                Kind::GsubType5 => {
                    SubstitutionLookup::Contextual(LookupBuilder::new(flags, filter))
                }
                Kind::GsubType7 => unimplemented!("extension"),
                Kind::GsubType8 => SubstitutionLookup::Reverse(LookupBuilder::new(flags, filter)),
                other => panic!("illegal kind for lookup: '{}'", other),
            };
            SomeLookup::GsubLookup(lookup)
        }
    }

    fn kind(&self) -> Kind {
        match self {
            SomeLookup::GsubContextual(_) => Kind::GsubType6,
            SomeLookup::GposContextual(_) => Kind::GposType8,
            SomeLookup::GsubLookup(gsub) => match gsub {
                SubstitutionLookup::Single(_) => Kind::GsubType1,
                SubstitutionLookup::Multiple(_) => Kind::GsubType2,
                SubstitutionLookup::Alternate(_) => Kind::GsubType3,
                SubstitutionLookup::Ligature(_) => Kind::GsubType4,
                SubstitutionLookup::Reverse(_) => Kind::GsubType8,
                _ => panic!("unhandled table kind"),
            },
            SomeLookup::GposLookup(gpos) => match gpos {
                PositionLookup::Single(_) => Kind::GposType1,
                PositionLookup::Pair(_) => Kind::GposType2,
                PositionLookup::Cursive(_) => Kind::GposType3,
                PositionLookup::MarkToBase(_) => Kind::GposType4,
                PositionLookup::MarkToLig(_) => Kind::GposType5,
                PositionLookup::MarkToMark(_) => Kind::GposType6,
                PositionLookup::Contextual(_) => Kind::GposType7,
                PositionLookup::ChainedContextual(_) => Kind::GposType8,
            },
        }
    }

    pub(crate) fn add_gpos_type_1(&mut self, id: GlyphId, record: ValueRecord) {
        if let SomeLookup::GposLookup(PositionLookup::Single(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.insert(id, record);
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gpos_type_2_pair(
        &mut self,
        one: GlyphId,
        two: GlyphId,
        val_one: ValueRecord,
        val_two: ValueRecord,
    ) {
        if let SomeLookup::GposLookup(PositionLookup::Pair(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.insert_pair(one, val_one, two, val_two)
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gpos_type_2_class(
        &mut self,
        one: GlyphSet,
        two: GlyphSet,
        val_one: ValueRecord,
        val_two: ValueRecord,
    ) {
        if let SomeLookup::GposLookup(PositionLookup::Pair(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.insert_classes(one, val_one, two, val_two)
        } else {
            panic!("lookup mismatch");
        }
    }
    pub(crate) fn add_gpos_type_3(
        &mut self,
        id: GlyphId,
        entry: Option<Anchor>,
        exit: Option<Anchor>,
    ) {
        if let SomeLookup::GposLookup(PositionLookup::Cursive(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.insert(id, entry, exit);
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn with_gpos_type_4<R>(&mut self, f: impl FnOnce(&mut MarkToBaseBuilder) -> R) -> R {
        if let SomeLookup::GposLookup(PositionLookup::MarkToBase(table)) = self {
            let subtable = table.last_mut().unwrap();
            f(subtable)
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn with_gpos_type_5<R>(&mut self, f: impl FnOnce(&mut MarkToLigBuilder) -> R) -> R {
        if let SomeLookup::GposLookup(PositionLookup::MarkToLig(table)) = self {
            let subtable = table.last_mut().unwrap();
            f(subtable)
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn with_gpos_type_6<R>(&mut self, f: impl FnOnce(&mut MarkToMarkBuilder) -> R) -> R {
        if let SomeLookup::GposLookup(PositionLookup::MarkToMark(table)) = self {
            let subtable = table.last_mut().unwrap();
            f(subtable)
        } else {
            panic!("lookup mismatch");
        }
    }

    // shared between GSUB/GPOS contextual and chain contextual rules
    pub(crate) fn add_contextual_rule(
        &mut self,
        backtrack: Vec<GlyphOrClass>,
        input: Vec<(GlyphOrClass, Vec<LookupId>)>,
        lookahead: Vec<GlyphOrClass>,
    ) {
        match self {
            SomeLookup::GposContextual(lookup) => {
                lookup.last_mut().add(backtrack, input, lookahead)
            }
            SomeLookup::GsubContextual(lookup) => {
                lookup.last_mut().add(backtrack, input, lookahead)
            }
            _ => panic!("lookup mismatch : '{}'", self.kind()),
        }
    }

    pub(crate) fn add_gsub_type_1(&mut self, id: GlyphId, replacement: GlyphId) {
        if let SomeLookup::GsubLookup(SubstitutionLookup::Single(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.insert(id, replacement);
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gsub_type_2(&mut self, id: GlyphId, replacement: Vec<GlyphId>) {
        if let SomeLookup::GsubLookup(SubstitutionLookup::Multiple(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.insert(id, replacement);
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gsub_type_3(&mut self, id: GlyphId, alternates: Vec<GlyphId>) {
        if let SomeLookup::GsubLookup(SubstitutionLookup::Alternate(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.insert(id, alternates);
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gsub_type_4(&mut self, target: Vec<GlyphId>, replacement: GlyphId) {
        if let SomeLookup::GsubLookup(SubstitutionLookup::Ligature(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.insert(target, replacement);
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gsub_type_8(
        &mut self,
        backtrack: Vec<GlyphOrClass>,
        input: BTreeMap<GlyphId, GlyphId>,
        lookahead: Vec<GlyphOrClass>,
    ) {
        if let SomeLookup::GsubLookup(SubstitutionLookup::Reverse(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.add(backtrack, input, lookahead);
        }
    }

    pub(crate) fn as_gsub_contextual(
        &mut self,
    ) -> &mut ContextualLookupBuilder<SubstitutionLookup> {
        let SomeLookup::GsubContextual(table) = self else {
            panic!("lookup mismatch")
        };
        table
    }

    pub(crate) fn as_gpos_contextual(&mut self) -> &mut ContextualLookupBuilder<PositionLookup> {
        if let SomeLookup::GposContextual(table) = self {
            table
        } else {
            panic!("lookup mismatch")
        }
    }
}

impl<T> PosSubBuilder<T> {
    fn new(lookups: Vec<T>) -> Self {
        PosSubBuilder {
            lookups,
            scripts: Default::default(),
            features: Default::default(),
            variations: Default::default(),
        }
    }

    fn add(&mut self, key: FeatureKey, lookups: Vec<LookupIdx>, required: bool) -> FeatureIdx {
        let feat_key = (key.feature, lookups);
        let next_feature = self.features.len();
        let idx = *self
            .features
            .entry(feat_key)
            .or_insert_with(|| next_feature.try_into().expect("ran out of u16s"));

        let lang_sys = self
            .scripts
            .entry(key.script)
            .or_default()
            .entry(key.language)
            .or_default();

        if required {
            lang_sys.required_feature_index = idx;
        } else {
            lang_sys.feature_indices.push(idx);
        }
        idx
    }

    fn add_variation(
        &mut self,
        idx: FeatureIdx,
        conditions: &RawConditionSet,
        lookups: Vec<LookupIdx>,
    ) {
        // not using entry to avoid cloning conditions all the time?
        if !self.variations.contains_key(conditions) {
            self.variations
                .insert(conditions.clone(), Default::default());
        }
        self.variations
            .get_mut(conditions)
            .unwrap()
            .insert(idx, lookups);
    }
}

impl<T> PosSubBuilder<T>
where
    T: Builder,
    T::Output: Default,
{
    #[allow(clippy::type_complexity)] // i love my big dumb tuple
    fn build_raw(
        self,
        var_store: &mut VariationStoreBuilder,
    ) -> Option<(
        LookupList<T::Output>,
        ScriptList,
        FeatureList,
        Option<FeatureVariations>,
    )> {
        if self.lookups.is_empty() && self.features.is_empty() {
            return None;
        }

        // push empty items so we can insert by index
        let mut features = vec![Default::default(); self.features.len()];
        for ((tag, lookups), idx) in self.features {
            features[idx as usize] = FeatureRecord::new(tag, Feature::new(None, lookups));
        }

        let scripts = self
            .scripts
            .into_iter()
            .map(|(script_tag, entry)| {
                let mut script = Script::default();
                for (lang_tag, lang_sys) in entry {
                    if lang_tag == tags::LANG_DFLT {
                        script.default_lang_sys = lang_sys.into();
                    } else {
                        script
                            .lang_sys_records
                            .push(LangSysRecord::new(lang_tag, lang_sys));
                    }
                }
                ScriptRecord::new(script_tag, script)
            })
            .collect::<Vec<_>>();

        let lookups = self
            .lookups
            .into_iter()
            .map(|x| x.build(var_store))
            .collect();

        let variations = if self.variations.is_empty() {
            None
        } else {
            let records = self
                .variations
                .into_iter()
                .map(|(condset, features)| {
                    // if this is an empty conditionset, leave the offset null
                    let condset = (!condset.conditions.is_empty()).then_some(condset);
                    FeatureVariationRecord::new(
                        condset,
                        FeatureTableSubstitution::new(
                            features
                                .into_iter()
                                .map(|(feat_id, lookup_ids)| {
                                    FeatureTableSubstitutionRecord::new(
                                        feat_id,
                                        Feature::new(None, lookup_ids),
                                    )
                                })
                                .collect(),
                        )
                        .into(),
                    )
                })
                .collect();
            Some(FeatureVariations::new(records))
        };
        Some((
            LookupList::new(lookups),
            ScriptList::new(scripts),
            FeatureList::new(features),
            variations,
        ))
    }
}

impl Builder for PosSubBuilder<PositionLookup> {
    type Output = Option<write_gpos::Gpos>;

    fn build(self, var_store: &mut VariationStoreBuilder) -> Self::Output {
        self.build_raw(var_store)
            .map(|(lookups, scripts, features, variations)| {
                let mut gpos = write_gpos::Gpos::new(scripts, features, lookups);
                gpos.feature_variations = variations.into();
                gpos
            })
    }
}

impl Builder for PosSubBuilder<SubstitutionLookup> {
    type Output = Option<write_gsub::Gsub>;

    fn build(self, var_store: &mut VariationStoreBuilder) -> Self::Output {
        self.build_raw(var_store)
            .map(|(lookups, scripts, features, variations)| {
                let mut gsub = write_gsub::Gsub::new(scripts, features, lookups);
                gsub.feature_variations = variations.into();
                gsub
            })
    }
}

impl FeatureKey {
    /// Create a new feature key for the provided feature, language, and script.
    ///
    /// If you already have a [`super::LanguageSystem`], you can create a [`FeatureKey`]
    /// with the [`super::LanguageSystem::to_feature_key`] method.
    pub fn new(feature: Tag, language: Tag, script: Tag) -> Self {
        FeatureKey {
            feature,
            language,
            script,
        }
    }
}

impl Debug for FeatureKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}/{}", self.feature, self.script, self.language)
    }
}

fn is_gpos_rule(kind: Kind) -> bool {
    matches!(
        kind,
        Kind::GposType1
            | Kind::GposType2
            | Kind::GposType3
            | Kind::GposType4
            | Kind::GposType5
            | Kind::GposType6
            | Kind::GposType7
            | Kind::GposType8
    )
}
