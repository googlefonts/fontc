//! gsub/gpos lookup table stuff

mod contextual;
mod gpos;
mod gsub;

use std::{
    collections::{BTreeMap, HashMap},
    convert::TryInto,
};

use font_types::Tag;
use smol_str::SmolStr;

use write_fonts::tables::{
    gpos::{self as write_gpos, ValueRecord},
    gsub as write_gsub,
    layout::{
        Feature, FeatureList, FeatureRecord, LangSys, LangSysRecord, Lookup as RawLookup,
        LookupFlag, LookupList, Script, ScriptList, ScriptRecord,
    },
};

use crate::{
    compile::lookups::contextual::ChainOrNot,
    types::{Anchor, GlyphId, GlyphOrClass},
    Kind,
};

use self::contextual::{ChainContextBuilder, ReverseChainBuilder};

use super::{
    consts::{LANG_DFLT_TAG, SCRIPT_DFLT_TAG, SIZE_TAG},
    tables::ClassId,
};

use contextual::{ContextBuilder, ContextualLookupBuilder};
pub use gpos::PreviouslyAssignedClass;
use gpos::{
    CursivePosBuilder, MarkToBaseBuilder, MarkToLigBuilder, MarkToMarkBuilder, PairPosBuilder,
    SinglePosBuilder,
};
use gsub::{AlternateSubBuilder, LigatureSubBuilder, MultipleSubBuilder, SingleSubBuilder};

pub trait Builder {
    type Output;
    fn build(self) -> Self::Output;
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
    Contextual(LookupBuilder<ContextBuilder>),
    ChainedContextual(LookupBuilder<ChainContextBuilder>),
}

#[derive(Clone, Debug)]
pub(crate) enum SubstitutionLookup {
    Single(LookupBuilder<SingleSubBuilder>),
    Multiple(LookupBuilder<MultipleSubBuilder>),
    Alternate(LookupBuilder<AlternateSubBuilder>),
    Ligature(LookupBuilder<LigatureSubBuilder>),
    Contextual(LookupBuilder<ContextBuilder>),
    ChainedContextual(LookupBuilder<ChainContextBuilder>),
    Reverse(LookupBuilder<ReverseChainBuilder>),
}

#[derive(Clone, Debug)]
pub(crate) enum SomeLookup {
    GsubLookup(SubstitutionLookup),
    GposLookup(PositionLookup),
    GposContextual(ContextualLookupBuilder<PositionLookup>),
    GsubContextual(ContextualLookupBuilder<SubstitutionLookup>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub(crate) enum LookupId {
    Gpos(usize),
    Gsub(usize),
    /// Used when a named lookup block has no rules.
    ///
    /// We parse this, but then discard it immediately whenever it is referenced.
    Empty,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct FeatureKey {
    pub(crate) feature: Tag,
    pub(crate) script: Tag,
    pub(crate) language: Tag,
}

/// A helper for building GSUB/GPOS tables
pub(crate) struct PosSubBuilder<T> {
    lookups: Vec<T>,
    scripts: BTreeMap<Tag, BTreeMap<Tag, Vec<u16>>>,
    features: BTreeMap<(Tag, Vec<u16>), usize>,
}

impl<T: Default> LookupBuilder<T> {
    fn new(flags: LookupFlag, mark_set: Option<FilterSetId>) -> Self {
        LookupBuilder {
            flags,
            mark_set,
            subtables: vec![Default::default()],
        }
    }

    fn new_with_lookups(
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

    fn build(self) -> Self::Output {
        let subtables = self
            .subtables
            .into_iter()
            .map(|b| b.build().into_iter())
            .flatten()
            .collect();
        RawLookup::new(self.flags, subtables, self.mark_set.unwrap_or_default())
    }
}

impl Builder for PositionLookup {
    type Output = write_gpos::PositionLookup;

    fn build(self) -> Self::Output {
        match self {
            PositionLookup::Single(lookup) => write_gpos::PositionLookup::Single(lookup.build()),
            PositionLookup::Pair(lookup) => write_gpos::PositionLookup::Pair(lookup.build()),
            PositionLookup::Cursive(lookup) => write_gpos::PositionLookup::Cursive(lookup.build()),
            PositionLookup::MarkToBase(lookup) => {
                write_gpos::PositionLookup::MarkToBase(lookup.build())
            }
            PositionLookup::MarkToLig(lookup) => {
                write_gpos::PositionLookup::MarkToLig(lookup.build())
            }
            PositionLookup::MarkToMark(lookup) => {
                write_gpos::PositionLookup::MarkToMark(lookup.build())
            }
            PositionLookup::Contextual(lookup) => {
                write_gpos::PositionLookup::Contextual(lookup.build().into_concrete())
            }
            PositionLookup::ChainedContextual(lookup) => {
                write_gpos::PositionLookup::ChainContextual(lookup.build().into_concrete())
            }
        }
    }
}

impl Builder for SubstitutionLookup {
    type Output = write_gsub::SubstitutionLookup;

    fn build(self) -> Self::Output {
        match self {
            SubstitutionLookup::Single(lookup) => {
                write_gsub::SubstitutionLookup::Single(lookup.build())
            }
            SubstitutionLookup::Multiple(lookup) => {
                write_gsub::SubstitutionLookup::Multiple(lookup.build())
            }
            SubstitutionLookup::Alternate(lookup) => {
                write_gsub::SubstitutionLookup::Alternate(lookup.build())
            }
            SubstitutionLookup::Ligature(lookup) => {
                write_gsub::SubstitutionLookup::Ligature(lookup.build())
            }
            SubstitutionLookup::Contextual(lookup) => {
                write_gsub::SubstitutionLookup::Contextual(lookup.build().into_concrete())
            }
            SubstitutionLookup::ChainedContextual(lookup) => {
                write_gsub::SubstitutionLookup::ChainContextual(lookup.build().into_concrete())
            }
            SubstitutionLookup::Reverse(lookup) => {
                write_gsub::SubstitutionLookup::Reverse(lookup.build())
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
                    ChainOrNot::Context(lookup) => {
                        self.gpos.push(PositionLookup::Contextual(lookup))
                    }
                    ChainOrNot::Chain(lookup) => {
                        self.gpos.push(PositionLookup::ChainedContextual(lookup))
                    }
                }
                self.gpos.extend(anon_lookups);
                id
            }
            SomeLookup::GsubContextual(lookup) => {
                let id = LookupId::Gsub(self.gsub.len());
                assert_eq!(id, lookup.root_id); // sanity check
                let (lookup, anon_lookups) = lookup.into_lookups();
                match lookup {
                    ChainOrNot::Context(lookup) => {
                        self.gsub.push(SubstitutionLookup::Contextual(lookup))
                    }
                    ChainOrNot::Chain(lookup) => self
                        .gsub
                        .push(SubstitutionLookup::ChainedContextual(lookup)),
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

    /// should be called before each new rule.
    pub(crate) fn needs_new_lookup(&self, kind: Kind) -> bool {
        self.current.is_none() || self.current.as_ref().map(SomeLookup::kind) != Some(kind)
    }

    // `false` if we didn't have an active lookup
    pub(crate) fn add_subtable_break(&mut self) -> bool {
        if let Some(current) = self.current.as_mut() {
            //FIXME
            match current {
                SomeLookup::GsubLookup(lookup) => lookup.force_subtable_break(),
                SomeLookup::GposLookup(lookup) => lookup.force_subtable_break(),
                _ => (),
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

    pub(crate) fn start_lookup(
        &mut self,
        kind: Kind,
        flags: LookupFlag,
        mark_set: Option<FilterSetId>,
    ) -> Option<LookupId> {
        let finished_id = self.current.take().map(|lookup| self.push(lookup));
        let mut new_one = SomeLookup::new(kind, flags, mark_set);

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

    pub(crate) fn infer_glyph_classes(&self, mut f: impl FnMut(GlyphId, ClassId)) {
        for lookup in &self.gpos {
            match lookup {
                PositionLookup::MarkToBase(lookup) => {
                    for subtable in &lookup.subtables {
                        subtable.base_glyphs().for_each(|k| f(k, ClassId::Base));
                        subtable.mark_glyphs().for_each(|k| f(k, ClassId::Mark));
                    }
                }
                PositionLookup::MarkToLig(lookup) => {
                    for subtable in &lookup.subtables {
                        subtable.lig_glyphs().for_each(|k| f(k, ClassId::Ligature));
                        subtable.mark_glyphs().for_each(|k| f(k, ClassId::Mark));
                    }
                }
                PositionLookup::MarkToMark(lookup) => {
                    for subtable in &lookup.subtables {
                        subtable
                            .mark1_glyphs()
                            .chain(subtable.mark2_glyphs())
                            .for_each(|k| f(k, ClassId::Mark));
                    }
                }
                _ => (),
            }
        }
        //TODO: the spec says to do gsub too, but fonttools doesn't?
    }

    pub(crate) fn build(
        &self,
        features: &BTreeMap<FeatureKey, Vec<LookupId>>,
    ) -> (Option<write_gsub::Gsub>, Option<write_gpos::Gpos>) {
        let mut gpos_builder = PosSubBuilder::new(self.gpos.clone());
        let mut gsub_builder = PosSubBuilder::new(self.gsub.clone());

        for (key, feature_indices) in features {
            assert!(crate::util::is_sorted(feature_indices));
            let split_idx = feature_indices
                .iter()
                .position(|x| matches!(x, LookupId::Gsub(_)))
                .unwrap_or(feature_indices.len());

            if key.feature == SIZE_TAG {
                gpos_builder.add(*key, &[]);
                continue;
            }

            let (gpos_idxes, gsub_idxes) = feature_indices.split_at(split_idx);
            if !gpos_idxes.is_empty() {
                gpos_builder.add(*key, gpos_idxes);
            }

            if !gsub_idxes.is_empty() {
                gsub_builder.add(*key, gsub_idxes);
            }
        }

        (gsub_builder.build(), gpos_builder.build())
    }
}

impl FeatureKey {
    pub(crate) fn for_feature(feature: Tag) -> Self {
        FeatureKey {
            feature,
            script: SCRIPT_DFLT_TAG,
            language: LANG_DFLT_TAG,
        }
    }

    pub(crate) fn script(mut self, script: Tag) -> Self {
        self.script = script;
        self
    }

    pub(crate) fn language(mut self, language: Tag) -> Self {
        self.language = language;
        self
    }
}

impl LookupId {
    fn to_raw(self) -> usize {
        match self {
            LookupId::Gpos(idx) => idx,
            LookupId::Gsub(idx) => idx,
            LookupId::Empty => usize::MAX,
        }
    }

    pub(crate) fn to_u16_or_die(self) -> u16 {
        self.to_raw().try_into().unwrap()
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
                //FIXME: should be a kind? idk
                //PositionLookup::Extension => Kind::GposNode,
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

    pub(crate) fn add_gpos_type_2_specific(
        &mut self,
        one: GlyphId,
        two: GlyphId,
        val_one: ValueRecord,
        val_two: ValueRecord,
    ) {
        if let SomeLookup::GposLookup(PositionLookup::Pair(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.insert(one, val_one, two, val_two)
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gpos_type_3(&mut self, id: GlyphId, entry: Anchor, exit: Anchor) {
        if let SomeLookup::GposLookup(PositionLookup::Cursive(table)) = self {
            let subtable = table.last_mut().unwrap();
            subtable.insert(id, entry.to_raw(), exit.to_raw());
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
        }
    }

    fn add(&mut self, key: FeatureKey, lookups: &[LookupId]) {
        let lookups = lookups.iter().map(|idx| idx.to_u16_or_die()).collect();
        let feat_key = (key.feature, lookups);
        let idx = match self.features.get(&feat_key) {
            Some(idx) => *idx,
            None => {
                let idx = self.features.len();
                self.features.insert(feat_key, idx);
                idx
            }
        };
        self.scripts
            .entry(key.script)
            .or_default()
            .entry(key.language)
            .or_default()
            .push(idx.try_into().unwrap());
    }
}

impl<T> PosSubBuilder<T>
where
    T: Builder,
    T::Output: Default,
{
    fn build_raw(self) -> Option<(LookupList<T::Output>, ScriptList, FeatureList)> {
        if self.lookups.is_empty() && self.features.is_empty() {
            return None;
        }

        // push empty items so we can insert by index
        let mut features = Vec::with_capacity(self.features.len());
        features.resize_with(self.features.len(), Default::default);
        for ((tag, lookups), idx) in self.features {
            features[idx] = FeatureRecord::new(tag, Feature::new(None, lookups));
        }

        let scripts = self
            .scripts
            .into_iter()
            .map(|(script_tag, entry)| {
                let mut script = Script::default();
                for (lang_tag, feature_indices) in entry {
                    let sys = LangSys::new(0xffff, feature_indices);
                    if lang_tag == LANG_DFLT_TAG {
                        script.default_lang_sys = sys.into();
                    } else {
                        script
                            .lang_sys_records
                            .push(LangSysRecord::new(lang_tag, sys));
                    }
                }
                ScriptRecord::new(script_tag, script)
            })
            .collect::<Vec<_>>();

        let lookups = self.lookups.into_iter().map(|x| x.build()).collect();
        Some((
            LookupList::new(lookups),
            ScriptList::new(scripts),
            FeatureList::new(features),
        ))
    }
}

impl Builder for PosSubBuilder<PositionLookup> {
    type Output = Option<write_gpos::Gpos>;

    fn build(self) -> Self::Output {
        self.build_raw()
            .map(|(lookups, scripts, features)| write_gpos::Gpos::new(scripts, features, lookups))
    }
}

impl Builder for PosSubBuilder<SubstitutionLookup> {
    type Output = Option<write_gsub::Gsub>;

    fn build(self) -> Self::Output {
        self.build_raw()
            .map(|(lookups, scripts, features)| write_gsub::Gsub::new(scripts, features, lookups))
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
