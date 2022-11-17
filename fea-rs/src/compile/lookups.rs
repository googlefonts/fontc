//! gsub/gpos lookup table stuff

use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
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
    types::{Anchor, GlyphId},
    Kind,
};

use super::{
    builders::{
        AlternateSubBuilder, Builder, CursivePosBuilder, LigatureSubBuilder, MarkToBaseBuilder,
        MarkToLigBuilder, MarkToMarkBuilder, MultipleSubBuilder, PairPosBuilder, SinglePosBuilder,
        SingleSubBuilder,
    },
    consts::{LANG_DFLT_TAG, SCRIPT_DFLT_TAG, SIZE_TAG},
    tables::ClassId,
};

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
pub(crate) struct Lookup<T> {
    flags: LookupFlag,
    mark_set: Option<FilterSetId>,
    subtables: Vec<T>,
}

#[derive(Clone, Debug)]
pub(crate) enum PositionLookup {
    Single(Lookup<SinglePosBuilder>),
    Pair(Lookup<PairPosBuilder>),
    Cursive(Lookup<CursivePosBuilder>),
    MarkToBase(Lookup<MarkToBaseBuilder>),
    MarkToLig(Lookup<MarkToLigBuilder>),
    MarkToMark(Lookup<MarkToMarkBuilder>),
    Contextual(Lookup<()>),
    ChainedContextual(Lookup<()>),
}

#[derive(Clone, Debug)]
pub(crate) enum SubstitutionLookup {
    Single(Lookup<SingleSubBuilder>),
    Multiple(Lookup<MultipleSubBuilder>),
    Alternate(Lookup<AlternateSubBuilder>),
    Ligature(Lookup<LigatureSubBuilder>),
    Contextual(Lookup<()>),
    ChainedContextual(Lookup<()>),
    Reverse(Lookup<()>),
}

#[derive(Clone, Debug)]
pub(crate) enum SomeLookup {
    GsubLookup(SubstitutionLookup),
    GposLookup(PositionLookup),
    //GsubContextual(ContextualLookup<SubstitutionSequenceContext, SubstitutionLookup>),
    //GsubReverse(ContextualLookup<ReverseChainSingleSubstFormat1, SubstitutionLookup>),
    //GposContextual(ContextualLookup<PositionSequenceContext, PositionLookup>),
}

/// When building a contextual/chaining contextual rule, we also build a
/// bunch of anonymous lookups.
#[derive(Debug, Clone)]
pub(crate) struct ContextualLookup<T, U> {
    flags: LookupFlag,
    mark_set: Option<FilterSetId>,
    subtables: Vec<T>,
    anon_lookups: Vec<U>,
    root_id: LookupId,
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

impl<T> Lookup<T> {
    pub fn brand_new(flags: LookupFlag, mark_set: Option<FilterSetId>) -> Self
    where
        T: Default,
    {
        Lookup {
            flags,
            mark_set,
            subtables: vec![Default::default()],
        }
    }

    pub fn new(flags: LookupFlag, mark_set: Option<FilterSetId>, subtables: Vec<T>) -> Self {
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
}

impl<U, T> Builder for Lookup<T>
where
    T: Builder<Output = Vec<U>>,
    U: Default,
{
    type Output = RawLookup<U>;

    fn build(self) -> Result<Self::Output, ()> {
        let subtables = self
            .subtables
            .into_iter()
            .map(|b| b.build().unwrap().into_iter())
            .flatten()
            .collect();
        Ok(RawLookup::new(
            self.flags,
            subtables,
            self.mark_set.unwrap_or_default(),
        ))
    }
}

impl Builder for PositionLookup {
    type Output = write_gpos::PositionLookup;

    fn build(self) -> Result<Self::Output, ()> {
        match self {
            PositionLookup::Single(lookup) => {
                lookup.build().map(write_gpos::PositionLookup::Single)
            }
            PositionLookup::Pair(lookup) => lookup.build().map(write_gpos::PositionLookup::Pair),
            PositionLookup::Cursive(lookup) => {
                lookup.build().map(write_gpos::PositionLookup::Cursive)
            }
            PositionLookup::MarkToBase(lookup) => {
                lookup.build().map(write_gpos::PositionLookup::MarkToBase)
            }
            PositionLookup::MarkToLig(lookup) => {
                lookup.build().map(write_gpos::PositionLookup::MarkToLig)
            }
            PositionLookup::MarkToMark(lookup) => {
                lookup.build().map(write_gpos::PositionLookup::MarkToMark)
            }
            PositionLookup::Contextual(_) => {
                Ok(write_gpos::PositionLookup::Contextual(Default::default()))
            }
            PositionLookup::ChainedContextual(_) => Ok(
                write_gpos::PositionLookup::ChainContextual(Default::default()),
            ),
        }
    }
}

impl Builder for SubstitutionLookup {
    type Output = write_gsub::SubstitutionLookup;

    fn build(self) -> Result<Self::Output, ()> {
        match self {
            SubstitutionLookup::Single(lookup) => {
                lookup.build().map(write_gsub::SubstitutionLookup::Single)
            }
            SubstitutionLookup::Multiple(_) => {
                Ok(write_gsub::SubstitutionLookup::Multiple(Default::default()))
            }
            SubstitutionLookup::Alternate(_) => {
                Ok(write_gsub::SubstitutionLookup::Alternate(Default::default()))
            }
            SubstitutionLookup::Ligature(_) => {
                Ok(write_gsub::SubstitutionLookup::Ligature(Default::default()))
            }
            SubstitutionLookup::Contextual(_) => Ok(write_gsub::SubstitutionLookup::Contextual(
                Default::default(),
            )),
            SubstitutionLookup::ChainedContextual(_) => Ok(
                write_gsub::SubstitutionLookup::ChainContextual(Default::default()),
            ),
            SubstitutionLookup::Reverse(_) => {
                Ok(write_gsub::SubstitutionLookup::Reverse(Default::default()))
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
            } //SomeLookup::GsubContextual(ContextualLookup {
              //flags,
              //mark_set,
              //subtables,
              //anon_lookups,
              //root_id,
              //}) => {
              //let id = LookupId::Gsub(self.gsub.len());
              //assert_eq!(id, root_id); // sanity check
              //self.gsub.push(SubstitutionLookup::Contextual(Lookup::new(
              //flags, mark_set, subtables,
              //)));
              ////lookup_flag: flags,
              ////mark_filtering_set: mark_set,
              ////subtable_offsets: subtables.into_iter().map(OffsetMarker::new).collect(),
              ////}));
              //self.gsub.extend(anon_lookups);
              //id
              //}
              //SomeLookup::GsubReverse(ContextualLookup {
              //flags,
              //mark_set,
              //subtables,
              //anon_lookups,
              //root_id,
              //}) => {
              //let id = LookupId::Gsub(self.gsub.len());
              //assert_eq!(id, root_id); // sanity check
              //self.gsub.push(SubstitutionLookup::Reverse(Lookup::new(
              //flags, mark_set, subtables,
              //)));
              //self.gsub.extend(anon_lookups);
              //id
              //}
              //SomeLookup::GposContextual(ContextualLookup {
              //flags,
              //mark_set,
              //subtables,
              //anon_lookups,
              //root_id,
              //}) => {
              //let id = LookupId::Gpos(self.gpos.len());
              //assert_eq!(id, root_id); // sanity check
              //self.gpos.push(PositionLookup::Contextual(Lookup::new(
              //flags, mark_set, subtables,
              //)));
              //self.gpos.extend(anon_lookups);
              //id
              //}
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
            //match current {
            //SomeLookup::GsubLookup(lookup) => lookup.rule.add_subtable_break(),
            //SomeLookup::GposLookup(lookup) => lookup.rule.add_subtable_break(),
            //_ => (),
            //}
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
            //SomeLookup::GsubContextual(lookup) => lookup.root_id = new_id,
            //SomeLookup::GsubReverse(lookup) => lookup.root_id = new_id,
            //SomeLookup::GposContextual(lookup) => lookup.root_id = new_id,
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

        (gsub_builder.build().unwrap(), gpos_builder.build().unwrap())
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

impl<T: Default, U> ContextualLookup<T, U> {
    fn new(flags: LookupFlag, mark_set: Option<FilterSetId>) -> Self {
        ContextualLookup {
            flags,
            mark_set,
            anon_lookups: Vec::new(),
            subtables: vec![Default::default()],
            root_id: LookupId::Empty,
        }
    }
}

impl SomeLookup {
    fn new(kind: Kind, flags: LookupFlag, filter_set: Option<FilterSetId>) -> Self {
        //if kind == Kind::GsubType6 {
        //return SomeLookup::GsubContextual(ContextualLookup::new(flags, mark_filtering_set));
        //} else if kind == Kind::GsubType8 {
        //return SomeLookup::GsubReverse(ContextualLookup::new(flags, mark_filtering_set));
        //} else if kind == Kind::GposType8 {
        //return SomeLookup::GposContextual(ContextualLookup::new(flags, mark_filtering_set));
        //}
        if is_gpos_rule(kind) {
            SomeLookup::GposLookup(
                //flags,
                //mark_filtering_set,
                match kind {
                    Kind::GposType1 => PositionLookup::Single(Lookup::brand_new(flags, filter_set)),
                    Kind::GposType2 => PositionLookup::Pair(Lookup::brand_new(flags, filter_set)),
                    Kind::GposType3 => {
                        PositionLookup::Cursive(Lookup::brand_new(flags, filter_set))
                    }
                    Kind::GposType4 => {
                        PositionLookup::MarkToBase(Lookup::brand_new(flags, filter_set))
                    }
                    Kind::GposType5 => {
                        PositionLookup::MarkToLig(Lookup::brand_new(flags, filter_set))
                    }
                    Kind::GposType6 => {
                        PositionLookup::MarkToMark(Lookup::brand_new(flags, filter_set))
                    }
                    Kind::GposNode => unimplemented!("other gpos type?"),
                    other => panic!("illegal kind for lookup: '{}'", other),
                },
            )
        } else {
            SomeLookup::GsubLookup(
                //flags,
                //mark_filtering_set,
                match kind {
                    Kind::GsubType1 => {
                        SubstitutionLookup::Single(Lookup::brand_new(flags, filter_set))
                    }
                    Kind::GsubType2 => {
                        SubstitutionLookup::Multiple(Lookup::brand_new(flags, filter_set))
                    }
                    Kind::GsubType3 => {
                        SubstitutionLookup::Alternate(Lookup::brand_new(flags, filter_set))
                    }
                    Kind::GsubType4 => {
                        SubstitutionLookup::Ligature(Lookup::brand_new(flags, filter_set))
                    }
                    Kind::GsubType5 => {
                        SubstitutionLookup::Contextual(Lookup::brand_new(flags, filter_set))
                    }
                    Kind::GsubType7 => unimplemented!("extension"),
                    other => panic!("illegal kind for lookup: '{}'", other),
                },
            )
        }
    }

    fn kind(&self) -> Kind {
        match self {
            //SomeLookup::GsubContextual(_) => Kind::GsubType6,
            //SomeLookup::GsubReverse(_) => Kind::GsubType8,
            //SomeLookup::GposContextual(_) => Kind::GposType8,
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

    //pub(crate) fn add_gpos_type_8(
    //&mut self,
    //backtrack: Vec<BTreeSet<u16>>,
    //input: Vec<(BTreeSet<u16>, Vec<u16>)>,
    //lookahead: Vec<BTreeSet<u16>>,
    //) {
    //if let SomeLookup::GposContextual(lookup) = self {
    //let mut subtable = ChainedSequenceContext::default();
    //subtable.rules.push(ChainedSequenceRule {
    //backtrack,
    //input,
    //lookahead,
    //});
    //lookup.subtables.push(subtable);
    //} else {
    //panic!("lookup mismatch : '{}'", self.kind());
    //}
    //}

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

    pub(crate) fn add_gsub_type_6(
        &mut self,
        backtrack: Vec<BTreeSet<u16>>,
        input: Vec<(BTreeSet<u16>, Vec<u16>)>,
        lookahead: Vec<BTreeSet<u16>>,
    ) {
        //FIXME:
        //if let SomeLookup::GsubContextual(lookup) = self {
        //let mut subtable = ChainedSequenceContext::default();
        //subtable.rules.push(ChainedSequenceContextRule {
        //backtrack,
        //input,
        //lookahead,
        //});
        //lookup.subtables.push(subtable);
        //}
    }

    pub(crate) fn add_gsub_type_8(
        &mut self,
        backtrack: Vec<BTreeSet<u16>>,
        input: BTreeMap<u16, u16>,
        lookahead: Vec<BTreeSet<u16>>,
    ) {
        //FIXME
        //if let SomeLookup::GsubReverse(lookup) = self {
        //if lookup
        //.subtables
        //.last()
        //.map(|t| !t.mapping.is_empty())
        //.unwrap_or(true)
        //{
        //lookup.subtables.push(ReverseChainSubst::default());
        //}
        //let subtable = lookup.subtables.last_mut().unwrap();
        //subtable.mapping = input;
        //subtable.backtrack = backtrack;
        //subtable.lookahead = lookahead;
        //}
    }

    //pub(crate) fn as_gsub_type_6(
    //&mut self,
    //) -> &mut ContextualLookup<ChainedSequenceContext, SubstitutionLookup> {
    //if let SomeLookup::GsubContextual(table) = self {
    //table
    //} else {
    //panic!("lookup mismatch")
    //}
    //}

    //pub(crate) fn as_gpos_type_8(
    //&mut self,
    //) -> &mut ContextualLookup<ChainedSequenceContext, PositionLookup> {
    //if let SomeLookup::GposContextual(table) = self {
    //table
    //} else {
    //panic!("lookup mismatch")
    //}
    //}
}

//impl<T, U> ContextualLookup<T, U> {
//fn add_new_lookup_if_necessary(
//&mut self,
//check_fn: impl FnOnce(&U) -> bool,
//new_fn: impl FnOnce() -> U,
//) {
//if self
//.anon_lookups
//.last()
//.map(|lookup| check_fn(&lookup.rule))
//.unwrap_or(true)
//{
//let mut rule = new_fn();
//rule.loookup_flag = self.flags;
//rule.mark_filtering_set = self.mark_set;
//self.anon_lookups.push(rule);
//}
//}
//}

//impl<T> ContextualLookup<T, SubstitutionLookup> {
//fn current_anon_lookup_id(&self) -> LookupId {
//LookupId::Gsub(self.root_id.to_raw() + self.anon_lookups.len())
//}

//pub(crate) fn add_anon_gsub_type_1(
//&mut self,
//target: GlyphOrClass,
//replacement: GlyphOrClass,
//) -> LookupId {
//// do we need a new lookup or can we use the existing one?
//self.add_new_lookup_if_necessary(
//|existing| match existing {
//SubstitutionLookup::Single(subtables) => subtables
//.iter()
//.any(|sub| target.iter().any(|t| sub.mapping.contains_key(&t.to_raw()))),
//_ => true,
//},
//|| SubstitutionLookup::Single(vec![Default::default()]),
//);

//let lookup = self.anon_lookups.last_mut().unwrap();
//if let SubstitutionLookup::Single(subtables) = &mut lookup.rule {
//let sub = subtables.last_mut().unwrap();
//for (target, replacement) in target.iter().zip(replacement.into_iter_for_target()) {
//let r = sub.mapping.insert(target.to_raw(), replacement.to_raw());
//debug_assert!(r.is_none());
//}
//}
//self.current_anon_lookup_id()
//}

//pub(crate) fn add_anon_gsub_type_4(
//&mut self,
//target: Vec<u16>,
//replacement: GlyphId,
//) -> LookupId {
//// do we need a new lookup or can we use the existing one?
//self.add_new_lookup_if_necessary(
//|existing| match existing {
//SubstitutionLookup::Ligature(subtables) => subtables.iter().any(|sub| {
//sub.mapping
//.get(&target)
//.map(|existing| *existing != replacement.to_raw())
//.unwrap_or(false)
//}),
//_ => true,
//},
//|| SubstitutionLookup::Ligature(vec![Default::default()]),
//);

//let lookup = self.anon_lookups.last_mut().unwrap();
//if let SubstitutionLookup::Ligature(subtables) = &mut lookup.rule {
//let sub = subtables.last_mut().unwrap();
//sub.mapping.insert(target, replacement.to_raw());
//}
//self.current_anon_lookup_id()
//}
//}

//impl<T> ContextualLookup<T, PositionLookup> {
//fn current_anon_lookup_id(&self) -> LookupId {
//LookupId::Gpos(self.root_id.to_raw() + self.anon_lookups.len())
//}

//pub(crate) fn add_anon_gpos_type_1(
//&mut self,
//glyphs: &GlyphOrClass,
//value: ValueRecord,
//) -> LookupId {
//self.add_new_lookup_if_necessary(
//|existing| match existing {
//PositionLookup::Single(subtables) => subtables.iter().any(|t| {
//glyphs.iter().any(|gid| {
//t.mapping
//.get(&gid.to_raw())
//.map(|existing| existing != &value)
//.unwrap_or(false)
//})
//}),
//_ => true,
//},
//|| PositionLookup::Single(vec![Default::default()]),
//);

//let lookup = self.anon_lookups.last_mut().unwrap();
//if let PositionLookup::Single(subtables) = &mut lookup.rule {
//let sub = subtables.last_mut().unwrap();
//for id in glyphs.iter() {
//sub.mapping.insert(id.to_raw(), value.clone());
//}
//}
//self.current_anon_lookup_id()
//}
//}

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

        let lookups = self
            .lookups
            .into_iter()
            .map(|x| x.build().unwrap())
            .collect();
        Some((
            LookupList::new(lookups),
            ScriptList::new(scripts),
            FeatureList::new(features),
        ))
    }
}

impl Builder for PosSubBuilder<PositionLookup> {
    type Output = Option<write_gpos::Gpos>;

    fn build(self) -> Result<Self::Output, ()> {
        Ok(self
            .build_raw()
            .map(|(lookups, scripts, features)| write_gpos::Gpos::new(scripts, features, lookups)))
    }
}

impl Builder for PosSubBuilder<SubstitutionLookup> {
    type Output = Option<write_gsub::Gsub>;

    fn build(self) -> Result<Self::Output, ()> {
        Ok(self
            .build_raw()
            .map(|(lookups, scripts, features)| write_gsub::Gsub::new(scripts, features, lookups)))
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
