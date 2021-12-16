//! gsub/gpos lookup table stuff

use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    convert::TryInto,
};

use smol_str::SmolStr;

use fonttools::{
    layout::{
        common::{FeatureList, LanguageSystem, Lookup, LookupFlags, Script, ValueRecord, GPOSGSUB},
        contextual::{ChainedSequenceContext, ChainedSequenceContextRule},
        gpos4::MarkBasePos,
        gpos5::MarkLigPos,
        gpos6::MarkMarkPos,
        gsub8::ReverseChainSubst,
    },
    tables::{self, GPOS::Positioning, GSUB::Substitution},
    tag,
    types::Tag,
};

use crate::{
    types::{Anchor, GlyphId, GlyphOrClass},
    Kind,
};

pub(crate) type FilterSetId = u16;

const LANG_DFLT_TAG: Tag = tag!("dflt");
const SCRIPT_DFLT_TAG: Tag = tag!("DFLT");

#[derive(Clone, Debug, Default)]
pub(crate) struct AllLookups {
    current: Option<SomeLookup>,
    current_name: Option<SmolStr>,
    gpos: Vec<Lookup<Positioning>>,
    gsub: Vec<Lookup<Substitution>>,
    named: HashMap<SmolStr, LookupId>,
}

#[derive(Clone, Debug)]
pub(crate) enum SomeLookup {
    GsubLookup(Lookup<Substitution>),
    GposLookup(Lookup<Positioning>),
    GsubContextual(ContextualLookup<ChainedSequenceContext, Substitution>),
    GsubReverse(ContextualLookup<ReverseChainSubst, Substitution>),
    GposContextual(ContextualLookup<ChainedSequenceContext, Positioning>),
}

/// When building a contextual/chaining contextual rule, we also build a
/// bunch of anonymous lookups.
#[derive(Debug, Clone)]
pub(crate) struct ContextualLookup<T, U> {
    flags: LookupFlags,
    mark_set: Option<FilterSetId>,
    subtables: Vec<T>,
    anon_lookups: Vec<Lookup<U>>,
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
    lookups: Vec<Lookup<T>>,
    scripts: HashMap<Tag, HashMap<Tag, Vec<usize>>>,
    features: HashMap<(Tag, Vec<usize>), usize>,
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
            SomeLookup::GsubContextual(ContextualLookup {
                flags,
                mark_set,
                subtables,
                anon_lookups,
                root_id,
            }) => {
                let id = LookupId::Gsub(self.gsub.len());
                assert_eq!(id, root_id); // sanity check
                self.gsub.push(Lookup {
                    flags,
                    mark_filtering_set: mark_set,
                    rule: Substitution::ChainedContextual(subtables),
                });
                self.gsub.extend(anon_lookups);
                id
            }
            SomeLookup::GsubReverse(ContextualLookup {
                flags,
                mark_set,
                subtables,
                anon_lookups,
                root_id,
            }) => {
                let id = LookupId::Gsub(self.gsub.len());
                assert_eq!(id, root_id); // sanity check
                self.gsub.push(Lookup {
                    flags,
                    mark_filtering_set: mark_set,
                    rule: Substitution::ReverseChainContextual(subtables),
                });
                self.gsub.extend(anon_lookups);
                id
            }
            SomeLookup::GposContextual(ContextualLookup {
                flags,
                mark_set,
                subtables,
                anon_lookups,
                root_id,
            }) => {
                let id = LookupId::Gpos(self.gpos.len());
                assert_eq!(id, root_id); // sanity check
                self.gpos.push(Lookup {
                    flags,
                    mark_filtering_set: mark_set,
                    rule: Positioning::ChainedContextual(subtables),
                });
                self.gpos.extend(anon_lookups);
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
            match current {
                SomeLookup::GsubLookup(lookup) => lookup.rule.add_subtable_break(),
                SomeLookup::GposLookup(lookup) => lookup.rule.add_subtable_break(),
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
        flags: LookupFlags,
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
            SomeLookup::GsubReverse(lookup) => lookup.root_id = new_id,
            SomeLookup::GposContextual(lookup) => lookup.root_id = new_id,
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

    pub(crate) fn build(
        &self,
        features: &BTreeMap<FeatureKey, Vec<LookupId>>,
    ) -> (Option<tables::GSUB::GSUB>, Option<tables::GPOS::GPOS>) {
        let mut gpos_builder = PosSubBuilder::new(self.gpos.clone());
        let mut gsub_builder = PosSubBuilder::new(self.gsub.clone());

        for (key, feature_indices) in features {
            assert!(crate::util::is_sorted(feature_indices));
            let split_idx = feature_indices
                .iter()
                .position(|x| matches!(x, LookupId::Gsub(_)))
                .unwrap_or_else(|| feature_indices.len());

            if key.feature.as_str() == "size" {
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

impl<T: Default, U> ContextualLookup<T, U> {
    fn new(flags: LookupFlags, mark_set: Option<FilterSetId>) -> Self {
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
    fn new(kind: Kind, flags: LookupFlags, mark_filtering_set: Option<FilterSetId>) -> Self {
        if kind == Kind::GsubType6 {
            return SomeLookup::GsubContextual(ContextualLookup::new(flags, mark_filtering_set));
        } else if kind == Kind::GsubType8 {
            return SomeLookup::GsubReverse(ContextualLookup::new(flags, mark_filtering_set));
        } else if kind == Kind::GposType8 {
            return SomeLookup::GposContextual(ContextualLookup::new(flags, mark_filtering_set));
        }
        if is_gpos_rule(kind) {
            SomeLookup::GposLookup(Lookup {
                flags,
                mark_filtering_set,
                rule: match kind {
                    Kind::GposType1 => Positioning::Single(vec![Default::default()]),
                    Kind::GposType2 => Positioning::Pair(vec![Default::default()]),
                    Kind::GposType3 => Positioning::Cursive(vec![Default::default()]),
                    Kind::GposType4 => Positioning::MarkToBase(vec![Default::default()]),
                    Kind::GposType5 => Positioning::MarkToLig(vec![Default::default()]),
                    Kind::GposType6 => Positioning::MarkToMark(vec![Default::default()]),
                    Kind::GposNode => unimplemented!("other gpos type?"),
                    other => panic!("illegal kind for lookup: '{}'", other),
                },
            })
        } else {
            SomeLookup::GsubLookup(Lookup {
                flags,
                mark_filtering_set,
                rule: match kind {
                    Kind::GsubType1 => Substitution::Single(vec![Default::default()]),
                    Kind::GsubType2 => Substitution::Multiple(vec![Default::default()]),
                    Kind::GsubType3 => Substitution::Alternate(vec![Default::default()]),
                    Kind::GsubType4 => Substitution::Ligature(vec![Default::default()]),
                    Kind::GsubType5 => Substitution::Contextual(vec![Default::default()]),
                    Kind::GsubType7 => unimplemented!("extension"),
                    other => panic!("illegal kind for lookup: '{}'", other),
                },
            })
        }
    }

    fn kind(&self) -> Kind {
        match self {
            SomeLookup::GsubContextual(_) => Kind::GsubType6,
            SomeLookup::GsubReverse(_) => Kind::GsubType8,
            SomeLookup::GposContextual(_) => Kind::GposType8,
            SomeLookup::GsubLookup(gsub) => match gsub.rule {
                Substitution::Single(_) => Kind::GsubType1,
                Substitution::Multiple(_) => Kind::GsubType2,
                Substitution::Alternate(_) => Kind::GsubType3,
                Substitution::Ligature(_) => Kind::GsubType4,
                Substitution::ReverseChainContextual(_) => Kind::GsubType8,
                _ => panic!("unhandled table kind"),
            },
            SomeLookup::GposLookup(gpos) => match gpos.rule {
                Positioning::Single(_) => Kind::GposType1,
                Positioning::Pair(_) => Kind::GposType2,
                Positioning::Cursive(_) => Kind::GposType3,
                Positioning::MarkToBase(_) => Kind::GposType4,
                Positioning::MarkToLig(_) => Kind::GposType5,
                Positioning::MarkToMark(_) => Kind::GposType6,
                Positioning::Contextual(_) => Kind::GposType7,
                Positioning::ChainedContextual(_) => Kind::GposType8,
                //FIXME: should be a kind? idk
                //Positioning::Extension => Kind::GposNode,
            },
        }
    }

    pub(crate) fn add_gpos_type_1(&mut self, id: GlyphId, record: ValueRecord) {
        if let SomeLookup::GposLookup(Lookup {
            rule: Positioning::Single(table),
            ..
        }) = self
        {
            let subtable = table.last_mut().unwrap();
            subtable.mapping.insert(id.to_raw(), record);
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
        if let SomeLookup::GposLookup(Lookup {
            rule: Positioning::Pair(table),
            ..
        }) = self
        {
            let subtable = table.last_mut().unwrap();
            subtable
                .mapping
                .insert((one.to_raw(), two.to_raw()), (val_one, val_two));
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gpos_type_3(&mut self, id: GlyphId, entry: Anchor, exit: Anchor) {
        if let SomeLookup::GposLookup(Lookup {
            rule: Positioning::Cursive(table),
            ..
        }) = self
        {
            let subtable = table.last_mut().unwrap();
            subtable
                .mapping
                .insert(id.to_raw(), (entry.to_raw(), exit.to_raw()));
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn with_gpos_type_4<R>(&mut self, f: impl FnOnce(&mut MarkBasePos) -> R) -> R {
        if let SomeLookup::GposLookup(Lookup {
            rule: Positioning::MarkToBase(table),
            ..
        }) = self
        {
            let subtable = table.last_mut().unwrap();
            f(subtable)
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn with_gpos_type_5<R>(&mut self, f: impl FnOnce(&mut MarkLigPos) -> R) -> R {
        if let SomeLookup::GposLookup(Lookup {
            rule: Positioning::MarkToLig(table),
            ..
        }) = self
        {
            let subtable = table.last_mut().unwrap();
            f(subtable)
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn with_gpos_type_6<R>(&mut self, f: impl FnOnce(&mut MarkMarkPos) -> R) -> R {
        if let SomeLookup::GposLookup(Lookup {
            rule: Positioning::MarkToMark(table),
            ..
        }) = self
        {
            let subtable = table.last_mut().unwrap();
            f(subtable)
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gpos_type_8(
        &mut self,
        backtrack: Vec<BTreeSet<u16>>,
        input: Vec<(BTreeSet<u16>, Vec<u16>)>,
        lookahead: Vec<BTreeSet<u16>>,
    ) {
        if let SomeLookup::GposContextual(lookup) = self {
            let mut subtable = ChainedSequenceContext::default();
            subtable.rules.push(ChainedSequenceContextRule {
                backtrack,
                input,
                lookahead,
            });
            lookup.subtables.push(subtable);
        } else {
            panic!("lookup mismatch : '{}'", self.kind());
        }
    }

    pub(crate) fn add_gsub_type_1(&mut self, id: GlyphId, replacement: GlyphId) {
        if let SomeLookup::GsubLookup(Lookup {
            rule: Substitution::Single(table),
            ..
        }) = self
        {
            let subtable = table.last_mut().unwrap();
            subtable.mapping.insert(id.to_raw(), replacement.to_raw());
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gsub_type_2(&mut self, id: GlyphId, replacement: Vec<u16>) {
        if let SomeLookup::GsubLookup(Lookup {
            rule: Substitution::Multiple(table),
            ..
        }) = self
        {
            let subtable = table.last_mut().unwrap();
            subtable.mapping.insert(id.to_raw(), replacement);
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gsub_type_3(&mut self, id: GlyphId, alternates: Vec<u16>) {
        if let SomeLookup::GsubLookup(Lookup {
            rule: Substitution::Alternate(table),
            ..
        }) = self
        {
            let subtable = table.last_mut().unwrap();
            subtable.mapping.insert(id.to_raw(), alternates);
        } else {
            panic!("lookup mismatch");
        }
    }

    pub(crate) fn add_gsub_type_4(&mut self, target: Vec<u16>, replacement: GlyphId) {
        if let SomeLookup::GsubLookup(Lookup {
            rule: Substitution::Ligature(table),
            ..
        }) = self
        {
            let subtable = table.last_mut().unwrap();
            subtable.mapping.insert(target, replacement.to_raw());
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
        if let SomeLookup::GsubContextual(lookup) = self {
            let mut subtable = ChainedSequenceContext::default();
            subtable.rules.push(ChainedSequenceContextRule {
                backtrack,
                input,
                lookahead,
            });
            lookup.subtables.push(subtable);
        }
    }

    pub(crate) fn add_gsub_type_8(
        &mut self,
        _backtrack: Vec<BTreeSet<u16>>,
        input: BTreeMap<u16, u16>,
        _lookahead: Vec<BTreeSet<u16>>,
    ) {
        //FIXME: use lookahead/backtrack
        if let SomeLookup::GsubReverse(lookup) = self {
            if lookup
                .subtables
                .last()
                .map(|t| !t.mapping.is_empty())
                .unwrap_or(true)
            {
                lookup.subtables.push(ReverseChainSubst::default());
            }
            lookup.subtables.last_mut().unwrap().mapping = input;
        }
    }

    pub(crate) fn as_gsub_type_6(
        &mut self,
    ) -> &mut ContextualLookup<ChainedSequenceContext, Substitution> {
        if let SomeLookup::GsubContextual(table) = self {
            table
        } else {
            panic!("lookup mismatch")
        }
    }

    pub(crate) fn as_gpos_type_8(
        &mut self,
    ) -> &mut ContextualLookup<ChainedSequenceContext, Positioning> {
        if let SomeLookup::GposContextual(table) = self {
            table
        } else {
            panic!("lookup mismatch")
        }
    }
}

impl<T, U> ContextualLookup<T, U> {
    fn add_new_lookup_if_necessary(
        &mut self,
        check_fn: impl FnOnce(&U) -> bool,
        new_fn: impl FnOnce() -> U,
    ) {
        if self
            .anon_lookups
            .last()
            .map(|lookup| check_fn(&lookup.rule))
            .unwrap_or(true)
        {
            self.anon_lookups.push(Lookup {
                flags: self.flags,
                mark_filtering_set: self.mark_set,
                rule: new_fn(),
            });
        }
    }
}

impl<T> ContextualLookup<T, Substitution> {
    fn current_anon_lookup_id(&self) -> LookupId {
        LookupId::Gsub(self.root_id.to_raw() + self.anon_lookups.len())
    }

    pub(crate) fn add_anon_gsub_type_1(
        &mut self,
        target: GlyphOrClass,
        replacement: GlyphOrClass,
    ) -> LookupId {
        // do we need a new lookup or can we use the existing one?
        self.add_new_lookup_if_necessary(
            |existing| match existing {
                Substitution::Single(subtables) => subtables
                    .iter()
                    .any(|sub| target.iter().any(|t| sub.mapping.contains_key(&t.to_raw()))),
                _ => true,
            },
            || Substitution::Single(vec![Default::default()]),
        );

        let lookup = self.anon_lookups.last_mut().unwrap();
        if let Substitution::Single(subtables) = &mut lookup.rule {
            let sub = subtables.last_mut().unwrap();
            for (target, replacement) in target.iter().zip(replacement.into_iter_for_target()) {
                let r = sub.mapping.insert(target.to_raw(), replacement.to_raw());
                debug_assert!(r.is_none());
            }
        }
        self.current_anon_lookup_id()
    }

    pub(crate) fn add_anon_gsub_type_4(
        &mut self,
        target: Vec<u16>,
        replacement: GlyphId,
    ) -> LookupId {
        // do we need a new lookup or can we use the existing one?
        self.add_new_lookup_if_necessary(
            |existing| match existing {
                Substitution::Ligature(subtables) => subtables.iter().any(|sub| {
                    sub.mapping
                        .get(&target)
                        .map(|existing| *existing != replacement.to_raw())
                        .unwrap_or(false)
                }),
                _ => true,
            },
            || Substitution::Ligature(vec![Default::default()]),
        );

        let lookup = self.anon_lookups.last_mut().unwrap();
        if let Substitution::Ligature(subtables) = &mut lookup.rule {
            let sub = subtables.last_mut().unwrap();
            sub.mapping.insert(target, replacement.to_raw());
        }
        self.current_anon_lookup_id()
    }
}

impl<T> ContextualLookup<T, Positioning> {
    fn current_anon_lookup_id(&self) -> LookupId {
        LookupId::Gpos(self.root_id.to_raw() + self.anon_lookups.len())
    }

    pub(crate) fn add_anon_gpos_type_1(
        &mut self,
        glyphs: &GlyphOrClass,
        value: ValueRecord,
    ) -> LookupId {
        self.add_new_lookup_if_necessary(
            |existing| match existing {
                Positioning::Single(subtables) => subtables.iter().any(|t| {
                    glyphs.iter().any(|gid| {
                        t.mapping
                            .get(&gid.to_raw())
                            .map(|existing| existing != &value)
                            .unwrap_or(false)
                    })
                }),
                _ => true,
            },
            || Positioning::Single(vec![Default::default()]),
        );

        let lookup = self.anon_lookups.last_mut().unwrap();
        if let Positioning::Single(subtables) = &mut lookup.rule {
            let sub = subtables.last_mut().unwrap();
            for id in glyphs.iter() {
                sub.mapping.insert(id.to_raw(), value.clone());
            }
        }
        self.current_anon_lookup_id()
    }
}

impl<T> PosSubBuilder<T> {
    fn new(lookups: Vec<Lookup<T>>) -> Self {
        PosSubBuilder {
            lookups,
            scripts: Default::default(),
            features: Default::default(),
        }
    }

    fn add(&mut self, key: FeatureKey, lookups: &[LookupId]) {
        let lookups = lookups.iter().map(|idx| idx.to_raw()).collect();
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
            .push(idx);
    }

    fn build(self) -> Option<GPOSGSUB<T>> {
        if self.lookups.is_empty() && self.features.is_empty() {
            return None;
        }

        let mut features = Vec::with_capacity(self.features.len());
        // push empty items so we can insert by index
        for _ in 0..self.features.len() {
            features.push((LANG_DFLT_TAG, Vec::new(), None));
        }
        for ((tag, lookups), idx) in self.features {
            features[idx] = (tag, lookups, None);
        }

        let mut result = GPOSGSUB {
            lookups: self.lookups,
            scripts: Default::default(),
            features: FeatureList::new(features),
        };

        for (script, entry) in self.scripts.into_iter() {
            let mut script_record = Script::default();
            for (lang, feature_indices) in entry {
                let ls = LanguageSystem {
                    required_feature: None, // XXX
                    feature_indices,
                };
                if lang == LANG_DFLT_TAG {
                    script_record.default_language_system = Some(ls);
                } else {
                    script_record.language_systems.insert(lang, ls);
                }
            }
            result.scripts.scripts.insert(script, script_record);
        }

        Some(result)
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
