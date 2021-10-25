//! gsub/gpos lookup table stuff

use std::collections::{BTreeMap, HashMap};

use smol_str::SmolStr;

use fonttools::{
    layout::{
        common::{LanguageSystem, Lookup, LookupFlags, Script, GPOSGSUB},
        valuerecord::ValueRecord,
    },
    tables::{self, GPOS::Positioning, GSUB::Substitution},
    tag,
    types::Tag,
};

use crate::{types::GlyphId, Kind};

pub(crate) type FilterSetId = u16;

const LANG_DFLT_TAG: Tag = tag!("dflt");
const SCRIPT_DFLT_TAG: Tag = tag!("DFLT");

#[derive(Debug, Default)]
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub(crate) enum LookupId {
    Gpos(usize),
    Gsub(usize),
    /// Used when a named lookup block has no rules.
    ///
    /// We parse this, but then discard it immediately whenever it is refernced.
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
        }
    }

    pub(crate) fn get_named(&self, name: &str) -> Option<LookupId> {
        self.named.get(name).copied()
    }

    pub(crate) fn current_mut(&mut self) -> Option<&mut SomeLookup> {
        self.current.as_mut()
    }

    pub(crate) fn is_named(&self) -> bool {
        self.current_name.is_some()
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
                SomeLookup::GsubLookup(lookup) => lookup.add_subtable_break(),
                SomeLookup::GposLookup(lookup) => lookup.add_subtable_break(),
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
        //assert!(self.current_name.is_none(), "named lookup not finished");
        let finished_id = self.current.take().map(|lookup| self.push(lookup));
        self.current = Some(SomeLookup::new(kind, flags, mark_set));
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
            let split_idx = feature_indices
                .iter()
                .position(|x| matches!(x, LookupId::Gsub(_)))
                .unwrap_or_else(|| feature_indices.len());

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
}

impl SomeLookup {
    fn new(kind: Kind, flags: LookupFlags, mark_filtering_set: Option<FilterSetId>) -> Self {
        if is_gpos_rule(kind) {
            SomeLookup::GposLookup(Lookup {
                flags,
                mark_filtering_set,
                rule: match kind {
                    Kind::GposType1 => Positioning::Single(vec![Default::default()]),
                    Kind::GposType2 => Positioning::Pair(vec![Default::default()]),
                    Kind::GposType3 => Positioning::Cursive(vec![Default::default()]),
                    Kind::GposType4 => Positioning::MarkToBase(vec![Default::default()]),
                    Kind::GposType5 => Positioning::MarkToLig,
                    Kind::GposType6 => Positioning::MarkToMark,
                    Kind::GposType7 => Positioning::Contextual(vec![Default::default()]),
                    Kind::GposType8 => Positioning::ChainedContextual(vec![Default::default()]),
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
                    Kind::GsubType6 => Substitution::ChainedContextual(vec![Default::default()]),
                    Kind::GsubType7 => unimplemented!("extension"),
                    Kind::GsubType8 => unimplemented!("reverse chaining"),
                    other => panic!("illegal kind for lookup: '{}'", other),
                },
            })
        }
    }

    fn kind(&self) -> Kind {
        match self {
            SomeLookup::GsubLookup(gsub) => match gsub.rule {
                Substitution::Single(_) => Kind::GsubType1,
                Substitution::Multiple(_) => Kind::GsubType2,
                Substitution::Alternate(_) => Kind::GsubType3,
                Substitution::Ligature(_) => Kind::GsubType4,
                Substitution::Contextual(_) => Kind::GsubType5,
                Substitution::ChainedContextual(_) => Kind::GsubType6,
                Substitution::Extension => Kind::GsubType7,
                Substitution::ReverseChaining => Kind::GsubType8,
            },
            SomeLookup::GposLookup(gpos) => match gpos.rule {
                Positioning::Single(_) => Kind::GposType1,
                Positioning::Pair(_) => Kind::GposType2,
                Positioning::Cursive(_) => Kind::GposType3,
                Positioning::MarkToBase(_) => Kind::GposType4,
                Positioning::MarkToLig => Kind::GposType5,
                Positioning::MarkToMark => Kind::GposType6,
                Positioning::Contextual(_) => Kind::GposType7,
                Positioning::ChainedContextual(_) => Kind::GposType8,
                //FIXME: should be a kind? idk
                Positioning::Extension => Kind::GposNode,
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
        if self.lookups.is_empty() {
            return None;
        }

        let mut result = GPOSGSUB {
            lookups: self.lookups,
            scripts: Default::default(),
            features: Vec::with_capacity(self.features.len()),
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

        // push empty items so we can insert by index
        for _ in 0..self.features.len() {
            result.features.push((LANG_DFLT_TAG, Vec::new(), None));
        }
        for ((tag, lookups), idx) in self.features {
            result.features[idx] = (tag, lookups, None);
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
