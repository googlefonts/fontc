//! general common utilities and types

use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
    io,
};

use write_fonts::{
    read::tables::layout::{FeatureList, ScriptList},
    tables::layout::LookupFlag,
    types::{GlyphId, Tag},
};

use crate::glyph_names::NameMap;

pub(crate) struct LanguageSystem {
    script: Tag,
    lang: Tag,
}

/// A trait for things that need a gid->name map to be printed
pub(crate) trait PrintNames {
    fn fmt_names(&self, f: &mut std::fmt::Formatter<'_>, names: &NameMap) -> std::fmt::Result;
}

/// A set of lookups for a specific feature and language system
pub(crate) struct Feature {
    pub(crate) feature: Tag,
    // script/lang; if a feature has identical lookups in multiple langsystems we combine them
    // (this is to reduce how much we print)
    pub(crate) lang_systems: Vec<LanguageSystem>,
    pub(crate) lookups: Vec<u16>,
}

/// A type to represent either one or multiple glyphs
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub(crate) enum GlyphSet {
    Single(GlyphId),
    Multiple(BTreeSet<GlyphId>),
}

/// A decomposed lookup.
///
/// This contains all the rules from all subtables in a lookup, normalized.
///
/// 'normalized' means that we don't care about the different subtable formats,
/// and that we do things like discard rules that occur in later subtables if
/// that rule would not be reached by a shaping implementation.
#[derive(Clone, Debug, Default)]
pub(crate) struct Lookup<Rule> {
    pub lookup_id: u16,
    flag: LookupFlag,
    filter_set: Option<u16>,
    rules: Vec<Rule>,
}

/// a single rule, carrying along info stored in its parent lookup
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SingleRule<'a, Rule> {
    pub rule: &'a Rule,
    pub lookup_id: u16,
    flag: LookupFlag,
    filter_set: Option<u16>,
}

impl LanguageSystem {
    fn sort_key(&self) -> impl Ord {
        (tag_to_int(self.script), tag_to_int(self.lang))
    }
}

impl Feature {
    fn sort_key(&self) -> impl Ord {
        (
            tag_to_int(self.feature),
            self.lang_systems
                .get(0)
                .map(|rec| (tag_to_int(rec.script), tag_to_int(rec.lang))),
        )
    }

    /// the header printed before each feature
    pub(crate) fn fmt_header(&self, f: &mut dyn io::Write) -> std::io::Result<()> {
        write!(f, "# {}: ", self.feature)?;
        let mut first = true;
        for LanguageSystem { script, lang } in &self.lang_systems {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{script}/{lang}")?;
        }
        write!(f, " #")
    }
}

// used in our sorting impls, so we always put DFLT/dflt above other tags
fn tag_to_int(tag: Tag) -> u32 {
    if tag == Tag::new(b"DFLT") {
        0
    } else if tag == Tag::new(b"dflt") {
        1
    } else {
        u32::from_be_bytes(tag.to_be_bytes())
    }
}

pub(crate) fn get_lang_systems(
    script_list: &ScriptList,
    feature_list: &FeatureList,
) -> Vec<Feature> {
    let data = script_list.offset_data();

    let mut group_identical_features = HashMap::new();
    for (feature, script, lang, lookups) in script_list
        .script_records()
        .iter()
        // first iterate all (script, lang, feature indices)
        .flat_map(|script| {
            let script_tag = script.script_tag();
            let script = script.script(data).unwrap();
            let maybe_default = script
                .default_lang_sys()
                .transpose()
                .unwrap()
                .map(|dflt| (script_tag, Tag::new(b"dflt"), dflt.feature_indices()));
            let lang_sys_iter = script.lang_sys_records().iter().map(move |lang_sys| {
                let lang_tag = lang_sys.lang_sys_tag();
                let lang = lang_sys.lang_sys(script.offset_data()).unwrap();
                (script_tag, lang_tag, lang.feature_indices())
            });
            maybe_default.into_iter().chain(lang_sys_iter)
        })
        // then convert these into script/lang/feature/lookup indices
        .flat_map(|(script, lang, indices)| {
            indices.iter().map(move |idx| {
                let rec = feature_list
                    .feature_records()
                    .get(idx.get() as usize)
                    .unwrap();
                let feature = rec.feature(feature_list.offset_data()).unwrap();
                let lookups = feature
                    .lookup_list_indices()
                    .iter()
                    .map(|x| x.get())
                    .collect();
                (rec.feature_tag(), script, lang, lookups)
            })
        })
    {
        // first for each combo of feature + set of lookups, collect which
        // language systems share it
        group_identical_features
            .entry((feature, lookups))
            .or_insert(Vec::new())
            .push(LanguageSystem { script, lang });
    }

    // then combine these into the feature types we return

    let mut result = group_identical_features
        .into_iter()
        .map(|((feature, lookups), lang_systems)| Feature {
            feature,
            lang_systems,
            lookups,
        })
        .collect::<Vec<_>>();
    // sort the list of language systems in a given feature
    result
        .iter_mut()
        .for_each(|sys| sys.lang_systems.sort_unstable_by_key(|sys| sys.sort_key()));
    // then sort the big list of features
    result.sort_unstable_by_key(|sys| sys.sort_key());

    result
}

impl GlyphSet {
    pub(crate) fn make_set(&mut self) {
        if let GlyphSet::Single(gid) = self {
            *self = GlyphSet::Multiple(BTreeSet::from([*gid]))
        }
    }

    pub(crate) fn combine(&mut self, other: GlyphSet) {
        self.make_set();
        let GlyphSet::Multiple(gids) = self else {
            unreachable!()
        };
        match other {
            GlyphSet::Single(gid) => {
                gids.insert(gid);
            }
            GlyphSet::Multiple(mut multi) => gids.append(&mut multi),
        }
    }

    pub(crate) fn add(&mut self, gid: GlyphId) {
        // if we're a single glyph, don't turn into a set if we're adding ourselves
        if matches!(self, GlyphSet::Single(x) if *x == gid) {
            return;
        }
        self.make_set();
        if let GlyphSet::Multiple(set) = self {
            set.insert(gid);
        }
    }

    #[cfg(test)]
    pub(crate) fn iter(&self) -> impl Iterator<Item = GlyphId> + '_ {
        let (left, right) = match self {
            GlyphSet::Single(gid) => (Some(*gid), None),
            GlyphSet::Multiple(gids) => (None, Some(gids.iter().copied())),
        };
        left.into_iter().chain(right.into_iter().flatten())
    }

    pub(crate) fn printer<'a>(&'a self, names: &'a NameMap) -> impl Display + 'a {
        // A helper for printing one or more glyphs
        struct GlyphPrinter<'a> {
            glyphs: &'a GlyphSet,
            names: &'a NameMap,
        }

        impl Display for GlyphPrinter<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self.glyphs {
                    GlyphSet::Single(single) => {
                        let name = self.names.get(*single);
                        f.write_str(name)
                    }
                    GlyphSet::Multiple(glyphs) => {
                        f.write_str("[")?;
                        let mut first = true;
                        for gid in glyphs {
                            let name = self.names.get(*gid);
                            if !first {
                                f.write_str(",")?;
                            }
                            f.write_str(name)?;
                            first = false;
                        }
                        f.write_str("]")
                    }
                }
            }
        }

        GlyphPrinter {
            glyphs: self,
            names,
        }
    }
}

impl<Rule> Lookup<Rule> {
    pub fn new(
        lookup_id: usize,
        rules: Vec<Rule>,
        flag: LookupFlag,
        filter_set: impl Into<Option<u16>>,
    ) -> Self {
        Lookup {
            lookup_id: lookup_id.try_into().unwrap(),
            flag,
            filter_set: filter_set.into(),
            rules,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = SingleRule<Rule>> + '_ {
        self.rules.iter().map(|rule| SingleRule {
            lookup_id: self.lookup_id,
            flag: self.flag,
            filter_set: self.filter_set,
            rule,
        })
    }
}

impl<T> SingleRule<'_, T> {
    pub fn lookup_flags(&self) -> (LookupFlag, Option<u16>) {
        (self.flag, self.filter_set)
    }
}

impl<T: PrintNames> SingleRule<'_, T> {
    pub fn printer<'a>(&'a self, names: &'a NameMap) -> impl std::fmt::Display + 'a {
        struct Printer<'a, T> {
            names: &'a NameMap,
            item: &'a T,
        }

        impl<T: PrintNames> std::fmt::Display for Printer<'_, T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.item.fmt_names(f, self.names)
            }
        }

        Printer {
            names,
            item: self.rule,
        }
    }
}

// if two rules are glyphwise equal, sort by lookup id
impl<T: Ord> Ord for SingleRule<'_, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.rule, self.lookup_id).cmp(&(other.rule, other.lookup_id))
    }
}

impl<T: Ord> PartialOrd for SingleRule<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl From<GlyphId> for GlyphSet {
    fn from(src: GlyphId) -> GlyphSet {
        GlyphSet::Single(src)
    }
}

impl FromIterator<GlyphId> for GlyphSet {
    fn from_iter<T: IntoIterator<Item = GlyphId>>(iter: T) -> Self {
        GlyphSet::Multiple(iter.into_iter().collect())
    }
}
