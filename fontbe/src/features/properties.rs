//! Properties and constants related to unicode data
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    hash::Hash,
};

use fontdrasil::unicode18;
use fontir::ir::{GlyphOrder, StaticMetadata};
use icu_properties::{
    PropertyParser,
    props::{BidiClass, Script},
};
use tinystr::tinystr;
use write_fonts::{
    read::{ReadError, collections::IntSet, tables::gsub::Gsub},
    types::{GlyphId, GlyphId16, Tag},
};

use crate::features::ot_tags::{NEW_SCRIPTS, SCRIPT_ALIASES, SCRIPT_EXCEPTIONS_REVERSED};

use super::ot_tags::{DFLT_SCRIPT, INDIC_SCRIPTS, NEW_SCRIPT_TAGS, SCRIPT_EXCEPTIONS, USE_SCRIPTS};

pub const COMMON_SCRIPT: UnicodeShortName = tinystr!(4, "Zyyy");
pub const INHERITED_SCRIPT: UnicodeShortName = tinystr!(4, "Zinh");

pub const HIRA: UnicodeShortName = tinystr!(4, "Hira");
pub const KANA: UnicodeShortName = tinystr!(4, "Kana");
pub const HRKT: UnicodeShortName = tinystr!(4, "Hrkt");

pub use fontdrasil::unicode18::UnicodeShortName;

/// The writing direction of a script
#[derive(Clone, Debug, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScriptDirection {
    /// any direction, for the 'common' script
    Auto,
    LeftToRight,
    RightToLeft,
}

/// A trait for mapping glyph ids to unicode values
///
/// This lets us write functions that don't need to know the concrete types we're
/// using, which is an implementation detail. It also makes it easier for us to
/// write tests.
pub trait CharMap {
    /// Iterate over all the defined (gid, unicode value) pairs.
    ///
    /// Note that a single glyph may appear multiple times, with different
    /// unicode values.
    fn iter_glyphs(&self) -> impl Iterator<Item = (GlyphId16, u32)>;
}

impl CharMap for HashMap<u32, GlyphId16> {
    fn iter_glyphs(&self) -> impl Iterator<Item = (GlyphId16, u32)> {
        self.iter().map(|(k, v)| (*v, *k))
    }
}

impl ScriptDirection {
    /// Returns the writing direction for the provided script
    // <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b340c/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L63>
    pub(crate) fn for_script(script: &UnicodeShortName) -> Self {
        match script.as_str() {
            // this list from
            // <https://github.com/fonttools/fonttools/blob/8697f91cdc/Lib/fontTools/unicodedata/__init__.py#L141>
            "Zyyy" => ScriptDirection::Auto,
            "Arab" | "Hebr" | "Syrc" | "Thaa" | "Cprt" | "Khar" | "Phnx" | "Nkoo" | "Lydi"
            | "Avst" | "Armi" | "Phli" | "Prti" | "Sarb" | "Orkh" | "Samr" | "Mand" | "Merc"
            | "Mero" | "Mani" | "Mend" | "Nbat" | "Narb" | "Palm" | "Phlp" | "Hatr" | "Hung"
            | "Adlm" | "Rohg" | "Sogo" | "Sogd" | "Elym" | "Chrs" | "Yezi" | "Ougr" => {
                ScriptDirection::RightToLeft
            }
            _ => ScriptDirection::LeftToRight,
        }
    }

    /// true if either side is auto, or both sides are equal
    pub(crate) fn plays_nicely_with(&self, other: &ScriptDirection) -> bool {
        matches!(
            (self, other),
            (ScriptDirection::Auto, _)
                | (_, ScriptDirection::Auto)
                | (ScriptDirection::LeftToRight, ScriptDirection::LeftToRight)
                | (ScriptDirection::RightToLeft, ScriptDirection::RightToLeft)
        )
    }
}

// <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b340c/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L49>
/// returns none for neutral characters
fn unicode_bidi_type(c: u32) -> Option<BidiClass> {
    match unicode18::bidi_class(c) {
        BidiClass::RightToLeft | BidiClass::ArabicLetter => Some(BidiClass::RightToLeft),
        BidiClass::LeftToRight | BidiClass::ArabicNumber | BidiClass::EuropeanNumber => {
            Some(BidiClass::LeftToRight)
        }
        _ => None,
    }
}

/// The glyphs that rules can substitute for each glyph.
///
/// This is ufo2ft's `extraSubstitutions`.
pub(crate) type ExtraSubstitutions = HashMap<GlyphId16, IntSet<GlyphId16>>;

/// Collect the substitutions made by the font's feature variation rules.
///
/// These come from designspace rules and Glyphs bracket layers. Substitutions
/// that name a glyph missing from the glyph order are skipped.
pub(crate) fn extra_substitutions(
    static_metadata: &StaticMetadata,
    glyph_order: &GlyphOrder,
) -> ExtraSubstitutions {
    let mut result = ExtraSubstitutions::new();
    let substitutions = static_metadata
        .variations
        .iter()
        .flat_map(|variations| &variations.rules)
        .flat_map(|rule| &rule.substitutions);
    for sub in substitutions {
        if let (Some(replace), Some(with)) = (
            glyph_order.glyph_id(&sub.replace),
            glyph_order.glyph_id(&sub.with),
        ) {
            result.entry(replace).or_default().insert(with);
        }
    }
    result
}

// equivalent to the 'classify' method in ufo2ft:
// <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfcf0b1c0f/Lib/ufo2ft/util.py#L287>
fn classify<T, F, CM>(
    char_map: &CM,
    mut props_fn: F,
    gsub: Option<&Gsub>,
    extra_substitutions: &ExtraSubstitutions,
) -> Result<BTreeMap<T, IntSet<GlyphId16>>, ReadError>
where
    T: Ord + Eq,
    // instead of returning an iterator, pushes items into the provided buffer
    F: FnMut(u32, &mut Vec<T>),
    CM: CharMap,
{
    let mut sets = BTreeMap::new();
    let mut neutral_glyphs = IntSet::new();
    let mut buf = Vec::new();
    for (gid, unicode_value) in char_map.iter_glyphs() {
        let mut has_props = false;
        props_fn(unicode_value, &mut buf);
        for prop in buf.drain(..) {
            sets.entry(prop).or_insert(IntSet::new()).insert(gid);
            has_props = true;
        }
        if !has_props {
            neutral_glyphs.insert(gid.into());
        }
    }

    if let Some(gsub) = gsub.as_ref() {
        let initial_lookups = gsub.collect_lookups(&IntSet::all())?;
        gsub.closure_glyphs(&initial_lookups, &mut neutral_glyphs)?;
        for glyphs in sets.values_mut() {
            let mut temp: IntSet<GlyphId> = glyphs.iter().map(|g| g.into()).collect();
            temp.union(&neutral_glyphs);
            gsub.closure_glyphs(&initial_lookups, &mut temp)?;
            glyphs.extend(
                temp.iter()
                    .filter(|gid| !neutral_glyphs.contains(*gid))
                    .map(|g| g.try_into().unwrap()),
            );
        }
    }

    for glyphs in sets.values_mut() {
        let mut to_append = IntSet::new();
        for glyph in glyphs.iter() {
            if let Some(substitutes) = extra_substitutions.get(&glyph) {
                to_append.union(substitutes);
            }
        }
        glyphs.union(&to_append);
    }
    Ok(sets)
}

pub(crate) fn glyphs_matching_predicate(
    glyphs: &impl CharMap,
    predicate: impl Fn(u32) -> Option<bool>,
    gsub: Option<&Gsub>,
    extra_substitutions: &ExtraSubstitutions,
) -> Result<IntSet<GlyphId16>, ReadError> {
    classify(
        glyphs,
        |cp, buf| {
            if let Some(val) = predicate(cp) {
                buf.push(val)
            }
        },
        gsub,
        extra_substitutions,
    )
    .map(|mut items| items.remove(&true).unwrap_or_default())
}

// the specific logic from
// https://github.com/googlefonts/ufo2ft/blob/01d3faee/Lib/ufo2ft/util.py#L591
pub fn unicode_script_extensions(cp: u32) -> impl Iterator<Item = UnicodeShortName> {
    let mut seen_hrkt = false;
    unicode18::script_extensions(cp)
        .into_iter()
        .filter_map(move |script| {
            if script == HIRA || script == KANA {
                if seen_hrkt {
                    None
                } else {
                    seen_hrkt = true;
                    Some(HRKT)
                }
            } else {
                Some(script)
            }
        })
}

/// Returns a map of gids to their scripts.
///
/// NOTE:
/// This precisely matches the logic in the ufo2ft kernFeatureWriter (<https://github.com/googlefonts/ufo2ft/blob/01d3faee/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L632>)
///
/// and may not be suited to other purposes
pub(crate) fn scripts_by_glyph(
    glyphs: &impl CharMap,
    known_scripts: &HashSet<UnicodeShortName>,
    gsub: Option<&Gsub>,
    extra_substitutions: &ExtraSubstitutions,
) -> Result<HashMap<GlyphId16, HashSet<UnicodeShortName>>, ReadError> {
    let mut result = HashMap::new();
    for (script, glyphs) in classify(
        glyphs,
        |cp, buf| {
            if known_scripts.is_empty() {
                buf.push(COMMON_SCRIPT);
            } else {
                buf.extend(unicode_script_extensions(cp).filter(|script| {
                    *script == COMMON_SCRIPT
                        || *script == INHERITED_SCRIPT
                        || known_scripts.contains(script)
                }));
            }
        },
        gsub,
        extra_substitutions,
    )? {
        for glyph in glyphs.iter() {
            result.entry(glyph).or_insert(HashSet::new()).insert(script);
        }
    }
    Ok(result)
}

/// A map of bidi class to glyphs in that class.
pub(crate) fn glyphs_by_bidi_class(
    glyphs: &impl CharMap,
    gsub: Option<&Gsub>,
    extra_substitutions: &ExtraSubstitutions,
) -> Result<BTreeMap<BidiClass, IntSet<GlyphId16>>, ReadError> {
    classify(
        glyphs,
        |codepoint, buf| buf.extend(unicode_bidi_type(codepoint)),
        gsub,
        extra_substitutions,
    )
}

/// Returns a map of script directions to glyphs with that property.
pub(crate) fn glyphs_by_script_direction(
    glyphs: &impl CharMap,
    gsub: Option<&Gsub>,
    extra_substitutions: &ExtraSubstitutions,
) -> Result<BTreeMap<ScriptDirection, IntSet<GlyphId16>>, ReadError> {
    classify(
        glyphs,
        |cp, buf| buf.extend(unicode_script_direction(cp)),
        gsub,
        extra_substitutions,
    )
}

// trying to match the logic in:
// - https://github.com/googlefonts/ufo2ft/blob/98e8916a8/Lib/ufo2ft/util.py#L373
// which calls,
// - https://github.com/fonttools/fonttools/blob/f15001e7f1/Lib/fontTools/unicodedata/__init__.py#L219
fn unicode_script_direction(cp: u32) -> Option<ScriptDirection> {
    let sc = script_for_codepoint(cp)?;
    if [COMMON_SCRIPT, INHERITED_SCRIPT].contains(&sc) {
        return None;
    }
    if ScriptDirection::for_script(&sc) == ScriptDirection::RightToLeft {
        Some(ScriptDirection::RightToLeft)
    } else {
        Some(ScriptDirection::LeftToRight)
    }
}

pub(crate) fn dist_feature_enabled_scripts() -> HashSet<UnicodeShortName> {
    INDIC_SCRIPTS
        .iter()
        .chain(USE_SCRIPTS)
        .chain(["Khmr", "Mymr"].iter())
        .map(|s| UnicodeShortName::try_from_str(s).unwrap())
        .collect()
}

// a little helper trait to handle binary searching an array of 2-tuples where
// the first item is a key and the second a value
trait BinarySearchExact<T, U> {
    fn binary_search_exact(&self, needle: &T) -> Option<U>;
}

impl<T: Ord + Eq, U: Clone> BinarySearchExact<T, U> for &[(T, U)] {
    fn binary_search_exact(&self, needle: &T) -> Option<U> {
        self.binary_search_by(|probe| probe.0.cmp(needle))
            .ok()
            .map(|idx| &self[idx].1)
            .cloned()
    }
}

/// Get the unicode script property for this code point
fn script_for_codepoint(cp: u32) -> Option<UnicodeShortName> {
    unicode18::script(cp)
}

/// Takes an OpenType script tag and returns a unicode script identifier
///
/// <https://github.com/fonttools/fonttools/blob/a7a0f41c90c0d/Lib/fontTools/unicodedata/__init__.py#L261>
pub(crate) fn ot_tag_to_script(script_tag: Tag) -> Option<UnicodeShortName> {
    const DFLT: Tag = Tag::new(b"DFLT");
    if script_tag == DFLT {
        return None;
    }

    let tag = SCRIPT_ALIASES
        .binary_search_exact(&script_tag)
        .unwrap_or(script_tag);

    if let Some(exception) = SCRIPT_EXCEPTIONS_REVERSED
        .binary_search_exact(&tag)
        .or_else(|| NEW_SCRIPTS.binary_search_exact(&tag))
    {
        return Some(UnicodeShortName::try_from_str(exception).unwrap());
    }

    // finally, algorithmic conversion
    Some(ot_tag_to_unicode_short_name(tag))
}

// first char is uppercased; any trailing spaces are replaced with last non-space letter
fn ot_tag_to_unicode_short_name(tag: Tag) -> UnicodeShortName {
    const SPACE: u8 = b' ';

    let tag_bytes = tag.into_bytes();
    let mut out = [b'\0'; 4];
    out[0] = tag_bytes[0].to_ascii_uppercase();
    let mut last_non_space = tag_bytes[1];
    for i in 1..=3 {
        if tag_bytes[i] != SPACE {
            out[i] = tag_bytes[i];
            last_non_space = tag_bytes[i];
        } else {
            out[i] = last_non_space;
        }
    }

    UnicodeShortName::try_from_raw(out).expect("cannot fail, as tag cannot have leading nul byte")
}

/// a script can correspond to one or two tags, because
pub(crate) fn script_to_ot_tags(script: &UnicodeShortName) -> impl Iterator<Item = Tag> + use<> {
    let mut out = [None, None];
    if let Some(tag) = SCRIPT_EXCEPTIONS.binary_search_exact(&script.as_str()) {
        out[0] = Some(tag);
    } else if !unicode18::is_new_script(*script)
        && PropertyParser::<Script>::new().get_strict(script).is_none()
    {
        out[0] = Some(DFLT_SCRIPT);
    } else {
        out[0] = NEW_SCRIPT_TAGS.binary_search_exact(&script.as_str());
        out[1] = Some(Tag::new(script.to_owned().to_ascii_lowercase().all_bytes()));
    }

    out.into_iter().flatten()
}

#[cfg(test)]
mod tests {
    use write_fonts::read::FontRead;

    use super::*;

    /// we want to binary search these, so let's enforce that they are sorted,
    /// to avoid future headaches
    #[test]
    fn const_arrays_are_sorted() {
        fn get_original_and_sorted_items<T: Clone + Ord + Eq, U>(
            items: &[(T, U)],
        ) -> (Vec<T>, Vec<T>) {
            let originals = items.iter().map(|(a, _)| a.clone()).collect::<Vec<_>>();
            let mut sorted = originals.clone();
            sorted.sort();
            (originals, sorted)
        }

        let (actual, expected) = get_original_and_sorted_items(SCRIPT_ALIASES);
        assert_eq!(actual, expected);
        let (actual, expected) = get_original_and_sorted_items(SCRIPT_EXCEPTIONS_REVERSED);
        assert_eq!(actual, expected);
        let (actual, expected) = get_original_and_sorted_items(NEW_SCRIPTS);
        assert_eq!(actual, expected);
        let (actual, expected) = get_original_and_sorted_items(NEW_SCRIPT_TAGS);
        assert_eq!(actual, expected);
        let (actual, expected) = get_original_and_sorted_items(SCRIPT_EXCEPTIONS);
        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_tag_conversion() {
        assert_eq!(ot_tag_to_unicode_short_name(Tag::new(b"deva")), "Deva");
        assert_eq!(ot_tag_to_unicode_short_name(Tag::new(b"yi  ")), "Yiii");
        assert_eq!(ot_tag_to_unicode_short_name(Tag::new(b"nko ")), "Nkoo");
    }

    #[test]
    fn expected_unicode_script_overrides() {
        // this codepoint did not have scriptext property in unicode 15 but does
        // in unicode 16, so we need to manually override
        let mut apostrophemod: Vec<_> = unicode_script_extensions(0x2bc).collect();
        apostrophemod.sort();
        assert_eq!(
            apostrophemod,
            ["Beng", "Cyrl", "Deva", "Latn", "Lisu", "Thai", "Toto",]
        );

        // this codepoint's scriptex property changed in unicode16, but shouldn't
        // need an override because it existed in unicode 16
        let other = unicode_script_extensions(0x0ce6);
        assert_eq!(other.collect::<Vec<_>>(), ["Knda", "Nand", "Tutg"]);
    }

    #[test]
    fn unicode_18_script_and_bidi_properties() {
        // U+1CF5 lost Deva from Script_Extensions in Unicode 18. When it is
        // unambiguous, the kern feature writer adds Bengali to known scripts.
        assert_eq!(
            unicode_script_extensions(0x1CF5).collect::<Vec<_>>(),
            ["Beng"]
        );

        let mut scripts = unicode_script_extensions(0x0B83).collect::<Vec<_>>();
        scripts.sort();
        assert_eq!(scripts, ["Knda", "Mlym", "Taml", "Telu"]);
        assert_eq!(script_for_codepoint(0x11DF0), Some(tinystr!(4, "Beng")));
        assert_eq!(unicode_bidi_type(0x11DF0), None);
        for (script, tag) in [
            (tinystr!(4, "Jurc"), Tag::new(b"jurc")),
            (tinystr!(4, "Pcun"), Tag::new(b"pcun")),
            (tinystr!(4, "Seal"), Tag::new(b"seal")),
        ] {
            assert_eq!(script_to_ot_tags(&script).collect::<Vec<_>>(), [tag]);
        }
    }

    // https://github.com/googlefonts/ufo2ft/issues/901
    // I'm not sure that ufo2ft's behaviour is the best choice, but for the times
    // being we will match it.
    #[test]
    fn glyphs_matching_predicate_behaves_like_ufo2ft() {
        use write_fonts::tables::{gsub as wgsub, layout as wlayout};

        let a_gid = GlyphId16::new(0);
        let b_gid = GlyphId16::new(1);
        let neutral_gid = GlyphId16::new(2);

        // now we go and manually create a GSUB table with a single rule,
        // `sub a neutral_glyph by b;`.
        // the point here is that we want to test that 'b' is not considered
        // reachable via closure from 'a' for the given predicate, because
        // the predicate is not true for the neutral glyph.
        let coverage = [a_gid].into_iter().collect();
        let lig_set = wgsub::LigatureSet::new(vec![wgsub::Ligature::new(b_gid, vec![neutral_gid])]);
        let subtable = wgsub::LigatureSubstFormat1::new(coverage, vec![lig_set]);
        let lookup = wlayout::Lookup::new(Default::default(), vec![subtable]);

        let lookup_list = wgsub::SubstitutionLookupList::new(vec![lookup.into()]);
        let features = wlayout::FeatureList::new(vec![wlayout::FeatureRecord::new(
            Tag::new(b"derp"),
            wlayout::Feature::new(None, vec![0]),
        )]);
        let gsub = wgsub::Gsub::new(Default::default(), features, lookup_list);

        let bytes = write_fonts::dump_table(&gsub).unwrap();
        let read_gsub =
            write_fonts::read::tables::gsub::Gsub::read(bytes.as_slice().into()).unwrap();

        let charmap = HashMap::from([('a' as u32, a_gid)]);

        // a contrived predicate that is only true for the 'a' glyph.
        let reachable_from_a = glyphs_matching_predicate(
            &charmap,
            |uv| Some(uv == 'a' as u32),
            Some(&read_gsub),
            &Default::default(),
        )
        .unwrap();

        // 'b' should not be reachable because 'neutral_glyph' doesn't match our
        // predicate
        assert!(reachable_from_a.contains(a_gid) && reachable_from_a.len() == 1);
    }

    // https://github.com/googlefonts/ufo2ft/blob/b4890b5bb5bf88ebf5256b442031eae83a6d6dd1/Lib/ufo2ft/util.py#L375-L380
    #[test]
    fn extra_substitutions_apply_once_after_gsub_closure() {
        use crate::features::test_helpers::LayoutOutputBuilder;
        use fontdrasil::types::GlyphName;
        use fontir::ir::{Rule, VariableFeature};

        let glyph_order: GlyphOrder = [
            ".notdef",
            "a",
            "a.sc",
            "a.sc.alt",
            "a.alt",
            "a.alt.alt",
            "a.alt.sc",
            "alpha",
            "alpha.alt",
        ]
        .into_iter()
        .map(GlyphName::new)
        .collect();
        let layout = LayoutOutputBuilder::new()
            .with_glyph_order(glyph_order.clone())
            .with_user_fea("feature smcp { sub a by a.sc; sub a.alt by a.alt.sc; } smcp;")
            .with_variations(VariableFeature {
                features: vec![Tag::new(b"rvrn")],
                rules: vec![Rule::for_test(
                    &[],
                    &[
                        ("a", "a.alt"),
                        ("a", "a.missing"),
                        ("a.alt", "a.alt.alt"),
                        ("a.sc", "a.sc.alt"),
                        ("alpha", "alpha.alt"),
                    ],
                )],
            })
            .build();
        let extra = extra_substitutions(&layout.static_metadata, &glyph_order);
        let gid = |name: &str| glyph_order.glyph_id(name).unwrap();
        assert_eq!(extra.len(), 4);
        assert_eq!(extra[&gid("a")].iter().collect::<Vec<_>>(), [gid("a.alt")]);

        // Greek is not a known script, so alpha is neutral
        let charmap = HashMap::from([('a' as u32, gid("a")), ('α' as u32, gid("alpha"))]);
        let known_scripts = HashSet::from([tinystr!(4, "Latn")]);
        let gsub = layout.first_pass_fea.gsub();
        let scripts = scripts_by_glyph(&charmap, &known_scripts, gsub.as_ref(), &extra).unwrap();

        let mut latin = scripts
            .iter()
            .map(|(gid, scripts)| {
                assert_eq!(scripts, &HashSet::from([tinystr!(4, "Latn")]));
                glyph_order
                    .glyph_name(gid.to_u16() as usize)
                    .unwrap()
                    .as_str()
            })
            .collect::<Vec<_>>();
        latin.sort();
        assert_eq!(latin, ["a", "a.alt", "a.sc", "a.sc.alt"]);
    }

    #[test]
    fn script_direction_smoke_test() {
        assert_eq!(
            unicode_script_direction('a' as u32),
            Some(ScriptDirection::LeftToRight)
        );
        assert_eq!(
            unicode_script_direction('ء' as u32), // hamza
            Some(ScriptDirection::RightToLeft)
        );
        assert_eq!(unicode_script_direction(' ' as u32), None)
    }

    #[test]
    fn aliases_for_hira_kata() {
        let cp = '\u{30a0}';
        assert_eq!(
            unicode_script_extensions(cp as _).collect::<Vec<_>>(),
            [HRKT]
        );
    }
}
