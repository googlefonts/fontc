//! Properties and constants related to unicode data
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    hash::Hash,
};

use icu_properties::{
    props::{BidiClass, Script},
    CodePointMapData, PropertyNamesShort, PropertyParser,
};
use tinystr::tinystr;
use write_fonts::{
    read::{collections::IntSet, tables::gsub::Gsub, ReadError},
    types::{GlyphId16, Tag},
};

use crate::features::ot_tags::{NEW_SCRIPTS, SCRIPT_ALIASES, SCRIPT_EXCEPTIONS_REVERSED};

use super::ot_tags::{DFLT_SCRIPT, INDIC_SCRIPTS, NEW_SCRIPT_TAGS, SCRIPT_EXCEPTIONS, USE_SCRIPTS};

pub const COMMON_SCRIPT: UnicodeShortName = tinystr!(4, "Zyyy");
pub const INHERITED_SCRIPT: UnicodeShortName = tinystr!(4, "Zinh");

/// The type used by icu4x for script names
pub type UnicodeShortName = tinystr::TinyAsciiStr<4>;

/// The writing direction of a script
#[derive(Clone, Debug, Copy, Hash, PartialEq, Eq)]
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

/// Iff a codepoint belongs to a single script, return it.
pub(crate) fn single_script_for_codepoint(cp: u32) -> Option<UnicodeShortName> {
    let mut scripts = scripts_for_codepoint(cp);

    match (scripts.next(), scripts.next()) {
        (Some(script), None) => Some(script),
        _ => None,
    }
}

// <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b340c/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L49>
/// returns none for neutral characters
fn unicode_bidi_type(c: u32) -> Option<BidiClass> {
    match CodePointMapData::<BidiClass>::new().get32(c) {
        BidiClass::RightToLeft | BidiClass::ArabicLetter => Some(BidiClass::RightToLeft),
        BidiClass::LeftToRight | BidiClass::ArabicNumber | BidiClass::EuropeanNumber => {
            Some(BidiClass::LeftToRight)
        }
        _ => None,
    }
}

// equivalent to the 'classify' method in ufo2ft:
// <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfcf0b1c0f/Lib/ufo2ft/util.py#L287>
fn classify<T, F, CM>(
    char_map: &CM,
    mut props_fn: F,
    gsub: Option<&Gsub>,
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
            neutral_glyphs.insert(gid);
        }
    }

    if let Some(gsub) = gsub.as_ref() {
        neutral_glyphs = gsub.closure_glyphs(neutral_glyphs)?;
        for glyphs in sets.values_mut() {
            let mut temp = glyphs.clone();
            temp.union(&neutral_glyphs);
            let temp = gsub.closure_glyphs(temp)?;
            glyphs.extend(temp.iter().filter(|gid| !neutral_glyphs.contains(*gid)));
        }
    }
    Ok(sets)
}

pub(crate) fn glyphs_matching_predicate(
    glyphs: &impl CharMap,
    predicate: impl Fn(u32) -> Option<bool>,
    gsub: Option<&Gsub>,
) -> Result<IntSet<GlyphId16>, ReadError> {
    classify(
        glyphs,
        |cp, buf| {
            if let Some(val) = predicate(cp) {
                buf.push(val)
            }
        },
        gsub,
    )
    .map(|mut items| items.remove(&true).unwrap_or_default())
}

/// Returns a map of gids to their scripts
pub(crate) fn scripts_by_glyph(
    glyphs: &impl CharMap,
    known_scripts: &HashSet<UnicodeShortName>,
    gsub: Option<&Gsub>,
) -> Result<HashMap<GlyphId16, HashSet<UnicodeShortName>>, ReadError> {
    let mut result = HashMap::new();
    for (script, glyphs) in classify(
        glyphs,
        |cp, buf| {
            if known_scripts.is_empty() {
                buf.push(COMMON_SCRIPT);
            } else {
                buf.extend(scripts_for_codepoint(cp).filter(|script| {
                    *script == COMMON_SCRIPT
                        || *script == INHERITED_SCRIPT
                        || known_scripts.contains(script)
                }));
            }
        },
        gsub,
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
) -> Result<BTreeMap<BidiClass, IntSet<GlyphId16>>, ReadError> {
    classify(
        glyphs,
        |codepoint, buf| buf.extend(unicode_bidi_type(codepoint)),
        gsub,
    )
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

/// Iterate over unicode scripts for the given codepoint
pub(crate) fn scripts_for_codepoint(cp: u32) -> impl Iterator<Item = UnicodeShortName> {
    let temp_fix = script_ext_for_cp_override(cp);
    let normal_path = if temp_fix.is_none() {
        Some(
            icu_properties::script::ScriptWithExtensions::new()
                .get_script_extensions_val32(cp)
                .iter()
                .flat_map(get_script_short_name),
        )
    } else {
        None
    };

    temp_fix
        .into_iter()
        .flat_map(|items| items.iter().copied())
        .chain(normal_path.into_iter().flatten())
}

// due to a bug in ICU4x, codepoints that were newly added to ScriptExtensions.txt
// in unicode 16 do not currently have the correct extensions returned.
// This is a temporary workaround, containing only those codepoints.
//
// This can all be deleted when icu4x resolves #6041.
//
// - https://github.com/unicode-org/icu4x/issues/6041
// - https://unicode-org.atlassian.net/browse/ICU-21821
// - https://unicode.org/Public/16.0.0/ucd/ScriptExtensions.txt
fn script_ext_for_cp_override(cp: u32) -> Option<&'static [UnicodeShortName]> {
    const AVST: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Avst", b' ');
    const CARI: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Cari", b' ');
    const COPT: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Copt", b' ');
    const DUPL: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Dupl", b' ');
    const ELBA: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Elba", b' ');
    const GEOR: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Geor", b' ');
    const GLAG: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Glag", b' ');
    const GONG: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Gong", b' ');
    const GOTH: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Goth", b' ');
    const GREK: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Grek", b' ');
    const HANI: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Hani", b' ');
    const LATN: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Latn", b' ');
    const LYDI: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Lydi", b' ');
    const MAHJ: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Mahj", b' ');
    const PERM: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Perm", b' ');
    const SHAW: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Shaw", b' ');
    const BENG: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Beng", b' ');
    const CYRL: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Cyrl", b' ');
    const DEVA: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Deva", b' ');
    const LISU: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Lisu", b' ');
    const THAI: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Thai", b' ');
    const TOTO: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Toto", b' ');
    const BOPO: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Bopo", b' ');
    const CHER: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Cher", b' ');
    const OSGE: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Osge", b' ');
    const SUNU: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Sunu", b' ');
    const TALE: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Tale", b' ');
    const SYRC: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Syrc", b' ');
    const TFNG: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Tfng", b' ');
    const TODR: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Todr", b' ');
    const AGHB: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Aghb", b' ');
    const KANA: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Kana", b' ');
    const HEBR: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Hebr", b' ');
    const ARMN: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Armn", b' ');
    const ETHI: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Ethi", b' ');
    const RUNR: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Runr", b' ');
    const ADLM: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Adlm", b' ');
    const ARAB: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Arab", b' ');
    const HUNG: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Hung", b' ');
    const KTHI: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Kthi", b' ');
    const LYCI: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Lyci", b' ');
    const ORKH: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Orkh", b' ');
    const MERO: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Mero", b' ');
    const SAMR: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Samr", b' ');
    const TANG: UnicodeShortName = UnicodeShortName::from_utf8_lossy(b"Tang", b' ');

    match cp {
        0x0B7 => Some(&[
            AVST, CARI, COPT, DUPL, ELBA, GEOR, GLAG, GONG, GOTH, GREK, HANI, LATN, LYDI, MAHJ,
            PERM, SHAW,
        ]),
        0x2BC => Some(&[BENG, CYRL, DEVA, LATN, LISU, THAI, TOTO]),
        0x2C7 | 0x2C9..0x2CB => Some(&[BOPO, LATN]),
        0x2CD => Some(&[LATN, LISU]),
        0x2D7 => Some(&[LATN, THAI]),
        0x2D9 => Some(&[BOPO, LATN]),
        0x300 => Some(&[CHER, COPT, CYRL, GREK, LATN, PERM, SUNU, TALE]),
        0x301 => Some(&[CHER, CYRL, GREK, LATN, OSGE, SUNU, TALE, TODR]),
        0x302 => Some(&[CHER, CYRL, LATN, TFNG]),
        0x303 => Some(&[GLAG, LATN, SUNU, SYRC, THAI]),
        0x304 => Some(&[
            AGHB, CHER, COPT, CYRL, GOTH, GREK, LATN, OSGE, SYRC, TFNG, TODR,
        ]),
        0x305 => Some(&[COPT, ELBA, GLAG, GOTH, KANA, LATN]),
        0x306 => Some(&[CYRL, GREK, LATN, PERM]),
        0x307 => Some(&[COPT, DUPL, HEBR, LATN, PERM, SYRC, TALE, TFNG, TODR]),
        0x308 => Some(&[ARMN, CYRL, DUPL, GOTH, GREK, HEBR, LATN, PERM, SYRC, TALE]),
        0x309 => Some(&[LATN, TFNG]),
        0x30a => Some(&[DUPL, LATN, SYRC]),
        0x30b => Some(&[CHER, CYRL, LATN, OSGE]),
        0x30c => Some(&[CHER, LATN, SYRC]),
        0x30d => Some(&[LATN, SUNU]),
        0x30e => Some(&[ETHI, LATN]),
        0x310 => Some(&[LATN, SUNU]),
        0x311 => Some(&[CYRL, LATN, TODR]),
        0x313 => Some(&[GREK, LATN, PERM, TODR]),
        0x320 => Some(&[LATN, SYRC]),
        0x323 => Some(&[CHER, DUPL, KANA, LATN, SYRC]),
        0x324 => Some(&[CHER, DUPL, LATN, SYRC]),
        0x325 => Some(&[LATN, SYRC]),
        0x32D => Some(&[LATN, SUNU, SYRC]),
        0x32E => Some(&[LATN, SYRC]),
        0x330 => Some(&[CHER, LATN, SYRC]),
        0x331 => Some(&[AGHB, CHER, GOTH, LATN, SUNU, THAI]),
        0x358 => Some(&[LATN, OSGE]),
        0x35E => Some(&[AGHB, LATN, TODR]),
        0x374 | 0x375 => Some(&[COPT, GREK]),
        0x589 => Some(&[ARMN, GEOR, GLAG]),
        0x16EB..=0x16ED => Some(&[RUNR]),
        0x204F => Some(&[ADLM, ARAB]),
        0x205A => Some(&[CARI, GEOR, GLAG, HUNG, LYCI, ORKH]),
        0x205D => Some(&[CARI, GREK, HUNG, MERO]),
        0x2E17 => Some(&[COPT, LATN]),
        0x2E30 => Some(&[AVST, ORKH]),
        0x2E31 => Some(&[AVST, CARI, GEOR, HUNG, KTHI, LYDI, SAMR]),
        0x2E3C => Some(&[DUPL]),
        0x2E41 => Some(&[ADLM, ARAB, HUNG]),
        0x2FF0..=0x2FFF => Some(&[HANI, TANG]),
        0x31E4..=0x31E5 => Some(&[HANI]),
        0x31EF => Some(&[HANI, TANG]),
        _ => None,
    }
}

fn get_script_short_name(script: Script) -> Option<UnicodeShortName> {
    let lookup = PropertyNamesShort::<Script>::new();
    lookup
        .get(script)
        .and_then(|script| tinystr::TinyStr4::try_from_str(script).ok())
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
pub(crate) fn script_to_ot_tags(script: &UnicodeShortName) -> impl Iterator<Item = Tag> {
    let mut out = [None, None];
    if let Some(tag) = SCRIPT_EXCEPTIONS.binary_search_exact(&script.as_str()) {
        out[0] = Some(tag);
    } else if PropertyParser::<Script>::new().get_strict(script).is_none() {
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
        let apostrophemod = scripts_for_codepoint(0x2bc);
        assert_eq!(
            apostrophemod.collect::<Vec<_>>(),
            ["Beng", "Cyrl", "Deva", "Latn", "Lisu", "Thai", "Toto",]
        );

        // this codepoint's scriptex property changed in unicode16, but shouldn't
        // need an override because it existed in unicode 16
        let other = scripts_for_codepoint(0x0ce6);
        assert_eq!(other.collect::<Vec<_>>(), ["Knda", "Nand", "Tutg"]);
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
        let reachable_from_a =
            glyphs_matching_predicate(&charmap, |uv| Some(uv == 'a' as u32), Some(&read_gsub))
                .unwrap();

        // 'b' should not be reachable because 'neutral_glyph' doesn't match our
        // predicate
        assert!(reachable_from_a.contains(a_gid) && reachable_from_a.len() == 1);
    }
}
