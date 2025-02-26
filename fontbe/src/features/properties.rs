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
    read::{tables::gsub::Gsub, ReadError},
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
) -> Result<BTreeMap<T, HashSet<GlyphId16>>, ReadError>
where
    T: Ord + Eq,
    // instead of returning an iterator, pushes items into the provided buffer
    F: FnMut(u32, &mut Vec<T>),
    CM: CharMap,
{
    let mut sets = BTreeMap::new();
    let mut neutral_glyphs = HashSet::new();
    let mut buf = Vec::new();
    for (gid, unicode_value) in char_map.iter_glyphs() {
        let mut has_props = false;
        props_fn(unicode_value, &mut buf);
        for prop in buf.drain(..) {
            sets.entry(prop).or_insert(HashSet::new()).insert(gid);
            has_props = true;
        }
        if !has_props {
            neutral_glyphs.insert(gid);
        }
    }

    if let Some(gsub) = gsub.as_ref() {
        neutral_glyphs = gsub.closure_glyphs(neutral_glyphs)?;
        for glyphs in sets.values_mut() {
            let temp = glyphs
                .union(&neutral_glyphs)
                .copied()
                .collect::<HashSet<_>>();
            let temp = gsub.closure_glyphs(temp)?;
            glyphs.extend(temp.difference(&neutral_glyphs).copied())
        }
    }
    Ok(sets)
}

pub(crate) fn glyphs_matching_predicate(
    glyphs: &impl CharMap,
    predicate: impl Fn(u32) -> bool,
    gsub: Option<&Gsub>,
) -> Result<HashSet<GlyphId16>, ReadError> {
    classify(
        glyphs,
        |cp, buf| {
            if predicate(cp) {
                buf.push(true);
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
        for glyph in glyphs {
            result.entry(glyph).or_insert(HashSet::new()).insert(script);
        }
    }
    Ok(result)
}

/// A map of bidi class to glyphs in that class.
pub(crate) fn glyphs_by_bidi_class(
    glyphs: &impl CharMap,
    gsub: Option<&Gsub>,
) -> Result<BTreeMap<BidiClass, HashSet<GlyphId16>>, ReadError> {
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
        .map(|s| UnicodeShortName::from_str(s).unwrap())
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
    const AVST: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Avst");
    const CARI: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Cari");
    const COPT: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Copt");
    const DUPL: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Dupl");
    const ELBA: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Elba");
    const GEOR: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Geor");
    const GLAG: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Glag");
    const GONG: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Gong");
    const GOTH: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Goth");
    const GREK: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Grek");
    const HANI: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Hani");
    const LATN: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Latn");
    const LYDI: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Lydi");
    const MAHJ: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Mahj");
    const PERM: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Perm");
    const SHAW: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Shaw");
    const BENG: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Beng");
    const CYRL: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Cyrl");
    const DEVA: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Deva");
    const LISU: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Lisu");
    const THAI: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Thai");
    const TOTO: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Toto");
    const BOPO: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Bopo");
    const CHER: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Cher");
    const OSGE: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Osge");
    const SUNU: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Sunu");
    const TALE: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Tale");
    const SYRC: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Syrc");
    const TFNG: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Tfng");
    const TODR: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Todr");
    const AGHB: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Aghb");
    const KANA: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Kana");
    const HEBR: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Hebr");
    const ARMN: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Armn");
    const ETHI: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Ethi");
    const RUNR: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Runr");
    const ADLM: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Adlm");
    const ARAB: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Arab");
    const HUNG: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Hung");
    const KTHI: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Kthi");
    const LYCI: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Lyci");
    const ORKH: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Orkh");
    const MERO: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Mero");
    const SAMR: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Samr");
    const TANG: UnicodeShortName = UnicodeShortName::from_bytes_lossy(b"Tang");

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
        .and_then(|script| tinystr::TinyStr4::from_str(script).ok())
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
        return Some(UnicodeShortName::from_str(exception).unwrap());
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
}
