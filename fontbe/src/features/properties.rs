//! Properties and constants related to unicode data
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    sync::Arc,
};

use fontir::ir::Glyph;
use icu_properties::{script::ScriptWithExtensionsBorrowed, BidiClass, Script};
use write_fonts::{
    read::{tables::gsub::Gsub, ReadError},
    types::{GlyphId, Tag},
};

// SAFETY: we can easily verify that neither of these strings contains a nul byte.
// (this is the only way we can declare these as constants)
pub const COMMON_SCRIPT: UnicodeShortName =
    unsafe { UnicodeShortName::from_bytes_unchecked(*b"Zyyy") };
pub const INHERITED_SCRIPT: UnicodeShortName =
    unsafe { UnicodeShortName::from_bytes_unchecked(*b"Zinh") };

static SCRIPT_DATA: ScriptWithExtensionsBorrowed<'static> =
    icu_properties::script::script_with_extensions();

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
    fn iter_glyphs(&self) -> impl Iterator<Item = (GlyphId, u32)>;
}

impl CharMap for Vec<(Arc<Glyph>, GlyphId)> {
    fn iter_glyphs(&self) -> impl Iterator<Item = (GlyphId, u32)> {
        self.iter()
            .flat_map(|(glyph, gid)| glyph.codepoints.iter().map(|uv| (*gid, *uv)))
    }
}

impl ScriptDirection {
    /// Returns the writing direction for the provided script
    pub(crate) fn for_script(script: &UnicodeShortName) -> Self {
        match script.as_str() {
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

/// Iterate over the unicode scripts + extensions for the provided codepoint
///
/// This returns the scripts as shortnames, because that's what python does.
/// It would probably make more sense for us to use the Script type defined by
/// icu4x, but I want to get a more direct port working first.
pub(crate) fn unicode_script_extensions(
    c: u32,
) -> impl Iterator<Item = UnicodeShortName> + 'static {
    let lookup = Script::enum_to_short_name_mapper();
    SCRIPT_DATA
        .get_script_extensions_val(c)
        .iter()
        .map(move |script| {
            lookup
                .get(script)
                // if we get a script it is by definition a 4-char ascii string,
                // so this unwrap should never fail
                .expect("names should be available for all defined scripts")
        })
}

// <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b340c/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L49>
/// returns none for neutral characters
fn unicode_bidi_type(c: u32) -> Option<BidiClass> {
    match icu_properties::maps::bidi_class().get32(c) {
        BidiClass::RightToLeft | BidiClass::ArabicLetter => Some(BidiClass::RightToLeft),
        BidiClass::LeftToRight | BidiClass::ArabicNumber | BidiClass::EuropeanNumber => {
            Some(BidiClass::LeftToRight)
        }
        _ => None,
    }
}

// equivalent to the 'classify' method in ufo2ft:
// <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfcf0b1c0f/Lib/ufo2ft/util.py#L287>
fn classify<T, F, I, CM>(
    char_map: &CM,
    mut props_fn: F,
    gsub: Option<&Gsub>,
) -> Result<HashMap<T, HashSet<GlyphId>>, ReadError>
where
    T: Hash + Eq,
    I: Iterator<Item = T>,
    F: FnMut(u32) -> I,
    CM: CharMap,
{
    let mut sets = HashMap::new();
    let mut neutral_glyphs = HashSet::new();
    for (gid, unicode_value) in char_map.iter_glyphs() {
        let mut has_props = false;
        for prop in props_fn(unicode_value) {
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

/// Returns a map of gids to their scripts
pub(crate) fn scripts_by_glyph(
    glyphs: &impl CharMap,
    known_scripts: &HashSet<UnicodeShortName>,
    gsub: Option<&Gsub>,
) -> Result<HashMap<GlyphId, HashSet<UnicodeShortName>>, ReadError> {
    let mut result = HashMap::new();
    for (script, glyphs) in classify(
        glyphs,
        |cp| {
            // we need to write this in such a way as to return a single concrete type;
            // this is basically two branches, one or the other option is always `None`
            let common = known_scripts.is_empty().then_some(COMMON_SCRIPT);
            let other_branch = if known_scripts.is_empty() {
                None
            } else {
                Some(unicode_script_extensions(cp).filter(|script| {
                    *script == COMMON_SCRIPT
                        || *script == INHERITED_SCRIPT
                        || known_scripts.contains(script)
                }))
            };

            common.into_iter().chain(other_branch.into_iter().flatten())
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
) -> Result<HashMap<BidiClass, HashSet<GlyphId>>, ReadError> {
    classify(
        glyphs,
        |codepoint| unicode_bidi_type(codepoint).into_iter(),
        gsub,
    )
}

const DFLT_SCRIPT: Tag = Tag::new(b"DFLT");

static SCRIPT_ALIASES: &[(Tag, Tag)] = &[(Tag::new(b"jamo"), Tag::new(b"hang"))];

static SCRIPT_EXCEPTIONS: &[(&str, Tag)] = &[
    ("Hira", Tag::new(b"kana")),
    ("Hrkt", Tag::new(b"kana")),
    ("Laoo", Tag::new(b"lao ")),
    ("Nkoo", Tag::new(b"nko ")),
    ("Vaii", Tag::new(b"vai ")),
    ("Yiii", Tag::new(b"yi  ")),
    ("Zinh", DFLT_SCRIPT),
    ("Zmth", Tag::new(b"math")),
    ("Zyyy", DFLT_SCRIPT),
    ("Zzzz", DFLT_SCRIPT),
];

// 'math' is used as a script in opentype features:
// <https://github.com/harfbuzz/harfbuzz/pull/3417>
static SCRIPT_EXCEPTIONS_REVERSED: &[(Tag, &str)] = &[(Tag::new(b"math"), "Zmth")];

// I don't know what's going on here, just copying OTTags.py
static NEW_SCRIPTS: &[(Tag, &str)] = &[
    (Tag::new(b"bng2"), "Beng"),
    (Tag::new(b"dev2"), "Deva"),
    (Tag::new(b"gjr2"), "Gujr"),
    (Tag::new(b"gur2"), "Guru"),
    (Tag::new(b"knd2"), "Knda"),
    (Tag::new(b"mlm2"), "Mlym"),
    (Tag::new(b"mym2"), "Mymr"),
    (Tag::new(b"ory2"), "Orya"),
    (Tag::new(b"tel2"), "Telu"),
    (Tag::new(b"tml2"), "Taml"),
];

// I don't know what's going on here, just copying OTTags.py
static NEW_SCRIPT_TAGS: &[(&str, Tag)] = &[
    ("Beng", Tag::new(b"bng2")),
    ("Deva", Tag::new(b"dev2")),
    ("Gujr", Tag::new(b"gjr2")),
    ("Guru", Tag::new(b"gur2")),
    ("Kana", Tag::new(b"knd2")),
    ("Mlym", Tag::new(b"mlm2")),
    ("Mymr", Tag::new(b"mym2")),
    ("Orya", Tag::new(b"ory2")),
    ("Taml", Tag::new(b"tml2")),
    ("Telu", Tag::new(b"tel2")),
];

static INDIC_SCRIPTS: &[&str] = &[
    "Beng", // Bengali
    "Deva", // Devanagari
    "Gujr", // Gujarati
    "Guru", // Gurmukhi
    "Knda", // Kannada
    "Mlym", // Malayalam
    "Orya", // Oriya
    "Sinh", // Sinhala
    "Taml", // Tamil
    "Telu", // Telugu
];

static USE_SCRIPTS: &[&str] = &[
    // Correct as at Unicode 15.0
    "Adlm", // Adlam
    "Ahom", // Ahom
    "Bali", // Balinese
    "Batk", // Batak
    "Brah", // Brahmi
    "Bugi", // Buginese
    "Buhd", // Buhid
    "Cakm", // Chakma
    "Cham", // Cham
    "Chrs", // Chorasmian
    "Cpmn", // Cypro Minoan
    "Diak", // Dives Akuru
    "Dogr", // Dogra
    "Dupl", // Duployan
    "Egyp", // Egyptian Hieroglyphs
    "Elym", // Elymaic
    "Gong", // Gunjala Gondi
    "Gonm", // Masaram Gondi
    "Gran", // Grantha
    "Hano", // Hanunoo
    "Hmng", // Pahawh Hmong
    "Hmnp", // Nyiakeng Puachue Hmong
    "Java", // Javanese
    "Kali", // Kayah Li
    "Kawi", // Kawi
    "Khar", // Kharosthi
    "Khoj", // Khojki
    "Kits", // Khitan Small Script
    "Kthi", // Kaithi
    "Lana", // Tai Tham
    "Lepc", // Lepcha
    "Limb", // Limbu
    "Mahj", // Mahajani
    "Maka", // Makasar
    "Mand", // Mandaic
    "Mani", // Manichaean
    "Marc", // Marchen
    "Medf", // Medefaidrin
    "Modi", // Modi
    "Mong", // Mongolian
    "Mtei", // Meetei Mayek
    "Mult", // Multani
    "Nagm", // Nag Mundari
    "Nand", // Nandinagari
    "Newa", // Newa
    "Nhks", // Bhaiksuki
    "Nko ", // Nko
    "Ougr", // Old Uyghur
    "Phag", // Phags Pa
    "Phlp", // Psalter Pahlavi
    "Plrd", // Miao
    "Rjng", // Rejang
    "Rohg", // Hanifi Rohingya
    "Saur", // Saurashtra
    "Shrd", // Sharada
    "Sidd", // Siddham
    "Sind", // Khudawadi
    "Sogd", // Sogdian
    "Sogo", // Old Sogdian
    "Soyo", // Soyombo
    "Sund", // Sundanese
    "Sylo", // Syloti Nagri
    "Tagb", // Tagbanwa
    "Takr", // Takri
    "Tale", // Tai Le
    "Tavt", // Tai Viet
    "Tfng", // Tifinagh
    "Tglg", // Tagalog
    "Tibt", // Tibetan
    "Tirh", // Tirhuta
    "Tnsa", // Tangsa
    "Toto", // Toto
    "Vith", // Vithkuqi
    "Wcho", // Wancho
    "Yezi", // Yezidi
    "Zanb", // Zanabazar Square
];

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
    } else if Script::name_to_enum_mapper().get_strict(script).is_none() {
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
}
