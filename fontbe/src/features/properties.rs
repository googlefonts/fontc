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

static SCRIPT_DATA: ScriptWithExtensionsBorrowed<'static> =
    icu_properties::script::script_with_extensions();

/// The type used by icu4x for script names
pub type UnicodeShortName = tinystr::TinyAsciiStr<4>;

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

fn unicode_bidi_type(c: u32) -> BidiClass {
    icu_properties::maps::bidi_class().get32(c)
}

// equivalent to the 'classify' method in ufo2ft:
// <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfcf0b1c0f/Lib/ufo2ft/util.py#L287>
fn classify<T, F, I>(
    glyphs: &[(Arc<Glyph>, GlyphId)],
    mut props_fn: F,
    gsub: Option<&Gsub>,
) -> Result<HashMap<T, HashSet<GlyphId>>, ReadError>
where
    T: Hash + Eq,
    I: Iterator<Item = T>,
    F: FnMut(u32) -> I,
{
    let mut sets = HashMap::new();
    let mut neutral_glyphs = HashSet::new();
    for (gid, unicode_value) in glyphs.iter().flat_map(|(glyph, gid)| {
        glyph
            .codepoints
            .iter()
            .copied()
            .map(|codepoint| (*gid, codepoint))
    }) {
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

/// Returns a map of gids their scripts
pub(crate) fn scripts_by_glyph(
    glyphs: &[(Arc<Glyph>, GlyphId)],
    gsub: Option<&Gsub>,
) -> Result<HashMap<GlyphId, HashSet<UnicodeShortName>>, ReadError> {
    let mut result = HashMap::with_capacity(glyphs.len());
    for (script, glyphs) in classify(glyphs, |cp| unicode_script_extensions(cp), gsub)? {
        for glyph in glyphs {
            result.entry(glyph).or_insert(HashSet::new()).insert(script);
        }
    }
    Ok(result)
}

/// A map of bidi class to glyphs in that class.
pub(crate) fn glyphs_by_bidi_class(
    glyphs: &[(Arc<Glyph>, GlyphId)],
    gsub: Option<&Gsub>,
) -> Result<HashMap<BidiClass, HashSet<GlyphId>>, ReadError> {
    classify(
        glyphs,
        |codepoint| Some(unicode_bidi_type(codepoint)).into_iter(),
        gsub,
    )
}
static SCRIPT_ALIASES: &[(Tag, Tag)] = &[(Tag::new(b"jamo"), Tag::new(b"hang"))];

//TODO: python maps 'math' to 'Zmth' but 'Mathematical Notation'
//is not a variant on icu4x's Script? But this is included in
//some datafiles so might be an oversight? But it's actually not listed
//in the official list of scripts, so idk?
static SCRIPT_EXCEPTIONS: &[(Tag, Script)] = &[];

// I don't know what's going on here, just copying OTTags.py
static NEW_SCRIPTS: &[(Tag, Script)] = &[
    (Tag::new(b"bng2"), Script::Bengali),
    (Tag::new(b"dev2"), Script::Devanagari),
    (Tag::new(b"gjr2"), Script::Gujarati),
    (Tag::new(b"gur2"), Script::Gurmukhi),
    (Tag::new(b"knd2"), Script::Kannada),
    (Tag::new(b"mlm2"), Script::Malayalam),
    (Tag::new(b"mym2"), Script::Myanmar),
    (Tag::new(b"ory2"), Script::Oriya),
    (Tag::new(b"tel2"), Script::Telugu),
    (Tag::new(b"tml2"), Script::Tamil),
];

// a little helper trait to handle binary searching an array of 2-tuples where
// the first item is a key and the second a value
trait BinarySearchExact<T, U> {
    fn binary_search_exact(&self, needle: &T) -> Option<U>;
}

impl<T: Ord + Eq, U: Clone> BinarySearchExact<T, U> for &[(T, U)] {
    fn binary_search_exact(&self, needle: &T) -> Option<U> {
        self.binary_search_by(|probe| probe.0.cmp(&needle))
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

    if let Some(exception) = SCRIPT_EXCEPTIONS
        .binary_search_exact(&tag)
        .or_else(|| NEW_SCRIPTS.binary_search_exact(&tag))
    {
        let mapping = Script::enum_to_short_name_mapper();
        return mapping.get(exception);
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
        let (actual, expected) = get_original_and_sorted_items(SCRIPT_EXCEPTIONS);
        assert_eq!(actual, expected);
        let (actual, expected) = get_original_and_sorted_items(NEW_SCRIPTS);
        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_tag_conversion() {
        assert_eq!(ot_tag_to_unicode_short_name(Tag::new(b"deva")), "Deva");
        assert_eq!(ot_tag_to_unicode_short_name(Tag::new(b"yi  ")), "Yiii");
        assert_eq!(ot_tag_to_unicode_short_name(Tag::new(b"nko ")), "Nkoo");
    }
}
