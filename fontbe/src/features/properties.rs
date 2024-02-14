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
