//! Glyph classification by a Unicode property, closed over GSUB.

use std::collections::{BTreeMap, HashMap};

use write_fonts::{
    read::{ReadError, collections::IntSet, tables::gsub::Gsub},
    types::{GlyphId, GlyphId16},
};

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

// equivalent to the 'classify' method in ufo2ft:
// <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfcf0b1c0f/Lib/ufo2ft/util.py#L287>
pub fn classify<T, F, CM>(
    char_map: &CM,
    mut props_fn: F,
    gsub: Option<&Gsub>,
    extra_substitutions: Option<&HashMap<GlyphId16, IntSet<GlyphId16>>>,
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

    if let Some(extra_substitutions) = extra_substitutions {
        for glyphs in sets.values_mut() {
            let mut to_append = IntSet::new();
            for gid in glyphs.iter() {
                if let Some(substitutes) = extra_substitutions.get(&gid) {
                    to_append.union(substitutes);
                }
            }
            glyphs.union(&to_append);
        }
    }
    Ok(sets)
}
