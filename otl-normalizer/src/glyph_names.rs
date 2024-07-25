//! Human readable names for glyphs
//!
//! Much of this code was originally part of runebender.

use std::collections::{BTreeMap, HashMap};

use fontdrasil::types::GlyphName;
use write_fonts::read::{
    tables::cmap::{CmapSubtable, EncodingRecord, PlatformId},
    types::{GlyphId16, Tag},
    FontRef, TableProvider,
};

use crate::error::Error;

/// A map for gids to human-readable names
#[derive(Clone, Debug, Default)]
pub struct NameMap(pub(crate) BTreeMap<GlyphId16, GlyphName>);

impl NameMap {
    /// Create a new name mapping for the glyphs in the provided font
    pub fn from_font(font: &FontRef) -> Result<NameMap, Error> {
        let num_glyphs = font
            .maxp()
            .map_err(|_| Error::MissingTable(Tag::new(b"maxp")))?
            .num_glyphs();
        let reverse_cmap = reverse_cmap(font)?;
        let post = font.post().ok();
        let mut name_map = (1..num_glyphs)
            .map(|gid| {
                let gid = GlyphId16::new(gid);
                // first check post, then do fallback
                if let Some(name) = post
                    .as_ref()
                    .and_then(|post| post.glyph_name(gid).map(GlyphName::from))
                {
                    return (gid, name);
                }
                // fallback to unicode or gid
                let name = match reverse_cmap.get(&gid).and_then(|cp| char::from_u32(*cp)) {
                    Some(codepoint) => match glyph_name_for_char(codepoint) {
                        Some(name) => name,
                        // we have a codepoint but it doesn't have a name:
                        None => {
                            let raw = codepoint as u32;
                            if raw <= 0xFFFF {
                                smol_str::format_smolstr!("uni{raw:04X}")
                            } else {
                                smol_str::format_smolstr!("u{raw:X}")
                            }
                            .into()
                        }
                    },
                    // we have no codepoint, just use glyph ID
                    None => smol_str::format_smolstr!("glyph.{:05}", gid.to_u16()).into(),
                };
                (gid, name)
            })
            .collect::<BTreeMap<_, _>>();
        name_map.insert(GlyphId16::NOTDEF, ".notdef".into());

        Ok(NameMap(name_map))
    }

    /// Returns a human readable name for this gid.
    ///
    /// This will panic if the gid is not in the font used to create this map.
    pub fn get(&self, gid: GlyphId16) -> &GlyphName {
        // map contains a name for every gid in the font
        self.0.get(&gid).unwrap()
    }

    #[allow(dead_code)]
    pub(crate) fn iter(&self) -> impl Iterator<Item = &GlyphName> + '_ {
        self.0.values()
    }
}

fn reverse_cmap(font: &FontRef) -> Result<HashMap<GlyphId16, u32>, Error> {
    // <https://github.com/fonttools/fonttools/blob/6fa1a76e061c2e84243d8cac/Lib/fontTools/ttLib/tables/_c_m_a_p.py#L334>
    fn is_unicode(record: &&EncodingRecord) -> bool {
        record.platform_id() == PlatformId::Unicode
            || record.platform_id() == PlatformId::Unicode
                && [0, 1, 10].contains(&record.encoding_id())
    }

    let cmap = font
        .cmap()
        .map_err(|_| Error::MissingTable(Tag::new(b"cmap")))?;
    let offset_data = cmap.offset_data();

    let mut reverse_cmap = HashMap::new();

    let mut add_to_map = |args: (u32, GlyphId16)| {
        // because multiple glyphs may map to the same codepoint,
        // we always use the lowest codepoint to determine the name.
        let val = reverse_cmap.entry(args.1).or_insert(args.0);
        *val = args.0.min(*val);
    };

    for subtable in cmap
        .encoding_records()
        .iter()
        .filter(is_unicode)
        .map(|rec| rec.subtable(offset_data).unwrap())
    {
        match subtable {
            CmapSubtable::Format4(subtable) => subtable
                .iter()
                .map(|(unicode, gid)| (unicode, GlyphId16::try_from(gid).unwrap()))
                .for_each(&mut add_to_map),
            CmapSubtable::Format12(subtable) => subtable
                .iter()
                .map(|(unicode, gid)| (unicode, GlyphId16::try_from(gid).unwrap()))
                .for_each(&mut add_to_map),
            _ => (),
        }
    }

    Ok(reverse_cmap)
}

impl FromIterator<GlyphName> for NameMap {
    fn from_iter<T: IntoIterator<Item = GlyphName>>(iter: T) -> Self {
        Self(
            iter.into_iter()
                .enumerate()
                .map(|(i, name)| (GlyphId16::new(i as _), name))
                .collect(),
        )
    }
}

/// Given a `char`, returns the postscript name for that `char`s glyph,
/// if one exists in the aglfn.
fn glyph_name_for_char(chr: char) -> Option<GlyphName> {
    fontdrasil::agl::agl_name_for_char(chr).map(Into::into)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_test() {
        assert_eq!(glyph_name_for_char('c').unwrap(), "c");
        assert_eq!(glyph_name_for_char('C').unwrap(), "C");

        assert_eq!(glyph_name_for_char('é').unwrap(), "eacute");

        assert_eq!(glyph_name_for_char('<').unwrap(), "less");
        assert!(glyph_name_for_char('ء').is_none());
        assert_eq!(glyph_name_for_char('!').unwrap(), "exclam");
    }
}
