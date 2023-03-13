//! Generates a [cmap](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap) table.

use fontdrasil::orchestration::Work;
use read_fonts::types::GlyphId;
use write_fonts::tables::cmap::Cmap;

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct CmapWork {}

pub fn create_cmap_work() -> Box<BeWork> {
    Box::new(CmapWork {})
}

impl Work<Context, Error> for CmapWork {
    /// Generate [cmap](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // cmap only accomodates single codepoint : glyph mappings; collect all of those
        let static_metadata = context.ir.get_static_metadata();

        let mappings = static_metadata
            .glyph_order
            .iter()
            .map(|glyph_name| context.ir.get_glyph_ir(glyph_name))
            .enumerate()
            .flat_map(|(gid, glyph)| {
                glyph
                    .codepoints
                    .iter()
                    .map(|codepoint| {
                        (
                            char::from_u32(*codepoint).expect("We have an invalid codepoint!"),
                            GlyphId::new(gid as u16),
                        )
                    })
                    .collect::<Vec<_>>()
            });

        let cmap = Cmap::from_mappings(mappings);
        context.set_cmap(cmap);
        Ok(())
    }
}
