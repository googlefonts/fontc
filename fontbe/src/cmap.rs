//! Generates a [cmap](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap) table.

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;

use write_fonts::{tables::cmap::Cmap, types::GlyphId16};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct CmapWork {}

pub fn create_cmap_work() -> Box<BeWork> {
    Box::new(CmapWork {})
}

impl Work<Context, AnyWorkId, Error> for CmapWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Cmap.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::ALL_GLYPHS)
            .build()
    }

    /// Generate [cmap](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // cmap only accomodates single codepoint : glyph mappings; collect all of those
        let glyph_order = context.ir.glyph_order.get();

        let mappings = glyph_order
            .names()
            .map(|glyph_name| context.ir.get_glyph(glyph_name.clone()))
            .enumerate()
            .flat_map(|(gid, glyph)| {
                glyph
                    .codepoints
                    .iter()
                    .map(|codepoint| {
                        (
                            char::from_u32(*codepoint).expect("We have an invalid codepoint!"),
                            GlyphId16::new(gid as u16).into(),
                        )
                    })
                    .collect::<Vec<_>>()
            });

        let cmap = Cmap::from_mappings(mappings)?;
        context.cmap.set(cmap);
        Ok(())
    }
}
