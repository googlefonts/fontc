//! Generates a [gvar](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar) table.

use font_types::GlyphId;
use fontdrasil::orchestration::Work;
use write_fonts::{
    dump_table,
    tables::gvar::{GlyphVariations, Gvar},
};

use crate::{
    error::Error,
    orchestration::{BeWork, Bytes, Context},
};

struct GvarWork {}

pub fn create_gvar_work() -> Box<BeWork> {
    Box::new(GvarWork {})
}

impl Work<Context, Error> for GvarWork {
    /// Generate [gvar](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // We built the gvar fragments alongside glyphs, now we need to glue them together into a gvar table
        let static_metadata = context.ir.get_final_static_metadata();

        let variations: Vec<_> = static_metadata
            .glyph_order
            .iter()
            .enumerate()
            .map(|(gid, gn)| {
                let gid = GlyphId::new(gid as u16);
                let gvar_fragment = context.get_gvar_fragment(gn);
                (gid, gvar_fragment.to_deltas())
            })
            .map(|(gid, deltas)| GlyphVariations::new(gid, deltas))
            .collect();
        let gvar = Gvar::new(variations).map_err(Error::GvarError)?;

        let raw_gvar = Bytes::new(dump_table(&gvar).map_err(|e| Error::DumpTableError {
            e,
            context: "gvar".into(),
        })?);
        context.set_gvar(raw_gvar);

        Ok(())
    }
}
