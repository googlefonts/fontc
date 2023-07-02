//! Generates a [gvar](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar) table.

use font_types::GlyphId;
use fontdrasil::{orchestration::Work, types::GlyphName};
use fontir::ir::StaticMetadata;
use write_fonts::{
    dump_table,
    tables::gvar::{GlyphDeltas, GlyphVariations, Gvar},
};

use crate::{
    error::Error,
    orchestration::{BeWork, Bytes, Context, WorkId},
};

struct GvarWork {}

pub fn create_gvar_work() -> Box<BeWork> {
    Box::new(GvarWork {})
}

fn make_variations(
    static_metadata: &StaticMetadata,
    get_deltas: impl Fn(&GlyphName) -> Vec<GlyphDeltas>,
) -> Vec<GlyphVariations> {
    static_metadata
        .glyph_order
        .iter()
        .enumerate()
        .filter_map(|(gid, gn)| {
            let deltas = get_deltas(gn);
            if deltas.is_empty() {
                return None;
            }
            Some((GlyphId::new(gid as u16), deltas))
        })
        .map(|(gid, deltas)| GlyphVariations::new(gid, deltas))
        .collect()
}

impl Work<Context, WorkId, Error> for GvarWork {
    fn id(&self) -> WorkId {
        WorkId::Gvar
    }

    /// Generate [gvar](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // We built the gvar fragments alongside glyphs, now we need to glue them together into a gvar table
        let static_metadata = context.ir.get_final_static_metadata();

        let variations: Vec<_> = make_variations(&static_metadata, |gid| {
            context.get_gvar_fragment(gid).to_deltas()
        });
        let gvar = Gvar::new(variations).map_err(Error::GvarError)?;

        let raw_gvar = Bytes::new(dump_table(&gvar).map_err(|e| Error::DumpTableError {
            e,
            context: "gvar".into(),
        })?);
        context.set_gvar(raw_gvar);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use font_types::F2Dot14;
    use fontdrasil::types::GlyphName;
    use fontir::ir::StaticMetadata;
    use indexmap::IndexSet;
    use write_fonts::tables::{gvar::GlyphDeltas, variations::Tuple};

    use crate::test_util::axis;

    use super::make_variations;

    fn create_static_metadata(glyph_order: &[&str]) -> StaticMetadata {
        StaticMetadata::new(
            1000,
            Default::default(),
            [axis(400.0, 400.0, 700.0)].to_vec(),
            Default::default(),
            IndexSet::from_iter(glyph_order.iter().map(|n| GlyphName::from(*n))),
            Default::default(),
        )
        .unwrap()
    }

    #[test]
    fn skips_empty_variations() {
        let glyph_with_var = "has_var";
        let glyph_without_var = "no_var";
        let static_metadata = create_static_metadata(&[glyph_with_var, glyph_without_var]);

        let variations = make_variations(&static_metadata, |name| {
            match name.as_str() {
                v if v == glyph_without_var => Vec::new(),
                // At the maximum extent (normalized pos 1.0) of our axis, add +1, +1
                v if v == glyph_with_var => vec![GlyphDeltas::new(
                    Tuple::new(vec![F2Dot14::from_f32(1.0)]),
                    vec![Some((1, 1))],
                    None,
                )],
                v => panic!("unexpected {v}"),
            }
        });

        assert_eq!(1, variations.len(), "{variations:?}");
    }
}
