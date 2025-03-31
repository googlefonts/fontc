//! Generates a [gvar](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar) table.

use fontdrasil::{
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{ir::GlyphOrder, orchestration::WorkId as FeWorkId};
use write_fonts::{
    dump_table,
    tables::gvar::{GlyphDeltas, GlyphVariations, Gvar},
    types::GlyphId16,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct GvarWork {}

pub fn create_gvar_work() -> Box<BeWork> {
    Box::new(GvarWork {})
}

fn make_variations(
    glyph_order: &GlyphOrder,
    get_deltas: impl Fn(&GlyphName) -> Vec<GlyphDeltas>,
) -> Vec<GlyphVariations> {
    glyph_order
        .names()
        .enumerate()
        .map(|(gid, gn)| GlyphVariations::new(GlyphId16::new(gid as u16).into(), get_deltas(gn)))
        .collect()
}

impl Work<Context, AnyWorkId, Error> for GvarWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Gvar.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlyphOrder)
            .variant(WorkId::ALL_GVAR_FRAGMENTS)
            .build()
    }

    /// Generate [gvar](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // We built the gvar fragments alongside glyphs, now we need to glue them together into a gvar table
        let static_metadata = context.ir.static_metadata.get();
        let axis_order: Vec<_> = static_metadata.axes.iter().map(|a| a.tag).collect();
        if axis_order.is_empty() {
            log::debug!("skipping gvar, font has no axes");
            return Ok(());
        }
        let axis_count: u16 = axis_order
            .len()
            .try_into()
            // in the unlikely event that we have more than 65535 axes...
            .map_err(|_| Error::OutOfBounds {
                what: "axis count".into(),
                value: axis_order.len().to_string(),
            })?;
        let glyph_order = context.ir.glyph_order.get();

        let variations: Vec<_> = make_variations(&glyph_order, |glyph_name| {
            context
                .gvar_fragments
                .get(&WorkId::GvarFragment(glyph_name.clone()).into())
                .to_deltas(&axis_order)
        });
        let gvar = Gvar::new(variations, axis_count).map_err(Error::GvarError)?;

        let raw_gvar = dump_table(&gvar)
            .map_err(|e| Error::DumpTableError {
                e,
                context: "gvar".into(),
            })?
            .into();
        context.gvar.set(raw_gvar);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontir::ir::GlyphOrder;
    use write_fonts::{
        tables::gvar::{self, GlyphDelta, GlyphDeltas},
        types::F2Dot14,
    };

    use super::make_variations;

    #[test]
    fn do_not_skip_empty_variations() {
        // gvar contains a GlyphVariationData for each glyph in the 'glyf' table
        // whether or not it contains any deltas for that glyph.
        let glyph_with_var = "has_var";
        let glyph_without_var = "no_var";
        let mut glyph_order = GlyphOrder::new();
        glyph_order.insert(glyph_with_var.into());
        glyph_order.insert(glyph_without_var.into());

        let variations = make_variations(&glyph_order, |name| {
            match name.as_str() {
                v if v == glyph_without_var => Vec::new(),
                // At the maximum extent (normalized pos 1.0) of our axis, add +1, +1
                v if v == glyph_with_var => vec![GlyphDeltas::new(
                    vec![gvar::Tent::new(F2Dot14::from_f32(1.0), None)],
                    vec![GlyphDelta::new(1, 1, false)],
                )],
                v => panic!("unexpected {v}"),
            }
        });

        assert_eq!(2, variations.len(), "{variations:?}");
    }
}
