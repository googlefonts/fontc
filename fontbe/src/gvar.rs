//! Generates a [gvar](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar) table.

use log::trace;

use font_types::{F2Dot14, GlyphId};
use fontdrasil::orchestration::Work;
use fontir::variations::VariationRegion;
use write_fonts::{
    dump_table,
    tables::{
        gvar::{GlyphDeltas, GlyphVariations, Gvar},
        variations::Tuple,
    },
};

use crate::{
    error::Error,
    orchestration::{BeWork, Bytes, Context, GvarFragment},
};

struct GvarWork {}

pub fn create_gvar_work() -> Box<BeWork> {
    Box::new(GvarWork {})
}

/// <https://learn.microsoft.com/en-us/typography/opentype/spec/otvaroverview#variation-data>
#[derive(Debug, Default)]
struct TupleBuilder {
    axis_names: Vec<String>,
    min: Vec<F2Dot14>,
    peak: Vec<F2Dot14>,
    max: Vec<F2Dot14>,
}

impl TupleBuilder {
    fn build(self) -> (Tuple, Tuple, Tuple) {
        (
            Tuple::new(self.min),
            Tuple::new(self.peak),
            Tuple::new(self.max),
        )
    }
}

impl From<&VariationRegion> for TupleBuilder {
    fn from(region: &VariationRegion) -> Self {
        let mut builder = TupleBuilder::default();
        for (axis_name, tent) in region.iter() {
            builder.axis_names.push(axis_name.clone());
            builder.min.push(F2Dot14::from_f32(tent.lower.to_f32()));
            builder.peak.push(F2Dot14::from_f32(tent.peak.to_f32()));
            builder.max.push(F2Dot14::from_f32(tent.upper.to_f32()));
        }
        trace!("{builder:?}");
        builder
    }
}

fn to_deltas(fragment: &GvarFragment) -> Vec<GlyphDeltas> {
    fragment
        .deltas
        .iter()
        .filter_map(|(region, deltas)| {
            if region.is_default() {
                return None;
            }

            // Variation of no point has limited entertainment value
            if deltas.is_empty() {
                return None;
            }

            // TODO: nice rounding on deltas
            let deltas: Vec<_> = deltas.iter().map(|v| (v.x as i16, v.y as i16)).collect();

            let tuple_builder: TupleBuilder = region.into();
            let (min, peak, max) = tuple_builder.build();
            Some(GlyphDeltas::new(peak, deltas, Some((min, max))))
        })
        .collect()
}

impl Work<Context, Error> for GvarWork {
    /// Generate [gvar](https://learn.microsoft.com/en-us/typography/opentype/spec/gvar)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // We built the gvar fragments alongside glyphs, now we need to glue them together into a gvar table
        let static_metadata = context.ir.get_final_static_metadata();

        let mut gids_and_deltas = Vec::new();

        let variations: Vec<_> = static_metadata
            .glyph_order
            .iter()
            .enumerate()
            .map(|(gid, gn)| {
                let gid = GlyphId::new(gid as u16);
                let gvar_fragment = context.get_gvar_fragment(gn);
                gids_and_deltas.push((gid, gvar_fragment.deltas.clone()));
                (gid, to_deltas(&gvar_fragment))
            })
            .map(|(gid, deltas)| GlyphVariations::new(gid, deltas))
            .collect();
        let gvar = Gvar::new(variations).map_err(Error::GvarError)?;

        let raw_gvar = Bytes::new(dump_table(&gvar).map_err(|report| Error::DumpTableError {
            report,
            context: "gvar".into(),
        })?);
        context.set_gvar(raw_gvar);

        Ok(())
    }
}
