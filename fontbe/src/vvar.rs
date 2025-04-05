//! Generates a [VVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/VVAR) table.

use std::collections::BTreeSet;

use fontdrasil::orchestration::{Access, AccessBuilder, Work};

use fontdrasil::types::GlyphName;
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::tables::vvar::Vvar;
use write_fonts::types::MajorMinor;

use crate::hvar::AdvanceDeltasBuilder;
use crate::metrics_and_limits::advance_height;
use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct VvarWork {}

pub fn create_vvar_work() -> Box<BeWork> {
    Box::new(VvarWork {})
}

impl Work<Context, AnyWorkId, Error> for VvarWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Vvar.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlobalMetrics)
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::Glyph(GlyphName::NOTDEF)) // Assumed in AdvanceDeltasBuilder
            .build()
    }

    /// Generate [VVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/VVAR)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();

        if static_metadata.axes.is_empty() {
            log::debug!("skipping VVAR, font has no axes");
            return Ok(());
        }
        if !static_metadata.build_vertical {
            log::debug!("skipping VVAR, font does not have vertical glyph metrics");
            return Ok(());
        }

        let var_model = &static_metadata.variation_model;

        let glyph_order = context.ir.glyph_order.get();
        let glyphs: Vec<_> = glyph_order
            .names()
            .map(|name| context.ir.glyphs.get(&FeWorkId::Glyph(name.clone())))
            .collect();
        let glyph_locations = glyphs.iter().flat_map(|glyph| glyph.sources().keys());

        let default_metrics = context
            .ir
            .global_metrics
            .get()
            .at(static_metadata.default_location());

        let mut glyph_advance_deltas =
            AdvanceDeltasBuilder::new(var_model.clone(), glyph_locations);
        let axis_tags = var_model
            .axes()
            .map(|axis| axis.tag)
            .collect::<BTreeSet<_>>();
        for glyph in glyphs.into_iter() {
            let name = glyph.name.clone();
            let advances = glyph
                .sources()
                .iter()
                // advances must be rounded before the computing deltas to match fontmake
                // https://github.com/googlefonts/fontc/issues/1043
                .map(|(loc, src)| {
                    (
                        loc.subset_axes(&axis_tags),
                        // TODO: Use metrics at location - blocked on interpolated metrics.
                        vec![advance_height(src, &default_metrics) as f64],
                    )
                })
                .collect();
            glyph_advance_deltas.add(name, advances)?;
        }

        let (varstore, varidx_map) = glyph_advance_deltas.build(glyph_order.len())?;

        let vvar = Vvar::new(
            MajorMinor::VERSION_1_0,
            varstore,
            varidx_map,
            None,
            None,
            None,
        );
        context.vvar.set(vvar);

        Ok(())
    }
}
