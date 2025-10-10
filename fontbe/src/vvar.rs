//! Generates an [VVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/VVAR) table.

use fontdrasil::orchestration::AccessBuilder;

use fontdrasil::{
    orchestration::{Access, Work},
    types::GlyphName,
};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::tables::{
    variations::{DeltaSetIndexMap, ivs_builder::VariationStoreBuilder},
    vvar::Vvar,
};

use crate::metric_variations::{AdvanceDeltas, DeltaDirection, table_size};
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
            .variant(FeWorkId::Glyph(GlyphName::NOTDEF))
            .build()
    }

    /// Generate [VVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/VVAR)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        if static_metadata.axes.is_empty() {
            log::debug!("skipping VVAR, font has no axes");
            return Ok(());
        }
        // In Python we add VVAR only if we have a vmtx. If we are building vertical then we have a vmtx.
        // <https://github.com/googlefonts/fonttools/blob/03a3c8ed9e7ab35b5f219a6d25ee8081107ff2bb/Lib/fontTools/varLib/__init__.py#L1312>
        if !static_metadata.build_vertical {
            log::debug!("skipping VVAR, we aren't building vertical tables");
            return Ok(());
        }
        let var_model = &static_metadata.variation_model;
        let glyph_order = context.ir.glyph_order.get();
        let axis_count = var_model.axis_order().len().try_into().unwrap();
        let glyphs: Vec<_> = glyph_order
            .names()
            .map(|name| context.ir.glyphs.get(&FeWorkId::Glyph(name.clone())))
            .collect();
        let glyph_locations = glyphs.iter().flat_map(|glyph| glyph.sources().keys());
        let metrics = context.ir.global_metrics.get();

        let mut glyph_deltas = AdvanceDeltas::new(
            &static_metadata,
            glyph_locations,
            &metrics,
            DeltaDirection::Vertical,
        );
        for glyph in glyphs.into_iter() {
            glyph_deltas.add(glyph.as_ref())?;
        }

        // if we have a single model, we can try to build a VariationStore with implicit variation
        // indices (a single ItemVariationData, outer index 0, inner index => gid).
        let mut var_idxes = Vec::new();
        let direct_store = if glyph_deltas.is_single_model() {
            let mut direct_builder = VariationStoreBuilder::new_with_implicit_indices(axis_count);
            for deltas in glyph_deltas.iter() {
                var_idxes.push(direct_builder.add_deltas(deltas.clone()));
            }
            // sanity checks
            assert_eq!(var_idxes.len(), glyph_order.len());
            assert!(
                var_idxes
                    .drain(..)
                    .enumerate()
                    .all(|(i, idx)| i as u32 == idx)
            );
            // we don't use the returned (identity) map in this case
            Some(direct_builder.build().0)
        } else {
            None
        };

        // also build an indirect VariationStore with a DeltaSetIndexMap to map gid => varidx
        let mut indirect_builder = VariationStoreBuilder::new(axis_count);
        for deltas in glyph_deltas.iter() {
            var_idxes.push(indirect_builder.add_deltas(deltas.clone()));
        }
        let (indirect_store, varidx_map) = indirect_builder.build();

        // unwrap since VariationStoreBuilder guarantees that any temporary index returned by
        // add_deltas will exist in the returned map
        let varidx_map: DeltaSetIndexMap = var_idxes
            .into_iter()
            .map(|idx| varidx_map.get(idx).unwrap())
            .collect();

        // Default to indirect, switch to direct if it's available and smaller
        let (mut varidx_map, mut varstore) = (Some(varidx_map), indirect_store);
        if let Some(direct_store) = direct_store {
            let direct_store_size = table_size(&direct_store)?;
            let indirect_store_size = table_size(&varstore)?;
            let varidx_map_size = table_size(&varidx_map)?;

            if direct_store_size <= indirect_store_size + varidx_map_size {
                varidx_map = None;
                varstore = direct_store;
            }
        }

        let vvar = Vvar::new(varstore, varidx_map, None, None, None);
        context.vvar.set(vvar);

        Ok(())
    }
}
