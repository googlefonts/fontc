//! Generates an [HVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/HVAR) table.

use std::collections::HashMap;
use std::sync::Arc;

use font_types::MajorMinor;
use fontdrasil::orchestration::{Access, Work};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::{
    tables::{
        hvar::Hvar,
        layout::VariationIndex,
        variations::{
            ivs_builder::{DirectVariationStoreBuilder, VariationStoreBuilder},
            DeltaSetIndexMap, VariationRegion,
        },
    },
    OtRound,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct HvarWork {}

pub fn create_hvar_work() -> Box<BeWork> {
    Box::new(HvarWork {})
}

impl Work<Context, AnyWorkId, Error> for HvarWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Hvar.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Custom(Arc::new(|id| {
            matches!(
                id,
                AnyWorkId::Fe(FeWorkId::Glyph(..))
                    | AnyWorkId::Fe(FeWorkId::StaticMetadata)
                    | AnyWorkId::Fe(FeWorkId::GlyphOrder)
            )
        }))
    }

    /// Generate [HVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/HVAR)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        // TODO one sparse model per glyph
        let var_model = &static_metadata.variation_model;
        let glyph_order = context.ir.glyph_order.get();

        let all_glyph_width_deltas: Vec<Vec<(VariationRegion, i16)>> = glyph_order
            .iter()
            .map(|gn| {
                let advance_widths: HashMap<_, _> = context
                    .ir
                    .glyphs
                    .get(&FeWorkId::Glyph(gn.clone()))
                    .sources()
                    .iter()
                    .map(|(loc, src)| (loc.clone(), vec![src.width]))
                    .collect();
                var_model
                    .deltas(&advance_widths)
                    .unwrap()
                    .into_iter()
                    .filter_map(|(region, values)| {
                        if region.is_default() {
                            return None;
                        }
                        let mut region_axes = Vec::with_capacity(static_metadata.axes.len());
                        for axis in static_metadata.axes.iter() {
                            let tent = region.get(&axis.tag).unwrap();
                            region_axes.push(tent.to_region_axis_coords());
                        }
                        // Only 1 value per region for our input
                        assert!(values.len() == 1, "{} values?!", values.len());
                        let value = values[0].ot_round();
                        Some((VariationRegion { region_axes }, value))
                    })
                    .collect()
            })
            .collect();

        let mut direct_builder = DirectVariationStoreBuilder::default();
        let mut var_idxes = Vec::new();
        for deltas in &all_glyph_width_deltas {
            var_idxes.push(direct_builder.add_deltas(deltas.clone()));
        }
        // sanity check
        assert_eq!(var_idxes, (0..glyph_order.len() as u32).collect::<Vec<_>>());
        let direct_store = direct_builder.build();

        var_idxes.clear();
        let mut indirect_builder = VariationStoreBuilder::default();
        for deltas in all_glyph_width_deltas {
            var_idxes.push(indirect_builder.add_deltas(deltas));
        }
        let (indirect_store, varidx_map) = indirect_builder.build();

        let varidx_map = DeltaSetIndexMap::from_vec(
            var_idxes
                .into_iter()
                .map(|idx| {
                    varidx_map
                        .get(idx as u32)
                        .unwrap_or_else(|| VariationIndex::new(0xFFFF_u16, 0xFFFF_u16))
                })
                .collect(),
        );

        // let hvar = Hvar::new(MajorMinor::VERSION_1_0, direct_store, None, None, None);
        let hvar = Hvar::new(
            MajorMinor::VERSION_1_0,
            indirect_store,
            Some(varidx_map),
            None,
            None,
        );

        context.hvar.set_unconditionally(hvar.into());

        Ok(())
    }
}
