//! Generates an [HVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/HVAR) table.

use std::collections::{BTreeSet, HashMap};
use std::mem;
use std::sync::Arc;

use font_types::MajorMinor;
use fontdrasil::orchestration::{Access, Work};
use fontir::{orchestration::WorkId as FeWorkId, variations::VariationModel};
use write_fonts::{
    dump_table,
    tables::{
        hvar::Hvar,
        variations::{ivs_builder::VariationStoreBuilder, DeltaSetIndexMap, VariationRegion},
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
        let axes = &static_metadata.axes;
        let var_model = &static_metadata.variation_model;
        let global_locations = var_model.locations().cloned().collect::<BTreeSet<_>>();
        let mut models = HashMap::new();
        models.insert(global_locations.clone(), var_model.clone());
        let glyph_order = context.ir.glyph_order.get();

        let mut single_model = true;
        let all_glyph_width_deltas: Vec<Vec<(VariationRegion, i16)>> = glyph_order
            .iter()
            .enumerate()
            .map(|(i, gn)| {
                let mut advance_widths: HashMap<_, _> = context
                    .ir
                    .glyphs
                    .get(&FeWorkId::Glyph(gn.clone()))
                    .sources()
                    .iter()
                    .map(|(loc, src)| (loc.clone(), vec![src.width]))
                    .collect();
                if advance_widths.len() == 1 {
                    assert!(advance_widths.keys().next().unwrap().is_default());
                    // this glyph has no variations (it's only defined at the default location),
                    // therefore the deltas returned from VariationModel will be an empty Vec.
                    // However, when this is the first .notdef glyph we would like to treat it
                    // specially in order to match the output of fontTools.varLib.
                    // In fonttools, all master TTFs have a .notdef glyph as their first glyph; in fontc,
                    // unless the input source defines a .notdef, only a default instance is generated.
                    // And that's ok for gvar, however for HVAR the order in which regions and associated
                    // deltas are added to VariationStoreBuilder, one glyph at a time, can produce
                    // different orderings of the ItemVariationStore.VariationRegionList (newly seen
                    // regions get appended, and existing regions reused).
                    // So, to match the VarRegionList produced by fontTools, we need to make the deltaset
                    // for the first .notdef glyph similarly "dense", by copying its default instance to
                    // all other glyph locations...
                    if i == 0 && gn.as_str() == ".notdef" {
                        let notdef_width = advance_widths.values().next().unwrap()[0];
                        for loc in global_locations.iter() {
                            advance_widths
                                .entry(loc.clone())
                                .or_insert_with(|| vec![notdef_width]);
                        }
                    } else {
                        return Vec::new();
                    }
                }
                let locations = advance_widths.keys().cloned().collect::<BTreeSet<_>>();
                let model = models.entry(locations).or_insert_with(|| {
                    single_model = false;
                    VariationModel::new(advance_widths.keys().cloned().collect(), axes.clone())
                        .unwrap()
                });
                model
                    .deltas(&advance_widths)
                    .unwrap() // TODO handle error
                    .into_iter()
                    .filter_map(|(region, values)| {
                        if region.is_default() {
                            return None;
                        }
                        let mut region_axes = Vec::with_capacity(static_metadata.axes.len());
                        for axis in static_metadata.axes.iter() {
                            let tent = region.get(&axis.tag).unwrap(); // TODO handle error?
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

        let mut var_idxes = Vec::new();

        let direct_store = if single_model {
            let mut direct_builder = VariationStoreBuilder::new_with_implicit_indices();
            for deltas in &all_glyph_width_deltas {
                var_idxes.push(direct_builder.add_deltas(deltas.clone()));
            }
            // sanity check
            assert_eq!(
                mem::take(&mut var_idxes),
                (0..glyph_order.len() as u32).collect::<Vec<_>>()
            );
            // we don't use the returned (identity) map in this case
            Some(direct_builder.build().0)
        } else {
            None
        };

        let mut indirect_builder = VariationStoreBuilder::new();
        for deltas in all_glyph_width_deltas {
            var_idxes.push(indirect_builder.add_deltas(deltas));
        }
        let (indirect_store, varidx_map) = indirect_builder.build();

        let varidx_map: DeltaSetIndexMap = var_idxes
            .into_iter()
            .map(|idx| {
                varidx_map
                    .get(idx)
                    .unwrap_or_else(|| panic!("varidx_map missing index {}", idx))
            })
            .collect();

        // use the most compact representation
        // TODO handle errors
        let use_direct = if direct_store.is_some() {
            let direct_store_bytes = dump_table(direct_store.as_ref().unwrap()).unwrap();
            let direct_store_size = direct_store_bytes.len();

            let indirect_store_bytes = dump_table(&indirect_store).unwrap();
            let varidx_map_bytes = dump_table(&varidx_map).unwrap();
            let indirect_store_size = indirect_store_bytes.len() + varidx_map_bytes.len();

            direct_store_size <= indirect_store_size
        } else {
            false
        };

        let hvar = if use_direct {
            Hvar::new(
                MajorMinor::VERSION_1_0,
                direct_store.unwrap(),
                None,
                None,
                None,
            )
        } else {
            Hvar::new(
                MajorMinor::VERSION_1_0,
                indirect_store,
                Some(varidx_map),
                None,
                None,
            )
        };

        context.hvar.set_unconditionally(hvar.into());

        Ok(())
    }
}
