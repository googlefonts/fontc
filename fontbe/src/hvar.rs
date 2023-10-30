//! Generates an [HVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/HVAR) table.

use std::collections::{BTreeSet, HashMap};
use std::mem;
use std::sync::Arc;

use indexmap::IndexMap;

use font_types::MajorMinor;
use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, Work},
    types::Axis,
};
use fontir::{ir::Glyph, orchestration::WorkId as FeWorkId, variations::VariationModel};
use write_fonts::{
    dump_table,
    tables::{
        hvar::Hvar,
        variations::{ivs_builder::VariationStoreBuilder, DeltaSetIndexMap, VariationRegion},
    },
    validate::Validate,
    FontWrite, OtRound,
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

/// Compute the final size of a table, after it has been serialized to bytes
fn table_size<T>(table: &T, context: &str) -> Result<usize, Error>
where
    T: FontWrite + Validate,
{
    let data = dump_table(table).map_err(|e| Error::DumpTableError {
        e,
        context: context.to_string(),
    })?;
    Ok(data.len())
}

/// Helper to collect advance width deltas for all glyphs in a font
struct AdvanceWidthDeltas {
    /// Variation axes
    axes: Vec<Axis>,
    /// Sparse variation models, keyed by the set of locations they define
    models: IndexMap<BTreeSet<NormalizedLocation>, VariationModel>,
    /// Glyph's advance width deltas sorted by glyph order
    deltas: Vec<Vec<(VariationRegion, i16)>>,
}

impl AdvanceWidthDeltas {
    fn new(global_model: VariationModel) -> Self {
        let axes = global_model.axes().cloned().collect::<Vec<_>>();
        let global_locations = global_model.locations().cloned().collect::<BTreeSet<_>>();
        // using IndexMap to quickly get a ref to the first entry (in insertion order)
        // in the global_locations() method below
        let mut models = IndexMap::new();
        models.insert(global_locations, global_model);
        AdvanceWidthDeltas {
            axes,
            models,
            deltas: Vec::new(),
        }
    }

    fn global_locations(&self) -> &BTreeSet<NormalizedLocation> {
        self.models.first().unwrap().0
    }

    fn add(&mut self, glyph: &Glyph) -> Result<(), Error> {
        let mut advance_widths: HashMap<_, _> = glyph
            .sources()
            .iter()
            .map(|(loc, src)| (loc.clone(), vec![src.width]))
            .collect();
        let name = glyph.name.clone();
        let i = self.deltas.len();
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
            if i == 0 && name.as_str() == ".notdef" {
                let notdef_width = advance_widths.values().next().unwrap()[0];
                for loc in self.global_locations().iter() {
                    advance_widths
                        .entry(loc.clone())
                        .or_insert_with(|| vec![notdef_width]);
                }
            } else {
                // spare the model the work of computing no-op deltas
                self.deltas.push(Vec::new());
                return Ok(());
            }
        }
        let locations = advance_widths.keys().cloned().collect::<BTreeSet<_>>();
        let model = self.models.entry(locations).or_insert_with(|| {
            // this glyph defines its own set of locations, a new sparse model is needed
            VariationModel::new(advance_widths.keys().cloned().collect(), self.axes.clone())
                .unwrap()
        });
        self.deltas.push(
            model
                .deltas(&advance_widths)
                .map_err(|e| Error::GlyphDeltaError(name.clone(), e))?
                .into_iter()
                .filter_map(|(region, values)| {
                    if region.is_default() {
                        return None;
                    }
                    // Only 1 value per region for our input
                    assert!(values.len() == 1, "{} values?!", values.len());
                    Some((
                        region.to_write_fonts_variation_region(&self.axes),
                        values[0].ot_round(),
                    ))
                })
                .collect(),
        );
        Ok(())
    }

    fn is_single_model(&self) -> bool {
        self.models.len() == 1
    }

    fn iter(&self) -> impl Iterator<Item = &Vec<(VariationRegion, i16)>> {
        self.deltas.iter()
    }
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
        let var_model = &static_metadata.variation_model;
        let glyph_order = context.ir.glyph_order.get();

        let mut glyph_width_deltas = AdvanceWidthDeltas::new(var_model.clone());
        for name in glyph_order.iter() {
            let glyph = context.ir.glyphs.get(&FeWorkId::Glyph(name.clone()));
            glyph_width_deltas.add(glyph.as_ref())?;
        }

        let mut var_idxes = Vec::new();

        // if we have a single model, we can try to build a VariationStore with implicit variation
        // indices (a single ItemVariationData, outer index 0, inner index => gid).
        let direct_store = if glyph_width_deltas.is_single_model() {
            let mut direct_builder = VariationStoreBuilder::new_with_implicit_indices();
            for deltas in glyph_width_deltas.iter() {
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

        // also build an indirect VariationStore with a DeltaSetIndexMap to map gid => varidx
        let mut indirect_builder = VariationStoreBuilder::new();
        for deltas in glyph_width_deltas.iter() {
            var_idxes.push(indirect_builder.add_deltas(deltas.clone()));
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

        // Default to indirect, switch to direct if it's available and smaller
        let (mut varidx_map, mut varstore) = (Some(varidx_map), indirect_store);
        if let Some(direct_store) = direct_store {
            let direct_store_size = table_size(&direct_store, "ItemVariationStore")?;
            let indirect_store_size = table_size(&varstore, "ItemVariationStore")?;
            let varidx_map_size = table_size(&varidx_map, "DeltaSetIndexMap")?;

            if direct_store_size <= indirect_store_size + varidx_map_size {
                varidx_map = None;
                varstore = direct_store;
            }
        }

        let hvar = Hvar::new(MajorMinor::VERSION_1_0, varstore, varidx_map, None, None);
        context.hvar.set_unconditionally(hvar.into());

        Ok(())
    }
}
