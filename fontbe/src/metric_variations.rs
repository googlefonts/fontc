//! Helpers for production of the
//! [HVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/HVAR),
//! [VVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/VVAR) tables

use std::any::type_name;
use std::collections::{BTreeSet, HashMap, HashSet};

use fontdrasil::{
    coords::NormalizedLocation,
    types::{Axes, GlyphName},
    variations::VariationModel,
};
use fontir::ir::{GlobalMetrics, GlobalMetricsInstance, Glyph, StaticMetadata};
use write_fonts::{
    FontWrite, OtRound, dump_table, tables::variations::VariationRegion, validate::Validate,
};

use crate::error::Error;

/// Compute the final size of a table, after it has been serialized to bytes
pub fn table_size<T>(table: &T) -> Result<usize, Error>
where
    T: FontWrite + Validate,
{
    let data = dump_table(table).map_err(|e| Error::DumpTableError {
        e,
        context: type_name::<T>().to_string(),
    })?;
    Ok(data.len())
}

/// Which way a delta goes.
///
/// Impacts how the size of the glyph is accessed
pub(crate) enum DeltaDirection {
    Horizontal,
    Vertical,
}

/// Helper to collect advance width or height deltas for all glyphs in a font
pub(crate) struct AdvanceDeltas {
    /// Variation axes
    axes: Axes,
    /// Sparse variation models, keyed by the set of locations they define
    models: HashMap<BTreeSet<NormalizedLocation>, VariationModel>,
    /// Glyph's advance width deltas sorted by glyph order
    deltas: Vec<Vec<(VariationRegion, i16)>>,
    /// All the glyph locations that are defined in the font
    glyph_locations: HashSet<NormalizedLocation>,
    /// Cached global metrics at each location (only populated for Vertical direction)
    metrics_cache: HashMap<NormalizedLocation, GlobalMetricsInstance>,
    direction: DeltaDirection,
}

impl AdvanceDeltas {
    pub(crate) fn new<'a>(
        static_metadata: &StaticMetadata,
        glyph_locations: impl IntoIterator<Item = &'a NormalizedLocation>,
        global_metrics: &GlobalMetrics,
        direction: DeltaDirection,
    ) -> Self {
        let axes = static_metadata.axes.clone();
        let global_locations = static_metadata
            .variation_model
            .locations()
            .cloned()
            .collect::<BTreeSet<_>>();
        let mut models = HashMap::new();
        models.insert(global_locations, static_metadata.variation_model.clone());

        // Collect unique glyph locations, pruning axes that are not in the global model
        // (e.g. 'point' axes) which might be confused for a distinct sub-model
        // https://github.com/googlefonts/fontc/issues/1256
        let glyph_locations: HashSet<NormalizedLocation> = glyph_locations
            .into_iter()
            .map(|loc| loc.subset_axes(&static_metadata.axes))
            .collect();

        // Pre-compute metrics for all locations if we're computing vertical metrics
        // This avoids repeated interpolation when processing each glyph
        let metrics_cache = match direction {
            DeltaDirection::Vertical => glyph_locations
                .iter()
                .map(|loc| (loc.clone(), global_metrics.at(loc)))
                .collect(),
            DeltaDirection::Horizontal => HashMap::new(),
        };

        AdvanceDeltas {
            axes,
            models,
            deltas: Vec::new(),
            glyph_locations,
            metrics_cache,
            direction,
        }
    }

    pub(crate) fn add(&mut self, glyph: &Glyph) -> Result<(), Error> {
        let mut advances: HashMap<_, Vec<f64>> = Default::default();
        for (loc, glyph_instance) in glyph.sources().iter() {
            let loc = loc.subset_axes(&self.axes);
            // Only compute metrics when needed (for vertical direction)
            // For horizontal, we just need glyph_instance.width which doesn't require metrics
            let advance = match self.direction {
                DeltaDirection::Horizontal => glyph_instance.width.ot_round(),
                DeltaDirection::Vertical => {
                    let metrics = self
                        .metrics_cache
                        .get(&loc)
                        .expect("metrics should be pre-computed for all glyph locations");
                    glyph_instance.height(metrics) as f64
                }
            };
            advances.insert(loc, vec![advance]);
        }
        let name = glyph.name.clone();
        let i = self.deltas.len();
        if advances.len() == 1 {
            assert!(advances.keys().next().unwrap().is_default());
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
            if i == 0 && name == GlyphName::NOTDEF {
                let notdef_dim = advances.values().next().unwrap()[0];
                for loc in self.glyph_locations.iter() {
                    advances
                        .entry(loc.clone())
                        .or_insert_with(|| vec![notdef_dim]);
                }
            } else {
                // spare the model the work of computing no-op deltas
                self.deltas.push(Vec::new());
                return Ok(());
            }
        }
        let locations = advances.keys().cloned().collect::<BTreeSet<_>>();
        let model = self.models.entry(locations).or_insert_with(|| {
            // this glyph defines its own set of locations, a new sparse model is needed
            VariationModel::new(advances.keys().cloned().collect(), self.axes.axis_order())
        });
        self.deltas.push(
            model
                .deltas(&advances)
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

    pub(crate) fn is_single_model(&self) -> bool {
        self.models.len() == 1
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Vec<(VariationRegion, i16)>> {
        self.deltas.iter()
    }
}
