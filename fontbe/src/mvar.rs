//! Generates an [MVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/MVAR) table.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use fontdrasil::orchestration::AccessBuilder;

use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, Work},
    types::Axis,
};
use fontir::{
    ir::GlobalMetricValues, orchestration::WorkId as FeWorkId, variations::VariationModel,
};
use write_fonts::types::MajorMinor;
use write_fonts::{
    tables::{
        mvar::{Mvar, ValueRecord},
        variations::{ivs_builder::VariationStoreBuilder, VariationRegion},
    },
    types::Tag,
    OtRound,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct MvarWork {}

pub fn create_mvar_work() -> Box<BeWork> {
    Box::new(MvarWork {})
}

/// Helper to build MVAR table from global metrics sources.
struct MvarBuilder {
    /// Variation axes
    axes: Vec<Axis>,
    /// Sparse variation models, keyed by the set of locations they define
    models: HashMap<BTreeSet<NormalizedLocation>, VariationModel>,
    /// Metrics deltas keyed by MVAR tag
    deltas: BTreeMap<Tag, Vec<(VariationRegion, i16)>>,
}

impl MvarBuilder {
    fn new(global_model: VariationModel) -> Self {
        let axes = global_model.axes().cloned().collect::<Vec<_>>();
        let global_locations = global_model.locations().cloned().collect::<BTreeSet<_>>();
        let mut models = HashMap::new();
        models.insert(global_locations, global_model);
        MvarBuilder {
            axes,
            models,
            deltas: BTreeMap::new(),
        }
    }

    fn add_sources(&mut self, mvar_tag: Tag, sources: &GlobalMetricValues) -> Result<(), Error> {
        let sources: HashMap<_, _> = sources
            .iter()
            .map(|(loc, src)| (loc.clone(), vec![src.into_inner() as f64]))
            .collect();
        if sources.len() == 1 {
            assert!(sources.keys().next().unwrap().is_default());
            // spare the model the work of computing no-op deltas
            return Ok(());
        }
        let locations = sources.keys().cloned().collect::<BTreeSet<_>>();
        let model = self.models.entry(locations).or_insert_with(|| {
            // this glyph defines its own set of locations, a new sparse model is needed
            VariationModel::new(sources.keys().cloned().collect(), self.axes.clone()).unwrap()
        });
        let deltas: Vec<_> = model
            .deltas(&sources)
            .map_err(|e| Error::MvarDeltaError(mvar_tag, e))?
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
            .collect();
        // don't encode no-op deltas
        if deltas.iter().all(|(_, delta)| *delta == 0) {
            return Ok(());
        }
        self.deltas.insert(mvar_tag, deltas);
        Ok(())
    }

    fn build(self) -> Option<Mvar> {
        let mut builder = VariationStoreBuilder::new(self.axes.len() as u16);
        let delta_ids = self
            .deltas
            .into_iter()
            .map(|(tag, deltas)| (tag, builder.add_deltas(deltas)))
            .collect::<Vec<_>>();

        let (varstore, index_map) = builder.build();

        let records = delta_ids
            .into_iter()
            .map(|(tag, temp_id)| {
                let varidx = index_map.get(temp_id).unwrap();
                ValueRecord::new(
                    tag,
                    varidx.delta_set_outer_index,
                    varidx.delta_set_inner_index,
                )
            })
            .collect::<Vec<_>>();

        (!records.is_empty()).then(|| Mvar::new(MajorMinor::VERSION_1_0, Some(varstore), records))
    }
}

impl Work<Context, AnyWorkId, Error> for MvarWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Mvar.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlobalMetrics)
            .build()
    }

    /// Generate [MVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/MVAR)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // for reference, fontTools MVAR building code can be found here:
        // https://github.com/fonttools/fonttools/blob/2dc887c/Lib/fontTools/varLib/__init__.py#L661-L736
        let static_metadata = context.ir.static_metadata.get();
        let metrics = context.ir.global_metrics.get();
        let var_model = &static_metadata.variation_model;

        let mut mvar_builder = MvarBuilder::new(var_model.clone());
        for (metric, values) in metrics.iter() {
            // some of the GlobalMetric variants are not MVAR-relevant, e.g.
            // hhea ascender/descender/lineGap so we just skip those
            if let Some(mvar_tag) = metric.mvar_tag() {
                mvar_builder.add_sources(mvar_tag, values)?;
            }
        }
        let mvar = mvar_builder.build();

        context.mvar.set_unconditionally(mvar.into());

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use fontdrasil::{
        coords::{CoordConverter, UserCoord},
        types::Axis,
    };
    use write_fonts::{
        dump_table,
        read::{
            tables::{mvar as read_mvar, variations::ItemVariationData},
            FontData, FontRead,
        },
        types::F2Dot14,
    };

    use super::*;

    fn axis(tag: &str, min: f32, default: f32, max: f32) -> Axis {
        let min = UserCoord::new(min);
        let default = UserCoord::new(default);
        let max = UserCoord::new(max);
        Axis {
            name: tag.to_string(),
            tag: Tag::from_str(tag).unwrap(),
            min,
            default,
            max,
            hidden: false,
            converter: CoordConverter::unmapped(min, default, max),
        }
    }

    fn new_mvar_builder(locations: Vec<&NormalizedLocation>, axes: Vec<Axis>) -> MvarBuilder {
        let locations = locations.into_iter().cloned().collect();
        let model = VariationModel::new(locations, axes).unwrap();
        MvarBuilder::new(model)
    }

    fn add_sources(
        builder: &mut MvarBuilder,
        mvar_tag: &str,
        sources: &[(&NormalizedLocation, f32)],
    ) {
        let sources = sources
            .iter()
            .map(|(loc, value)| ((*loc).clone(), (*value).into()))
            .collect::<HashMap<_, _>>();
        builder
            .add_sources(Tag::from_str(mvar_tag).unwrap(), &sources)
            .unwrap();
    }

    fn delta_sets(var_data: &ItemVariationData) -> Vec<Vec<i32>> {
        (0..var_data.item_count())
            .map(|i| var_data.delta_set(i).collect::<Vec<_>>())
            .collect()
    }

    #[test]
    fn smoke_test() {
        let regular = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let bold = NormalizedLocation::for_pos(&[("wght", 1.0)]);
        let mut builder = new_mvar_builder(
            vec![&regular, &bold],
            vec![axis("wght", 400.0, 400.0, 700.0)],
        );

        add_sources(&mut builder, "xhgt", &[(&regular, 500.0), (&bold, 550.0)]);

        let Some(mvar) = builder.build() else {
            panic!("no MVAR?!");
        };

        let bytes = dump_table(&mvar).unwrap();
        let mvar = read_mvar::Mvar::read(FontData::new(&bytes)).unwrap();

        assert_eq!(mvar.version(), MajorMinor::VERSION_1_0);
        assert_eq!(mvar.value_records().len(), 1);

        let rec = &mvar.value_records()[0];
        assert_eq!(rec.value_tag(), Tag::new(b"xhgt"));
        assert_eq!(rec.delta_set_outer_index(), 0);
        assert_eq!(rec.delta_set_inner_index(), 0);

        let Some(Ok(varstore)) = mvar.item_variation_store() else {
            panic!("MVAR has no ItemVariationStore?!");
        };

        assert_eq!(varstore.variation_region_list().unwrap().region_count(), 1);
        assert_eq!(varstore.item_variation_data_count(), 1);

        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(vardata.region_indexes(), &[0]);
        assert_eq!(delta_sets(&vardata), vec![vec![50]]);
    }

    #[test]
    fn no_variations_no_mvar() {
        let regular = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let bold = NormalizedLocation::for_pos(&[("wght", 1.0)]);
        let mut builder = new_mvar_builder(
            vec![&regular, &bold],
            vec![axis("wght", 400.0, 400.0, 700.0)],
        );

        add_sources(&mut builder, "xhgt", &[(&regular, 500.0), (&bold, 500.0)]);
        add_sources(&mut builder, "cpht", &[(&regular, 800.0), (&bold, 800.0)]);

        // hence no MVAR needed
        assert!(builder.build().is_none());
    }

    struct MvarReader<'a> {
        mvar: read_mvar::Mvar<'a>,
    }

    impl<'a> MvarReader<'a> {
        fn new(mvar: read_mvar::Mvar<'a>) -> Self {
            Self { mvar }
        }

        fn metric_delta(&self, mvar_tag: &str, coords: &[f32]) -> f64 {
            let mvar_tag = Tag::from_str(mvar_tag).unwrap();
            let coords: Vec<F2Dot14> = coords
                .iter()
                .map(|coord| F2Dot14::from_f32(*coord))
                .collect();
            self.mvar.metric_delta(mvar_tag, &coords).unwrap().to_f64()
        }
    }

    #[test]
    fn sparse_global_metrics() {
        let regular = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let medium = NormalizedLocation::for_pos(&[("wght", 0.5)]);
        let bold = NormalizedLocation::for_pos(&[("wght", 1.0)]);
        let mut builder = new_mvar_builder(
            vec![&regular, &medium, &bold],
            vec![axis("wght", 400.0, 400.0, 700.0)],
        );
        // 'xhgt' defines a value for all three locations
        add_sources(
            &mut builder,
            "xhgt",
            &[(&regular, 500.0), (&medium, 530.0), (&bold, 550.0)],
        );
        // 'strs' is sparse: defines a value for regular and bold, not medium
        add_sources(&mut builder, "strs", &[(&regular, 50.0), (&bold, 100.0)]);

        let Some(mvar) = builder.build() else {
            panic!("no MVAR?!");
        };

        let bytes = dump_table(&mvar).unwrap();
        let mvar = read_mvar::Mvar::read(FontData::new(&bytes)).unwrap();

        assert_eq!(mvar.value_records().len(), 2);
        assert_eq!(mvar.value_records()[0].value_tag(), Tag::new(b"strs"));
        assert_eq!(mvar.value_records()[1].value_tag(), Tag::new(b"xhgt"));

        let mvar = MvarReader::new(mvar);
        assert_eq!(mvar.metric_delta("xhgt", &[0.0]), 0.0);
        assert_eq!(mvar.metric_delta("xhgt", &[0.5]), 30.0); // not 25.0
        assert_eq!(mvar.metric_delta("xhgt", &[1.0]), 50.0);

        assert_eq!(mvar.metric_delta("strs", &[0.0]), 0.0);
        assert_eq!(mvar.metric_delta("strs", &[0.5]), 25.0); // interpolated
        assert_eq!(mvar.metric_delta("strs", &[1.0]), 50.0);
    }
}
