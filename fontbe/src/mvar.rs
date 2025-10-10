//! Generates an [MVAR](https://learn.microsoft.com/en-us/typography/opentype/spec/MVAR) table.

use std::collections::BTreeMap;

use fontdrasil::{
    orchestration::{Access, AccessBuilder, Work},
    types::Axes,
    variations::ModelDeltas,
};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::{
    OtRound,
    tables::{
        mvar::{Mvar, ValueRecord},
        variations::{VariationRegion, ivs_builder::VariationStoreBuilder},
    },
    types::{MajorMinor, Tag},
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
    axes: Axes,
    /// Metrics deltas keyed by MVAR tag
    deltas: BTreeMap<Tag, Vec<(VariationRegion, i16)>>,
}

impl MvarBuilder {
    fn new(axes: Axes) -> Self {
        MvarBuilder {
            axes,
            deltas: BTreeMap::new(),
        }
    }

    fn add_deltas(&mut self, mvar_tag: Tag, deltas: &ModelDeltas<f64>) {
        if deltas.len() == 1 {
            let (region, _) = deltas.first().unwrap();
            assert!(region.is_default());
            // spare the model the work of encoding no-op deltas
            return;
        }
        let deltas: Vec<_> = deltas
            .iter()
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
            return;
        }
        self.deltas.insert(mvar_tag, deltas);
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
        let mut mvar_builder = MvarBuilder::new(static_metadata.axes.clone());
        for (metric, deltas) in metrics.iter() {
            // some of the GlobalMetric variants are not MVAR-relevant, e.g.
            // hhea ascender/descender/lineGap so we just skip those
            if let Some(mvar_tag) = metric.mvar_tag() {
                mvar_builder.add_deltas(mvar_tag, deltas);
            }
        }
        if let Some(mvar) = mvar_builder.build() {
            context.mvar.set(mvar);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use fontdrasil::{coords::NormalizedLocation, types::Axis};
    use fontir::ir::{GlobalMetric, GlobalMetrics, GlobalMetricsBuilder};
    use write_fonts::{
        dump_table,
        read::{
            FontData, FontRead,
            tables::{mvar as read_mvar, variations::ItemVariationData},
        },
        types::F2Dot14,
    };

    use super::*;
    use crate::test_util;

    fn new_mvar_builder(axes: Vec<Axis>) -> MvarBuilder {
        let axes = Axes::new(axes);
        MvarBuilder::new(axes)
    }

    fn build_metrics(
        metrics: &[(GlobalMetric, &[(&NormalizedLocation, f64)])],
        axes: &Axes,
    ) -> GlobalMetrics {
        metrics
            .iter()
            .flat_map(|(metric, values)| values.iter().map(move |value| (metric, value)))
            .fold(
                GlobalMetricsBuilder::new(),
                |mut builder, (&metric, (pos, value))| {
                    builder.set(metric, (*pos).clone(), *value);
                    builder
                },
            )
            .build(axes)
            .unwrap()
    }

    fn add_deltas(builder: &mut MvarBuilder, metrics: GlobalMetrics) {
        metrics.iter().for_each(|(metric, values)| {
            builder.add_deltas(metric.mvar_tag().unwrap(), values);
        });
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
        let axes = vec![test_util::axis("wght", 400.0, 400.0, 700.0)];
        let mut builder = new_mvar_builder(axes.clone());

        let metrics = build_metrics(
            &[(GlobalMetric::XHeight, &[(&regular, 500.0), (&bold, 550.0)])],
            &axes.into(),
        );
        add_deltas(&mut builder, metrics);

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
        let axes = vec![test_util::axis("wght", 400.0, 400.0, 700.0)];
        let mut builder = new_mvar_builder(axes.clone());

        let metrics = build_metrics(
            &[
                (GlobalMetric::XHeight, &[(&regular, 500.0), (&bold, 500.0)]),
                (
                    GlobalMetric::CapHeight,
                    &[(&regular, 800.0), (&bold, 800.0)],
                ),
            ],
            &axes.into(),
        );
        add_deltas(&mut builder, metrics);

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

        fn metric_delta(&self, mvar_tag: &str, coords: &[f64]) -> f64 {
            let mvar_tag = Tag::from_str(mvar_tag).unwrap();
            let coords: Vec<F2Dot14> = coords
                .iter()
                .map(|coord| F2Dot14::from_f32(*coord as _))
                .collect();
            self.mvar.metric_delta(mvar_tag, &coords).unwrap().to_f64()
        }
    }

    #[test]
    fn sparse_global_metrics() {
        let regular = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let medium = NormalizedLocation::for_pos(&[("wght", 0.5)]);
        let bold = NormalizedLocation::for_pos(&[("wght", 1.0)]);
        let axes = vec![test_util::axis("wght", 400.0, 400.0, 700.0)];
        let mut builder = new_mvar_builder(axes.clone());

        let metrics = build_metrics(
            &[
                (
                    // 'xhgt' defines a value for all three locations
                    GlobalMetric::XHeight,
                    &[(&regular, 500.0), (&medium, 530.0), (&bold, 550.0)],
                ),
                (
                    // 'strs' is sparse: defines a value for regular and bold, not medium
                    GlobalMetric::StrikeoutSize,
                    &[(&regular, 50.0), (&bold, 100.0)],
                ),
            ],
            &axes.into(),
        );
        add_deltas(&mut builder, metrics);

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
