//! Generates a [avar](https://learn.microsoft.com/en-us/typography/opentype/spec/avar) table.

use std::collections::{HashMap, HashSet};

use fontdrasil::{
    coords::{NormalizedCoord, NormalizedLocation},
    orchestration::{Access, Work},
    types::Axis,
    variations::{VariationModel, resolve_variable_metric},
};
use fontir::{ir::StaticMetadata, orchestration::WorkId as FeWorkId};
use log::debug;
use write_fonts::{
    read::tables::variations::NO_VARIATION_INDEX,
    tables::{
        avar::{Avar, AxisValueMap, SegmentMaps},
        variations::{DeltaSetIndexMap, ItemVariationStore, ivs_builder::VariationStoreBuilder},
    },
    types::F2Dot14,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct AvarWork {}

pub fn create_avar_work() -> Box<BeWork> {
    Box::new(AvarWork {})
}

/// Return a default avar SegmentMaps containing the required {-1:-1, 0:0, 1:1} maps
fn default_segment_map() -> SegmentMaps {
    // The OT avar spec would allow us to leave the axis value maps empty, however some
    // implementations want the 3 required maps to always be present even when the default
    // normalization for an axis was not modified.
    // We are matching fontTools.varLib here:
    // https://github.com/fonttools/fonttools/blob/51e70f9/Lib/fontTools/varLib/__init__.py#L151-L157
    // https://learn.microsoft.com/en-us/typography/opentype/spec/avar#table-formats
    SegmentMaps::new(vec![
        AxisValueMap::new(F2Dot14::from_f32(-1.0), F2Dot14::from_f32(-1.0)),
        AxisValueMap::new(F2Dot14::from_f32(0.0), F2Dot14::from_f32(0.0)),
        AxisValueMap::new(F2Dot14::from_f32(1.0), F2Dot14::from_f32(1.0)),
    ])
}

fn to_segment_map(axis: &Axis) -> SegmentMaps {
    let default_converter = axis.default_converter();

    // We have to walk twice but we don't expect there to be a lot of values so don't stress

    // (default normalization, actual normalization) tuples
    let mut mappings: Vec<(NormalizedCoord, NormalizedCoord)> = axis
        .converter
        .iter()
        .map(|(user, _, norm)| (user.to_normalized(&default_converter), norm))
        .collect();

    // Coordinate conversion MUST have a default, but it might only extend in one direction from it
    // For example, weight 400-700 with default 400 will have no entry for -1 in coordinate conversion
    let (min, max) = mappings
        .iter()
        .map(|(n1, n2)| (*n1.into_inner(), *n2.into_inner()))
        .reduce(|(min, max), (maybe_min, maybe_max)| (min.min(maybe_min), max.max(maybe_max)))
        .unwrap();
    if min != -1.0 {
        mappings.insert(0, (NormalizedCoord::new(-1.0), NormalizedCoord::new(-1.0)));
    }
    if max != 1.0 {
        mappings.push((NormalizedCoord::new(1.0), NormalizedCoord::new(1.0)));
    }

    // avar maps from the default normalization to the actual one,
    // using normalized values on both sides.
    // All identity mappings are not interesting so we return the default mapping.
    if mappings.iter().all(|(k, v)| k == v) {
        return default_segment_map();
    }

    let mappings = mappings
        .iter()
        .map(|(default_norm, actual_norm)| {
            AxisValueMap::new((*default_norm).into(), (*actual_norm).into())
        })
        .collect();

    SegmentMaps::new(mappings)
}

/// The avar version 2 variation store, one delta set per fvar axis.
///
/// Port of the mapping part of fontTools `_add_avar`:
/// <https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Lib/fontTools/varLib/__init__.py#L245-L293>
fn to_var_store(
    static_metadata: &StaticMetadata,
) -> Result<Option<(Option<DeltaSetIndexMap>, ItemVariationStore)>, Error> {
    let mappings = &static_metadata.axis_mappings;
    if mappings.is_empty() {
        return Ok(None);
    }
    let axes = &static_metadata.axes;
    let axis_tags = axes.axis_order();

    let (mut input_locations, mut output_locations): (Vec<_>, Vec<_>) = mappings
        .iter()
        .map(|mapping| {
            let mut input = mapping.input.clone();
            input.fit_to_axes(&axis_tags);
            (input, mapping.output.clone())
        })
        .unzip();

    // If base-master is missing, insert it at zero location.
    if !input_locations.iter().any(NormalizedLocation::is_default) {
        input_locations.insert(0, NormalizedLocation::new());
        output_locations.insert(0, NormalizedLocation::new());
    }

    let mut locations = HashSet::new();
    for loc in input_locations.iter() {
        if !locations.insert(loc.clone()) {
            return Err(Error::DuplicateAxisMapping(loc.clone()));
        }
    }
    let model = VariationModel::new(locations, axis_tags.clone());

    let mut store_builder = VariationStoreBuilder::new(axes.len() as u16);
    let mut var_idxes = Vec::with_capacity(axis_tags.len());
    for tag in axis_tags.iter() {
        let master_values: HashMap<_, _> = output_locations
            .iter()
            .zip(input_locations.iter())
            .map(|(vo, vi)| {
                let value = vo
                    .get(*tag)
                    .map(|vo| {
                        let v = vo.to_f64() - vi.get(*tag).unwrap_or_default().to_f64();
                        F2Dot14::from_f64(v).to_bits()
                    })
                    .unwrap_or(0);
                (vi.clone(), value)
            })
            .collect();
        let (_, deltas) =
            resolve_variable_metric(&model, axes, &master_values).map_err(Error::DeltaError)?;
        var_idxes.push(
            (!deltas.iter().all(|(_, delta)| *delta == 0))
                .then(|| store_builder.add_deltas(deltas)),
        );
    }

    let (store, optimized) = store_builder.build();
    let var_idxes: Vec<u32> = var_idxes
        .into_iter()
        .map(|value| {
            value
                .map(|value| optimized.get(value).unwrap().into())
                .unwrap_or(NO_VARIATION_INDEX)
        })
        .collect();
    let var_idx_map = (!var_idxes.iter().enumerate().all(|(i, v)| *v == i as u32))
        .then(|| var_idxes.into_iter().collect());
    Ok(Some((var_idx_map, store)))
}

impl Work<Context, AnyWorkId, Error> for AvarWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Avar.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Variant(AnyWorkId::Fe(FeWorkId::StaticMetadata))
    }

    /// Generate [avar](https://learn.microsoft.com/en-us/typography/opentype/spec/avar)
    ///
    /// See also <https://learn.microsoft.com/en-us/typography/opentype/spec/otvaroverview#CSN>
    #[tracing::instrument(name = "fontbe::AvarWork::exec", skip_all)]
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        // Guard clause: don't produce avar for a static font
        if static_metadata.axes.is_empty() {
            debug!("Skip avar; this is not a variable font");
            return Ok(());
        }
        let axis_segment_maps: Vec<_> = static_metadata.axes.iter().map(to_segment_map).collect();
        // only when all the segment maps are uninteresting and there are no
        // mappings, we can omit avar
        let avar = match to_var_store(&static_metadata)? {
            Some((axis_index_map, var_store)) => Some(Avar {
                axis_segment_maps,
                axis_index_map: axis_index_map.into(),
                var_store: var_store.into(),
            }),
            None if axis_segment_maps.iter().any(|segmap| !segmap.is_identity()) => {
                Some(Avar::new(axis_segment_maps))
            }
            None => None,
        };
        context.avar.set(avar);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::{
        coords::{CoordConverter, DesignCoord, NormalizedCoord, NormalizedLocation, UserCoord},
        types::Axis,
    };
    use fontir::ir::{AxisMapping, StaticMetadata};
    use std::{cmp, str::FromStr};
    use write_fonts::{
        dump_table,
        read::{
            FontData, FontRead,
            tables::{
                avar::Avar,
                variations::{DeltaSetIndex, ItemVariationStore},
            },
        },
        tables::avar::SegmentMaps,
        types::{MajorMinor, Tag},
    };

    use crate::error::Error;

    use super::{default_segment_map, to_segment_map, to_var_store};

    fn axis(tag: &str, mappings: Vec<(UserCoord, DesignCoord)>, default_idx: usize) -> Axis {
        let default_idx = cmp::min(mappings.len() - 1, default_idx);
        Axis {
            name: tag.to_string(),
            tag: Tag::from_str(tag).unwrap(),
            min: *mappings.iter().map(|(u, _)| u).min().unwrap(),
            default: mappings[default_idx].0,
            max: *mappings.iter().map(|(u, _)| u).max().unwrap(),
            hidden: false,
            converter: CoordConverter::new(mappings, default_idx).unwrap(),
            localized_names: Default::default(),
        }
    }

    fn round4(v: f32) -> f32 {
        (v * 10000.0).round() / 10000.0
    }

    fn dump(segmap: SegmentMaps) -> Vec<(f32, f32)> {
        segmap
            .axis_value_maps
            .iter()
            .map(|av| (av.from_coordinate.to_f32(), av.to_coordinate.to_f32()))
            .map(|(from, to)| (round4(from), round4(to)))
            .collect()
    }

    #[test]
    fn up_to_three_points_does_nothing() {
        let mappings = [
            (UserCoord::new(100.0), DesignCoord::new(-10.0)),
            (UserCoord::new(400.0), DesignCoord::new(0.0)),
            (UserCoord::new(700.0), DesignCoord::new(20.0)),
        ];
        for i in 1..mappings.len() {
            let mappings = mappings[0..i].to_vec();
            assert_eq!(
                to_segment_map(&axis("TEST", mappings, 1)),
                default_segment_map()
            );
        }
    }

    #[test]
    fn simple_functional_segment_map() {
        let mappings = vec![
            (UserCoord::new(100.0), DesignCoord::new(-10.0)),
            (UserCoord::new(400.0), DesignCoord::new(0.0)),
            (UserCoord::new(700.0), DesignCoord::new(19.0)),
            (UserCoord::new(800.0), DesignCoord::new(20.0)),
        ];
        assert_eq!(
            vec![(-1.0, -1.0), (0.0, 0.0), (0.75, 0.95), (1.0, 1.0),],
            dump(to_segment_map(&axis("TEST", mappings, 1)))
        );
    }

    /// In fonts that have 3+ mappings but all are right or left of default
    /// we were doing silly things
    #[test]
    fn adds_implicit_mappings() {
        let mappings = vec![
            (UserCoord::new(400.0), DesignCoord::new(380.0)),
            (UserCoord::new(500.0), DesignCoord::new(555.0)),
            (UserCoord::new(700.0), DesignCoord::new(734.0)),
        ];
        assert_eq!(
            vec![(-1.0, -1.0), (0.0, 0.0), (0.3333, 0.4943), (1.0, 1.0),],
            dump(to_segment_map(&axis("TEST", mappings, 0)))
        );
    }

    #[test]
    fn zero_zero_map_should_always_be_present() {
        // "wdth" axis mappings from NotoSerif.glyphspackage, followed by the
        // expected avar mappings.
        // A change in the implementation of core::slice::binary_search in Rust
        // 1.83-nightly was causing the 0:0 map to be omitted from the avar table.
        // https://github.com/googlefonts/fontc/issues/933
        let mappings = vec![
            (UserCoord::new(62.5), DesignCoord::new(70.0)),
            (UserCoord::new(75.0), DesignCoord::new(79.0)),
            (UserCoord::new(87.5), DesignCoord::new(89.0)),
            (UserCoord::new(100.0), DesignCoord::new(100.0)),
        ];
        assert_eq!(
            vec![
                (-1.0, -1.0),
                (-0.6667, -0.7),
                (-0.3333, -0.3666),
                (0.0, 0.0),
                (1.0, 1.0)
            ],
            dump(to_segment_map(&axis("TEST", mappings, 3)))
        );
    }

    fn unmapped_axis(tag: &str, min: f64, default: f64, max: f64) -> Axis {
        let (min, default, max) = (
            UserCoord::new(min),
            UserCoord::new(default),
            UserCoord::new(max),
        );
        Axis {
            name: tag.to_string(),
            tag: Tag::from_str(tag).unwrap(),
            min,
            default,
            max,
            hidden: false,
            converter: CoordConverter::unmapped(min, default, max),
            localized_names: Default::default(),
        }
    }

    fn static_metadata(axes: Vec<Axis>, axis_mappings: Vec<AxisMapping>) -> StaticMetadata {
        let mut static_metadata = StaticMetadata::new(
            1000,
            Default::default(),
            axes,
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
            None,
            false,
        )
        .unwrap();
        static_metadata.axis_mappings = axis_mappings;
        static_metadata
    }

    fn location(coords: &[(&str, f64)]) -> NormalizedLocation {
        coords
            .iter()
            .map(|(tag, value)| (Tag::from_str(tag).unwrap(), NormalizedCoord::new(*value)))
            .collect()
    }

    fn mapping(input: &[(&str, f64)], output: &[(&str, f64)]) -> AxisMapping {
        AxisMapping {
            input: location(input),
            output: location(output),
        }
    }

    /// The avar table of `static_metadata`, built like `AvarWork::exec` builds it.
    fn avar_bytes(static_metadata: &StaticMetadata) -> Vec<u8> {
        let axis_segment_maps = static_metadata.axes.iter().map(to_segment_map).collect();
        let (axis_index_map, var_store) = to_var_store(static_metadata).unwrap().unwrap();
        let avar = write_fonts::tables::avar::Avar {
            axis_segment_maps,
            axis_index_map: axis_index_map.into(),
            var_store: var_store.into(),
        };
        dump_table(&avar).unwrap()
    }

    /// The (from, to) pairs of every segment map.
    fn segment_maps(avar: &Avar) -> Vec<Vec<(f32, f32)>> {
        avar.axis_segment_maps()
            .iter()
            .take(avar.axis_count() as usize)
            .map(|segmap| {
                segmap
                    .unwrap()
                    .axis_value_maps()
                    .iter()
                    .map(|av| {
                        (
                            round4(av.from_coordinate().to_f32()),
                            round4(av.to_coordinate().to_f32()),
                        )
                    })
                    .collect()
            })
            .collect()
    }

    /// The (start, peak, end) coordinates of every region axis of every region.
    fn regions(var_store: &ItemVariationStore) -> Vec<Vec<(f32, f32, f32)>> {
        var_store
            .variation_region_list()
            .unwrap()
            .variation_regions()
            .iter()
            .map(|region| {
                region
                    .unwrap()
                    .region_axes()
                    .iter()
                    .map(|axis| {
                        (
                            round4(axis.start_coord().to_f32()),
                            round4(axis.peak_coord().to_f32()),
                            round4(axis.end_coord().to_f32()),
                        )
                    })
                    .collect()
            })
            .collect()
    }

    /// The delta sets of every item variation data subtable.
    fn items(var_store: &ItemVariationStore) -> Vec<Vec<i32>> {
        var_store
            .item_variation_data()
            .iter()
            .flat_map(|data| {
                let data = data.unwrap().unwrap();
                let items: Vec<Vec<i32>> = (0..data.item_count())
                    .map(|i| data.delta_set(i).collect())
                    .collect();
                items
            })
            .collect()
    }

    /// Port of fontTools `test_varlib_avar2`:
    /// <https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Tests/varLib/varLib_test.py#L314-L325>
    #[test]
    fn varlib_avar2() {
        let wght = axis(
            "wght",
            vec![
                (UserCoord::new(100.0), DesignCoord::new(26.0)),
                (UserCoord::new(200.0), DesignCoord::new(39.0)),
                (UserCoord::new(300.0), DesignCoord::new(58.0)),
                (UserCoord::new(400.0), DesignCoord::new(90.0)),
                (UserCoord::new(500.0), DesignCoord::new(108.0)),
                (UserCoord::new(600.0), DesignCoord::new(128.0)),
                (UserCoord::new(700.0), DesignCoord::new(151.0)),
                (UserCoord::new(800.0), DesignCoord::new(169.0)),
                (UserCoord::new(900.0), DesignCoord::new(190.0)),
            ],
            3,
        );
        let input = DesignCoord::new(128.0).to_normalized(&wght.converter);
        let output = DesignCoord::new(138.0).to_normalized(&wght.converter);
        let static_metadata = static_metadata(
            vec![wght],
            vec![mapping(
                &[("wght", input.to_f64())],
                &[("wght", output.to_f64())],
            )],
        );
        let bytes = avar_bytes(&static_metadata);
        let avar = Avar::read(FontData::new(&bytes)).unwrap();
        assert_eq!(MajorMinor::VERSION_2_0, avar.version());
        assert_eq!(
            vec![vec![
                (-1.0, -1.0),
                (-0.6667, -0.7969),
                (-0.3333, -0.5),
                (0.0, 0.0),
                (0.2, 0.18),
                (0.4, 0.38),
                (0.6, 0.61),
                (0.8, 0.79),
                (1.0, 1.0),
            ]],
            segment_maps(&avar)
        );
        assert!(avar.axis_index_map().is_none());
        let var_store = avar.var_store().unwrap().unwrap();
        assert_eq!(vec![vec![(0.0, 0.38, 1.0)]], regions(&var_store));
        assert_eq!(vec![vec![1638]], items(&var_store));
    }

    /// Port of fontTools `Avar2Test.test_roundtrip`:
    /// <https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Tests/ttLib/tables/_a_v_a_r_test.py#L107-L178>
    #[test]
    fn avar2_roundtrip() {
        let wdth = axis(
            "wdth",
            vec![
                (UserCoord::new(50.0), DesignCoord::new(50.0)),
                (UserCoord::new(100.0), DesignCoord::new(100.0)),
                (UserCoord::new(140.0), DesignCoord::new(150.0)),
                (UserCoord::new(200.0), DesignCoord::new(200.0)),
            ],
            1,
        );
        let static_metadata = static_metadata(
            vec![unmapped_axis("wght", 100.0, 400.0, 900.0), wdth],
            // The fontTools test stores the master value -0.8 directly. Here the
            // master value is the output minus the input, so the output wdth is -1.8.
            vec![mapping(&[("wght", 1.0), ("wdth", -1.0)], &[("wdth", -1.8)])],
        );
        let bytes = avar_bytes(&static_metadata);
        let avar = Avar::read(FontData::new(&bytes)).unwrap();
        assert_eq!(MajorMinor::VERSION_2_0, avar.version());
        assert_eq!(
            vec![
                vec![(-1.0, -1.0), (0.0, 0.0), (1.0, 1.0)],
                vec![(-1.0, -1.0), (0.0, 0.0), (0.4, 0.5), (1.0, 1.0)],
            ],
            segment_maps(&avar)
        );
        let var_store = avar.var_store().unwrap().unwrap();
        assert_eq!(2, var_store.variation_region_list().unwrap().axis_count());
        assert_eq!(
            vec![vec![(0.0, 1.0, 1.0), (-1.0, -1.0, 0.0)]],
            regions(&var_store)
        );
        assert_eq!(vec![vec![-13107]], items(&var_store));
        let axis_index_map = avar.axis_index_map().unwrap().unwrap();
        assert_eq!(
            DeltaSetIndex::NO_VARIATION_INDEX,
            axis_index_map.get(0).unwrap()
        );
        assert_eq!(
            DeltaSetIndex { outer: 0, inner: 0 },
            axis_index_map.get(1).unwrap()
        );
    }

    /// Port of fontTools `test_init_duplicate_locations`:
    /// <https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Tests/varLib/models_test.py#L400-L408>
    #[test]
    fn init_duplicate_locations() {
        let static_metadata = static_metadata(
            vec![
                unmapped_axis("wght", 100.0, 400.0, 900.0),
                unmapped_axis("wdth", 50.0, 100.0, 200.0),
            ],
            vec![
                mapping(&[("wght", 0.0), ("wdth", 0.0)], &[("wdth", 0.5)]),
                mapping(&[("wght", 1.0), ("wdth", 1.0)], &[("wdth", 0.5)]),
                mapping(&[("wdth", 1.0), ("wght", 1.0)], &[("wdth", 0.5)]),
            ],
        );
        assert!(matches!(
            to_var_store(&static_metadata),
            Err(Error::DuplicateAxisMapping(_))
        ));
    }

    /// Port of fontTools `test_init_duplicate_locations_after_stripping_zero_axes`:
    /// <https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Tests/varLib/models_test.py#L410-L412>
    #[test]
    fn init_duplicate_locations_after_stripping_zero_axes() {
        let static_metadata = static_metadata(
            vec![
                unmapped_axis("wght", 100.0, 400.0, 900.0),
                unmapped_axis("wdth", 50.0, 100.0, 200.0),
            ],
            vec![
                mapping(&[], &[("wdth", 0.5)]),
                mapping(&[("wght", 0.0)], &[("wdth", 0.5)]),
            ],
        );
        assert!(matches!(
            to_var_store(&static_metadata),
            Err(Error::DuplicateAxisMapping(_))
        ));
    }
}
