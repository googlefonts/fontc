//! Merging per-master values into variable ones.

use std::collections::HashMap;

use fontdrasil::coords::NormalizedLocation;
use write_fonts::tables::{
    gpos::builders::{AnchorBuilder, ValueRecordBuilder},
    layout::builders::{DeviceOrDeltas, Metric},
};

use super::{MergeCtx, MergeError};
use crate::compile::{VariationInfo, compile_ctx::metric_from_deltas};

/// What a master without a value contributes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Missing {
    /// Zero: a positioning rule that a master does not have applies no
    /// adjustment there.
    Zero,
    /// Nothing: the master is left out of the variation model, as varLib does
    /// for anchors.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L1227-L1251>
    Sparse,
}

impl<V: VariationInfo> MergeCtx<'_, V> {
    /// Merge one metric across masters.
    ///
    /// `values[i]` is master `i`'s value, or `None` if that master has none;
    /// at least one must be present. A value that is the same wherever it is
    /// present stays a plain scalar.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L1254-L1258>
    pub(super) fn merge_metric(
        &self,
        values: &[Option<&Metric>],
        missing: Missing,
        index: usize,
    ) -> Result<Metric, MergeError> {
        let present: Vec<&Metric> = values.iter().flatten().copied().collect();
        let first = *present.first().expect("caller passes at least one value");
        assert!(
            present
                .iter()
                .all(|metric| !matches!(metric.device_or_deltas, DeviceOrDeltas::Deltas(_))),
            "compile_for_merge has no variation info, so it cannot produce deltas"
        );

        if present
            .iter()
            .any(|metric| matches!(metric.device_or_deltas, DeviceOrDeltas::Device(_)))
        {
            return if values.iter().all(|value| *value == Some(first)) {
                Ok(first.clone())
            } else {
                Err(MergeError::DeviceDiffers {
                    lookup: self.lookup_ref(index),
                })
            };
        }

        let per_master: Vec<Option<i16>> = values
            .iter()
            .map(|value| match (value, missing) {
                (Some(metric), _) => Some(metric.default),
                (None, Missing::Zero) => Some(0),
                (None, Missing::Sparse) => None,
            })
            .collect();
        let Some(default) = per_master[0] else {
            return Err(MergeError::MissingAtDefault {
                lookup: self.lookup_ref(index),
            });
        };
        if per_master.iter().flatten().all(|value| *value == default) {
            return Ok(default.into());
        }

        let locations: HashMap<NormalizedLocation, i16> = self
            .locations
            .iter()
            .zip(&per_master)
            .filter_map(|(location, value)| value.map(|value| (location.clone(), value)))
            .collect();
        let (default, deltas) = self
            .var_info
            .resolve_variable_metric(&locations)
            .map_err(|e| MergeError::Deltas {
                lookup: self.lookup_ref(index),
                message: e.to_string(),
            })?;
        Ok(metric_from_deltas(default, deltas))
    }

    /// Merge one value record across masters.
    ///
    /// A field that is set in any master is merged with [`Missing::Zero`]:
    /// masters that do not set it, or have no record at all, contribute zero.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L1301-L1315>
    pub(super) fn merge_value_record(
        &self,
        records: &[Option<&ValueRecordBuilder>],
        index: usize,
    ) -> Result<ValueRecordBuilder, MergeError> {
        let field = |get: fn(&ValueRecordBuilder) -> Option<&Metric>| {
            let values: Vec<_> = records.iter().map(|r| r.and_then(get)).collect();
            if values.iter().all(Option::is_none) {
                return Ok(None);
            }
            self.merge_metric(&values, Missing::Zero, index).map(Some)
        };
        Ok(ValueRecordBuilder {
            x_advance: field(|r| r.x_advance.as_ref())?,
            y_advance: field(|r| r.y_advance.as_ref())?,
            x_placement: field(|r| r.x_placement.as_ref())?,
            y_placement: field(|r| r.y_placement.as_ref())?,
        })
    }

    /// Merge one anchor across masters; masters without it are left out, and
    /// an anchor that no master has stays absent.
    ///
    /// varLib only merges format 1 anchors. A format 2 anchor's contour point
    /// has no home in the format 3 anchor a varying position needs, so an
    /// anchor with a contour point may not vary at all.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L1285-L1298>
    pub(super) fn merge_anchor(
        &self,
        anchors: &[Option<&AnchorBuilder>],
        index: usize,
    ) -> Result<Option<AnchorBuilder>, MergeError> {
        let present: Vec<&AnchorBuilder> = anchors.iter().flatten().copied().collect();
        let Some(first) = present.first() else {
            return Ok(None);
        };
        let contourpoint = first.contourpoint;
        let anchor_point_error = || MergeError::AnchorPoint {
            lookup: self.lookup_ref(index),
        };
        if present
            .iter()
            .any(|anchor| anchor.contourpoint != contourpoint)
        {
            return Err(anchor_point_error());
        }
        let x: Vec<_> = anchors.iter().map(|a| a.map(|a| &a.x)).collect();
        let y: Vec<_> = anchors.iter().map(|a| a.map(|a| &a.y)).collect();
        let x = self.merge_metric(&x, Missing::Sparse, index)?;
        let y = self.merge_metric(&y, Missing::Sparse, index)?;
        if contourpoint.is_some() && (x.has_device_or_deltas() || y.has_device_or_deltas()) {
            return Err(anchor_point_error());
        }
        Ok(Some(AnchorBuilder { x, y, contourpoint }))
    }
}

#[cfg(test)]
mod tests {
    use write_fonts::tables::layout::Device;

    use super::{super::test_helpers::*, *};
    use crate::compile::MockVariationInfo;

    fn ctx<'a>(var_info: &'a MockVariationInfo, wghts: &[f64]) -> MergeCtx<'a, MockVariationInfo> {
        let masters = wghts
            .iter()
            .map(|wght| (location(*wght), pending("languagesystem DFLT dflt;")))
            .collect();
        MergeCtx::new(masters, var_info).unwrap()
    }

    fn deltas(metric: &Metric) -> Vec<i16> {
        match &metric.device_or_deltas {
            DeviceOrDeltas::Deltas(deltas) => deltas.iter().map(|(_, delta)| *delta).collect(),
            DeviceOrDeltas::None => Vec::new(),
            DeviceOrDeltas::Device(_) => panic!("unexpected device table"),
        }
    }

    #[test]
    fn equal_values_stay_scalar() {
        let var_info = var_info();
        let ctx = ctx(&var_info, &[0.0, 1.0]);
        let ten = Metric::from(10);
        let merged = ctx
            .merge_metric(&[Some(&ten), Some(&ten)], Missing::Sparse, 0)
            .unwrap();
        assert_eq!(merged, ten);
    }

    #[test]
    fn differing_values_get_deltas() {
        let var_info = var_info();
        let ctx = ctx(&var_info, &[0.0, 1.0]);
        let merged = ctx
            .merge_metric(&[Some(&10.into()), Some(&30.into())], Missing::Sparse, 0)
            .unwrap();
        assert_eq!(merged.default, 10);
        assert_eq!(deltas(&merged), vec![20]);
    }

    #[test]
    fn missing_is_zero() {
        let var_info = var_info();
        let ctx = ctx(&var_info, &[0.0, 1.0]);
        let merged = ctx
            .merge_metric(&[Some(&10.into()), None], Missing::Zero, 0)
            .unwrap();
        assert_eq!(merged.default, 10);
        assert_eq!(deltas(&merged), vec![-10]);
    }

    #[test]
    fn missing_is_sparse() {
        let var_info = var_info();
        let ctx = ctx(&var_info, &[0.0, 0.5, 1.0]);
        let merged = ctx
            .merge_metric(
                &[Some(&10.into()), None, Some(&30.into())],
                Missing::Sparse,
                0,
            )
            .unwrap();
        assert_eq!(merged.default, 10);
        // the absent middle master is not a region, only the end
        assert_eq!(deltas(&merged), vec![20]);
    }

    #[test]
    fn sparse_at_default_is_an_error() {
        let var_info = var_info();
        let ctx = ctx(&var_info, &[0.0, 1.0]);
        assert!(matches!(
            ctx.merge_metric(&[None, Some(&5.into())], Missing::Sparse, 0),
            Err(MergeError::MissingAtDefault { .. })
        ));
    }

    #[test]
    fn device_tables_must_agree() {
        let var_info = var_info();
        let ctx = ctx(&var_info, &[0.0, 1.0]);
        let device = Metric {
            default: 10,
            device_or_deltas: Device::new(11, 12, &[1, 1]).into(),
        };
        let merged = ctx
            .merge_metric(&[Some(&device), Some(&device)], Missing::Zero, 0)
            .unwrap();
        assert_eq!(merged, device);
        assert!(matches!(
            ctx.merge_metric(&[Some(&device), Some(&10.into())], Missing::Zero, 0),
            Err(MergeError::DeviceDiffers { .. })
        ));
    }

    #[test]
    fn anchors_are_sparse() {
        let var_info = var_info();
        let ctx = ctx(&var_info, &[0.0, 0.5, 1.0]);
        let merged = ctx
            .merge_anchor(
                &[
                    Some(&AnchorBuilder::new(100, 200)),
                    None,
                    Some(&AnchorBuilder::new(100, 250)),
                ],
                0,
            )
            .unwrap()
            .unwrap();
        assert_eq!(merged.x, 100.into());
        assert_eq!(merged.y.default, 200);
        assert_eq!(deltas(&merged.y), vec![50]);
    }

    #[test]
    fn contour_point_anchors_cannot_vary() {
        let var_info = var_info();
        let ctx = ctx(&var_info, &[0.0, 1.0]);
        let point = AnchorBuilder::new(100, 200).with_contourpoint(3);
        let merged = ctx
            .merge_anchor(&[Some(&point), Some(&point)], 0)
            .unwrap()
            .unwrap();
        assert_eq!(merged, point);

        let other_point = AnchorBuilder::new(100, 200).with_contourpoint(4);
        assert!(matches!(
            ctx.merge_anchor(&[Some(&point), Some(&other_point)], 0),
            Err(MergeError::AnchorPoint { .. })
        ));
        let moved = AnchorBuilder::new(100, 250).with_contourpoint(3);
        assert!(matches!(
            ctx.merge_anchor(&[Some(&point), Some(&moved)], 0),
            Err(MergeError::AnchorPoint { .. })
        ));
    }
}
