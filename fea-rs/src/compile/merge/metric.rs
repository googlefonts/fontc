//! Merging per-master values into variable ones.

use std::collections::HashMap;

use fontdrasil::coords::NormalizedLocation;
use write_fonts::tables::{
    gpos::builders::ValueRecordBuilder,
    layout::builders::{DeviceOrDeltas, Metric},
};

use super::{MergeCtx, MergeError};
use crate::compile::{VariationInfo, compile_ctx::metric_from_deltas};

impl<V: VariationInfo> MergeCtx<'_, V> {
    /// Merge one metric across masters.
    ///
    /// `values[i]` is master `i`'s value, or `None` if that master has none,
    /// in which case it contributes zero: a positioning rule a master does not
    /// have applies no adjustment there. At least one value must be present.
    /// A value that is the same in every master stays a plain scalar.
    ///
    /// <https://github.com/fonttools/fonttools/blob/34be2443a/Lib/fontTools/varLib/merger.py#L1254-L1258>
    pub(super) fn merge_metric(
        &self,
        values: &[Option<&Metric>],
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

        let per_master: Vec<i16> = values
            .iter()
            .map(|value| value.map(|metric| metric.default).unwrap_or(0))
            .collect();
        let default = per_master[0];
        if per_master.iter().all(|value| *value == default) {
            return Ok(default.into());
        }

        let locations: HashMap<NormalizedLocation, i16> =
            self.locations.iter().cloned().zip(per_master).collect();
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
    /// A field that is set in any master is merged across all of them; masters
    /// that do not set it, or have no record at all, contribute zero.
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
            self.merge_metric(&values, index).map(Some)
        };
        Ok(ValueRecordBuilder {
            x_advance: field(|r| r.x_advance.as_ref())?,
            y_advance: field(|r| r.y_advance.as_ref())?,
            x_placement: field(|r| r.x_placement.as_ref())?,
            y_placement: field(|r| r.y_placement.as_ref())?,
        })
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
        let merged = ctx.merge_metric(&[Some(&ten), Some(&ten)], 0).unwrap();
        assert_eq!(merged, ten);
    }

    #[test]
    fn differing_values_get_deltas() {
        let var_info = var_info();
        let ctx = ctx(&var_info, &[0.0, 1.0]);
        let merged = ctx
            .merge_metric(&[Some(&10.into()), Some(&30.into())], 0)
            .unwrap();
        assert_eq!(merged.default, 10);
        assert_eq!(deltas(&merged), vec![20]);
    }

    #[test]
    fn missing_is_zero() {
        let var_info = var_info();
        let ctx = ctx(&var_info, &[0.0, 1.0]);
        let merged = ctx.merge_metric(&[Some(&10.into()), None], 0).unwrap();
        assert_eq!(merged.default, 10);
        assert_eq!(deltas(&merged), vec![-10]);
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
            .merge_metric(&[Some(&device), Some(&device)], 0)
            .unwrap();
        assert_eq!(merged, device);
        assert!(matches!(
            ctx.merge_metric(&[Some(&device), Some(&10.into())], 0),
            Err(MergeError::DeviceDiffers { .. })
        ));
    }
}
