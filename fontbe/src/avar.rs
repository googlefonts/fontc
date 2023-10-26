//! Generates a [avar](https://learn.microsoft.com/en-us/typography/opentype/spec/avar) table.

use fontdrasil::{
    coords::{CoordConverter, DesignCoord, NormalizedCoord},
    orchestration::{Access, Work},
    types::Axis,
};
use fontir::orchestration::WorkId as FeWorkId;
use log::debug;
use write_fonts::tables::avar::{Avar, AxisValueMap, SegmentMaps};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct AvarWork {}

pub fn create_avar_work() -> Box<BeWork> {
    Box::new(AvarWork {})
}

fn to_segment_map(axis: &Axis) -> Option<SegmentMaps> {
    // default normalization
    let default_converter = CoordConverter::new(
        vec![
            (axis.min, DesignCoord::new(-1.0)),
            (axis.default, DesignCoord::new(0.0)),
            (axis.max, DesignCoord::new(1.0)),
        ],
        1,
    );

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
    // All identity mappings are not interesting so we can skip them.
    if mappings.iter().all(|(k, v)| k == v) {
        return None;
    }

    let mappings = mappings
        .iter()
        .map(|(default_norm, actual_norm)| {
            AxisValueMap::new((*default_norm).into(), (*actual_norm).into())
        })
        .collect();

    Some(SegmentMaps::new(mappings))
}

impl Work<Context, AnyWorkId, Error> for AvarWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Avar.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::One(FeWorkId::StaticMetadata.into())
    }

    /// Generate [avar](https://learn.microsoft.com/en-us/typography/opentype/spec/avar)
    ///
    /// See also <https://learn.microsoft.com/en-us/typography/opentype/spec/otvaroverview#CSN>
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        // Guard clause: don't produce avar for a static font
        if static_metadata.axes.is_empty() {
            debug!("Skip avar; this is not a variable font");
            return Ok(());
        }
        let axis_segment_maps: Vec<_> = static_metadata
            .axes
            .iter()
            .filter_map(to_segment_map)
            .filter(|sm| !sm.axis_value_maps.is_empty())
            .collect();
        let avar = (!axis_segment_maps.is_empty()).then_some(Avar::new(axis_segment_maps));
        context.avar.set_unconditionally(avar.into());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::{
        coords::{CoordConverter, DesignCoord, UserCoord},
        types::Axis,
    };
    use read_fonts::types::Tag;
    use std::{cmp, str::FromStr};
    use write_fonts::tables::avar::SegmentMaps;

    use super::to_segment_map;

    fn axis(mappings: Vec<(UserCoord, DesignCoord)>, default_idx: usize) -> Axis {
        let default_idx = cmp::min(mappings.len() - 1, default_idx);
        Axis {
            name: "Test".to_string(),
            tag: Tag::from_str("TEST").unwrap(),
            min: *mappings.iter().map(|(u, _)| u).min().unwrap(),
            default: mappings[default_idx].0,
            max: *mappings.iter().map(|(u, _)| u).max().unwrap(),
            hidden: false,
            converter: CoordConverter::new(mappings, default_idx),
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
            assert!(to_segment_map(&axis(mappings, 1)).is_none());
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
            dump(to_segment_map(&axis(mappings, 1)).unwrap())
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
            dump(to_segment_map(&axis(mappings, 0)).unwrap())
        );
    }
}
