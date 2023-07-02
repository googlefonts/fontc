//! Generates a [avar](https://learn.microsoft.com/en-us/typography/opentype/spec/avar) table.

use font_types::F2Dot14;
use fontdrasil::orchestration::{Access, Work};
use fontir::{
    coords::{CoordConverter, DesignCoord},
    ir::Axis,
    orchestration::WorkId as FeWorkId,
};
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

fn to_segment_map(axis: &Axis) -> SegmentMaps {
    // You need at least four values for an avar to DO anything
    // because, per spec, "If the segment map for a given axis has any value maps,
    // then it must include at least three value maps: -1 to -1, 0 to 0, and 1 to 1"
    // so three value maps *must* produce identity.
    //
    // 0 entry avar may confuse some clients so emit a default one
    if axis.converter.len() < 4 {
        return SegmentMaps::new(vec![
            AxisValueMap::new(F2Dot14::from_f32(-1.0), F2Dot14::from_f32(-1.0)),
            AxisValueMap::new(F2Dot14::from_f32(0.0), F2Dot14::from_f32(0.0)),
            AxisValueMap::new(F2Dot14::from_f32(1.0), F2Dot14::from_f32(1.0)),
        ]);
    }

    // default normalization
    let default_converter = CoordConverter::new(
        vec![
            (axis.min, DesignCoord::new(-1.0)),
            (axis.default, DesignCoord::new(0.0)),
            (axis.max, DesignCoord::new(1.0)),
        ],
        1,
    );

    // avar maps from the default normalization to the actual one,
    // using normalized values on both sides.
    let mappings = axis
        .converter
        .iter()
        .map(|(user, _, norm)| {
            AxisValueMap::new(user.to_normalized(&default_converter).into(), norm.into())
        })
        .collect();

    SegmentMaps::new(mappings)
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
        if static_metadata.variable_axes.is_empty() {
            debug!("Skip avar; this is not a variable font");
            return Ok(());
        }
        context.avar.set_unconditionally(
            Avar::new(
                static_metadata
                    .variable_axes
                    .iter()
                    .map(to_segment_map)
                    .filter(|sm| !sm.axis_value_maps.is_empty())
                    .collect(),
            )
            .into(),
        );
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontir::{
        coords::{CoordConverter, DesignCoord, UserCoord},
        ir::Axis,
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

    fn round2(v: f32) -> f32 {
        (v * 100.0).round() / 100.0
    }

    fn dump(segmap: SegmentMaps) -> Vec<(f32, f32)> {
        segmap
            .axis_value_maps
            .iter()
            .map(|av| (av.from_coordinate.to_f32(), av.to_coordinate.to_f32()))
            .map(|(from, to)| (round2(from), round2(to)))
            .collect()
    }

    #[test]
    fn up_to_three_points_does_nothing() {
        let mappings = vec![
            (UserCoord::new(100.0), DesignCoord::new(-10.0)),
            (UserCoord::new(400.0), DesignCoord::new(0.0)),
            (UserCoord::new(700.0), DesignCoord::new(20.0)),
        ];
        for i in 1..mappings.len() {
            let mappings = mappings[0..i].to_vec();
            let segmap = to_segment_map(&axis(mappings, 1));
            assert_eq!(vec![(-1.0, -1.0), (0.0, 0.0), (1.0, 1.0),], dump(segmap));
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
            dump(to_segment_map(&axis(mappings, 1)))
        );
    }
}
