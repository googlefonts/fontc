//! Generates a [avar](https://learn.microsoft.com/en-us/typography/opentype/spec/avar) table.

use fontdrasil::{
    coords::NormalizedCoord,
    orchestration::{Access, Work},
    types::Axis,
};
use fontir::orchestration::{Persistable, WorkId as FeWorkId};
use log::debug;
use write_fonts::{
    read::FontRead,
    tables::avar::{Avar, AxisValueMap, SegmentMaps},
    types::F2Dot14,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

/// Avar is a special case where sometimes we want to explicitly have an empty one.
///
/// We can't just store `Option` because we can't impl Persistable for it
/// (doing so conflicts with the generic impl for write-fonts types)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PossiblyEmptyAvar {
    NonEmpty(Avar),
    Empty,
}

impl Persistable for PossiblyEmptyAvar {
    fn read(from: &mut dyn std::io::Read) -> Self {
        let mut bytes = Vec::new();
        from.read_to_end(&mut bytes).unwrap();
        if bytes.is_empty() {
            Self::Empty
        } else {
            let table =
                FontRead::read(bytes.as_slice().into()).expect("we wrote it, we can write it");
            Self::NonEmpty(table)
        }
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        if let Self::NonEmpty(table) = self {
            table.write(to);
        }
    }
}

impl PossiblyEmptyAvar {
    pub fn as_ref(&self) -> Option<&Avar> {
        match self {
            PossiblyEmptyAvar::NonEmpty(avar) => Some(avar),
            PossiblyEmptyAvar::Empty => None,
        }
    }
}

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
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        // Guard clause: don't produce avar for a static font
        if static_metadata.axes.is_empty() {
            debug!("Skip avar; this is not a variable font");
            return Ok(());
        }
        let axis_segment_maps: Vec<_> = static_metadata.axes.iter().map(to_segment_map).collect();
        // only when all the segment maps are uninteresting, we can omit avar
        let avar = if axis_segment_maps.iter().any(|segmap| !segmap.is_identity()) {
            PossiblyEmptyAvar::NonEmpty(Avar::new(axis_segment_maps))
        } else {
            PossiblyEmptyAvar::Empty
        };
        context.avar.set(avar);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::{
        coords::{CoordConverter, DesignCoord, UserCoord},
        types::Axis,
    };
    use std::{cmp, str::FromStr};
    use write_fonts::tables::avar::SegmentMaps;
    use write_fonts::types::Tag;

    use super::{default_segment_map, to_segment_map};

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
            assert_eq!(to_segment_map(&axis(mappings, 1)), default_segment_map());
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
            dump(to_segment_map(&axis(mappings, 0)))
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
            dump(to_segment_map(&axis(mappings, 3)))
        );
    }
}
