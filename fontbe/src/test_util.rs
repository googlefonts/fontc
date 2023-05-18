//! Only included in test

use font_types::Tag;
use fontir::{
    coords::{CoordConverter, DesignCoord, UserCoord},
    ir::Axis,
};
use std::cmp;

#[cfg(test)]
pub(crate) fn axis(min: f32, default: f32, max: f32) -> Axis {
    use std::str::FromStr;

    let mut mappings = Vec::new();
    if min < default {
        mappings.push((UserCoord::new(min), DesignCoord::new(min / 10.0)));
    }
    mappings.push((UserCoord::new(default), DesignCoord::new(default / 10.0)));
    if max > default {
        mappings.push((UserCoord::new(max), DesignCoord::new(max / 10.0)));
    }
    let default_idx = cmp::min(mappings.len() - 1, 1);
    Axis {
        name: "Test".to_string(),
        tag: Tag::from_str("TEST").unwrap(),
        min: UserCoord::new(min),
        default: UserCoord::new(default),
        max: UserCoord::new(max),
        hidden: false,
        converter: CoordConverter::new(mappings, default_idx),
    }
}
