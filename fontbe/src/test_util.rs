//! Only included in test

use std::{cmp, str::FromStr};

use fontdrasil::{
    coords::{CoordConverter, DesignCoord, UserCoord},
    types::Axis,
};
use write_fonts::types::Tag;

#[cfg(test)]
pub(crate) fn axis(tag: &str, min: f64, default: f64, max: f64) -> Axis {
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
        name: tag.to_string(),
        tag: Tag::from_str(tag).unwrap(),
        min: UserCoord::new(min),
        default: UserCoord::new(default),
        max: UserCoord::new(max),
        hidden: false,
        converter: CoordConverter::new(mappings, default_idx),
        localized_names: Default::default(),
    }
}
