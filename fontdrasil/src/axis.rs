//! An axis in a variable font with enough self-awareness to convert units.

use font_types::Tag;
use serde::{Deserialize, Serialize};

use crate::coords::{CoordConverter, UserCoord};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Axis {
    pub name: String,
    pub tag: Tag,
    pub min: UserCoord,
    pub default: UserCoord,
    pub max: UserCoord,
    pub hidden: bool,
    pub converter: CoordConverter,
}

impl Axis {
    pub fn is_point(&self) -> bool {
        self.min == self.default && self.max == self.default
    }
}
