//! Using confusable types for coords is an endless source of confusion; don't.

use std::collections::BTreeMap;

use log::warn;
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};

/// A coordinate in some arbitrary space the designer dreamed up.
///
/// In .designspace, an xvalue. <https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#dimension-element>.
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DesignSpaceCoord(OrderedFloat<f32>);

/// A coordinate in the same space as the axis in the font, e.g. what 'fvar' uses.
///
/// In .designspace, a uservalue. <https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#dimension-element>.
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct UserSpaceCoord(OrderedFloat<f32>);

// TODO: normalized coord for [-1, 1] variant

// Using BTreeMap instead of HashMap and OrderedFloat instead of f32 so that
// the location is hashable and can be used as a key in Glyph::sources HashMap
pub type DesignSpaceLocation = BTreeMap<String, DesignSpaceCoord>;
pub type UserSpaceLocation = BTreeMap<String, UserSpaceCoord>;

// Will be removed once remapping is properly implemented, for now marks where we need to update
// Tentatively expected to put maps onto StaticMetadata so anyone who wants design:user gets from there
pub fn temporary_design_to_user_conversion(coord: DesignSpaceCoord) -> UserSpaceCoord {
    warn!("Illegal and invalid temporary conversion");
    UserSpaceCoord(coord.into_inner())
}

impl DesignSpaceCoord {
    /// We do *not* provide From because we want conversion to be explicit
    pub fn new(value: OrderedFloat<f32>) -> DesignSpaceCoord {
        DesignSpaceCoord(value)
    }
}

impl UserSpaceCoord {
    /// We do *not* provide From because we want conversion to be explicit
    pub fn new(value: OrderedFloat<f32>) -> UserSpaceCoord {
        UserSpaceCoord(value)
    }
}

impl DesignSpaceCoord {
    pub fn into_inner(self) -> OrderedFloat<f32> {
        self.0
    }
}

impl UserSpaceCoord {
    pub fn into_inner(self) -> OrderedFloat<f32> {
        self.0
    }
}
