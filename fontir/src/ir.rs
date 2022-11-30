//! Serde types for font IR. See TODO:PublicLink.

use std::collections::{BTreeMap, HashMap};

use serde::{Deserialize, Serialize};

///Global font info that cannot vary.
///
/// For example, upem, axis definitions, etc, as distinct from
/// metadata that varies across design space such as ascender/descender.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct StaticMetadata {}

use ordered_float::OrderedFloat;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Axis {
    pub name: String,
    pub tag: String,
    pub min: f32,
    pub default: f32,
    pub max: f32,
    pub hidden: bool,
}

// Using BTreeMap instead of HashMap and OrderedFloat instead of f32 so that
// the location is hashable and can be used as a key in Glyph::sources HashMap
pub type DesignSpaceLocation = BTreeMap<String, OrderedFloat<f32>>;

/// A variable definition of a single glyph.
///
/// Defined in at least once position in designspace. If defined in
/// many, presumed to vary continuously between positions and required
/// to have variation compatible structure.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Glyph {
    pub name: String,
    pub sources: HashMap<DesignSpaceLocation, GlyphInstance>,
}

/// A Glyph at a specific position in designspace.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct GlyphInstance {
    pub width: Option<f32>,
    pub height: Option<f32>,
    // TODO: outlines, a Vec<Shape>
}

#[cfg(test)]
mod tests {
    use crate::ir::Axis;

    fn test_axis() -> Axis {
        Axis {
            name: String::from("Weight"),
            tag: String::from("wght"),
            min: 100.,
            default: 400.,
            max: 900.,
            hidden: false,
        }
    }

    #[test]
    fn axis_yaml() {
        let test_axis = test_axis();
        let yml = serde_yaml::to_string(&test_axis).unwrap();
        assert_eq!(test_axis, serde_yaml::from_str(&yml).unwrap());
    }

    #[test]
    fn axis_bincode() {
        let test_axis = test_axis();
        let bin = bincode::serialize(&test_axis).unwrap();
        assert_eq!(test_axis, bincode::deserialize(&bin).unwrap());
    }
}
