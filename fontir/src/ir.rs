//! Serde types for font IR. See TODO:PublicLink.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Font {
    pub upem: u16,
    pub axes: Vec<Axis>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Axis {
    pub tag: String,
    pub min: f32,
    pub default: f32,
    pub max: f32,
    pub hidden: bool,
}

pub type DesignSpaceLocation = HashMap<String, f32>;

/// A variable definition of a single glyph.
///
/// Defined in at least once position in designspace. If defined in
/// many, presumed to vary continuously between positions and required
/// to have variation compatible structure.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Glyph {
    pub name: String,
    pub sources: Vec<GlyphInstance>,
}

/// A Glyph at a specific position in designspace.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct GlyphInstance {
    pub location: DesignSpaceLocation,
    pub width: Option<f32>,
    pub height: Option<f32>,
    // TODO: outlines, a Vec<Shape>
}

#[cfg(test)]
mod tests {
    use crate::ir::Axis;

    fn test_axis() -> Axis {
        Axis {
            tag: String::from("wght"),
            min: 100.,
            default: 400.,
            max: 900.,
            hidden: false,
        }
    }

    #[test]
    fn axis_toml() {
        let test_axis = test_axis();
        let toml = toml::ser::to_string_pretty(&test_axis).unwrap();
        assert_eq!(test_axis, toml::from_str(&toml).unwrap());
    }

    #[test]
    fn axis_bincode() {
        let test_axis = test_axis();
        let bin = bincode::serialize(&test_axis).unwrap();
        assert_eq!(test_axis, bincode::deserialize(&bin).unwrap());
    }
}
