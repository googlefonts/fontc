//! Serde types for font IR. See TODO:PublicLink.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

///Global font info that cannot vary.
///
/// For example, upem, axis definitions, etc, as distinct from
/// metadata that varies across design space such as ascender/descender.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct StaticMetadata {}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct GlyphIr {}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Axis {
    pub tag: String,
    pub min: f32,
    pub default: f32,
    pub max: f32,
    pub hidden: bool,
}

// TODO(https://github.com/googlefonts/fontmake-rs/pull/4) fix this type
pub type DesignSpaceLocation = BTreeMap<String, i32>;

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
