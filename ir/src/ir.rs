//! Serde types for font IR. See TODO:PublicLink.

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Axis<'a> {
    tag: &'a str,
    min: f32,
    default: f32,
    max: f32,
    hidden: bool,
}

#[cfg(test)]
mod tests {
    use crate::ir::Axis;

    const TEST_AXIS: Axis = Axis {
        tag: "wght",
        min: 100.,
        default: 400.,
        max: 900.,
        hidden: false,
    };

    #[test]
    fn axis_toml() {
        let toml = toml::ser::to_string_pretty(&TEST_AXIS).unwrap();
        assert_eq!(TEST_AXIS, toml::from_str(&toml).unwrap());
    }

    #[test]
    fn axis_bincode() {
        let bin = bincode::serialize(&TEST_AXIS).unwrap();
        assert_eq!(TEST_AXIS, bincode::deserialize(&bin).unwrap());
    }
}
