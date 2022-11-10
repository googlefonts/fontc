//! Serde types for font IR. See TODO:PublicLink.

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Axis {
    tag: String,
    min: f32,
    default: f32,
    max: f32,
    hidden: bool,
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
