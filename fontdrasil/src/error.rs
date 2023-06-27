use std::fmt::{self, Display, Formatter};

use crate::coords::NormalizedLocation;

#[derive(Debug)]
pub enum VariationModelError {
    AxesWithoutAssignedOrder {
        axis_names: Vec<String>,
        location: NormalizedLocation,
    },
    PointAxis(String),
}

impl std::error::Error for VariationModelError {}

impl Display for VariationModelError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            VariationModelError::AxesWithoutAssignedOrder {
                axis_names,
                location,
            } => write!(f, "{axis_names:?} in {location:?} have no assigned order"),
            VariationModelError::PointAxis(name) => write!(
                f,
                "{name} is is an axis of variation defined only at a single point"
            ),
        }
    }
}

#[derive(Debug)]
pub enum DeltaError {
    DefaultUndefined,
    InconsistentNumbersOfPoints,
    UnknownLocation(NormalizedLocation),
}

impl std::error::Error for DeltaError {}

impl Display for DeltaError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            DeltaError::DefaultUndefined => write!(f, "The default must have a point sequence"),
            DeltaError::InconsistentNumbersOfPoints => {
                write!(f, "Every point sequence must have the same length")
            }
            DeltaError::UnknownLocation(pos) => {
                write!(f, "{pos:?} is not present in the variation model")
            }
        }
    }
}
