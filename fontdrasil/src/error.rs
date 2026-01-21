use thiserror::Error;

use crate::coords::{Coord, UserSpace};
use crate::types::Tag;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Default index {0} is out of bounds for design coordinates of length {1}")]
    DefaultOutOfBounds(usize, usize),
    #[error("Default coordinate {0:?} not found in mappings")]
    DefaultNotFoundInMappings(Coord<UserSpace>),
    #[error("Location involved undefined axis {0}")]
    UnknownAxis(Tag),
}
