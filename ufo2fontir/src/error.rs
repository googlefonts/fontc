use std::io;

use norad::error::{DesignSpaceLoadError, FontLoadError, GlifLoadError};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum UfoToIrError {
    #[error("failed to load .designspace")]
    DesignSpaceLoadError(#[from] DesignSpaceLoadError),
    #[error("failed to load")]
    UfoLoadError(#[from] FontLoadError),
    #[error("failed to load glif")]
    GlifLoadError(#[from] GlifLoadError),
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("failed to identify upem")]
    AmbiguousUpemError,
    #[error("Dimension missing uservalue")]
    DimensionError,
}
