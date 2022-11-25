use norad::error::{DesignSpaceLoadError, FontLoadError};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum UfoToIrError {
    #[error("failed to load .designspace")]
    DesignSpaceLoadError(#[from] DesignSpaceLoadError),
    #[error("failed to load")]
    UfoLoadError(#[from] FontLoadError),
    #[error("failed to identify upem")]
    AmbiguousUpemError,
    #[error("Dimension missing uservalue")]
    DimensionError,
    #[error("Duplicate location")]
    DuplicateLocationError,
    #[error("layer not found")]
    LayerNotFoundError(String),
}
