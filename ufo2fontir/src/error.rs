use std::io;
use std::path::PathBuf;

use norad::error::{DesignSpaceLoadError, FontLoadError, GlifLoadError};
use thiserror::Error;

use plist::Error as PlistError;

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
    #[error("Duplicate location")]
    DuplicateLocationError,
    #[error("required file is missing")]
    MissingRequiredFileError(PathBuf),
    #[error("failed to load plist")]
    PlistLoadError(#[from] PlistError),
    #[error("layer not found")]
    LayerNotFoundError(String),
}
