use std::{io, num::TryFromIntError, path::PathBuf};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("Unable to parse {0}: {1}")]
    ParseError(PathBuf, String),
    #[error("Unexpected file structure {0}")]
    StructuralError(String),
    #[error("No upem")]
    NoUnitsPerEm,
    #[error("Invalid upem")]
    InvalidUpem(#[from] TryFromIntError),
    #[error("Unrecognized name {0}")]
    UnknownValueName(String),
}
