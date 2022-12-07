use std::{io, path::PathBuf};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("Unable to parse")]
    ParseError(PathBuf, String),
    #[error("Unexpected file structure")]
    StructuralError(String),
}
