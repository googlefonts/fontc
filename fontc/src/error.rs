use std::{io, path::PathBuf};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Unrecognized source")]
    UnrecognizedSource(PathBuf),
    #[error("toml error")]
    TomlSerError(#[from] toml::ser::Error),
    #[error("IO failures")]
    IoError(#[from] io::Error),
    #[error("Font IR error")]
    FontIrError(#[from] fontir::error::Error),
}
