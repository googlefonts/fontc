use std::{io, path::PathBuf};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Unrecognized source")]
    UnrecognizedSource(PathBuf),
    #[error("yaml error")]
    YamlSerError(#[from] serde_yaml::Error),
    #[error("IO failures")]
    IoError(#[from] io::Error),
    #[error("Font IR error")]
    FontIrError(#[from] fontir::error::Error),
    #[error("Unable to produce IR")]
    IrGenerationError,
}
