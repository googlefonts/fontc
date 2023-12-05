use std::{io, path::PathBuf};

use fontbe::orchestration::AnyWorkId;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Unrecognized source {0}")]
    UnrecognizedSource(PathBuf),
    #[error("yaml error: '{0}'")]
    YamlSerError(#[from] serde_yaml::Error),
    #[error("IO error: '{0}'")]
    IoError(#[from] io::Error),
    #[error("Font IR error: '{0}'")]
    FontIrError(#[from] fontir::error::Error),
    #[error("Unable to produce IR")]
    IrGenerationError,
    #[error("Missing file '{0}'")]
    FileExpected(PathBuf),
    #[error("Tasks failed: {0:?}")]
    TasksFailed(Vec<(AnyWorkId, String)>),
    #[error("Unable to proceed; {0} jobs stuck pending")]
    UnableToProceed(usize),
}
