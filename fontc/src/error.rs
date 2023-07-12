use std::{io, path::PathBuf};

use fontbe::orchestration::AnyWorkId;
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
    #[error("Does not exist")]
    FileExpected(PathBuf),
    #[error("At least one work item failed")]
    TasksFailed(Vec<(AnyWorkId, String)>),
    #[error("Invalid regex")]
    BadRegex(#[from] regex::Error),
    #[error("Unable to proceed")]
    UnableToProceed,
    // Not sure what the best message is, for this; it probably means that
    // a thread has panicked?
    #[error("A task stopped responding unexpectedly")]
    InternalError,
}
