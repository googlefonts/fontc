use std::{io, path::PathBuf};

use thiserror::Error;
use toml::de::Error as TomlError;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum Error {
    #[error("'{0}' exists but is not a directory")]
    ExpectedDirectory(PathBuf),
    #[error("io failed for '{path}': '{source}'")]
    FileIo {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error("failed to parse COLRv1 config '{path}': '{source}'")]
    ColrV1Config {
        path: PathBuf,
        #[source]
        source: TomlError,
    },
    #[error("failed to write to stdout or stderr: '{0}'")]
    StdioWriteFail(#[source] io::Error),
    #[error("Unrecognized source {0}")]
    UnrecognizedSource(PathBuf),
    #[error(transparent)]
    FontIrError(#[from] fontir::error::Error),
    #[error(transparent)]
    Backend(#[from] fontbe::error::Error),
    #[error("Missing file '{0}'")]
    FileExpected(PathBuf),
    #[error("Unable to proceed; {0} jobs stuck pending")]
    UnableToProceed(usize),
    #[error("No output file specified")]
    NoOutputFile,
    #[error("A task panicked: '{0}'")]
    Panic(String),
}
