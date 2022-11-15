use std::{error::Error, io, path::PathBuf};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum FontIrError {
    #[error("Directory expected")]
    DirectoryExpected(PathBuf),
    #[error("File expected")]
    FileExpected(PathBuf),
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("Unable to parse")]
    ParseError(PathBuf, Box<dyn Error>),
    #[error("Illegible source")]
    UnableToLoadSource(Box<dyn Error>),
}
