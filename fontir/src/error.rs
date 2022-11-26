use std::{error, io, path::PathBuf};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Directory expected")]
    DirectoryExpected(PathBuf),
    #[error("File expected")]
    FileExpected(PathBuf),
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("Unable to parse")]
    ParseError(PathBuf, Box<dyn error::Error>),
    #[error("Illegible source")]
    UnableToLoadSource(Box<dyn error::Error>),
}

/// An async work error, hence one that must be Send
#[derive(Debug)]
pub enum WorkError {}
