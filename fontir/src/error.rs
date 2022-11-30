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
    ParseError(PathBuf, String),
    #[error("Illegible source")]
    UnableToLoadSource(Box<dyn error::Error>),
    #[error("Missing layer")]
    NoSuchLayer(String),
    #[error("No files associated with glyph")]
    NoFilesForGlyph(String),
    #[error("No design space location(s) associated with glyph")]
    NoLocationsForGlyph(String),
    #[error("Asked to create work for something other than the last input we created")]
    UnableToCreateGlyphIrWork,
}

/// An async work error, hence one that must be Send
#[derive(Debug, Error)]
pub enum WorkError {
    #[error("IO failure")]
    IoError(#[from] io::Error),
}
