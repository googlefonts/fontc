use std::{error, io, path::PathBuf};

use thiserror::Error;

use crate::ir::DesignSpaceLocation;

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
    NoStateForGlyph(String),
    #[error("No design space location(s) associated with glyph")]
    NoLocationsForGlyph(String),
    #[error("Asked to create work for something other than the last input we created")]
    UnableToCreateGlyphIrWork,
    #[error("Unexpected state encountered in a state set")]
    UnexpectedState,
    #[error("Duplicate location for {what}: {loc:?}")]
    DuplicateLocation {
        what: String,
        loc: DesignSpaceLocation,
    },
}

/// An async work error, hence one that must be Send
#[derive(Debug, Error)]
pub enum WorkError {
    #[error("IO failure")]
    IoError(#[from] io::Error),
    // I can't use Box(<dyn error::Error>) here because it's not Send, but
    // if I convert error to string I lose the backtrace... What to do?
    #[error("Conversion of glyph '{0}' to IR failed: {1}")]
    GlyphIrWorkError(String, String),
    #[error("yaml error")]
    YamlSerError(#[from] serde_yaml::Error),
    #[error("No axes are defined")]
    NoAxisDefinitions,
    #[error("Axis definitions are inconsistent")]
    InconsistentAxisDefinitions,
}
