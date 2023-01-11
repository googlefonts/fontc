use std::{error, io, path::PathBuf};

use fontdrasil::types::GlyphName;
use thiserror::Error;

use crate::coords::{DesignCoord, NormalizedCoord, NormalizedLocation, UserLocation};

// TODO: eliminate dyn Error and collapse Error/WorkError

#[derive(Debug, Error)]
pub enum Error {
    #[error("Directory expected")]
    DirectoryExpected(PathBuf),
    #[error("File expected")]
    FileExpected(PathBuf),
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("Unable to parse {0:?}: {1}")]
    ParseError(PathBuf, String),
    #[error("Missing required axis values for {0}")]
    NoAxisDefinitions(String),
    #[error("Axis definitions are inconsistent")]
    InconsistentAxisDefinitions(String),
    #[error("Illegible source")]
    UnableToLoadSource(Box<dyn error::Error>),
    #[error("Missing layer")]
    NoSuchLayer(String),
    #[error("No files associated with glyph {0}")]
    NoStateForGlyph(GlyphName),
    #[error("No design space location(s) associated with glyph {0}")]
    NoLocationsForGlyph(GlyphName),
    #[error("Asked to create work for something other than the last input we created")]
    UnableToCreateGlyphIrWork,
    #[error("Unexpected state encountered in a state set")]
    UnexpectedState,
    #[error("Duplicate location for {what}: {loc:?}")]
    DuplicateUserLocation { what: String, loc: UserLocation },
    #[error("Duplicate location for {what}: {loc:?}")]
    DuplicateNormalizedLocation {
        what: String,
        loc: NormalizedLocation,
    },
    #[error("Global metadata very bad, very very bad")]
    InvalidGlobalMetadata,
    #[error("No default master in {0:?}")]
    NoDefaultMaster(PathBuf),
    #[error("Missing mapping on {axis} for {field} at {value:?}")]
    MissingMappingForDesignCoord {
        axis: String,
        field: String,
        value: DesignCoord,
    },
}

/// An async work error, hence one that must be Send
#[derive(Debug, Error)]
pub enum WorkError {
    #[error("IO failure")]
    IoError(#[from] io::Error),
    // I can't use Box(<dyn error::Error>) here because it's not Send, but
    // if I convert error to string I lose the backtrace... What to do?
    #[error("Conversion of glyph '{0:?}' to IR failed: {1}")]
    GlyphIrWorkError(GlyphName, String),
    #[error("yaml error")]
    YamlSerError(#[from] serde_yaml::Error),
    #[error("No axes are defined")]
    NoAxisDefinitions,
    #[error("Axis definitions are inconsistent")]
    InconsistentAxisDefinitions(String),
    #[error("I am the glyph with gid, {0}")]
    NoGlyphIdForName(String),
    #[error("No Glyph for name {0:?}")]
    NoGlyphForName(GlyphName),
    #[error("File expected: {0:?}")]
    FileExpected(PathBuf),
    #[error("Expected to match: {0:?}, {1:?}")]
    FileMismatch(PathBuf, PathBuf),
    #[error("Metadata access expected: {0:?}")]
    MetadataFailed(PathBuf),
    #[error("Unable to parse {0:?}: {1}")]
    ParseError(PathBuf, String),
    #[error("No default master in {0:?}")]
    NoDefaultMaster(PathBuf),
    #[error("No master {master} exists. Referenced by glyph {glyph:?}.")]
    NoMasterForGlyph { master: String, glyph: GlyphName },
    #[error("Failed to add glyph source: {0}")]
    AddGlyphSource(String),
    #[error("{glyph_name} undefined on {axis} at required position {pos:?}")]
    GlyphUndefAtNormalizedPosition {
        glyph_name: GlyphName,
        axis: String,
        pos: NormalizedCoord,
    },
}
