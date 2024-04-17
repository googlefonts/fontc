use std::{io, num::TryFromIntError, path::PathBuf};

use smol_str::SmolStr;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("Unable to parse {0}: {1}")]
    ParseError(PathBuf, String),
    #[error("Unexpected file structure {0}")]
    StructuralError(String),
    #[error("No upem")]
    NoUnitsPerEm,
    #[error("Invalid upem")]
    InvalidUpem(#[from] TryFromIntError),
    #[error("Unrecognized name {0}")]
    UnknownValueName(String),
    #[error("Not a .glyphspackage directory: {0}")]
    NotAGlyphsPackage(PathBuf),
    #[error("Invalid plist")]
    WorstPlistEver(#[from] crate::plist::Error),
    #[error("Unknown glyph category '{category}' for glyph '{glyph}'")]
    BadCategory { glyph: SmolStr, category: SmolStr },
}
