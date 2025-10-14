use std::{io, num::TryFromIntError, path::PathBuf};

use smol_str::SmolStr;
use thiserror::Error;

use crate::smart_components::BadSmartComponent;

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
    #[error("Invalid code page {0}")]
    InvalidCodePage(u32),
    #[error("Invalid value: {0}")]
    BadValue(String),
    #[error("Bad smart component '{component}' in glyph '{glyph}': {issue}")]
    BadSmartComponent {
        glyph: SmolStr,
        component: SmolStr,
        issue: BadSmartComponent,
    },
    #[error("{value} expected to be between {lbound} and {ubound}")]
    ProductionNameOutOfBounds {
        value: u32,
        lbound: u32,
        ubound: u32,
    },
}
