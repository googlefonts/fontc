use std::{error, io, path::PathBuf};

use font_types::{InvalidTag, Tag};
use fontdrasil::{
    coords::{DesignCoord, NormalizedCoord, NormalizedLocation, UserCoord, UserLocation},
    types::{AnchorName, GlyphName},
};
use kurbo::Point;
use thiserror::Error;

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
    #[error("Axis {0} has no entry in axes")]
    NoEntryInAxes(String),
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
    #[error("Global metadata very bad, very very bad")]
    InvalidGlobalMetadata,
    #[error("No default master in {0:?}")]
    NoDefaultMaster(PathBuf),
    #[error("Missing mapping on {axis_name} for {field} at {value:?}. Mappings {mappings:?}")]
    MissingMappingForDesignCoord {
        axis_name: String,
        field: String,
        mappings: Vec<(UserCoord, DesignCoord)>,
        value: DesignCoord,
    },
    #[error("Invalid tag")]
    InvalidTag(#[from] InvalidTag),
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
    #[error("'{0}' has no position on {1}")]
    NoAxisPosition(GlyphName, String),
    #[error("I am the glyph with gid, {0}")]
    NoGlyphIdForName(String),
    #[error("No Glyph for name {0:?}")]
    NoGlyphForName(GlyphName),
    #[error("Directory expected {0}: {1}")]
    DirectoryExpected(String, PathBuf),
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
        axis: Tag,
        pos: NormalizedCoord,
    },
    #[error("{glyph_name} undefined at required position {pos:?}")]
    GlyphUndefAtNormalizedLocation {
        glyph_name: GlyphName,
        pos: NormalizedLocation,
    },
    #[error("Duplicate location for {what}: {loc:?}")]
    DuplicateNormalizedLocation {
        what: String,
        loc: NormalizedLocation,
    },
    #[error("{glyph_name} invalid {message}")]
    InvalidSourceGlyph {
        glyph_name: GlyphName,
        message: String,
    },
    #[error("Path conversion error")]
    PathConversionError(#[from] PathConversionError),
    #[error("Variation model error")]
    VariationModelError(#[from] VariationModelError),
    #[error("Contour reversal error {0}")]
    ContourReversalError(String),
    #[error("Unable to determine units per em")]
    NoUnitsPerEm,
    #[error("Invalid units per em {0}")]
    InvalidUpem(String),
    #[error("Must have exactly one units per em, got {0:?}")]
    InconsistentUpem(Vec<u16>),
    #[error("Invalid tag")]
    InvalidTag(#[from] InvalidTag),
    #[error("Axis '{0}' must map default if it maps anything")]
    AxisMustMapDefault(Tag),
    #[error("Axis '{0}' must map min if it maps anything")]
    AxisMustMapMin(Tag),
    #[error("Axis '{0}' must map max if it maps anything")]
    AxisMustMapMax(Tag),
    #[error("No kerning group or glyph for name {0:?}")]
    InvalidKernSide(String),
    #[error("Multiple definitions for glyph {glyph} anchor {anchor} at {loc:?}")]
    AmbiguousAnchor {
        glyph: GlyphName,
        anchor: AnchorName,
        loc: NormalizedLocation,
    },
    #[error("No value at default for glyph {glyph} anchor {anchor}")]
    NoDefaultForAnchor {
        glyph: GlyphName,
        anchor: AnchorName,
    },
}

/// An async work error, hence one that must be Send
#[derive(Debug, Error)]
pub enum PathConversionError {
    #[error("{glyph_name} has {num_offcurve} consecutive offcurve points {points:?}")]
    TooManyOffcurvePoints {
        glyph_name: GlyphName,
        num_offcurve: usize,
        points: Vec<Point>,
    },
    #[error("{glyph_name} contour contains a 'move' that is not the first point: {point:?}")]
    MoveAfterFirstPoint { glyph_name: GlyphName, point: Point },
}

#[derive(Debug, Error)]
pub enum VariationModelError {
    #[error("{axis_names:?} in {location:?} have no assigned order")]
    AxesWithoutAssignedOrder {
        axis_names: Vec<Tag>,
        location: NormalizedLocation,
    },
    #[error("{0} is is an axis of variation defined only at a single point")]
    PointAxis(Tag),
}
