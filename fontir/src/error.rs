use std::{error, fmt::Display, io, path::PathBuf};

use fontdrasil::{
    coords::{DesignCoord, NormalizedCoord, NormalizedLocation, UserCoord, UserLocation},
    types::GlyphName,
};
use kurbo::Point;
use smol_str::SmolStr;
use thiserror::Error;
use write_fonts::types::{InvalidTag, Tag};

// TODO: eliminate dyn Error and collapse Error/WorkError

#[derive(Debug, Error)]
pub enum Error {
    #[error("Missing directory '{0}'")]
    DirectoryExpected(PathBuf),
    #[error("Missing expected file '{0}'")]
    FileExpected(PathBuf),
    #[error("No extension identified for '{0}'")]
    Unrecognized(PathBuf),
    #[error("No parent for '{0}'")]
    ParentExpected(PathBuf),
    #[error("IO failure: '{0}'")]
    IoError(#[from] io::Error),
    #[error("Unable to parse {0:?}: {1}")]
    ParseError(PathBuf, String),
    #[error("Missing required axis values for {0}")]
    NoAxisDefinitions(String),
    #[error("Axis {0} has no entry in axes")]
    NoEntryInAxes(String),
    #[error("Axis definitions are inconsistent: '{0}'")]
    InconsistentAxisDefinitions(String),
    #[error("Illegible source: '{0}'")]
    UnableToLoadSource(Box<dyn error::Error>),
    #[error("Missing layer '{0}'")]
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
    #[error("Invalid data {0}")]
    InvalidInputData(String),
}

/// An async work error, hence one that must be Send
#[derive(Debug, Error)]
pub enum WorkError {
    #[error("IO failure: '{0}'")]
    IoError(#[from] io::Error),
    // I can't use Box(<dyn error::Error>) here because it's not Send, but
    // if I convert error to string I lose the backtrace... What to do?
    #[error("Conversion of glyph '{0:?}' to IR failed: {1}")]
    GlyphIrWorkError(GlyphName, String),
    #[error("yaml error: '{0}'")]
    YamlSerError(#[from] serde_yaml::Error),
    #[error("No axes are defined")]
    NoAxisDefinitions,
    #[error("Axis definitions are inconsistent: '{0}'")]
    InconsistentAxisDefinitions(String),
    #[error("'{0}' has no position on {1}")]
    NoAxisPosition(GlyphName, String),
    #[error("'{0}' has a position on {1}, what's {1}??")]
    UnexpectedAxisPosition(GlyphName, String),
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
    #[error("Bad anchor '{anchor}' for glyph '{glyph}': '{reason}'")]
    BadAnchor {
        glyph: GlyphName,
        anchor: SmolStr,
        reason: BadAnchorReason,
    },
    #[error("No source with layerName \"{0}\" exists")]
    NoSourceForName(String),
    #[error("Source file contained a construct we don't yet support: {0}")]
    UnsupportedConstruct(String),
}

/// Reasons an anchor can be malformed
#[derive(Clone, Debug, PartialEq)]
pub enum BadAnchorReason {
    /// Multiple definitions at a given location
    Ambiguous(NormalizedLocation),
    NoDefault,
    // top_0 looks like a ligature base, but 0 is an invalid index
    ZeroIndex,
    // _top_1 looks like a numbered mark, which is not allowed
    NumberedMarkAnchor,
    // _ is not a valid group name
    NilMarkGroup,
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

impl Display for BadAnchorReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BadAnchorReason::NoDefault => write!(f, "no value at default location"),
            BadAnchorReason::Ambiguous(loc) => write!(f, "multiple definitions at {loc:?}"),
            BadAnchorReason::ZeroIndex => write!(f, "ligature indexes must begin with '1'"),
            BadAnchorReason::NumberedMarkAnchor => write!(f, "mark anchors cannot be numbered"),
            BadAnchorReason::NilMarkGroup => write!(f, "mark anchor key is nil"),
        }
    }
}
