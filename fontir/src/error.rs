use std::{fmt::Display, io, path::PathBuf};

use fontdrasil::{
    coords::{DesignCoord, NormalizedCoord, NormalizedLocation, UserCoord, UserLocation},
    types::GlyphName,
};
use kurbo::Point;
use smol_str::SmolStr;
use thiserror::Error;
use write_fonts::types::{InvalidTag, Tag};

#[derive(Debug, Error)]
pub enum Error {
    /// A source file was not understood
    #[error(transparent)]
    BadSource(#[from] BadSource),
    /// What path?!
    #[error("{0} does not exist")]
    NoSuchPath(PathBuf),
    /// An error occured while converting a glyph to IR
    #[error(transparent)]
    BadGlyph(#[from] BadGlyph),
    #[error("Failed to delete file {path}: '{source}'")]
    DeleteFailed {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error("UPEM value {0} outside valid range 16..=16384")]
    InvalidUpem(f64),
    #[error("Inconsistent UPEM values: {0:?}")]
    InconsistentUpem(Vec<u16>),
    #[error("Variation model error: '{0}'")]
    VariationModelError(
        #[from]
        #[source]
        VariationModelError,
    ),
    #[error("feature files are non-identical: {0}, {1}")]
    NonIdenticalFea(PathBuf, PathBuf),
    #[error("axis '{0}' missing at least one of default/min/max mapping")]
    MissingAxisMapping(Tag),
    #[error("no glyph for name '{0}'")]
    NoGlyphForName(GlyphName),
    #[error("Missing required axis values for {0}")]
    NoAxisDefinitions(String),
    #[error("Axis {0} has no entry in axes")]
    NoEntryInAxes(String),
    #[error("Axis definitions are inconsistent: '{0}'")]
    InconsistentAxisDefinitions(String),
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
    #[error("No default master in {0}")]
    NoDefaultMaster(PathBuf),
    #[error("Missing mapping on {axis_name} for {field} at {value:?}. Mappings {mappings:?}")]
    MissingMappingForDesignCoord {
        axis_name: String,
        field: String,
        mappings: Vec<(UserCoord, DesignCoord)>,
        value: DesignCoord,
    },
    #[error("Invalid tag '{raw_tag}': {cause}")]
    InvalidTag { raw_tag: String, cause: InvalidTag },
    #[error("Source file contained a construct we don't yet support: {0}")]
    UnsupportedConstruct(String),
    #[error("Inconsistent palette size, [0] has {size_0}, [{n}] has {size_n}")]
    InconsistentPaletteLength {
        size_0: usize,
        n: usize,
        size_n: usize,
    },
    // Look ma, I replace UnknownAxis as well!
    #[error("Unknown {0}: {1}")]
    UnknownEntry(&'static str, String),
    #[error("Invalid {0}: {1}")]
    InvalidEntry(&'static str, String),
}

/// An error related to loading source input files
#[derive(Debug, Error)]
#[error("Reading source failed for '{path}': '{kind}'")]
pub struct BadSource {
    /// The path to the file where the error occured
    path: PathBuf,
    /// The specific error condition encountered
    pub kind: BadSourceKind,
}

/// Conditions under which we can fail to read a source file
#[derive(Debug)]
pub enum BadSourceKind {
    ExpectedDirectory,
    ExpectedFile,
    UnrecognizedExtension,
    ExpectedParent,
    Io(io::Error),
    /// Payload is a message to print; this error can originate from various parsers
    Custom(String),
}

/// An error that occurs while trying to convert a glyph to IR.
///
/// This bundles up various failure cases along with the name of the glyph in
/// question, which forces us to preserve more context.
#[derive(Debug, Error)]
#[error("Invalid source glyph '{name}': '{kind}'")]
pub struct BadGlyph {
    name: GlyphName,
    kind: BadGlyphKind,
}

#[derive(Debug)]
pub enum BadGlyphKind {
    NoInstances,
    DuplicateLocation(NormalizedLocation),
    NoDefaultLocation,
    MissingLayer(String),
    //TODO: can this be collapsed with layer, above?
    MissingMaster(String),
    MultipleDefaultLocations,
    UndefinedAtNormalizedLocation(NormalizedLocation),
    UndefinedAtNormalizedPosition { axis: Tag, pos: NormalizedCoord },
    NoAxisPosition(Tag),
    PathConversion(PathConversionError),
    Anchor(BadAnchor),
}

/// An error that occurs while parsing glyph anchors
#[derive(Debug, Error)]
#[error("Invalid anchor '{name}': '{kind}'")]
pub struct BadAnchor {
    name: SmolStr,
    kind: BadAnchorReason,
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
#[derive(Debug, Error, PartialEq)]
pub enum PathConversionError {
    #[error("has {num_offcurve} consecutive offcurve points {points:?}")]
    TooManyOffcurvePoints {
        num_offcurve: usize,
        points: Vec<Point>,
    },
    #[error("contour contains a 'move' that is not the first point: {point}")]
    MoveAfterFirstPoint { point: Point },
    /// The source data could not be parsed or interpreted
    #[error("{0}")]
    Parse(String),
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

impl BadSource {
    /// Convenience method for creating a `BadSource` error.
    ///
    /// To create a custom error from any printable thing, use [`BadSource::custom`]
    pub fn new(path: impl Into<PathBuf>, kind: impl Into<BadSourceKind>) -> Self {
        Self {
            path: path.into(),
            kind: kind.into(),
        }
    }

    /// A catch-all constructor for additional kinds of errors, such as various parsing failures
    pub fn custom(path: impl Into<PathBuf>, msg: impl Display) -> Self {
        Self::new(path, BadSourceKind::Custom(msg.to_string()))
    }
}

impl BadGlyph {
    /// Convenience method for creating a `BadGlyph` error.
    pub fn new(name: impl Into<GlyphName>, kind: impl Into<BadGlyphKind>) -> Self {
        Self {
            name: name.into(),
            kind: kind.into(),
        }
    }
}

impl BadAnchor {
    pub(crate) fn new(name: impl Into<SmolStr>, kind: impl Into<BadAnchorReason>) -> Self {
        Self {
            name: name.into(),
            kind: kind.into(),
        }
    }
}

impl From<std::io::Error> for BadSourceKind {
    fn from(src: std::io::Error) -> BadSourceKind {
        BadSourceKind::Io(src)
    }
}

impl From<PathConversionError> for BadGlyphKind {
    fn from(src: PathConversionError) -> BadGlyphKind {
        BadGlyphKind::PathConversion(src)
    }
}

impl From<BadAnchor> for BadGlyphKind {
    fn from(src: BadAnchor) -> BadGlyphKind {
        BadGlyphKind::Anchor(src)
    }
}

impl std::fmt::Display for BadSourceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BadSourceKind::ExpectedDirectory => f.write_str("expected directory"),
            BadSourceKind::ExpectedFile => f.write_str("expected file"),
            BadSourceKind::UnrecognizedExtension => f.write_str("unknown file extension"),
            BadSourceKind::ExpectedParent => f.write_str("missing parent directory"),
            BadSourceKind::Io(e) => e.fmt(f),
            BadSourceKind::Custom(e) => f.write_str(e),
        }
    }
}

impl std::fmt::Display for BadGlyphKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BadGlyphKind::NoInstances => f.write_str("no instances"),
            BadGlyphKind::DuplicateLocation(loc) => write!(f, "duplicate location {loc:?}"),
            BadGlyphKind::NoDefaultLocation => f.write_str("no default location"),
            BadGlyphKind::MultipleDefaultLocations => f.write_str("multiple default locations"),
            BadGlyphKind::UndefinedAtNormalizedLocation(loc) => {
                write!(f, "undefined at required location {loc:?}")
            }
            BadGlyphKind::UndefinedAtNormalizedPosition { axis, pos } => {
                write!(f, "undefined on {axis} at required position {pos:?}")
            }
            BadGlyphKind::PathConversion(e) => write!(f, "invalid path: '{e}'"),
            BadGlyphKind::MissingLayer(name) => write!(f, "missing layer '{name}'"),
            BadGlyphKind::MissingMaster(name) => write!(f, "missing master '{name}'"),
            BadGlyphKind::NoAxisPosition(axis) => write!(f, "no position on '{axis}' axis"),
            BadGlyphKind::Anchor(e) => write!(f, "bad anchor: '{e}'"),
        }
    }
}
