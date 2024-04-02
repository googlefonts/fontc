use std::{fmt::Display, io, path::PathBuf};

use fea_rs::compile::error::CompilerError;
use fontdrasil::{coords::NormalizedLocation, types::GlyphName};
use fontir::{
    error::VariationModelError, ir::KernPair, orchestration::WorkId as FeWorkId,
    variations::DeltaError,
};
use smol_str::SmolStr;
use thiserror::Error;
use write_fonts::{
    read::ReadError,
    tables::{
        cmap::CmapConflict,
        glyf::MalformedPath,
        gvar::{iup::IupError, GvarInputError},
    },
    types::Tag,
};

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("Fea compilation failure {0}")]
    FeaCompileError(#[from] CompilerError),
    #[error("'{0}' {1}")]
    GlyphError(GlyphName, GlyphProblem),
    #[error("'{glyph_name}' {kurbo_problem:?} {context}")]
    KurboError {
        glyph_name: GlyphName,
        kurbo_problem: MalformedPath,
        context: String,
    },
    #[error("'{glyph}' references {referenced_glyph}, {problem}")]
    ComponentError {
        glyph: GlyphName,
        referenced_glyph: GlyphName,
        problem: GlyphProblem,
    },
    #[error("'{glyph}' {errors:?}")]
    ComponentErrors {
        glyph: GlyphName,
        errors: Vec<Error>,
    },
    #[error("Generating bytes for {context} failed: {e}")]
    DumpTableError {
        e: write_fonts::error::Error,
        context: String,
    },
    #[error("{what} out of bounds: {value}")]
    OutOfBounds { what: String, value: String },
    #[error("Unable to compute deltas for {0}: {1}")]
    GlyphDeltaError(GlyphName, DeltaError),
    #[error("Unable to compute deltas for MVAR {0}: {1}")]
    MvarDeltaError(Tag, DeltaError),
    #[error("Unable to compute deltas for anchor on '{0}': '{1}'")]
    AnchorDeltaError(GlyphName, DeltaError),
    #[error("Unable to compute deltas for kern pair '{}/{}': '{error}'", .pair.0, .pair.1)]
    KernDeltaError { pair: KernPair, error: DeltaError },
    #[error("Unable to assemble gvar")]
    GvarError(#[from] GvarInputError),
    #[error("Unable to read")]
    ReadFontsReadError(#[from] ReadError),
    #[error("IUP error for {0}: {1:?}")]
    IupError(GlyphName, IupError),
    #[error("Unable to interpret bytes as {0}")]
    InvalidTableBytes(Tag),
    #[error("Missing directory:{0}")]
    MissingDirectory(PathBuf),
    #[error("Variation model error in '{0}': {1}")]
    VariationModelError(GlyphName, VariationModelError),
    #[error("Missing file:{0}")]
    FileExpected(PathBuf),
    #[error("Missing {0}")]
    MissingTable(Tag),
    #[error("Expected an anchor, got {0:?}")]
    ExpectedAnchor(FeWorkId),
    #[error("No glyph class '{0}'")]
    MissingGlyphClass(GlyphName),
    #[error("Mark glyph '{glyph}' in conflicting classes '{old_class}' and '{new_class}'")]
    //FIXME: this can be deleted eventually, we will manually ensure classes are disjoint
    PreviouslyAssignedMarkClass {
        old_class: SmolStr,
        new_class: SmolStr,
        glyph: GlyphName,
    },
    #[error("No variation model for '{0:?}'")]
    NoVariationModel(NormalizedLocation),
    #[error("Delta error '{0:?}'")]
    DeltaError(DeltaError),
    #[error("No glyph id for '{0}'")]
    MissingGlyphId(GlyphName),
    #[error("Error making CMap: {0}")]
    CmapConflict(#[from] CmapConflict),
}

#[derive(Debug)]
pub enum GlyphProblem {
    InconsistentComponents,
    InconsistentPathElements,
    HasComponentsAndPath,
    MissingDefault,
    NoComponents,
    NotInGlyphOrder,
}

impl Display for GlyphProblem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            GlyphProblem::HasComponentsAndPath => "has components *and* paths",
            GlyphProblem::InconsistentComponents => {
                "has different components at different points in designspace"
            }
            GlyphProblem::InconsistentPathElements => "has interpolation-incompatible paths",
            GlyphProblem::MissingDefault => "has no default master",
            GlyphProblem::NoComponents => "has no components",
            GlyphProblem::NotInGlyphOrder => "has no entry in glyph order",
        };
        f.write_str(message)
    }
}
