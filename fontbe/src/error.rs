use std::{fmt::Display, io};

use fea_rs::compile::error::{BinaryCompilationError, CompilerError};
use fontdrasil::types::GlyphName;
use thiserror::Error;
use write_fonts::tables::glyf::BadKurbo;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("Fea binary assembly failure")]
    FeaAssembleError(#[from] BinaryCompilationError),
    #[error("Fea compilation failure")]
    FeaCompileError(#[from] CompilerError),
    #[error("'{0}' {1}")]
    GlyphError(GlyphName, GlyphProblem),
    #[error("'{0}' {1:?} {2}")]
    KurboError(GlyphName, BadKurbo, String),
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
