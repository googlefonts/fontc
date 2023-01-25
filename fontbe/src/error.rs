use std::io;

use fea_rs::compile::error::{BinaryCompilationError, CompilerError};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("Fea binary assembly failure")]
    FeaAssembleError(#[from] BinaryCompilationError),
    #[error("Fea compilation failure")]
    FeaCompileError(#[from] CompilerError),
}
