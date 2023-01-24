use std::io;

use fea_rs::compile::error::BinaryCompilationError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO failure")]
    IoError(#[from] io::Error),
    #[error("Fea binary assembly failure")]
    FeaAssembleError(#[from] BinaryCompilationError),
    // String because https://github.com/cmyr/fea-rs/pull/119/files#r1084797298
    #[error("Fea compilation failure {0}")]
    FeaCompileError(String),
}
