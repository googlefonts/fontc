use fontir::error::Error as IrError;
use norad::error::GlifLoadError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Failed to load glif")]
    GlifLoadError(#[from] GlifLoadError),
    #[error("Failed to convert to IR")]
    IrError(#[from] IrError),
}
