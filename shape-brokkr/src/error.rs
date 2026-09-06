use kurbo::BezPath;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("{1} {0:?}")]
    InvalidPath(BezPath, String),
    #[error("No curve contains significant movement")]
    NoSignificantActivity,
}
