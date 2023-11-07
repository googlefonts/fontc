use std::path::PathBuf;

use read_fonts::{types::Tag, ReadError};

#[derive(Debug, thiserror::Error)]
pub(crate) enum Error {
    #[error("could not read path '{path}': '{inner}'")]
    LoadError {
        path: PathBuf,
        inner: std::io::Error,
    },
    #[error("could not read font data: '{0}")]
    ReadError(ReadError),
    #[error("missing table '{0}'")]
    MissingTable(Tag),
}
