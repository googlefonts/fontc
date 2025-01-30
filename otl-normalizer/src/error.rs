use std::path::PathBuf;

use write_fonts::{read::ReadError, types::Tag};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("could not read path '{path}': '{inner}'")]
    Load {
        path: PathBuf,
        inner: std::io::Error,
    },
    #[error("could not create file '{path}': '{inner}'")]
    FileWrite {
        path: PathBuf,
        inner: std::io::Error,
    },
    #[error("write error: '{0}'")]
    Write(#[from] std::io::Error),
    #[error("could not read font data: '{0}")]
    FontRead(#[from] ReadError),
    #[error("missing table '{0}'")]
    MissingTable(Tag),
}
