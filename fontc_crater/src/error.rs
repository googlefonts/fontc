use std::path::PathBuf;

use thiserror::Error;

#[derive(Debug, Error)]
pub(super) enum Error {
    #[error("Failed to load input file: '{0}'")]
    InputFile(std::io::Error),
    #[error("Failed to write file '{path}': '{error}'")]
    WriteFile {
        path: PathBuf,
        #[source]
        error: std::io::Error,
    },
    #[error("Failed to parse input json: '{0}'")]
    InputJson(#[source] serde_json::Error),
    #[error("Failed to encode json: '{0}'")]
    OutputJson(#[source] serde_json::Error),
    #[error("Failed to create cache directory: '{0}'")]
    CacheDir(std::io::Error),
}
