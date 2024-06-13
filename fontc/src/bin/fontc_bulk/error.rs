use thiserror::Error;

#[derive(Debug, Error)]
pub(super) enum Error {
    #[error("Failed to load input file: '{0}'")]
    InputFile(std::io::Error),
    #[error("Failed to write output file: '{0}'")]
    OutputFile(std::io::Error),
    #[error("Failed to parse input json: '{0}'")]
    InputJson(#[from] serde_json::Error),
    #[error("Failed to create cache directory: '{0}'")]
    CacheDir(std::io::Error),
}
