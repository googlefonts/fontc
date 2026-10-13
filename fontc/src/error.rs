use std::{
    fmt::{self, Display},
    io,
    path::PathBuf,
};

use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum Error {
    #[error("'{0}' exists but is not a directory")]
    ExpectedDirectory(PathBuf),
    #[error("io failed for '{path}': '{source}'")]
    FileIo {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error("failed to write to stdout or stderr: '{0}'")]
    StdioWriteFail(#[source] io::Error),
    #[error("Unrecognized source {0}")]
    UnrecognizedSource(PathBuf),
    #[error(transparent)]
    FontIrError(#[from] fontir::error::Error),
    #[error(transparent)]
    Backend(#[from] fontbe::error::Error),
    #[error("Missing file '{0}'")]
    FileExpected(PathBuf),
    #[error("Unable to proceed; {0} jobs stuck pending")]
    UnableToProceed(usize),
    #[error("No output file specified")]
    NoOutputFile,
    #[error("A task panicked: '{0}'")]
    Panic(String),
}

impl Error {
    /// Display this error followed by its chain of causes.
    ///
    /// Each cause is appended after a colon, unless the message before it
    /// already includes it, as happens with errors that both print their
    /// source and return it.
    pub fn display_with_causes(&self) -> impl Display + '_ {
        WithCauses(self)
    }
}

struct WithCauses<'a>(&'a dyn std::error::Error);

impl Display for WithCauses<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut parent = self.0.to_string();
        f.write_str(&parent)?;
        let mut next = self.0.source();
        while let Some(cause) = next {
            let message = cause.to_string();
            if !parent.contains(&message) {
                write!(f, ": {message}")?;
            }
            parent = message;
            next = cause.source();
        }
        Ok(())
    }
}
