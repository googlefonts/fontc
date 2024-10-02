//! Error types related to compilation

use std::fmt::Display;

use write_fonts::{read::ReadError, BuilderError};

use crate::{parse::SourceLoadError, DiagnosticSet};

/// An error that occurs when extracting a glyph order from a UFO.
#[derive(Clone, Debug, thiserror::Error)]
pub enum UfoGlyphOrderError {
    /// Missing 'public.glyphOrder' key
    #[error("No public.glyphOrder key in lib.plist")]
    KeyNotSet,
    /// Glyph order is present, but malformed
    #[error("public.glyphOrder exists, but is not an array of strings")]
    Malformed,
}

/// An error that occurs when extracting a glyph order from a font file.
#[derive(Clone, Debug, thiserror::Error)]
pub enum FontGlyphOrderError {
    /// Failed to read font data
    #[error("Failed to read font data: '{0}'")]
    ReadError(
        #[from]
        #[source]
        ReadError,
    ),
    /// Post table is missing glyph names
    #[error("The post table exists, but did not include all glyph names")]
    MissingNames,
}

/// An error that occurs when loading a raw glyph order.
#[derive(Clone, Debug, thiserror::Error)]
pub enum GlyphOrderError {
    /// Invalid name
    #[error("Invalid name '{name}' in glyph order")]
    #[allow(missing_docs)]
    NameError { name: String },
    /// Missing .notdef glyph
    #[error("The first glyph must be '.notdef'")]
    MissingNotDef,
}

/// An error reported by the compiler
#[derive(Debug, thiserror::Error)]
#[allow(missing_docs)]
pub enum CompilerError {
    #[error(transparent)]
    SourceLoad(#[from] SourceLoadError),
    #[error("FEA parsing failed with {} errors", .0.messages.len())]
    ParseFail(DiagnosticSet),
    #[error("FEA validation failed with {} errors", .0.messages.len())]
    ValidationFail(DiagnosticSet),
    #[error("FEA compilation failed with {} errors", .0.messages.len())]
    CompilationFail(DiagnosticSet),
    #[error(transparent)]
    WriteFail(#[from] BuilderError),
}

impl CompilerError {
    /// Return a `Display` type that reports the location and nature of syntax errors
    pub fn display_verbose(&self) -> impl Display + '_ {
        struct Verbose<'a>(&'a CompilerError);
        impl std::fmt::Display for Verbose<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.0)?;
                if let Some(diagnostics) = self.0.diagnostics() {
                    write!(f, "\n{}", diagnostics.display())?;
                }
                Ok(())
            }
        }
        Verbose(self)
    }

    /// Return the `DiagnosticSet` associated, if any
    pub fn diagnostics(&self) -> Option<&DiagnosticSet> {
        match self {
            CompilerError::ParseFail(x)
            | CompilerError::ValidationFail(x)
            | CompilerError::CompilationFail(x) => Some(x),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Some clients, notably fontc, expect this.
    #[test]
    fn assert_compiler_error_is_send() {
        fn send_me_baby<T: Send>() {}
        send_me_baby::<CompilerError>();
    }
}
