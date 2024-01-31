//! Error types related to compilation

use std::sync::Arc;

use write_fonts::{read::ReadError, BuilderError};

use crate::{
    parse::{SourceList, SourceLoadError},
    Diagnostic, ParseTree,
};

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
    #[error("{0}")]
    SourceLoad(
        #[from]
        #[source]
        SourceLoadError,
    ),
    #[error("Parsing failed with {} errors\n{}", .0.messages.len(), .0.display())]
    ParseFail(DiagnosticSet),
    #[error("Validation failed with {} errors\n{}", .0.messages.len(), .0.display())]
    ValidationFail(DiagnosticSet),
    #[error("Compilation failed with {} errors\n{}", .0.messages.len(), .0.display())]
    CompilationFail(DiagnosticSet),
    #[error("{0}")]
    WriteFail(#[from] BuilderError),
}

/// A set of diagnostics with the associated source info
#[derive(Clone)]
pub struct DiagnosticSet {
    pub(crate) messages: Vec<Diagnostic>,
    pub(crate) sources: Arc<SourceList>,
    pub(crate) max_to_print: usize,
}

// we don't want diagnostic set to impl display itself, because we want to change
// behaviour based on whether we think we're writing to a terminal, and that is
// error prone.
struct DiagnosticDisplayer<'a>(&'a DiagnosticSet);

impl DiagnosticSet {
    /// Create a new `DiagnosticSet`.
    pub fn new(messages: Vec<Diagnostic>, tree: &ParseTree, max_to_print: usize) -> Self {
        Self {
            messages,
            sources: tree.sources.clone(),
            max_to_print,
        }
    }
    /// The number of diagnostic messages
    pub fn len(&self) -> usize {
        self.messages.len()
    }

    /// `true` if there are no diagnostic messages
    pub fn is_empty(&self) -> bool {
        self.messages.is_empty()
    }

    /// `true` if any of the messages in this set indicate hard errors
    pub fn has_errors(&self) -> bool {
        self.messages.iter().any(|msg| msg.is_error())
    }

    /// Set the max number of messages to print.
    pub fn set_max_to_print(&mut self, max_to_print: usize) {
        self.max_to_print = max_to_print;
    }

    /// Remove and return any warnings in this set.
    pub fn split_off_warnings(&mut self) -> Option<DiagnosticSet> {
        self.messages.sort_unstable_by_key(|d| d.level);
        let split_at = self.messages.iter().position(|x| !x.is_error())?;
        let warnings = self.messages.split_off(split_at);
        Some(Self {
            messages: warnings,
            sources: self.sources.clone(),
            max_to_print: self.max_to_print,
        })
    }

    /// Returns an opaque type that can pretty-print the diagnostics
    pub fn display(&self) -> impl std::fmt::Display + '_ {
        DiagnosticDisplayer(self)
    }

    pub(crate) fn write(&self, f: &mut impl std::fmt::Write, colorize: bool) -> std::fmt::Result {
        let mut first = true;
        for err in self.messages.iter().take(self.max_to_print) {
            if !first {
                writeln!(f)?;
            }
            write!(f, "{}", self.sources.format_diagnostic(err, colorize))?;
            first = false;
        }
        if let Some(overflow) = self.messages.len().checked_sub(self.max_to_print) {
            writeln!(f, "... and {overflow} more errors")?;
        }
        Ok(())
    }

    #[cfg(any(test, feature = "test"))]
    pub(crate) fn to_string(&self, colorize: bool) -> String {
        let mut out = String::new();
        self.write(&mut out, colorize).unwrap();
        out
    }
}

impl std::fmt::Display for DiagnosticDisplayer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use std::io::IsTerminal;
        let colorize = std::io::stderr().is_terminal();
        self.0.write(f, colorize)
    }
}

impl std::fmt::Debug for DiagnosticSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiagnosticSet")
            .field("messages", &self.messages)
            .field("tree", &"ParseTree")
            .finish()
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
