//! Reporting errors, warnings, and other information to the user.
use crate::{
    parse::{FileId, SourceList},
    ParseTree,
};
use std::{convert::TryInto, ops::Range, sync::Arc};

/// A span of a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: u32,
    end: u32,
}

/// A diagnostic level
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Level {
    /// An unrecoverable error
    Error,
    /// A warning: something the user may want to address, but which is non-fatal
    Warning,
    /// Info. unused?
    Info,
}

/// A message, associated with a location in a file.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Message {
    pub text: String,
    pub file: FileId,
    pub span: Span,
}

/// A diagnostic, including a message and additional annotations
//TODO: would this be more useful with additional annotations or a help field?
//some fancy error reporting crates have these.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    /// The main message for this diagnostic
    pub message: Message,
    /// The diagnostic level
    pub level: Level,
}

/// A set of diagnostics with the associated source info
#[derive(Clone)]
pub struct DiagnosticSet {
    pub(crate) messages: Vec<Diagnostic>,
    pub(crate) sources: Arc<SourceList>,
    pub(crate) max_to_print: usize,
}

impl Span {
    /// Convert this span to a `Range<usize>`
    pub fn range(&self) -> Range<usize> {
        self.start as usize..self.end as usize
    }
}

impl Diagnostic {
    /// Create a new diagnostic
    pub fn new(
        level: Level,
        file: FileId,
        range: Range<usize>,
        message: impl Into<String>,
    ) -> Self {
        Diagnostic {
            message: Message {
                text: message.into(),
                span: Span {
                    start: range.start.try_into().unwrap(),
                    end: range.end.try_into().unwrap(),
                },
                file,
            },
            level,
        }
    }

    /// Create a new error, at the provided location
    pub fn error(file: FileId, span: Range<usize>, message: impl Into<String>) -> Self {
        Diagnostic::new(Level::Error, file, span, message)
    }

    /// Create a new warning, at the provided location
    pub fn warning(file: FileId, span: Range<usize>, message: impl Into<String>) -> Self {
        Diagnostic::new(Level::Warning, file, span, message)
    }

    /// The diagnostic's message text
    pub fn text(&self) -> &str {
        &self.message.text
    }

    /// The location of the main span, as a `Range<usize>`
    pub fn span(&self) -> Range<usize> {
        self.message.span.range()
    }

    /// `true` if this diagnostic is an error
    pub fn is_error(&self) -> bool {
        matches!(self.level, Level::Error)
    }
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

    /// Discard any warnings, keeping only errors
    pub fn discard_warnings(&mut self) {
        self.messages.retain(|x| x.is_error());
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

    /// Return the underlying diagnostics, as a slice
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.messages
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
