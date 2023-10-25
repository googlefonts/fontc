//! Reporting errors, warnings, and other information to the user.
use crate::parse::FileId;
use std::{convert::TryInto, ops::Range};

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
