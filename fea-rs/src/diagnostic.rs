//! Reporting errors, warnings, and other information to the user.
use crate::parse::FileId;
use std::{convert::TryInto, ops::Range};

/// A span of a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: u32,
    end: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
    Info,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Message {
    pub text: String,
    pub file: FileId,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub message: Message,
    pub level: Level,
    annotations: Vec<Message>,
    help: Option<String>,
}

// internal: a diagnostic generated during parsing, that doesn't have an explicit
// FileId; the FileId is added when the tree is built.
#[derive(Clone)]
pub struct LocalDiagnostic(Diagnostic);

impl Span {
    pub fn range(&self) -> Range<usize> {
        self.start as usize..self.end as usize
    }
}

impl Diagnostic {
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
            annotations: Vec::new(),
            help: None,
        }
    }

    pub fn error(file: FileId, span: Range<usize>, message: impl Into<String>) -> Self {
        Diagnostic::new(Level::Error, file, span, message)
    }

    pub fn warning(file: FileId, span: Range<usize>, message: impl Into<String>) -> Self {
        Diagnostic::new(Level::Warning, file, span, message)
    }

    //pub fn annotation(mut self, text: impl Into<String>, span: Range<usize>) -> Self {
    //self.annotations.push(Message {
    //text: text.into(),
    //span: span.into(),
    //});
    //self
    //}

    pub fn help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn text(&self) -> &str {
        &self.message.text
    }

    pub fn span(&self) -> Range<usize> {
        self.message.span.range()
    }

    pub fn is_error(&self) -> bool {
        matches!(self.level, Level::Error)
    }
}

impl LocalDiagnostic {
    pub fn new(level: Level, range: Range<usize>, message: impl Into<String>) -> Self {
        LocalDiagnostic(Diagnostic::new(level, FileId::CURRENT_FILE, range, message))
    }

    pub fn error(span: Range<usize>, message: impl Into<String>) -> Self {
        Self::new(Level::Error, span, message)
    }

    pub fn warning(span: Range<usize>, message: impl Into<String>) -> Self {
        Self::new(Level::Warning, span, message)
    }

    pub fn into_diagnostic(mut self, in_file: FileId) -> Diagnostic {
        self.0.message.file = in_file;
        self.0.annotations.iter_mut().for_each(|m| m.file = in_file);
        self.0
    }

    pub fn text(&self) -> &str {
        self.0.text()
    }

    pub fn span(&self) -> Range<usize> {
        self.0.span()
    }

    pub fn is_error(&self) -> bool {
        matches!(self.0.level, Level::Error)
    }
}
