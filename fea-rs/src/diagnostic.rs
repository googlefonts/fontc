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
