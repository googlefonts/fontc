//! Reporting errors, warnings, and other information to the user.

use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
    Info,
}

#[derive(Clone, Debug)]
pub struct Message {
    pub text: String,
    pub span: Range<usize>,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub message: Message,
    pub level: Level,
    annotations: Vec<Message>,
    help: Option<String>,
}

impl Diagnostic {
    pub fn new(level: Level, span: Range<usize>, message: impl Into<String>) -> Self {
        Diagnostic {
            message: Message {
                text: message.into(),
                span,
            },
            level,
            annotations: Vec::new(),
            help: None,
        }
    }

    pub fn error(span: Range<usize>, message: impl Into<String>) -> Self {
        Diagnostic::new(Level::Error, span, message)
    }

    pub fn warning(span: Range<usize>, message: impl Into<String>) -> Self {
        Diagnostic::new(Level::Warning, span, message)
    }

    pub fn annotation(mut self, text: impl Into<String>, span: Range<usize>) -> Self {
        self.annotations.push(Message {
            text: text.into(),
            span,
        });
        self
    }

    pub fn help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn text(&self) -> &str {
        &self.message.text
    }

    pub fn span(&self) -> Range<usize> {
        self.message.span.clone()
    }

    pub fn is_error(&self) -> bool {
        matches!(self.level, Level::Error)
    }
}
