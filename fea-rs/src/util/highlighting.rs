//! syntax highlighting functions

use std::fmt::Write;

use crate::{parse::Source, Diagnostic, Kind, Level};
use ansi_term::{Colour, Style};

pub fn style_for_kind(kind: Kind) -> Style {
    match kind {
        Kind::Comment => Style::new().fg(Colour::Yellow).dimmed(),
        Kind::Number | Kind::Metric | Kind::Octal | Kind::Hex | Kind::Float | Kind::String => {
            Style::new().fg(Colour::Green)
        }
        Kind::Ident | Kind::Tag | Kind::Label => Style::new().fg(Colour::Purple),
        Kind::TableKw
        | Kind::IncludeKw
        | Kind::LookupKw
        | Kind::LanguagesystemKw
        | Kind::AnchorDefKw
        | Kind::FeatureKw
        | Kind::MarkClassKw
        | Kind::AnonKw
        | Kind::GlyphClassDefKw => Style::new().fg(Colour::Cyan),
        Kind::NamedGlyphClass => Style::new().fg(Colour::Blue).italic(),
        Kind::LookupflagKw | Kind::ScriptKw | Kind::LanguageKw => Colour::Blue.into(),
        Kind::Backslash => Style::new().fg(Colour::Yellow).dimmed(),
        Kind::SubKw
        | Kind::PosKw
        | Kind::IgnoreKw
        | Kind::EnumKw
        | Kind::RsubKw
        | Kind::ByKw
        | Kind::FromKw => Style::new().fg(Colour::Cyan).italic(),
        _ => Style::new().fg(Colour::White),
    }
}

//FIXME: get from terminal?
const MAX_PRINT_WIDTH: usize = 100;

/// Given an error and a line's text, write a fancy error message.
pub(crate) fn write_diagnostic(
    writer: &mut impl Write,
    err: &Diagnostic,
    source: &Source,
    line_width: Option<usize>,
) {
    write_header(writer, err, source);

    let line_width = line_width.unwrap_or(MAX_PRINT_WIDTH);
    let span = err.message.span.range();
    let (line_n, text) = source.line_containing_offset(span.start);
    let line_start = source.offset_for_line_number(line_n);
    let err_start = span.start - line_start;

    // if a line is really long, we clip it
    let trim_start = if text.len() > line_width {
        const SLOP: usize = 10; // buffer before start of error when clipping
        let max_trim = (text.len()) - line_width;
        err_start.saturating_sub(SLOP).min(max_trim)
    } else {
        0
    };

    let trim_end = (text.len() - trim_start).saturating_sub(line_width);
    let text = &text[trim_start..text.len() - trim_end];
    let ellipsis = if trim_start == 0 { "" } else { "..." };

    let line_ws = text.bytes().take_while(u8::is_ascii_whitespace).count();
    let n_digits = decimal_digits(line_n);
    let blue = Colour::Blue;

    // one blank line:
    writeln!(
        writer,
        "{}{} |{} ",
        blue.prefix(),
        &super::SPACES[..n_digits],
        blue.suffix()
    )
    .unwrap();
    write!(writer, "{}{} |{} ", blue.prefix(), line_n, blue.suffix()).unwrap();
    writeln!(writer, "{}{}", ellipsis, &text[trim_start..],).unwrap();
    let n_spaces = (span.start - line_start) - trim_start;
    // use the whitespace at the front of the line first, so that
    // we don't replace tabs with spaces
    let reuse_ws = n_spaces.min(line_ws);
    let extra_ws = n_spaces - reuse_ws;
    //FIXME: this stuff was used when we were painting the message inline.
    // we will want something like this again, when we add help to diagnostics?

    //let (extra_ws, _msg_first) = if extra_ws > (err.message.text.len() + 1) {
    //(extra_ws - err.message.text.len() - 1, true)
    //} else {
    //(extra_ws, false)
    //};

    let n_carets = span.end - span.start;
    let n_carets = n_carets.min(CARETS.len());
    let color = err.level.color();

    //let (first, second) = if msg_first {
    //(err.message.text.as_str(), &CARETS[..n_carets])
    //} else {
    //(&CARETS[..n_carets], err.message.text.as_str())
    //};
    write!(
        writer,
        "{}{} |{} ",
        blue.prefix(),
        &super::SPACES[..n_digits],
        blue.suffix()
    )
    .unwrap();
    writeln!(
        writer,
        "{}{}{}{}{}{}",
        &text[..reuse_ws],
        &super::SPACES[..extra_ws],
        &super::SPACES[..ellipsis.len()],
        color.prefix(),
        &CARETS[..n_carets],
        color.suffix(),
    )
    .unwrap();
}

fn write_header(writer: &mut impl Write, err: &Diagnostic, source: &Source) {
    let color = err.level.color();
    let text = err.level.label();

    write!(writer, "{}{}: {}", color.prefix(), text, color.suffix(),).unwrap();

    writeln!(writer, "{}", &err.message.text).unwrap();
    if let Some(path) = source.path() {
        let (line, column) = source.line_col_for_offset(err.message.span.range().start);
        let pre = Colour::Blue.italic().prefix();
        let suf = Colour::Blue.italic().suffix();
        writeln!(
            writer,
            "{}in{} {} {}at{} {}:{}",
            pre,
            suf,
            path.display(),
            pre,
            suf,
            line,
            column
        )
        .unwrap();
    }
}

impl Level {
    fn color(&self) -> Colour {
        match self {
            Level::Info => Colour::Cyan,
            Level::Warning => Colour::Yellow,
            Level::Error => Colour::Red,
        }
    }

    fn label(&self) -> &str {
        match self {
            Level::Info => "info",
            Level::Warning => "warning",
            Level::Error => "error",
        }
    }
}

static CARETS: &str = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";

pub(crate) fn decimal_digits(n: usize) -> usize {
    (n as f64).log10().floor() as usize + 1
}
