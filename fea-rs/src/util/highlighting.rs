//! syntax highlighting functions

use std::{fmt::Write, path::Path};

use crate::{parse::Source, Diagnostic, Kind, Level};
use ansi_term::{Colour, Style};

/// Return the appropriate visual style for this token kind.
pub fn style_for_kind(kind: Kind) -> Style {
    match kind {
        Kind::Comment => Style::new().fg(Colour::Yellow).dimmed(),
        Kind::Number | Kind::Octal | Kind::Hex | Kind::Float | Kind::String => {
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

macro_rules! style_or_dont {
    ($tty:expr, $kind:expr) => {
        if $tty {
            $kind.into()
        } else {
            Style::default()
        }
    };
}

/// Given an error and a line's text, write a fancy error message.
pub(crate) fn write_diagnostic(
    writer: &mut impl Write,
    err: &Diagnostic,
    source: &Source,
    line_width: Option<usize>,
    colorized: bool,
) {
    write_header(writer, err, source, colorized);

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
    let blue = style_or_dont!(colorized, Colour::Blue);

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
    writeln!(writer, "{ellipsis}{text}").unwrap();
    let n_spaces = (span.start - line_start) - trim_start;
    // use the whitespace at the front of the line first, so that
    // we don't replace tabs with spaces
    let reuse_ws = n_spaces.min(line_ws);
    let extra_ws = n_spaces - reuse_ws;

    let n_carets = span.end - span.start;
    let n_carets = n_carets.min(CARETS.len());
    let color = style_or_dont!(colorized, err.level.color());

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

fn write_header(writer: &mut impl Write, err: &Diagnostic, source: &Source, colorized: bool) {
    let color = style_or_dont!(colorized, err.level.color());
    let text = err.level.label();

    write!(writer, "{}{}: {}", color.prefix(), text, color.suffix(),).unwrap();

    writeln!(writer, "{}", &err.message.text).unwrap();
    let (line, column) = source.line_col_for_offset(err.message.span.range().start);
    let pre = style_or_dont!(colorized, Colour::Blue.italic()).prefix();
    let suf = style_or_dont!(colorized, Colour::Blue.italic()).suffix();
    writeln!(
        writer,
        "{pre}in{suf} {} {pre}at{suf} {line}:{column}",
        Path::new(source.path()).display(),
    )
    .unwrap();
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

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    #[test]
    fn highlight_long_line() {
        static A_BAD_LINE: &str = "@COMBINING_MARKS = [ candrabindu-kannada nukta-kannada ssa-kannada.below.kssa ra-kannada.below rVocalicMatra-kannada rrVocalicMatra-kannada ailength-kannada ka-kannada.below kha-kannada.below ga-kannada.below gha-kannada.below nga-kannada.below ca-kannada.below cha-kannada.below ja-kannada.below jha-kannada.below nya-kannada.below tta-kannada.below ttha-kannada.below dda-kannada.below ddha-kannada.below nna-kannada.below ta-kannada.below tha-kannada.below da-kannada.below dha-kannada.below na-kannada.below pa-kannada.below pha-kannada.below ba-kannada.below bha-kannada.below ma-kannada.below ya-kannada.below la-kannada.below va-kannada.below sha-kannada.below ssa-kannada.below sa-kannada.below ha-kannada.below rra-kannada.below lla-kannada.below fa-kannada.below ka_ssa-kannada.below ta_ra-kannada.below ra-kannada.below.following rVocalicMatra-kannada.following rrVocalicMatra-kannada.following ailength-kannada.following ka-kannada.below.following kha-kannada.below.following ga-kannada.below.following gha-kannada.below.following nga-kannada.below.following ca-kannada.below.following cha-kannada.below.following ja-kannada.below.following jha-kannada.below.following nya-kannada.below.following tta-kannada.below.following ttha-kannada.below.following dda-kannada.below.following ddha-kannada.below.following nna-kannada.below.following ta-kannada.below.following tha-kannada.below.following da-kannada.below.following dha-kannada.below.following na-kannada.below.following pa-kannada.below.following pha-kannada.below.following ba-kannada.below.following bha-kannada.below.following ma-kannada.below.following ya-kannada.below.following la-kannada.below.following va-kannada.below.following sha-kannada.below.following ssa-kannada.below.following sa-kannada.below.following ha-kannada.below.following rra-kannada.below.following lla-kannada.below.following fa-kannada.below.following ka_ssa-kannada.below.following ta_ra-kannada.below.following ];";

        let source = Source::new(PathBuf::from("test"), A_BAD_LINE.into());
        let err = Diagnostic::warning(source.id(), 200..220, "bad!");
        let mut write_to = String::new();
        write_diagnostic(&mut write_to, &err, &source, None, true);
    }
}
