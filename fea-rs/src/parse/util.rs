use std::{fmt::Write, ops::Range};

use super::{Kind, SyntaxError};
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

fn format_line(text: &str, line_start: usize, tokens: &[(Kind, Range<usize>)]) -> String {
    let first = tokens.first().unwrap();
    let mut out = String::new();
    let mut pos = 0;
    let mut cur_len = 0;
    let mut current_style = style_for_kind(first.0);
    // only used once
    let mut extra_space = Some(line_start - first.1.start);
    for (kind, range) in tokens.iter() {
        let len = range.len() - extra_space.take().unwrap_or_default();
        if pos >= text.len() {
            break;
        }
        let style = style_for_kind(*kind);
        // if the style has changed, draw the previous range.
        if style != current_style {
            // we've drawn, so we reset.
            let end = (pos + cur_len).min(text.len());
            let slice = &text[pos..end];
            write!(&mut out, "{}", current_style.paint(slice)).unwrap();
            current_style = style;
            pos += cur_len;
            cur_len = len;
        } else {
            // still the same style; we just increment cur_len
            cur_len += len;
        }
    }

    out
}

//FIXME: get from terminal?
const MAX_PRINT_WIDTH: usize = 100;

pub fn stringify_errors(
    input: &str,
    tokens: &[(Kind, Range<usize>)],
    errs: &[SyntaxError],
) -> String {
    let total_lines = input.lines().count();
    let max_line_digit_width = decimal_digits(total_lines);
    let mut result = String::new();
    let mut pos = 0;
    let mut line_n = 0;
    let mut lines = iter_lines_including_breaks(input);
    let mut current_line = lines.next().unwrap_or("");

    let mut cur_tokens = tokens;
    for err in errs {
        while err.range.start >= pos + current_line.len() {
            pos += current_line.len();
            if pos == input.len() {
                break;
            }
            current_line = lines.next().unwrap();
            line_n += 1;
        }

        let n_skip = cur_tokens.iter().take_while(|t| t.1.end < pos).count();
        cur_tokens = &cur_tokens[n_skip..];

        write_line_error(
            &mut result,
            pos,
            current_line,
            cur_tokens,
            line_n,
            MAX_PRINT_WIDTH,
            err,
            max_line_digit_width,
        );
    }
    result
}

/// Given an error and a line's text, write a fancy error message.
#[allow(clippy::too_many_arguments)]
pub(crate) fn write_line_error(
    writer: &mut impl Write,
    line_start: usize, // relative to the start of the document
    text: &str,
    tokens: &[(Kind, Range<usize>)],
    line_n: usize,
    line_width: usize,
    err: &SyntaxError,
    max_digits: usize,
) {
    let err_start = err.range.start - line_start;

    // if a line is really long, we clip it
    let trim_start = if text.len() > line_width {
        const SLOP: usize = 10; // buffer before start of error when clipping
        let max_trim = (text.len()) - line_width;
        err_start.saturating_sub(SLOP).min(max_trim)
    } else {
        0
    };

    // adjust trim_start to a token boundary:
    let trim_start_pos = tokens
        .iter()
        .position(|t| (t.1.end - line_start) >= trim_start)
        .unwrap_or_default();
    let trim_start = tokens
        .get(trim_start_pos)
        .map(|t| t.1.start.saturating_sub(line_start))
        .unwrap_or_default();

    let trim_end = (text.len() - trim_start).saturating_sub(line_width);
    let text = &text[trim_start..text.len() - trim_end];
    let ellipsis = if trim_start == 0 { "" } else { "..." };

    let line_ws = text.bytes().take_while(u8::is_ascii_whitespace).count();
    let padding = max_digits - decimal_digits(line_n);
    let color_string = format_line(
        text.trim_end(),
        line_start + trim_start,
        &tokens[trim_start_pos..],
    );
    writeln!(
        writer,
        "\n{}{} | {}{}",
        &SPACES[..padding],
        line_n,
        ellipsis,
        color_string,
    )
    .unwrap();
    let n_spaces = (err.range.start - line_start) - trim_start;
    // use the whitespace at the front of the line first, so that
    // we don't replace tabs with spaces
    let reuse_ws = n_spaces.min(line_ws);
    let extra_ws = n_spaces - reuse_ws;
    let (extra_ws, msg_first) = if extra_ws > (err.message.len() + 1) {
        (extra_ws - err.message.len() - 1, true)
    } else {
        (extra_ws, false)
    };

    let n_carets = err.range.end - err.range.start;

    let (first, second) = if msg_first {
        (err.message.as_str(), &CARETS[..n_carets])
    } else {
        (&CARETS[..n_carets], err.message.as_str())
    };
    writeln!(
        writer,
        "{} | {}{}{}{} {}",
        &SPACES[..max_digits],
        &text[..reuse_ws],
        &SPACES[..extra_ws],
        &SPACES[..ellipsis.len()],
        first,
        second
    )
    .unwrap();
}

static CARETS: &str = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";

#[doc(hidden)]
pub static SPACES: &str = "                                                                                                                                                                                    ";

pub(crate) fn decimal_digits(n: usize) -> usize {
    (n as f64).log10().floor() as usize + 1
}

// we can't use str::lines because it strips newline chars and we need them
// to calculate error positions
pub(crate) fn iter_lines_including_breaks(s: &str) -> impl Iterator<Item = &str> {
    let mut slice = s;
    std::iter::from_fn(move || {
        if slice.is_empty() {
            return None;
        }
        let next_cr = match slice.bytes().position(|b| b == b'\n') {
            Some(idx) if slice.as_bytes().get(idx + 1) == Some(&b'\r') => idx + 2,
            Some(idx) => idx + 1,
            None => slice.len(),
        };
        let result = &slice[..next_cr];
        slice = &slice[next_cr..];
        Some(result)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iter_lines() {
        let text = "hi\nfriends\n\r\nhow u?\n";
        let lines = iter_lines_including_breaks(text).collect::<Vec<_>>();
        assert_eq!(lines, vec!["hi\n", "friends\n\r", "\n", "how u?\n"]);
    }
}
