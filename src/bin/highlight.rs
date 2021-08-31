use ansi_term::{Colour, Style};
use std::{env, ffi::OsStr, path::PathBuf};

use feature_parsing::{DebugSink, Kind, Parser};

fn main() {
    let args = Args::get_from_env_or_exit();
    let contents = std::fs::read_to_string(&args.path).expect("file read failed");
    let mut sink = DebugSink::default();
    let mut parser = Parser::new(&contents, &mut sink);
    feature_parsing::root(&mut parser);

    let mut pos = 0;
    let mut cur_len = 0;
    let mut current_style = Style::new().fg(Colour::White);

    let mut results = Vec::new();
    for (kind, len) in sink.iter_tokens() {
        let style = style_for_kind(kind);
        // if the style has changed, draw the previous range.
        if style != current_style {
            // we've drawn, so we reset.
            let slice = &contents[pos..pos + cur_len];
            print!("{}", current_style.paint(slice));
            current_style = style;
            pos += cur_len;
            cur_len = len;
        } else {
            // still the same style; we just increment cur_len
            cur_len += len;
        }
    }

    // draw the last span
    let slice = &contents[pos..pos + cur_len];
    print!("{}", current_style.paint(slice));
    results.push((current_style, slice));
}

fn style_for_kind(kind: Kind) -> Style {
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
        | Kind::ScriptKw
        | Kind::LanguageKw
        | Kind::AnonKw => Style::new().fg(Colour::Cyan),
        Kind::GlyphName | Kind::Cid => Style::new().fg(Colour::Blue),
        Kind::NamedGlyphClass => Style::new().fg(Colour::Blue).italic(),
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

macro_rules! exit_err {
    ($($arg:tt)*) => ({
        eprintln!($($arg)*);
        std::process::exit(1);
    })
}

struct Args {
    path: PathBuf,
}

impl Args {
    fn get_from_env_or_exit() -> Self {
        let mut args = env::args().skip(1);
        let path = match args.next().map(PathBuf::from) {
            Some(p) if p.is_dir() || (p.exists() && p.extension() == Some(OsStr::new("fea"))) => p,
            Some(p) => exit_err!("path {:?} is not an existing .fea file, exiting", p),
            None => exit_err!("Please supply a path to a .fea file"),
        };

        Args { path }
    }
}
