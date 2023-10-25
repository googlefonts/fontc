/// print a fea file to stdout, with syntax highlighting
use ansi_term::{Colour, Style};
use std::{env, ffi::OsStr, path::PathBuf};

fn main() {
    let args = Args::get_from_env_or_exit();
    let raw_fea = std::fs::read_to_string(args.path).unwrap();
    let (node, _errors) = fea_rs::parse::parse_string(raw_fea);
    let mut current_style = Style::new().fg(Colour::White);
    let mut needs_paint = String::new();

    for token in node.iter_tokens() {
        let style = fea_rs::util::style_for_kind(token.kind);
        // if the style has changed, draw the previous range.
        if style != current_style {
            // we've drawn, so we reset.
            if !needs_paint.is_empty() {
                print!("{}", current_style.paint(&needs_paint));
            }
            current_style = style;
            needs_paint.clear();
        }
        needs_paint.push_str(token.as_str());
    }

    // draw the last span
    print!("{}", current_style.paint(needs_paint));
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
