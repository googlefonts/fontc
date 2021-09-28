use ansi_term::{Colour, Style};
use std::{env, ffi::OsStr, path::PathBuf};

use fea_rs::{AstSink, Parser};

fn main() {
    let args = Args::get_from_env_or_exit();
    let contents = std::fs::read_to_string(&args.path).expect("file read failed");
    let mut sink = AstSink::new(&contents, None);
    let mut parser = Parser::new(&contents, &mut sink);
    fea_rs::root(&mut parser);

    let mut pos = 0;
    let mut cur_len = 0;
    let mut current_style = Style::new().fg(Colour::White);

    let mut results = Vec::new();
    let (root, _errs) = sink.finish();
    for token in root.iter_tokens() {
        let style = fea_rs::util::style_for_kind(token.kind);
        // if the style has changed, draw the previous range.
        if style != current_style {
            // we've drawn, so we reset.
            let slice = &contents[pos..pos + cur_len];
            print!("{}", current_style.paint(slice));
            current_style = style;
            pos += cur_len;
            cur_len = token.text.len();
        } else {
            // still the same style; we just increment cur_len
            cur_len += token.text.len();
        }
    }

    // draw the last span
    let slice = &contents[pos..pos + cur_len];
    print!("{}", current_style.paint(slice));
    results.push((current_style, slice));
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
