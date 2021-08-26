use std::{env, ffi::OsStr, fs, path::PathBuf};

mod grammar;
mod lexer;
mod parse;
mod pretty_diff;
mod token;
mod token_set;

use parse::{DebugSink, Parser};

fn main() {
    let args = Args::get_from_env_or_exit();
    let contents = fs::read_to_string(&args.path).expect("file read failed");
    let mut sink = DebugSink::default();
    let mut parser = Parser::new(&contents, &mut sink);
    grammar::root(&mut parser);
    let errs = sink.print_errs(&contents);
    if errs.is_empty() {
        println!("{}", sink);
    } else {
        eprintln!("{}", errs);
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
            Some(ref p) if p.exists() && p.extension() == Some(OsStr::new("fea")) => p.to_owned(),
            Some(ref p) => exit_err!("path {:?} is not an existing .fea file, exiting", p),
            None => exit_err!("Please supply a path to a .fea file"),
        };

        Args { path }
    }
}
