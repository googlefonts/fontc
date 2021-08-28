//! given a path to a fea file or a directory containing fea files,
//! attempt to parse them.
use std::{
    env,
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

mod grammar;
mod lexer;
mod parse;
mod pretty_diff;
mod token;
mod token_set;

use parse::{DebugSink, Parser};

fn main() {
    let args = Args::get_from_env_or_exit();
    if args.path.is_dir() {
        directory_arg(&args.path).unwrap();
    } else {
        single_file_arg(&args.path)
    }
}

fn directory_arg(path: &Path) -> std::io::Result<()> {
    let mut seen = 0;
    // tuple of path + was panic
    let mut failures = Vec::new();
    for entry in path.read_dir()? {
        let entry = entry?;
        let path = entry.path();
        if path.extension() == Some(OsStr::new("fea")) {
            seen += 1;
            eprintln!("parsing '{}'", path.display());
            match std::panic::catch_unwind(|| try_parse_file(&path)) {
                Err(_) => failures.push((path, true)),
                Ok((_, errs)) if !errs.is_empty() => failures.push((path, false)),
                _ => (),
            };
        }
    }

    println!("\nparsed {}/{} files.", seen - failures.len(), seen);
    if !failures.is_empty() {
        println!("\nFAILURES:");
        for (path, panic) in failures {
            let panic = if panic { "PANIC" } else { "     " };
            println!(" {} {}", panic, path.display());
        }
    }
    Ok(())
}

fn single_file_arg(path: &Path) {
    let (tree, errors) = try_parse_file(path);
    if !errors.is_empty() {
        eprintln!("{}", errors);
    } else {
        println!("{}", tree);
    }
}

/// returns the tree and any errors
fn try_parse_file(path: &Path) -> (String, String) {
    let contents = fs::read_to_string(path).expect("file read failed");
    let mut sink = DebugSink::default();
    let mut parser = Parser::new(&contents, &mut sink);
    grammar::root(&mut parser);
    (
        sink.simple_parse_tree(&contents),
        sink.print_errs(&contents),
    )
}

macro_rules! exit_err {
    ($($arg:tt)*) => ({
        eprintln!($($arg)*);
        std::process::exit(1);
    })
}

struct Args {
    path: PathBuf,
    //err
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
