//! Attempt to parse input, reporting errors.

use std::{
    env,
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
    time::Instant,
};

use fea_rs::{util, AstSink, Node, Parser, SyntaxError};

/// Attempt to parse fea files.
///
/// usage: PATH [-t|--tree]
///
/// PATH may be a single fea file or a directory containing fea files.
/// if --tree is present, and path is a single file, prints tree even when
/// encountering errors, otherwise only prints errors.
fn main() {
    let args = Args::get_from_env_or_exit();
    if args.path.is_dir() {
        directory_arg(&args.path).unwrap();
    } else {
        single_file_arg(&args.path, args.print_tree)
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

fn single_file_arg(path: &Path, print_tree: bool) {
    let time = Instant::now();
    let (tree, errors) = try_parse_file(path);
    let elapsed = time.elapsed();
    if errors.is_empty() || print_tree {
        println!("{}", tree);
    }
    if !errors.is_empty() {
        eprintln!("{}", errors);
    }

    let micros = elapsed.as_micros();
    let millis = (micros as f64) / 1000.0;
    println!("parsed in {:.2}ms", millis);
}

/// returns the tree and any errors
fn try_parse_file(path: &Path) -> (String, String) {
    let contents = fs::read_to_string(path).expect("file read failed");
    let mut sink = AstSink::new(&contents, None);
    let mut parser = Parser::new(&contents, &mut sink);
    fea_rs::root(&mut parser);
    let (root, errors) = sink.finish();
    (
        root.simple_parse_tree(),
        stringify_errors(&contents, &root, &errors), //sink.print_errs(&contents),
    )
}

fn stringify_errors(contents: &str, root: &Node, errors: &[SyntaxError]) -> String {
    let tokens = root
        .iter_tokens()
        .map(|t| (t.kind, t.range()))
        .collect::<Vec<_>>();
    util::stringify_errors(contents, &tokens, errors)
}

macro_rules! exit_err {
    ($($arg:tt)*) => ({
        eprintln!($($arg)*);
        std::process::exit(1);
    })
}

struct Args {
    path: PathBuf,
    print_tree: bool,
}

impl Args {
    fn get_from_env_or_exit() -> Self {
        let mut args = env::args().skip(1);
        let path = match args.next().map(PathBuf::from) {
            Some(p) if p.is_dir() || (p.exists() && p.extension() == Some(OsStr::new("fea"))) => p,
            Some(p) => exit_err!("path {:?} is not an existing .fea file, exiting", p),
            None => exit_err!("Please supply a path to a .fea file"),
        };

        let print_tree = matches!(args.next().as_deref(), Some("--tree" | "-t"));

        Args { path, print_tree }
    }
}
