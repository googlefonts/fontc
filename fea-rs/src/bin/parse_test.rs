//! Attempt to parse input, reporting errors.

use std::{
    env,
    ffi::OsStr,
    path::{Path, PathBuf},
    time::Instant,
};

use fea_rs::{DiagnosticSet, Kind, ParseTree};

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
    let mut successes = Vec::new();

    for entry in path.read_dir()? {
        let entry = entry?;
        let path = entry.path();
        if path.extension() == Some(OsStr::new("fea")) {
            seen += 1;
            log::info!("parsing '{}'", path.display());
            match std::panic::catch_unwind(|| try_parse_file(&path)) {
                Err(_) => failures.push((path, true)),
                Ok((_, errs)) if errs.has_errors() => failures.push((path, false)),
                Ok((node, _)) => successes.push((path, node)),
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
    look_at_nodes_if_you_want(&successes);
    Ok(())
}

fn single_file_arg(path: &Path, print_tree: bool) {
    let time = Instant::now();
    let (tree, errors) = try_parse_file(path);
    let elapsed = time.elapsed();
    if errors.is_empty() || print_tree {
        println!("{}", tree.root().simple_parse_tree());
    } else if !errors.is_empty() {
        eprintln!("{}", errors.display());
    }

    let micros = elapsed.as_micros();
    let millis = (micros as f64) / 1000.0;
    println!("parsed in {:.2}ms", millis);
}

/// returns the tree and any errors
fn try_parse_file(path: &Path) -> (ParseTree, DiagnosticSet) {
    fea_rs::parse::parse_root_file(path, None, None).unwrap()
}

fn look_at_nodes_if_you_want(nodes: &[(PathBuf, ParseTree)]) {
    for (path, node) in nodes {
        let mut printed_preamble = false;
        for child in node.root().iter_tokens() {
            if child.kind == Kind::Path {
                if !printed_preamble {
                    printed_preamble = true;
                    println!("{}", path.display());
                }
                println!("  {}", child.text);
            }
        }
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
