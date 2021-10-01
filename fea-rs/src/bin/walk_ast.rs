//! Attempt to parse input, reporting errors.

use std::{
    env,
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

use ansi_term::Colour;
use fea_rs::{AstSink, Diagnostic, Node, NodeOrToken, Parser};

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
        directory_arg(&args.path).unwrap()
    } else {
        single_file_arg(&args.path, args.print_tree)
    }
}

fn single_file_arg(path: &Path, _print_tree: bool) {
    let (tree, _errors) = try_parse_file(path);
    print_structure(&tree, &_errors);
    println!("deepest: {}", deepest_depth(&tree));
}

fn deepest_depth(node: &Node) -> usize {
    let mut deepest = 0;
    let mut cursor = node.cursor();

    while cursor.next_token().is_some() {
        deepest = deepest.max(cursor.depth());
    }

    deepest
}

fn print_structure(tree: &Node, _errors: &[Diagnostic]) {
    let mut cursor = tree.cursor();
    while let Some(thing) = cursor.current() {
        if let NodeOrToken::Node(node) = thing {
            let depth = cursor.depth();
            if node.error {
                print!("{}", Colour::Red.prefix());
            }
            println!(
                "{}{} ({})",
                &fea_rs::util::SPACES[..depth * 2],
                node.kind,
                node.text_len()
            );
            if node.error {
                print!("{}", Colour::Red.suffix());
            }
        }
        cursor.advance();
    }
}

fn directory_arg(path: &Path) -> std::io::Result<()> {
    let mut seen = 0;
    // tuple of path + was panic
    let mut failures = Vec::new();
    let (mut deepest, mut d_path) = (0, PathBuf::new());
    for entry in path.read_dir()? {
        let entry = entry?;
        let path = entry.path();
        if path.extension() == Some(OsStr::new("fea")) {
            seen += 1;
            //eprintln!("parsing '{}'", path.display());
            match std::panic::catch_unwind(|| try_parse_file(&path)) {
                Err(_) => failures.push((path, true)),
                Ok((root, errs)) => {
                    if !errs.is_empty() {
                        failures.push((path.clone(), false));
                    }

                    let text = root.iter_tokens().map(|t| t.as_str()).collect::<String>();
                    assert_eq!(text, std::fs::read_to_string(&path).unwrap());
                    let depth = deepest_depth(&root);
                    if depth > deepest {
                        deepest = depth;
                        d_path = path;
                    }
                }
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
    println!("deepest: {}: {}", d_path.display(), deepest);
    Ok(())
}

/// returns the tree and any errors
fn try_parse_file(path: &Path) -> (Node, Vec<Diagnostic>) {
    let contents = fs::read_to_string(path).expect("file read failed");
    let mut sink = AstSink::new(&contents, None);
    let mut parser = Parser::new(&contents, &mut sink);
    fea_rs::root(&mut parser);
    sink.finish()
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
