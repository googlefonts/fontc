//! Attempt to parse input, reporting errors.

use std::{
    env,
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

use fea_rs::{AstSink, Node, NodeOrToken, Parser, SyntaxError};

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
        panic!("no dirs in here");
        //directory_arg(&args.path).unwrap();
    } else {
        single_file_arg(&args.path, args.print_tree)
    }
}

fn single_file_arg(path: &Path, _print_tree: bool) {
    let (tree, errors) = try_parse_file(path);
    print_structure(&tree, &errors, 0, 0);
    //if errors.is_empty() || print_tree {
    ////println!("{}", tree);
    //}
    //if !errors.is_empty() {
    //eprintln!("{}", errors);
    //}
}

/// returns the tree and any errors
fn try_parse_file(path: &Path) -> (Node, Vec<SyntaxError>) {
    let contents = fs::read_to_string(path).expect("file read failed");
    let mut sink = AstSink::new(&contents);
    let mut parser = Parser::new(&contents, &mut sink);
    fea_rs::root(&mut parser);
    sink.finish()
    //(
    //sink.simple_parse_tree(&contents),
    //sink.print_errs(&contents),
    //)
}

static SPACES: &str = "                                                                                                                                                      ";

fn print_structure(tree: &Node, errors: &[SyntaxError], pos: usize, level: usize) {
    let n_spaces = level * 2;
    let split_error_pos = errors
        .iter()
        .position(|err| err.range.start >= pos)
        .unwrap_or_else(|| errors.len());
    //dbg!(split_error_pos);
    let errors = &errors[split_error_pos..];
    let n_errors = errors
        .iter()
        .take_while(|err| err.range.start <= pos + tree.text_len())
        .count();
    let err_text = match n_errors {
        0 => String::new(),
        1 => String::from(" 1 Error"),
        n => format!(" {} Errors", n),
    };
    println!(
        "{}{} {}..{}{}",
        &SPACES[..n_spaces],
        tree.kind(),
        pos,
        pos + tree.text_len(),
        err_text
    );
    let mut rel_pos = 0;
    for child in tree.children() {
        if let NodeOrToken::Node(n) = child {
            print_structure(n, errors, rel_pos + pos, level + 1);
        }
        rel_pos += child.text_len();
    }
    //let mut pos = 0;
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
