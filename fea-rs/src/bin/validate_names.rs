//! Attempt to parse input, reporting errors.

use std::{collections::BTreeSet, ops::Range, path::Path, sync::Arc};

use fea_rs::{AstSink, Kind, Node, NodeOrToken, Parser, SyntaxError};

/// Attempt to parse fea files.
///
/// usage: PATH [-t|--tree]
///
/// PATH may be a single fea file or a directory containing fea files.
/// if --tree is present, and path is a single file, prints tree even when
/// encountering errors, otherwise only prints errors.
fn main() {
    let args = flags::ValidateNames::from_env().unwrap();
    let (names, features) = load_font(&args.path);
    let (root, mut errors) = try_parse_fea(&features);

    validate_names(&root, 0, &names, &mut errors);
    if !errors.is_empty() {
        let mut tokens = Vec::new();
        collect_tokens(&root, 0, &mut tokens);
        errors.sort_unstable_by_key(|err| (err.range.start, err.range.end));
        println!(
            "{}",
            fea_rs::util::stringify_errors(&features, &tokens, &errors)
        );
    } else {
        println!("exited successfully!")
    }
}

fn load_font(filename: &Path) -> (BTreeSet<Arc<str>>, String) {
    let font = norad::Font::load(filename).expect("failed to load font");
    let features = match font.features.as_ref() {
        Some(features) => features.to_owned(),
        None => panic!("font contains no features"),
    };
    let glyphs = font
        .default_layer()
        .iter()
        .map(|g| g.name.clone())
        .collect::<BTreeSet<_>>();

    (glyphs, features)
}

/// returns the tree and any errors
fn try_parse_fea(contents: &str) -> (Node, Vec<SyntaxError>) {
    let mut sink = AstSink::new(&contents);
    let mut parser = Parser::new(&contents, &mut sink);
    fea_rs::root(&mut parser);
    sink.finish()
}

//static SPACES: &str = "                                                                                                                                                      ";

fn validate_names(root: &Node, pos: usize, names: &BTreeSet<Arc<str>>, out: &mut Vec<SyntaxError>) {
    let mut rel_pos = 0;
    for child in root.children() {
        match child {
            NodeOrToken::Node(node) => validate_names(node, pos + rel_pos, names, out),
            NodeOrToken::Token(token) => {
                if token.kind == Kind::GlyphName {
                    let name = token.text.trim_matches('\\');
                    if !names.contains(name) {
                        dbg!(name);
                        out.push(SyntaxError {
                            range: pos + rel_pos..pos + rel_pos + child.text_len(),
                            message: "Unknown glyph name".into(),
                        });
                    }
                }
            }
        }
        rel_pos += child.text_len();
    }
}

fn collect_tokens(root: &Node, pos: usize, collect: &mut Vec<(Kind, Range<usize>)>) {
    let mut rel_pos = 0;
    for child in root.children() {
        match child {
            NodeOrToken::Node(node) => collect_tokens(&node, pos + rel_pos, collect),
            NodeOrToken::Token(token) => {
                let range = pos + rel_pos..pos + rel_pos + child.text_len();
                collect.push((token.kind, range));
            }
        }
        rel_pos += child.text_len();
    }
}

//fn print_structure(tree: &Node, errors: &[SyntaxError], pos: usize, level: usize) {
//let n_spaces = level * 2;
//let split_error_pos = errors
//.iter()
//.position(|err| err.range.start >= pos)
//.unwrap_or_else(|| errors.len());
////dbg!(split_error_pos);
//let errors = &errors[split_error_pos..];
//let n_errors = errors
//.iter()
//.take_while(|err| err.range.start <= pos + tree.text_len())
//.count();
//let err_text = match n_errors {
//0 => String::new(),
//1 => String::from(" 1 Error"),
//n => format!(" {} Errors", n),
//};
//println!(
//"{}{} {}..{}{}",
//&SPACES[..n_spaces],
//tree.kind(),
//pos,
//pos + tree.text_len(),
//err_text
//);
//let mut rel_pos = 0;
//for child in tree.children() {
//if let NodeOrToken::Node(n) = child {
//print_structure(n, errors, rel_pos + pos, level + 1);
//}
//rel_pos += child.text_len();
//}
//}
//let mut pos = 0;
//}
mod flags {
    use std::path::PathBuf;
    xflags::xflags! {

        cmd validate-names
            required path: PathBuf
            //required fea: PathBuf
        {
            optional -v, --verbose
        }
    }
}
