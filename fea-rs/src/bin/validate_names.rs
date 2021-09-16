//! Attempt to parse input, reporting errors.

use std::{ops::Range, path::Path};

use fea_rs::{AstSink, GlyphMap, GlyphName, Kind, Node, NodeOrToken, Parser, SyntaxError};

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
    let (mut root, mut errors) = try_parse_fea(&features);
    let more_errors = fea_rs::validate(&mut root, &names);
    errors.extend(more_errors.into_iter());

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

fn load_font(filename: &Path) -> (GlyphMap, String) {
    let font = norad::Font::load(filename).expect("failed to load font");
    let features = match font.features.as_ref() {
        Some(features) => features.to_owned(),
        None => panic!("font contains no features"),
    };
    let glyphs: GlyphMap = font
        .default_layer()
        .iter()
        .map(|g| GlyphName::from(g.name.as_ref()))
        .collect();

    (glyphs, features)
}

/// returns the tree and any errors
fn try_parse_fea(contents: &str) -> (Node, Vec<SyntaxError>) {
    let mut sink = AstSink::new(&contents);
    let mut parser = Parser::new(&contents, &mut sink);
    fea_rs::root(&mut parser);
    sink.finish()
}

fn validate_names(root: &Node, pos: usize, names: &GlyphMap, out: &mut Vec<SyntaxError>) {
    let mut rel_pos = 0;
    for child in root.children() {
        match child {
            NodeOrToken::Node(node) => validate_names(node, pos + rel_pos, names, out),
            NodeOrToken::Token(token) => {
                if token.kind == Kind::GlyphName {
                    let name = token.text.trim_matches('\\');
                    if !names.contains(name) {
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

mod flags {
    use std::path::PathBuf;
    xflags::xflags! {

        cmd validate-names
            required path: PathBuf
        {
            optional -v, --verbose
        }
    }
}
