//! Attempt to parse input, reporting errors.

use std::{ops::Range, path::Path};

use fea_rs::{AstSink, GlyphMap, GlyphName, Kind, Node, Parser, SyntaxError};

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
    let (root, mut errors) = try_parse_fea(&features, &names);

    validate_names(&root, &names, &mut errors);
    if !errors.is_empty() {
        let mut tokens = Vec::new();
        collect_tokens(&root, &mut tokens);
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
fn try_parse_fea(contents: &str, names: &GlyphMap) -> (Node, Vec<SyntaxError>) {
    let mut sink = AstSink::new(contents, Some(names));
    let mut parser = Parser::new(contents, &mut sink);
    fea_rs::root(&mut parser);
    sink.finish()
}

fn validate_names(root: &Node, names: &GlyphMap, out: &mut Vec<SyntaxError>) {
    for token in root.iter_tokens() {
        if token.kind == Kind::GlyphName {
            let name = token.text.trim_matches('\\');
            if !names.contains(name) {
                out.push(SyntaxError {
                    range: token.range(),
                    message: "Unknown glyph name".into(),
                });
            }
        }
    }
}

fn collect_tokens(root: &Node, collect: &mut Vec<(Kind, Range<usize>)>) {
    for token in root.iter_tokens() {
        collect.push((token.kind, token.range()));
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
