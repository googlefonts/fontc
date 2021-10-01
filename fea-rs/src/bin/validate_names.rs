//! Attempt to parse input, reporting errors.

use std::{collections::HashMap, path::Path};

use fea_rs::typed::AstNode as _;
use fea_rs::{AstSink, Diagnostic, GlyphMap, GlyphName, Node, Parser};

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
    print_statement_info(&root);

    let val_ctx = fea_rs::validate(&root, &names);
    errors.extend(val_ctx.errors);
    if !errors.is_empty() {
        let tokens = root
            .iter_tokens()
            .map(|t| (t.kind, t.range()))
            .collect::<Vec<_>>();
        errors.sort_unstable_by_key(|err| (err.span().start, err.span().end));
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
fn try_parse_fea(contents: &str, names: &GlyphMap) -> (Node, Vec<Diagnostic>) {
    let mut sink = AstSink::new(contents, Some(names));
    let mut parser = Parser::new(contents, &mut sink);
    fea_rs::root(&mut parser);
    sink.finish()
}

fn print_statement_info(root: &Node) {
    let mut counter = HashMap::new();
    let mut statements = HashMap::new();
    for child in root.iter_children().filter(|t| !t.kind().is_trivia()) {
        *counter.entry(child.kind()).or_insert(0_u32) += 1;
        if let Some(lookup) = fea_rs::typed::LookupBlock::cast(child) {
            statements.insert(lookup.tag().text.clone(), lookup.statements().count());
        } else if let Some(feature) = fea_rs::typed::Feature::cast(child) {
            statements.insert(feature.tag().text().clone(), feature.statements().count());
        }
    }

    println!("## total statement count ##");
    for (kind, count) in counter {
        println!("{}: {}", kind, count);
    }

    println!("\n## feature/lookup statement counts ##");
    let mut statements: Vec<_> = statements.into_iter().collect();
    statements.sort_unstable_by_key(|(_, count)| *count);
    statements.reverse();
    for (kind, count) in statements {
        if count > 10 {
            println!("{:5}: {}", count, kind);
        }
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
