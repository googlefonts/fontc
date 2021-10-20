//! Compile features into a font file

use fea_rs::{AstSink, Diagnostic, GlyphMap, GlyphName, Level, Node, Parser};
use fonttools::tag;

/// Attempt to compile features into a font file.
///
/// usage: FONT_PATH FEA_PATH
fn main() {
    let args = flags::Compile::from_env().unwrap();
    let mut font = fonttools::font::Font::load(&args.path).expect("failed to load font");
    let features = std::fs::read_to_string(&args.fea).expect("failed to load fea");
    let names = font
        .tables
        .post()
        .unwrap()
        .expect("missing 'name' table")
        .glyphnames
        .as_ref()
        .map(|names| names.iter().map(GlyphName::new).collect())
        .expect("no glyph map");

    let (root, mut errors) = try_parse_fea(&features, &names);
    match fea_rs::compile(&root, &names) {
        Ok(compilation) => {
            match compilation.gpos {
                Some(table) => font.tables.insert(table),
                None => {
                    font.tables.remove(tag!("GPOS"));
                }
            }
            match compilation.gsub {
                Some(table) => font.tables.insert(table),
                None => {
                    font.tables.remove(tag!("GSUB"));
                }
            }
            if let Some(path) = args.out_path {
                font.save(path).unwrap()
            } else {
                font.save("compile-out.ttf").unwrap()
            }
        }

        Err(e) => {
            errors.extend(e);
            let tokens = root
                .iter_tokens()
                .map(|t| (t.kind, t.range()))
                .collect::<Vec<_>>();
            errors.sort_unstable_by_key(|err| (err.span().start, err.span().end));
            let mut prev_range = 0..usize::MAX;

            errors.retain(|x| {
                let retain = x.span() != prev_range;
                prev_range = x.span();
                retain
            });

            println!(
                "{}",
                fea_rs::util::stringify_errors(&features, &tokens, &errors)
            );

            let err_count = errors
                .iter()
                .filter(|err| err.level == Level::Error)
                .count();
            let warning_count = errors
                .iter()
                .filter(|err| err.level == Level::Warning)
                .count();
            println!("{} errors, {} warnings", err_count, warning_count);
        }
    }
    //let val_errors = fea_rs::validate(&root, &names);
    //errors.extend(val_errors);

    if !errors.is_empty() {
    } else {
        println!("exited successfully!")
    }
}

/// returns the tree and any errors
fn try_parse_fea(contents: &str, names: &GlyphMap) -> (Node, Vec<Diagnostic>) {
    let mut sink = AstSink::new(contents, Some(names));
    let mut parser = Parser::new(contents, &mut sink);
    fea_rs::root(&mut parser);
    sink.finish()
}

mod flags {
    use std::path::PathBuf;
    xflags::xflags! {

        cmd compile
            required path: PathBuf
            required fea: PathBuf
            {
                optional -o, --out-path out_path: PathBuf
                optional -v, --verbose
            }
    }
}
