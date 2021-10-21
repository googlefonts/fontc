//! Compile features into a font file

use std::collections::HashSet;

use fea_rs::{AstSink, Diagnostic, GlyphMap, GlyphName, Level, Node, Parser};
use fonttools::tag;

/// Attempt to compile features into a font file.
///
/// usage: FONT_PATH FEA_PATH
fn main() {
    let args = match flags::Args::from_env() {
        Ok(args) if args.help => {
            println!("{}", flags::Args::HELP);
            return;
        }
        Ok(args) => args,
        Err(err) => {
            eprintln!("Error: {}.\n\nUsage:\n{}", err, flags::Args::HELP);
            std::process::exit(1);
        }
    };

    let mut font = fonttools::font::Font::load(args.path()).expect("failed to load font");
    let features = std::fs::read_to_string(args.fea()).expect("failed to load fea");
    let names = font
        .tables
        .post()
        .unwrap()
        .expect("missing 'name' table")
        .glyphnames
        .as_ref()
        .map(|names| names.iter().map(GlyphName::new).collect())
        .expect("no glyph map");

    let (root, errors) = try_parse_fea(&features, &names);
    if errors.iter().any(Diagnostic::is_error) {
        print_diagnostics(&root, &features, &errors);
        std::process::exit(1);
    }

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

            if !compilation.warnings.is_empty() {
                print_diagnostics(&root, &features, &compilation.warnings);
            }
        }

        Err(errors) => {
            print_diagnostics(&root, &features, &errors);
            let err_count = errors
                .iter()
                .filter(|err| err.level == Level::Error)
                .count();
            let warning_count = errors
                .iter()
                .filter(|err| err.level == Level::Warning)
                .count();
            println!("{} errors, {} warnings", err_count, warning_count);
            std::process::exit(1);
        }
    }

    match &args.subcommand {
        flags::ArgsCmd::Compile(args) => {
            if let Some(path) = &args.out_path {
                font.save(path).unwrap()
            } else {
                font.save("compile-out.ttf").unwrap()
            }
        }
        flags::ArgsCmd::Debug(args) => {
            let to_print = args
                .print_tables
                .as_ref()
                .map(|s| s.split(',').map(|s| s.to_owned()).collect::<HashSet<_>>())
                .unwrap_or_default();
            if to_print.is_empty() {
                fea_rs::debug::explode_font(&font, args.verbose);
            }

            for table in to_print {
                if table == "GPOS" {
                    if let Some(gpos) = font.tables.GPOS().unwrap() {
                        fea_rs::debug::explode_gpos(&gpos, args.verbose);
                    } else {
                        eprintln!("no GPOS table exists");
                    }
                } else if table == "GSUB" {
                    if let Some(gsub) = font.tables.GSUB().unwrap() {
                        fea_rs::debug::explode_gsub(&gsub, args.verbose);
                    } else {
                        eprintln!("no GSUB table exists");
                    }
                } else {
                    eprintln!("unknown table '{}'", table);
                }
            }
        }
    }
}

fn print_diagnostics(root: &Node, features: &str, diagnostics: &[Diagnostic]) {
    let tokens = root
        .iter_tokens()
        .map(|t| (t.kind, t.range()))
        .collect::<Vec<_>>();

    println!(
        "{}",
        fea_rs::util::stringify_errors(&features, &tokens, &diagnostics)
    );
}

/// returns the tree and any errors
fn try_parse_fea(contents: &str, names: &GlyphMap) -> (Node, Vec<Diagnostic>) {
    let mut sink = AstSink::new(contents, Some(names));
    let mut parser = Parser::new(contents, &mut sink);
    fea_rs::root(&mut parser);
    sink.finish()
}

mod flags {
    use std::path::{Path, PathBuf};
    xflags::xflags! {

        /// Compile a fea file into a source font
        cmd args {
            cmd compile
                /// Path to the font
                required path: PathBuf
                /// Path to the fea file
                required fea: PathBuf
                {
                    optional -o, --out-path out_path: PathBuf
                }
            cmd debug
                /// Path to test FEA file. This should be in a directory that
                /// contains a 'font.ttf' file to be used for testing.
                /// Comma-separated list of tables to print (e.g: -p GSUB,GPOS)
                required fea: PathBuf
                {
                    optional -p, --print-tables tables: String
                    optional -v, --verbose
                }
            /// Print help
            optional -h, --help
        }
    }

    impl Args {
        pub fn fea(&self) -> &Path {
            match &self.subcommand {
                ArgsCmd::Compile(args) => &args.fea,
                ArgsCmd::Debug(args) => &args.fea,
            }
        }

        pub fn path(&self) -> PathBuf {
            match &self.subcommand {
                ArgsCmd::Compile(args) => args.path.clone(),
                ArgsCmd::Debug(args) => args.fea.with_file_name("font.ttf"),
            }
        }
    }
}
