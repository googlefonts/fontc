//! Compile features into a font file

use fea_rs::{GlyphMap, GlyphName};
use write_fonts::{
    read::{tables::post::Post, FontData, FontRef, TableProvider},
    types::GlyphId,
};

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

    let bytes = std::fs::read(args.path()).expect("failed to load font data");
    let data = FontData::new(&bytes);
    let font = FontRef::new(data).expect("failed to parse font");
    let post = font.post().expect("failed to read 'post' table");
    let names = try_to_make_glyph_map(post)
        .expect("no glyph names in post table, which is currently required");

    let parse = fea_rs::parse_root_file(args.fea(), Some(&names), None).unwrap();
    let (tree, diagnostics) = parse.generate_parse_tree();
    let mut has_error = false;
    for msg in &diagnostics {
        eprintln!("{}", tree.format_diagnostic(msg));
        has_error |= msg.is_error();
    }
    if has_error {
        std::process::exit(1);
    }

    match fea_rs::compile(&tree, &names) {
        Ok(compilation) => {
            compilation.apply(&font).unwrap();
            for warning in &compilation.warnings {
                eprintln!("{}", tree.format_diagnostic(warning));
            }
        }

        Err(errors) => {
            let mut err_count = 0;
            for msg in &errors {
                eprintln!("{}", tree.format_diagnostic(msg));
                if msg.is_error() {
                    err_count += 1;
                }
            }
            let warning_count = errors.len() - err_count;
            println!("{} errors, {} warnings", err_count, warning_count);
            std::process::exit(1);
        }
    }

    //match &args.subcommand {
    //flags::ArgsCmd::Compile(args) => {
    //if let Some(path) = &args.out_path {
    //font.save(path).unwrap()
    //} else {
    //font.save("compile-out.ttf").unwrap()
    //}
    //}
    //flags::ArgsCmd::Debug(args) => {
    //let to_print = args
    //.print_tables
    //.as_ref()
    //.map(|s| s.split(',').map(|s| s.to_owned()).collect::<HashSet<_>>())
    //.unwrap_or_default();
    //if to_print.is_empty() {
    //fea_rs::util::debug::explode_font(&font, args.verbose);
    //}

    //for table in to_print {
    //if table == "GPOS" {
    //if let Some(gpos) = font.tables.GPOS().unwrap() {
    //util::debug::explode_gpos(&gpos, args.verbose);
    //} else {
    //eprintln!("no GPOS table exists");
    //}
    //} else if table == "GSUB" {
    //if let Some(gsub) = font.tables.GSUB().unwrap() {
    //util::debug::explode_gsub(&gsub, args.verbose);
    //} else {
    //eprintln!("no GSUB table exists");
    //}
    //} else {
    //eprintln!("unknown table '{}'", table);
    //}
    //}
    //}
    //}
}

fn try_to_make_glyph_map(post: Post) -> Option<GlyphMap> {
    let Some(num_glyphs) = post.num_glyphs() else {
        return None;
    };

    Some(
        (0..num_glyphs)
            .map(GlyphId::new)
            .map(|id| post.glyph_name(id).unwrap())
            .map(GlyphName::new)
            .collect(),
    )
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
