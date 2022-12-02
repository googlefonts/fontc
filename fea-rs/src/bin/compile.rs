//! Compile features into a font file

use std::path::Path;

use fea_rs::{GlyphMap, GlyphName};
use write_fonts::types::GlyphId;

/// Attempt to compile features into a font file.
///
/// usage: FONT_PATH FEA_PATH
fn main() -> Result<(), Error> {
    let args = match flags::Args::from_env() {
        Ok(args) => args,
        Err(e) => e.exit(),
    };

    let names = parse_glyph_order(args.glyph_order())?;
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

    let compiled = match fea_rs::compile(&tree, &names) {
        Ok(compilation) => {
            for warning in &compilation.warnings {
                eprintln!("{}", tree.format_diagnostic(warning));
            }
            compilation
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
    };

    let path = args.out_path();
    let raw_font = compiled.build_raw(&names).expect("ttf compile failed");
    std::fs::write(path, raw_font).map_err(Into::into)
}

fn parse_glyph_order(path: &Path) -> Result<GlyphMap, Error> {
    let contents = std::fs::read_to_string(path)?;
    let map: GlyphMap = contents
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|line| {
            if line.bytes().any(|b| b.is_ascii_whitespace()) {
                Err(Error::InvalidGlyphMap(format!(
                    "name {line:?} contains whitespace"
                )))
            } else {
                Ok(GlyphName::new(line))
            }
        })
        .collect::<Result<_, _>>()?;
    if map.get(".notdef") != Some(GlyphId::NOTDEF) {
        Err(Error::InvalidGlyphMap("first glyph must be .notdef".into()))
    } else {
        Ok(map)
    }
}

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("invalid glyph map: '{0}'")]
    InvalidGlyphMap(String),
    #[error("io error: '{0}'")]
    File(#[from] std::io::Error),
}

mod flags {
    use std::path::{Path, PathBuf};
    xflags::xflags! {
        /// Compile a fea file into a source font
        cmd args {
            /// Path to the fea file
            required fea: PathBuf
            /// Path to a file containing the glyph order.
            ///
            /// This should be a utf-8 encoded file with one name per line,
            /// sorted in glyphid order.
            required glyphs: PathBuf

                /// path to write font. Defaults to 'compile-out.ttf'
                optional -o, --out-path out_path: PathBuf
            }
    }

    impl Args {
        pub fn fea(&self) -> &Path {
            &self.fea
        }

        pub fn glyph_order(&self) -> &Path {
            &self.glyphs
        }

        pub fn out_path(&self) -> &Path {
            self.out_path
                .as_deref()
                .unwrap_or_else(|| Path::new("compile-out.ttf"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_glyph_map() {
        let glyph_map = parse_glyph_order(Path::new("./test-data/simple_glyph_order.txt")).unwrap();
        assert_eq!(glyph_map.len(), 215);
        assert_eq!(glyph_map.get("space"), Some(GlyphId::new(1)));
        assert_eq!(glyph_map.get("e.fina"), Some(GlyphId::new(214)));
        assert!(!glyph_map.contains("e.nada"));
    }
}
