//! Compile features into a font file

use std::path::{Path, PathBuf};

use clap::Parser;
use fea_rs::{compile, GlyphMap, GlyphName};
use write_fonts::types::GlyphId;

/// Attempt to compile features into a font file.
///
/// usage: FONT_PATH GLYPH_ORDER
///
/// where glyph order is a file listing glyphs, one per line, in glyph id order.
fn main() -> Result<(), Error> {
    let args = Args::parse();
    let (fea, glyph_names) = args.get_inputs()?;
    if !fea.exists() {
        return Err(Error::EmptyFeatureFile);
    }
    let parse = fea_rs::parse_root_file(&fea, Some(&glyph_names), None).unwrap();
    let (tree, diagnostics) = parse.generate_parse_tree();
    let mut has_error = false;
    for msg in &diagnostics {
        eprintln!("{}", tree.format_diagnostic(msg));
        has_error |= msg.is_error();
    }
    if has_error {
        std::process::exit(1);
    }

    let compiled = match compile::compile(&tree, &glyph_names) {
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
    let raw_font = compiled
        .build_raw(&glyph_names)
        .expect("ttf compile failed")
        .build();

    println!("writing {} bytes to {}", raw_font.len(), path.display());
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
    #[error("Couldn't read UFO: '{0}'")]
    Ufo(Box<norad::error::FontLoadError>),
    #[error("UFO is missing public.glyphOrder key, or the value is not an array of strings")]
    UfoBadGlyphOrder,
    #[error("The font's 'post' table is missing or malformed")]
    FontBadPostTable,
    #[error("The provided feature file is empty")]
    EmptyFeatureFile,
    #[error("No glyph order provided")]
    MissingGlyphOrder,
}

/// Compile FEA files
#[derive(Parser, Debug)]
#[command(author, version, long_about = None)]
struct Args {
    /// Display more information about failures
    ///
    /// This includes errors encountered, as well as the generated diffs when
    /// comparison fails.
    #[arg(short, long)]
    verbose: bool,
    /// The main input; either a FEA file or a UFO.
    ///
    /// If a FEA file, you will also need to provide a glyph order.
    /// If a UFO file, the public.glyphOrder key must be present.
    input: PathBuf,
    /// Path to a file containing the glyph order.
    ///
    /// This should be a utf-8 encoded file with one name per line,
    /// sorted in glyphid order.
    #[arg(short, long, group = "glyph_source")]
    glyph_order: Option<PathBuf>,

    /// Path to a font file to be used to calculate glyph order.
    #[arg(short, long, group = "glyph_source")]
    font: Option<PathBuf>,

    /// path to write the generated font. Defaults to 'compile-out.ttf'
    #[arg(short, long)]
    out_path: Option<PathBuf>,
}

impl Args {
    pub fn get_inputs(&self) -> Result<(PathBuf, GlyphMap), Error> {
        if self.input.extension() == Some("ufo".as_ref()) {
            let request = norad::DataRequest::none().lib(true);
            let font = norad::Font::load_requested_data(&self.input, request)?;
            let glyph_order = compile::get_ufo_glyph_order(&font).ok_or(Error::UfoBadGlyphOrder)?;
            let fea_path = self.input.join("features.fea");
            Ok((fea_path, glyph_order))
        } else {
            let order = if let Some(path) = self.glyph_order() {
                parse_glyph_order(path)?
            } else if let Some(path) = self.font.as_deref() {
                let bytes = std::fs::read(path)?;
                compile::get_post_glyph_order(&bytes).ok_or(Error::FontBadPostTable)?
            } else {
                return Err(Error::MissingGlyphOrder);
            };
            Ok((self.input.clone(), order))
        }
    }

    fn glyph_order(&self) -> Option<&Path> {
        self.glyph_order.as_deref()
    }

    fn out_path(&self) -> &Path {
        self.out_path
            .as_deref()
            .unwrap_or_else(|| Path::new("compile-out.ttf"))
    }
}

impl From<norad::error::FontLoadError> for Error {
    fn from(src: norad::error::FontLoadError) -> Error {
        Error::Ufo(Box::new(src))
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
