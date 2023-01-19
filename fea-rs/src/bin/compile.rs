//! Compile features into a font file

use std::path::{Path, PathBuf};

use clap::Parser;
use fea_rs::{
    compile::{
        self,
        error::{FontGlyphOrderError, GlyphOrderError, UfoGlyphOrderError},
        Compiler, Opts,
    },
    GlyphMap,
};

/// Attempt to compile features into a font file.
///
/// usage: FONT_PATH GLYPH_ORDER
///
/// where glyph order is a file listing glyphs, one per line, in glyph id order.
fn main() -> Result<(), Error> {
    env_logger::init();
    let args = Args::parse();
    let (fea, glyph_names) = args.get_inputs()?;
    if !fea.exists() {
        return Err(Error::EmptyFeatureFile);
    }
    let compiled = Compiler::new(fea, &glyph_names)
        .with_opts(Opts::new().make_post_table(args.post))
        .compile()?;

    let path = args.out_path();
    let opts = Opts::new().make_post_table(args.post);
    let raw_font = compiled
        .assemble(&glyph_names, opts)
        .expect("ttf compile failed")
        .build();

    log::info!("writing {} bytes to {}", raw_font.len(), path.display());
    std::fs::write(path, raw_font).map_err(Into::into)
}

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("io error: '{0}'")]
    File(#[from] std::io::Error),
    #[error("Couldn't read UFO: '{0}'")]
    Ufo(Box<norad::error::FontLoadError>),
    #[error("invalid glyph map: '{0}'")]
    InvalidGlyphMap(#[from] GlyphOrderError),
    #[error("Couldn't get glyph order from UFO: '{0}'")]
    UfoBadGlyphOrder(#[from] UfoGlyphOrderError),
    #[error("Couldn't get glyph order from font: '{0}")]
    FontBadGlyphOrder(#[from] FontGlyphOrderError),
    #[error("The provided feature file is empty")]
    EmptyFeatureFile,
    #[error("No glyph order provided")]
    MissingGlyphOrder,
    #[error("{0}")]
    CompileFail(#[from] compile::error::CompilerError),
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

    /// Optionally write a post table to the generated font
    #[arg(short, long)]
    post: bool,
}

impl Args {
    pub fn get_inputs(&self) -> Result<(PathBuf, GlyphMap), Error> {
        if self.input.extension() == Some("ufo".as_ref()) {
            let request = norad::DataRequest::none().lib(true);
            let font = norad::Font::load_requested_data(&self.input, request)?;
            let glyph_order = compile::get_ufo_glyph_order(&font)?;
            let fea_path = self.input.join("features.fea");
            Ok((fea_path, glyph_order))
        } else {
            let order = if let Some(path) = self.glyph_order() {
                let contents = std::fs::read_to_string(path)?;
                compile::parse_glyph_order(&contents)?
            } else if let Some(path) = self.font.as_deref() {
                let bytes = std::fs::read(path)?;
                compile::get_post_glyph_order(&bytes)?
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
