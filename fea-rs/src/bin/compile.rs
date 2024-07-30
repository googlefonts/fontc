//! Compile features into a font file

use std::path::{Path, PathBuf};

use clap::Parser;
use fea_rs::{
    compile::{
        self,
        error::{CompilerError, FontGlyphOrderError, GlyphOrderError, UfoGlyphOrderError},
        Compiler, MockVariationInfo, NopFeatureProvider, Opts,
    },
    GlyphMap,
};

/// Attempt to compile features into a font file.
///
/// usage: FONT_PATH GLYPH_ORDER
///
/// where glyph order is a file listing glyphs, one per line, in glyph id order.
fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1)
    }
}

fn run() -> Result<(), Error> {
    env_logger::init();
    let args = Args::parse();
    let (fea, glyph_names) = args.get_inputs()?;
    if !fea.exists() {
        return Err(Error::EmptyFeatureFile);
    }

    let var_info = args.get_var_info().transpose()?;
    let opts = args.opts();

    let mut compiler: Compiler<'_, NopFeatureProvider, MockVariationInfo> =
        Compiler::new(fea, &glyph_names).with_opts(opts);
    if let Some(var_info) = var_info.as_ref() {
        log::info!("compiling with {} mock variation axes", var_info.axes.len());
        for axis in &var_info.axes {
            log::info!(
                "{}: ({}, {}, {})",
                axis.tag,
                axis.min.into_inner(),
                axis.default.into_inner(),
                axis.max.into_inner()
            );
        }

        compiler = compiler.with_variable_info(var_info);
    }
    let compiled = compiler.compile()?;

    let path = args.out_path();
    let raw_font = compiled
        .to_binary(&glyph_names)
        .expect("ttf compile failed");

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
    #[error("Error parsing axis info: L{line}, '{message}'")]
    BadAxisInfo { line: usize, message: String },
    #[error("{}", .0.display_verbose())]
    CompileFail(#[from] CompilerError),
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

    /// Variable fonts only: a path to a file containing info on variation axes.
    ///
    /// Note that we can not correctly compile variable fonts, because we do not
    /// have the ability to compile deltas. This functionality is provided for
    /// debugging.
    ///
    /// This should be a utf-8 encoded file containing a list of axes and their
    /// (min, default, max) values, in user coordinates.
    ///
    /// Blank lines and lines beginning with '#' will be skipped.
    ///
    /// e.g. it might look like,
    ///
    /// ```
    /// wght 100 400 900
    /// wdth 50 100 200
    /// ```
    #[arg(short, long)]
    axis_info: Option<PathBuf>,

    /// path to write the generated font. Defaults to 'compile-out.ttf'
    #[arg(short, long)]
    out_path: Option<PathBuf>,

    /// Optionally write a post table to the generated font
    #[arg(short, long)]
    post: bool,

    #[arg(long)]
    skip_gpos: bool,

    #[arg(long)]
    skip_gsub: bool,
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

    fn get_var_info(&self) -> Option<Result<MockVariationInfo, Error>> {
        let path = self.axis_info()?;

        let contents = match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => return Some(Err(e.into())),
        };
        Some(
            MockVariationInfo::from_cli_input(&contents)
                .map_err(|(line, message)| Error::BadAxisInfo { line, message }),
        )
    }

    fn axis_info(&self) -> Option<&Path> {
        self.axis_info.as_deref()
    }

    fn glyph_order(&self) -> Option<&Path> {
        self.glyph_order.as_deref()
    }

    fn opts(&self) -> Opts {
        Opts::new()
            .make_post_table(self.post)
            .compile_gpos(!self.skip_gpos)
            .compile_gsub(!self.skip_gsub)
    }

    fn out_path(&self) -> &Path {
        self.out_path
            .as_deref()
            .unwrap_or_else(|| Path::new("compile-out.ttf"))
    }
}

#[cfg(feature = "norad")]
impl From<norad::error::FontLoadError> for Error {
    fn from(src: norad::error::FontLoadError) -> Error {
        Error::Ufo(Box::new(src))
    }
}
