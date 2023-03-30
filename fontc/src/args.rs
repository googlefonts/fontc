//! Command line arguments

use std::path::PathBuf;

use clap::{ArgAction, Parser};
use fontir::orchestration::Flags;
use serde::{Deserialize, Serialize};

/// What font can we build for you today?
#[derive(Serialize, Deserialize, Parser, Debug, Clone, PartialEq)]
pub struct Args {
    /// A designspace, ufo, or glyphs file
    #[arg(short, long)]
    pub source: PathBuf,

    /// Whether to write IR to disk. Must be true if you want incremental compilation.
    #[arg(short, long, default_value = "true", action = ArgAction::Set)]
    pub emit_ir: bool,

    /// Whether to write additional debug files to disk.
    #[arg(long, default_value = "false")]
    pub emit_debug: bool,

    /// Whether to Try Hard(tm) to match fontmake (Python) behavior in cases where there are other options.
    ///
    /// See <https://github.com/googlefonts/fontmake-rs/pull/123> for an example of
    /// where this matters.
    #[arg(long, default_value = "true")]
    pub match_legacy: bool,

    /// Working directory for the build process. If emit-ir is on, written here.
    #[arg(short, long, default_value = "build")]
    pub build_dir: PathBuf,

    /// Glyph names must match this regex to be processed
    #[arg(short, long, default_value = None)]
    pub glyph_name_filter: Option<String>,
}

impl Args {
    /// Collect various relevant flags into a [`Flags`] object.
    pub fn flags(&self) -> Flags {
        let mut flags = Flags::default();

        flags.set(Flags::EMIT_IR, self.emit_ir);
        flags.set(Flags::EMIT_DEBUG, self.emit_debug);
        flags.set(Flags::MATCH_LEGACY, self.match_legacy);

        flags
    }

    /// Manually create args for testing
    #[cfg(test)]
    pub fn for_test(build_dir: &std::path::Path, source: &str) -> Args {
        fn testdata_dir() -> PathBuf {
            let path = PathBuf::from("../resources/testdata")
                .canonicalize()
                .unwrap();
            assert!(path.is_dir(), "{path:#?} isn't a dir");
            path
        }
        Args {
            glyph_name_filter: None,
            source: testdata_dir().join(source),
            emit_ir: true,
            emit_debug: false,
            build_dir: build_dir.to_path_buf(),
            match_legacy: true,
        }
    }
}
