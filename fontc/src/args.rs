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

    /// In cases where a source glyph uses a mixture of components and contours, convert
    /// all the components to contours.
    #[arg(long, default_value = "true")]
    pub prefer_simple_glyphs: bool,

    /// Eliminate component references to other glyphs using components (that is, nested components),
    /// emitting only component references to simple (contour) glyphs.
    #[arg(long, default_value = "false")]
    pub flatten_components: bool,

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
        flags.set(Flags::PREFER_SIMPLE_GLYPHS, self.prefer_simple_glyphs);
        flags.set(Flags::FLATTEN_COMPONENTS, self.flatten_components);

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
            prefer_simple_glyphs: Flags::default().contains(Flags::PREFER_SIMPLE_GLYPHS),
            flatten_components: Flags::default().contains(Flags::FLATTEN_COMPONENTS),
        }
    }
}
