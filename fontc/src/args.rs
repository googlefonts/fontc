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
    #[arg(long, default_value = "true", action = ArgAction::Set)]
    pub prefer_simple_glyphs: bool,

    /// Eliminate component references to other glyphs using components (that is, nested components),
    /// emitting only component references to simple (contour) glyphs.
    #[arg(long, default_value = "false")]
    pub flatten_components: bool,

    /// Whether all components with a non-identity 2x2 transform will be converted to outlines.
    // Named to match the ufo2ft flag as suggested in <https://github.com/googlefonts/fontc/pull/480#discussion_r1343801553>
    #[arg(long, default_value = "false")]
    pub decompose_transformed_components: bool,

    /// Whether to out timing data, notably a visualization of threadpool execution of tasks.
    ///
    /// See <https://github.com/googlefonts/fontc/pull/443>
    #[arg(long, default_value = "false")]
    pub emit_timing: bool,

    /// Working directory for the build process. If emit-ir is on, written here.
    #[arg(short, long, default_value = "build")]
    pub build_dir: PathBuf,

    /// Glyph names must match this regex to be processed
    #[arg(short, long, default_value = None)]
    pub glyph_name_filter: Option<String>,

    /// Set to skip compilation of OpenType Layout features
    #[arg(long, default_value = "false")]
    pub skip_features: bool,
}

impl Args {
    /// Collect various relevant flags into a [`Flags`] object.
    pub fn flags(&self) -> Flags {
        let mut flags = Flags::default();

        flags.set(Flags::EMIT_IR, self.emit_ir);
        flags.set(Flags::EMIT_DEBUG, self.emit_debug);
        flags.set(Flags::PREFER_SIMPLE_GLYPHS, self.prefer_simple_glyphs);
        flags.set(Flags::FLATTEN_COMPONENTS, self.flatten_components);
        flags.set(
            Flags::DECOMPOSE_TRANSFORMED_COMPONENTS,
            self.decompose_transformed_components,
        );
        flags.set(Flags::EMIT_TIMING, self.emit_timing);

        flags
    }

    /// Manually create args for testing
    #[cfg(test)]
    pub fn for_test(build_dir: &std::path::Path, source: &str) -> Args {
        use crate::testdata_dir;

        let source = testdata_dir().join(source).canonicalize().unwrap();

        Args {
            glyph_name_filter: None,
            source,
            emit_ir: true,
            emit_debug: false, // they get destroyed by test cleanup
            emit_timing: false,
            build_dir: build_dir.to_path_buf(),
            prefer_simple_glyphs: Flags::default().contains(Flags::PREFER_SIMPLE_GLYPHS),
            flatten_components: Flags::default().contains(Flags::FLATTEN_COMPONENTS),
            decompose_transformed_components: Flags::default()
                .contains(Flags::DECOMPOSE_TRANSFORMED_COMPONENTS),
            skip_features: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clap::Parser;
    use fontir::orchestration::Flags;

    use crate::Args;

    // It's awkward to get the Flags::default values into #[arg] so test for consistency
    #[test]
    fn arg_default_matches_flags_default() {
        let arg_default = Args::parse_from(vec!["program", "--source", "dont.care"]).flags();
        let flags_default = Flags::default();
        assert_eq!(flags_default.bits(), arg_default.bits());
    }
}
