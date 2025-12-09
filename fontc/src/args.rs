//! Command line arguments

use std::path::PathBuf;

use clap::{ArgAction, Parser};
use fontc::{DisableFlags, Input, Options};
use fontir::orchestration::Flags;

use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize};

use crate::Error;

/// What font can we build for you today?
#[derive(Serialize, Deserialize, Parser, Debug, Clone, PartialEq)]
#[command(version)]
pub struct Args {
    /// A designspace, ufo, or glyphs file
    #[arg(
        conflicts_with = "source",
        required_unless_present("source"),
        required_unless_present("verbose_version")
    )]
    pub input_source: Option<PathBuf>,

    /// DEPRECATED: old name for positional input file
    #[arg(short, long)]
    source: Option<PathBuf>,

    /// Whether to write IR to disk.
    #[arg(short, long, default_value = "false")]
    pub emit_ir: bool,

    /// Output file name (default: build/font.ttf)
    #[arg(short, long)]
    pub output_file: Option<PathBuf>,

    /// Whether to write additional debug files to disk.
    #[arg(long, default_value = "false")]
    pub emit_debug: bool,

    /// In cases where a source glyph uses a mixture of components and contours, convert
    /// all the components to contours.
    #[arg(long, default_value = "true", action = ArgAction::Set)]
    pub prefer_simple_glyphs: bool,

    /// Flatten nested components to reference only simple (contour) glyphs.
    ///
    /// Omit to use source default; use =true or =false to override.
    #[arg(long, num_args = 0..=1, default_missing_value = "true")]
    pub flatten_components: Option<bool>,

    /// Erase open corners (Glyphs-native feature).
    ///
    /// Omit to use source default; use =true or =false to override.
    #[arg(long, num_args = 0..=1, default_missing_value = "true")]
    pub erase_open_corners: Option<bool>,

    /// Whether all components with a non-identity 2x2 transform will be converted to outlines.
    // Named to match the ufo2ft flag as suggested in <https://github.com/googlefonts/fontc/pull/480#discussion_r1343801553>
    #[arg(long, default_value = "false")]
    pub decompose_transformed_components: bool,

    /// Whether to decompose all components (superseding the above two flags).
    #[arg(long, default_value = "false")]
    pub decompose_components: bool,

    /// Whether to out timing data, notably a visualization of threadpool execution of tasks.
    ///
    /// See <https://github.com/googlefonts/fontc/pull/443>
    #[arg(long, default_value = "false")]
    pub emit_timing: bool,

    /// Working directory for the build process. If emit-ir is on, written here.
    #[arg(short, long, default_value = "build")]
    pub build_dir: PathBuf,

    /// Glyph names must match this regex to be processed
    #[arg(short, long, default_value = None, value_parser = ValidatedRegex::parse)]
    pub glyph_name_filter: Option<ValidatedRegex>,

    /// Set to skip compilation of OpenType Layout features
    #[arg(long, default_value = "false")]
    pub skip_features: bool,

    /// Whether to keep the original glyph contour direction (TTF only).
    ///
    /// TrueType contours are recommended to follow clockwise orientation;
    /// By default, we assume the source were drawn in counter-clockwise direction
    /// as in PostScript outlines, and we flip the direction.
    // Named to match fontmake's homonymous flag:
    // https://github.com/googlefonts/fontmake/blob/6a8b2907/Lib/fontmake/__main__.py#L443
    #[arg(long, default_value = "false")]
    pub keep_direction: bool,

    /// Don't rename glyphs with production names
    // Named to match fontmake's homonymous flag:
    // https://github.com/googlefonts/fontmake/blob/6a8b2907/Lib/fontmake/__main__.py#L602
    #[arg(long, default_value = "false")]
    pub no_production_names: bool,

    /// Print verbose version information for debugging
    // Includes fontc git commit, rustc host triple, rustc version and channel, llvm version,
    // cargo profile, and cargo optimization level.
    #[arg(long = "vv", default_value = "false")]
    pub verbose_version: bool,

    /// Set the log level, either globally or per module.
    ///
    /// See <https://docs.rs/env_logger/latest/env_logger/#enabling-logging> for format.
    #[arg(long)]
    pub log: Option<String>,
}

/// A wrapper around a validated regex string
///
/// This is a wrapper because the Regex type itself does not implement PartialEq or
/// the serde traits, which we require.
#[derive(Clone, Debug)]
pub struct ValidatedRegex(Regex);

impl Args {
    /// Collect various relevant flags into a [`Flags`] object.
    pub fn flags(&self) -> Flags {
        let mut flags = Flags::default();

        flags.set(Flags::PREFER_SIMPLE_GLYPHS, self.prefer_simple_glyphs);

        // Tri-state flags: Some(true) enables here; Some(false) handled by flags_to_disable()
        if self.flatten_components == Some(true) {
            flags.set(Flags::FLATTEN_COMPONENTS, true);
        }
        if self.erase_open_corners == Some(true) {
            flags.set(Flags::ERASE_OPEN_CORNERS, true);
        }
        // TODO: add PROPAGATE_ANCHORS flag when we have that implemented

        flags.set(
            Flags::DECOMPOSE_TRANSFORMED_COMPONENTS,
            self.decompose_transformed_components,
        );
        flags.set(Flags::DECOMPOSE_COMPONENTS, self.decompose_components);
        flags.set(Flags::KEEP_DIRECTION, self.keep_direction);
        flags.set(Flags::PRODUCTION_NAMES, !self.no_production_names);

        flags
    }

    /// Returns flags that should be explicitly disabled, overriding source defaults.
    ///
    /// Tri-state flags use this logic:
    /// - `None` = use source default
    /// - `Some(true)` = force enable (handled in `flags()`)
    /// - `Some(false)` = force disable (returned here)
    pub fn flags_to_disable(&self) -> DisableFlags {
        let mut disable = Flags::empty();
        if self.flatten_components == Some(false) {
            disable.set(Flags::FLATTEN_COMPONENTS, true);
        }
        if self.erase_open_corners == Some(false) {
            disable.set(Flags::ERASE_OPEN_CORNERS, true);
        }
        // TODO: add PROPAGATE_ANCHORS flag when we have that implemented
        disable.into()
    }

    /// The input source to compile.
    pub fn source(&self) -> Result<Input, Error> {
        // safe to unwrap because clap ensures that the input_source is
        // required_unless_present("source")
        let path = self
            .source
            .as_ref()
            .unwrap_or_else(|| self.input_source.as_ref().unwrap());
        Input::try_from(path.as_path())
    }
}

impl ValidatedRegex {
    /// Create a new regex from a raw string.
    ///
    /// We use a string as the error type because this is mainly
    /// designed to be used with clap.
    pub fn parse(s: &str) -> Result<Self, String> {
        Regex::new(s).map_err(|e| e.to_string()).map(ValidatedRegex)
    }
}

impl PartialEq for ValidatedRegex {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}

impl<'de> Deserialize<'de> for ValidatedRegex {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Regex::new(&s)
            .map_err(|e| serde::de::Error::custom(format!("invalid regex: '{e}'")))
            .map(ValidatedRegex)
    }
}

impl Serialize for ValidatedRegex {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.as_str().serialize(serializer)
    }
}

impl TryInto<Options> for Args {
    type Error = Error;

    fn try_into(self) -> Result<Options, Self::Error> {
        let flags = self.flags();
        let flags_to_disable = self.flags_to_disable();
        let timing_file = self.emit_timing.then(|| self.build_dir.join("threads.svg"));
        let debug_dir = self.emit_debug.then(|| self.build_dir.join("debug/"));
        let ir_dir = self.emit_ir.then(|| self.build_dir.clone());
        Ok(Options {
            flags,
            flags_to_disable,
            skip_features: self.skip_features,
            output_file: self
                .output_file
                .or_else(|| Some(self.build_dir.join("font.ttf"))),
            timing_file,
            debug_dir,
            ir_dir,
        })
    }
}

#[cfg(test)]
mod tests {
    use clap::Parser;
    use fontir::orchestration::Flags;

    use crate::args::Args;

    // It's awkward to get the Flags::default values into #[arg] so test for consistency
    #[test]
    fn arg_default_matches_flags_default() {
        let arg_default = Args::parse_from(vec!["program", "--source", "dont.care"]).flags();
        let flags_default = Flags::default();
        assert_eq!(
            flags_default.bits(),
            arg_default.bits(),
            "mismatch in defaults. flags_default: {:#032b}. args_default: {:#032b}",
            flags_default.bits(),
            arg_default.bits(),
        );
    }
}
