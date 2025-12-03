//! Arguments to guide a font compilation

use std::path::PathBuf;

use fontir::orchestration::Flags;

use crate::Input;

#[derive(Debug)]
pub struct Args {
    pub flags: Flags,
    pub skip_features: bool,
    pub build_dir: PathBuf,
    pub output_file: Option<PathBuf>,
    pub input: Input,
}

impl Args {
    /// Manually create args for testing
    #[cfg(test)]
    pub fn for_test(build_dir: &std::path::Path, source: &str) -> Args {
        let input_source = crate::testdata_dir()
            .join(source)
            .canonicalize()
            .unwrap();
        Self {
            build_dir: build_dir.to_path_buf(),
            flags: Flags::default() | Flags::EMIT_IR,
            skip_features: false,
            output_file: Some(build_dir.join("font.ttf")),
            input: Input::new(&input_source)
                .unwrap_or_else(|e| panic!("Unable to create input for {input_source:?}: {e}")),
        }
    }
}
