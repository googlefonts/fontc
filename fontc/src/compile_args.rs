//! Arguments to guide a font compilation

use std::path::PathBuf;

use fontir::orchestration::Flags;

use crate::Input;

#[derive(Debug)]
pub struct Args {
    pub flags: Flags,
    pub skip_features: bool,
    pub output_file: Option<PathBuf>,
    pub timing_file: Option<PathBuf>,
    pub ir_dir: Option<PathBuf>,
    pub debug_dir: Option<PathBuf>,
    pub input: Input,
}

impl Args {
    /// Manually create args for testing
    #[cfg(test)]
    pub fn for_test(build_dir: &std::path::Path, source: &str) -> Args {
        let input_source = crate::testdata_dir().join(source).canonicalize().unwrap();
        Self {
            flags: Flags::default(),
            skip_features: false,
            output_file: Some(build_dir.join("font.ttf")),
            ir_dir: Some(build_dir.to_path_buf()),
            debug_dir: None,
            timing_file: None,
            input: Input::new(&input_source)
                .unwrap_or_else(|e| panic!("Unable to create input for {input_source:?}: {e}")),
        }
    }
}
