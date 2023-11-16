//! State for a (possibly incremental) compiler job

use std::{fs, io, path::PathBuf};

use fontir::{paths::Paths as IrPaths, source::Input, stateset::StateSet};
use serde::{Deserialize, Serialize};

use crate::{Args, Error};

/// The settings and compiler definition of a single compilation run.
///
/// This tracks the input arguments for a compilation, as well as the compiler
/// version used (so that we do not reuse incremental state between different
/// compiler versions)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Config {
    pub args: Args,
    // The compiler previously used so if the compiler changes config invalidates
    compiler: StateSet,
}

impl Config {
    /// Create a new config from the provided cli arguments
    pub fn new(args: Args) -> Result<Config, io::Error> {
        let mut compiler = StateSet::new();
        compiler.track_file(&std::env::current_exe()?)?;
        Ok(Config { args, compiler })
    }

    /// Returns the path to the config file for this compilation
    fn file(&self) -> PathBuf {
        self.args.build_dir.join("fontc.yml")
    }

    /// Attempt to initialize the compiler for this run.
    ///
    /// This serializes the configuration if necessary, and returns any existing
    /// incremental state for this run if it exists.
    pub fn init(&self) -> Result<Input, Error> {
        let config_file = self.file();

        let ir_paths = IrPaths::new(&self.args.build_dir);
        let ir_input_file = ir_paths.ir_input_file();
        if self.has_changed() {
            log::info!("Config changed, generating a new one");
            if ir_input_file.exists() {
                fs::remove_file(ir_input_file)
                    .map_err(|_| Error::FileExpected(ir_input_file.to_owned()))?;
            }
            if self.args.incremental {
                fs::write(config_file, serde_yaml::to_string(self)?)?;
            }
        };

        if !ir_input_file.exists() {
            return Ok(Input::new());
        }

        let yml = fs::read_to_string(ir_input_file)?;
        serde_yaml::from_str(&yml).map_err(Into::into)
    }

    /// Compare this config to the saved config at the provided path.
    fn has_changed(&self) -> bool {
        let config_file = self.file();
        if !config_file.is_file() {
            return true;
        }
        let yml = fs::read_to_string(config_file).expect("Unable to read config");
        match serde_yaml::from_str::<Config>(&yml) {
            Ok(prior_config) => self != &prior_config,
            Err(err) => {
                log::warn!("Unable to parse prior config {err:#?}");
                true
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use filetime::FileTime;
    use tempfile::tempdir;

    #[test]
    fn detect_compiler_change() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let args = Args::for_test(build_dir, "wght_var.designspace");
        let mut config = Config::new(args).unwrap();

        let compiler_location = std::env::current_exe().unwrap();
        let metadata = compiler_location.metadata().unwrap();
        // size +1, I'd give it all up for just a little more
        config.compiler.set_file_state(
            &compiler_location,
            FileTime::from_system_time(metadata.modified().unwrap()),
            metadata.len() + 1,
        );
        assert!(config.has_changed())
    }

    #[test]
    fn init_captures_state() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let args = Args::for_test(build_dir, "wght_var.designspace");
        let config = Config::new(args.clone()).unwrap();

        config.init().unwrap();
        let paths = IrPaths::new(build_dir);
        let ir_input_file = paths.ir_input_file();

        assert!(config.file().exists(),);
        assert!(
            !ir_input_file.exists(),
            "Should not exist: {ir_input_file:#?}"
        );
        assert!(!Config::new(args).unwrap().has_changed());
    }
}
