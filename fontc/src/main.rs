use std::{
    collections::HashSet,
    ffi::OsStr,
    fs, io,
    path::{Path, PathBuf},
};

use clap::Parser;
use fontc::Error;
use fontir::{
    filestate::FileStateSet,
    source::{Input, Source},
};
use log::{info, warn};
use serde::{Deserialize, Serialize};
use ufo2fontir::source::DesignSpaceIrSource;

/// What font can we build for you today?
#[derive(Serialize, Deserialize, Parser, Debug, Clone, PartialEq)]
struct Args {
    /// A designspace, ufo, or glyphs file
    #[arg(short, long)]
    source: PathBuf,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct Config {
    args: Args,
    // The compiler previously used so if the compiler changes config invalidates
    compiler: FileStateSet,
}

impl Config {
    fn new(args: Args) -> Result<Config, io::Error> {
        let mut compiler = FileStateSet::new();
        compiler.insert(&std::env::current_exe()?)?;
        Ok(Config { args, compiler })
    }

    fn has_changed(&self, config_file: &Path) -> bool {
        if !config_file.is_file() {
            return true;
        }
        let toml = fs::read_to_string(config_file).expect("Unable to read config");
        let prior_config = toml::from_str::<Config>(&toml);
        if prior_config.is_err() {
            warn!("Unable to parse prior config {:#?}", prior_config);
            return true;
        }
        let prior_config = prior_config.unwrap();
        *self != prior_config
    }
}

fn establish_build_dir() -> PathBuf {
    let build_dir = Path::new("build");
    if build_dir.exists() && !build_dir.is_dir() {
        panic!("The name build is taken by something that isn't a directory");
    }
    if !build_dir.exists() {
        fs::create_dir(build_dir).expect("Unable to create build/");
    }
    build_dir.to_path_buf()
}

fn config_file(build_dir: &Path) -> PathBuf {
    build_dir.join("fontc.toml")
}

fn ir_input_file(build_dir: &Path) -> PathBuf {
    build_dir.join("irinput.toml")
}

fn init(build_dir: &Path, args: Args) -> Result<(Config, Input), io::Error> {
    let config = Config::new(args)?;
    let config_file = config_file(build_dir);

    let ir_input_file = ir_input_file(build_dir);
    if config.has_changed(&config_file) {
        info!("Config changed, generating a new one");
        if ir_input_file.exists() {
            fs::remove_file(&ir_input_file).expect("Unable to delete old ir input file");
        }
        fs::write(
            config_file,
            toml::to_string_pretty(&config).expect("Unable to make toml for config"),
        )
        .expect("Unable to write updated config");
    };

    let ir_input = if ir_input_file.exists() {
        let toml = fs::read_to_string(ir_input_file).expect("Unable to load ir input file");
        toml::from_str(&toml).expect("Unable to parse ir input file")
    } else {
        Input::new()
    };
    Ok((config, ir_input))
}

fn ir_source(source: &Path) -> Result<Box<dyn Source>, Error> {
    let ext = source
        .extension()
        .and_then(OsStr::to_str)
        .ok_or_else(|| Error::UnrecognizedSource(source.to_path_buf()))?;
    match ext {
        "designspace" => Ok(Box::from(DesignSpaceIrSource::new(source))),
        _ => Err(Error::UnrecognizedSource(source.to_path_buf())),
    }
}

fn finish_successfully(build_dir: &Path, current_inputs: &Input) -> Result<(), Error> {
    let current_sources = toml::to_string_pretty(&current_inputs).map_err(Error::TomlSerError)?;
    fs::write(ir_input_file(build_dir), current_sources).map_err(Error::IoError)
}

fn global_change(current_inputs: &Input, prev_inputs: &Input) -> bool {
    current_inputs.font_info != prev_inputs.font_info
}

fn glyphs_changed<'a>(current_inputs: &'a Input, prev_inputs: &Input) -> HashSet<&'a str> {
    if global_change(current_inputs, prev_inputs) {
        return current_inputs.glyphs.keys().map(|e| e.as_str()).collect();
    }
    current_inputs
        .glyphs
        .iter()
        .filter(
            |(glyph_name, curr_state)| match prev_inputs.glyphs.get(*glyph_name) {
                Some(prev_state) => prev_state != *curr_state,
                None => true,
            },
        )
        .map(|(glyph_name, _)| glyph_name.as_str())
        .collect()
}

fn glyphs_deleted<'a>(current_inputs: &Input, prev_inputs: &'a Input) -> HashSet<&'a str> {
    prev_inputs
        .glyphs
        .keys()
        .filter(|glyph_name| !current_inputs.glyphs.contains_key(glyph_name.as_str()))
        .map(|e| e.as_str())
        .collect()
}

fn main() -> Result<(), Error> {
    env_logger::init();

    let build_dir = establish_build_dir();
    let (config, prev_inputs) = init(&build_dir, Args::parse()).map_err(Error::IoError)?;

    // What sources are we dealing with?
    let ir_source = ir_source(&config.args.source)?;
    let current_inputs = ir_source.inputs().map_err(Error::FontIrError)?;

    // TODO: contemplate generalizing
    let glyphs_changed = glyphs_changed(&current_inputs, &prev_inputs);
    let glyphs_deleted = glyphs_deleted(&current_inputs, &prev_inputs);

    // TODO: something useful with our newfound information
    eprintln!("GLYPHS CHANGED");
    for glyph_name in glyphs_changed {
        eprintln!("  {}", glyph_name);
    }

    eprintln!("GLYPHS DELETED (TODO delete associated IR)");
    for glyph_name in glyphs_deleted {
        eprintln!("  {}", glyph_name);
    }

    finish_successfully(&build_dir, &current_inputs)?;

    Ok(())
}

#[cfg(test)]
mod tests {

    use std::{
        collections::HashSet,
        path::{Path, PathBuf},
    };

    use filetime::FileTime;
    use fontir::{filestate::FileStateSet, source::Input};
    use tempfile::tempdir;

    use crate::{
        config_file, finish_successfully, glyphs_changed, glyphs_deleted, init, ir_input_file,
        ir_source, Args, Config,
    };

    fn testdata_dir() -> PathBuf {
        let path = PathBuf::from("../resources/testdata")
            .canonicalize()
            .unwrap();
        assert!(path.is_dir(), "{:#?} isn't a dir", path);
        path
    }

    struct CompileResult {
        prev_inputs: Input,
        current_inputs: Input,
    }

    impl CompileResult {
        fn glyphs_changed(&self) -> HashSet<&str> {
            glyphs_changed(&self.current_inputs, &self.prev_inputs)
        }

        fn glyphs_deleted(&self) -> HashSet<&str> {
            glyphs_deleted(&self.current_inputs, &self.prev_inputs)
        }
    }

    #[test]
    fn init_captures_state() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let args = Args {
            source: testdata_dir().join("wght_var.designspace"),
        };
        init(build_dir, args.clone()).unwrap();
        let config_file = config_file(build_dir);
        let ir_input_file = ir_input_file(build_dir);

        assert!(config_file.exists(), "Should exist: {:#?}", config_file);
        assert!(
            !ir_input_file.exists(),
            "Should not exist: {:#?}",
            ir_input_file
        );
        assert!(!Config::new(args).unwrap().has_changed(&config_file));
    }

    #[test]
    fn detect_compiler_change() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let args = Args {
            source: testdata_dir().join("wght_var.designspace"),
        };
        init(build_dir, args.clone()).unwrap();

        let compiler_location = std::env::current_exe().unwrap();
        let metadata = compiler_location.metadata().unwrap();
        let mut compiler = FileStateSet::new();
        // size +1, I'd give it all up for just a little more
        compiler.set_state(
            &compiler_location,
            FileTime::from_system_time(metadata.modified().unwrap()),
            metadata.len() + 1,
        );
        assert!(Config { args, compiler }.has_changed(&config_file(build_dir)));
    }

    fn compile(build_dir: &Path, source: &str) -> CompileResult {
        let args = Args {
            source: testdata_dir().join(source),
        };
        let (config, prev_inputs) = init(build_dir, args).unwrap();

        let current_inputs = ir_source(&config.args.source).unwrap().inputs().unwrap();

        finish_successfully(build_dir, &current_inputs).unwrap();

        CompileResult {
            current_inputs,
            prev_inputs,
        }
    }

    #[test]
    fn second_compile_is_nop() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let compile_result = compile(build_dir, "wght_var.designspace");
        assert_eq!(HashSet::from(["bar"]), compile_result.glyphs_changed());
        assert_eq!(HashSet::from([]), compile_result.glyphs_deleted());

        let compile_result = compile(build_dir, "wght_var.designspace");
        assert_eq!(HashSet::from([]), compile_result.glyphs_changed());
        assert_eq!(HashSet::from([]), compile_result.glyphs_deleted());
    }
}
