use std::{
    collections::HashSet,
    ffi::OsStr,
    fs, io,
    path::{Path, PathBuf},
};

use clap::Parser;
use fontc::Error;
use fontir::{
    error::WorkError,
    source::{DeleteWork, Input, Paths, Source, Work},
    stateset::StateSet,
};
use glyphs2fontir::source::GlyphsIrSource;
use log::{debug, error, info, warn};
use rayon::prelude::*;
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
    compiler: StateSet,
}

impl Config {
    fn new(args: Args) -> Result<Config, io::Error> {
        let mut compiler = StateSet::new();
        compiler.track_file(&std::env::current_exe()?)?;
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

fn require_dir(dir: &Path) -> Result<PathBuf, io::Error> {
    if dir.exists() && !dir.is_dir() {
        panic!("{:#?} is taken by something that isn't a directory", dir);
    }
    if !dir.exists() {
        fs::create_dir(dir)?
    }
    Ok(dir.to_path_buf())
}

fn config_file(build_dir: &Path) -> PathBuf {
    build_dir.join("fontc.toml")
}

fn init(build_dir: &Path, args: Args) -> Result<(Config, Input), io::Error> {
    let config = Config::new(args)?;
    let config_file = config_file(build_dir);

    let ir_paths = Paths::new(build_dir);
    let ir_input_file = ir_paths.ir_input_file();
    if config.has_changed(&config_file) {
        info!("Config changed, generating a new one");
        if ir_input_file.exists() {
            fs::remove_file(ir_input_file).expect("Unable to delete old ir input file");
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

fn ir_source(source: &Path, paths: Paths) -> Result<Box<dyn Source>, Error> {
    let ext = source
        .extension()
        .and_then(OsStr::to_str)
        .ok_or_else(|| Error::UnrecognizedSource(source.to_path_buf()))?;
    match ext {
        "designspace" => Ok(Box::from(DesignSpaceIrSource::new(
            source.to_path_buf(),
            paths,
        ))),
        "glyphs" => Ok(Box::from(GlyphsIrSource::new(source.to_path_buf(), paths))),
        _ => Err(Error::UnrecognizedSource(source.to_path_buf())),
    }
}

fn finish_successfully(context: CompileContext) -> Result<(), Error> {
    let current_sources =
        toml::to_string_pretty(&context.current_inputs).map_err(Error::TomlSerError)?;
    fs::write(context.paths.ir_input_file(), current_sources).map_err(Error::IoError)
}

fn global_change(current_inputs: &Input, prev_inputs: &Input) -> bool {
    current_inputs.font_info != prev_inputs.font_info
}

fn glyphs_changed(context: &CompileContext) -> HashSet<&str> {
    if global_change(&context.current_inputs, &context.prev_inputs) {
        return context
            .current_inputs
            .glyphs
            .keys()
            .map(|e| e.as_str())
            .collect();
    }
    context
        .current_inputs
        .glyphs
        .iter()
        .filter(
            |(glyph_name, curr_state)| match context.prev_inputs.glyphs.get(*glyph_name) {
                Some(prev_state) => {
                    // If the input changed or the output doesn't exist a rebuild is probably in order
                    prev_state != *curr_state || !context.paths.glyph_ir_file(glyph_name).exists()
                }
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

fn add_glyph_work(
    context: &mut CompileContext,
    work: &mut Vec<Box<dyn Work<()>>>,
) -> Result<(), Error> {
    // Destroy IR for deleted glyphs
    work.extend(
        glyphs_deleted(&context.current_inputs, &context.prev_inputs)
            .iter()
            .map(|glyph_name| DeleteWork::create(context.paths.glyph_ir_file(glyph_name))),
    );

    // Generate IR for changed glyphs
    let glyphs_changed = glyphs_changed(context);
    let glyph_work = context
        .ir_source
        .create_glyph_ir_work(&glyphs_changed, &context.current_inputs)?;
    work.extend(glyph_work);

    Ok(())
}

fn do_work(work: Vec<Box<dyn Work<()>>>) -> Result<(), Error> {
    let work_units = work.len();
    debug!("{} work units to execute", work_units);
    let errors: Vec<Result<(), WorkError>> = work
        .into_par_iter()
        .map(|work| work.exec())
        .filter(|r| r.is_err())
        .collect();
    for error in errors.iter() {
        warn!("{:#?}", error);
    }
    if !errors.is_empty() {
        error!(
            "{}/{} work units failed, details logged at warn",
            errors.len(),
            work_units
        );
        return Err(Error::IrGenerationError);
    } else {
        debug!("{}/{} work units successful", work_units, work_units);
    }
    Ok(())
}

struct CompileContext {
    paths: Paths,
    ir_source: Box<dyn Source>,
    prev_inputs: Input,
    current_inputs: Input,
}

impl CompileContext {
    fn new(paths: Paths, args: Args) -> Result<CompileContext, Error> {
        let build_dir = require_dir(paths.build_dir())?;
        require_dir(paths.glyph_ir_dir())?;
        let (config, prev_inputs) = init(&build_dir, args).map_err(Error::IoError)?;

        // What sources are we dealing with?
        let mut ir_source = ir_source(&config.args.source, paths.clone())?;
        let current_inputs = ir_source.inputs().map_err(Error::FontIrError)?;

        Ok(CompileContext {
            paths,
            ir_source,
            prev_inputs,
            current_inputs,
        })
    }
}

fn main() -> Result<(), Error> {
    env_logger::init();

    let paths = Paths::new(Path::new("build"));
    let mut context = CompileContext::new(paths, Args::parse())?;

    // TODO: consider making a more explicit model of a graph
    // TODO: Rayon focuses on CPU-bound tasks but we are doing IO too
    // perhaps we should do the IO first, e.g. read (but not parse) inputs, etc?

    // Accumulate work for the parts of IR that trivially parallelize
    let mut work: Vec<Box<dyn Work<()>>> = Vec::new();

    add_glyph_work(&mut context, &mut work)?;

    // Try to do the work
    do_work(work)?;

    // Goodness, it all seems to have worked
    finish_successfully(context)?;
    Ok(())
}

#[cfg(test)]
mod tests {

    use std::{
        collections::HashSet,
        fs,
        path::{Path, PathBuf},
    };

    use filetime::FileTime;
    use fontir::{source::Paths, stateset::StateSet};
    use tempfile::tempdir;

    use crate::{
        add_glyph_work, config_file, finish_successfully, glyphs_changed, glyphs_deleted, init,
        Args, CompileContext, Config,
    };

    fn testdata_dir() -> PathBuf {
        let path = PathBuf::from("../resources/testdata")
            .canonicalize()
            .unwrap();
        assert!(path.is_dir(), "{:#?} isn't a dir", path);
        path
    }

    struct TestCompile {
        glyphs_changed: HashSet<String>,
        glyphs_deleted: HashSet<String>,
    }

    impl TestCompile {
        fn new(context: &CompileContext) -> TestCompile {
            TestCompile {
                glyphs_changed: glyphs_changed(context)
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                glyphs_deleted: glyphs_deleted(&context.current_inputs, &context.prev_inputs)
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
            }
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
        let paths = Paths::new(build_dir);
        let ir_input_file = paths.ir_input_file();

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
        let mut compiler = StateSet::new();
        // size +1, I'd give it all up for just a little more
        compiler.set_file_state(
            &compiler_location,
            FileTime::from_system_time(metadata.modified().unwrap()),
            metadata.len() + 1,
        );
        assert!(Config { args, compiler }.has_changed(&config_file(build_dir)));
    }

    fn compile(build_dir: &Path, source: &str) -> TestCompile {
        let args = Args {
            source: testdata_dir().join(source),
        };
        let paths = Paths::new(build_dir);
        let mut context = CompileContext::new(paths, args).unwrap();
        let result = TestCompile::new(&context);

        let mut work = Vec::new();

        add_glyph_work(&mut context, &mut work).unwrap();

        // Try to do the work
        for work_item in work {
            work_item.exec().unwrap();
        }

        finish_successfully(context).unwrap();
        result
    }

    #[test]
    fn second_compile_is_nop() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(build_dir, "wght_var.designspace");
        assert_eq!(
            HashSet::from(["bar".to_string(), "plus".to_string()]),
            result.glyphs_changed
        );
        assert_eq!(HashSet::from([]), result.glyphs_deleted);

        let result = compile(build_dir, "wght_var.designspace");
        assert_eq!(HashSet::from([]), result.glyphs_changed);
        assert_eq!(HashSet::from([]), result.glyphs_deleted);
    }

    #[test]
    fn deleted_ir_recreated() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(build_dir, "wght_var.designspace");
        assert_eq!(
            HashSet::from(["bar".to_string(), "plus".to_string()]),
            result.glyphs_changed
        );
        assert_eq!(HashSet::from([]), result.glyphs_deleted);

        let bar_ir = build_dir.join("glyph_ir/bar.toml");
        assert!(bar_ir.is_file(), "no file {:#?}", bar_ir);
        fs::remove_file(bar_ir).unwrap();

        let result = compile(build_dir, "wght_var.designspace");
        assert_eq!(HashSet::from(["bar".to_string()]), result.glyphs_changed);
        assert_eq!(HashSet::from([]), result.glyphs_deleted);
    }
}
