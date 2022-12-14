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
    orchestration::{Context, WorkIdentifier},
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

    /// Whether to write IR to disk. Must be true if you want incremental compilation.
    #[arg(short, long)]
    #[clap(default_value = "true")]
    emit_ir: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
struct Config {
    args: Args,
    // The compiler previously used so if the compiler changes config invalidates
    compiler: StateSet,

    build_dir: PathBuf,
}

impl Config {
    fn new(args: Args, build_dir: PathBuf) -> Result<Config, io::Error> {
        let mut compiler = StateSet::new();
        compiler.track_file(&std::env::current_exe()?)?;
        Ok(Config {
            args,
            compiler,
            build_dir,
        })
    }

    fn file(&self) -> PathBuf {
        self.build_dir.join("fontc.yml")
    }

    fn has_changed(&self, config_file: &Path) -> bool {
        if !config_file.is_file() {
            return true;
        }
        let yml = fs::read_to_string(config_file).expect("Unable to read config");
        let prior_config = serde_yaml::from_str::<Config>(&yml);
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

fn init(config: &Config) -> Result<Input, io::Error> {
    let config_file = config.file();

    let ir_paths = Paths::new(&config.build_dir);
    let ir_input_file = ir_paths.ir_input_file();
    if config.has_changed(&config_file) {
        info!("Config changed, generating a new one");
        if ir_input_file.exists() {
            fs::remove_file(ir_input_file).expect("Unable to delete old ir input file");
        }
        fs::write(
            config_file,
            serde_yaml::to_string(&config).expect("Unable to make yaml for config"),
        )
        .expect("Unable to write updated config");
    };

    let ir_input = if ir_input_file.exists() {
        let yml = fs::read_to_string(ir_input_file).expect("Unable to load ir input file");
        serde_yaml::from_str(&yml).expect("Unable to parse ir input file")
    } else {
        Input::new()
    };
    Ok(ir_input)
}

fn ir_source(source: &Path) -> Result<Box<dyn Source>, Error> {
    if !source.exists() {
        return Err(Error::FileExpected(source.to_path_buf()));
    }
    let ext = source
        .extension()
        .and_then(OsStr::to_str)
        .ok_or_else(|| Error::UnrecognizedSource(source.to_path_buf()))?;
    match ext {
        "designspace" => Ok(Box::from(DesignSpaceIrSource::new(source.to_path_buf()))),
        "glyphs" => Ok(Box::from(GlyphsIrSource::new(source.to_path_buf()))),
        _ => Err(Error::UnrecognizedSource(source.to_path_buf())),
    }
}

fn finish_successfully(context: ChangeDetector) -> Result<(), Error> {
    let current_sources =
        serde_yaml::to_string(&context.current_inputs).map_err(Error::YamlSerError)?;
    fs::write(context.paths.ir_input_file(), current_sources).map_err(Error::IoError)
}

fn global_change(current_inputs: &Input, prev_inputs: &Input) -> bool {
    current_inputs.global_metadata != prev_inputs.global_metadata
}

fn glyphs_changed(context: &ChangeDetector) -> HashSet<&str> {
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

fn add_static_metadata_work(
    change_detector: &mut ChangeDetector,
    context: &Context,
    work: &mut Vec<Box<dyn Work>>,
) -> Result<(), Error> {
    let context = context.copy_for_work(WorkIdentifier::StaticMetadata, None);
    work.push(
        change_detector
            .ir_source
            .create_static_metadata_work(&context)?,
    );
    Ok(())
}

fn add_glyph_work(
    config: &Config,
    change_detector: &mut ChangeDetector,
    context: &Context,
    work: &mut Vec<Box<dyn Work>>,
) -> Result<(), Error> {
    // Destroy IR for deleted glyphs
    work.extend(
        glyphs_deleted(
            &change_detector.current_inputs,
            &change_detector.prev_inputs,
        )
        .iter()
        .map(|glyph_name| DeleteWork::create(change_detector.paths.glyph_ir_file(glyph_name))),
    );

    // Generate IR for changed glyphs
    let glyphs_changed = glyphs_changed(change_detector);
    let glyph_work = change_detector
        .ir_source
        .create_glyph_ir_work(&glyphs_changed, context)?;
    if config.args.emit_ir {}
    work.extend(glyph_work);

    Ok(())
}

fn do_work(context: &Context, work: Vec<Box<dyn Work>>) -> Result<(), Error> {
    let work_units = work.len();
    debug!("{} work units to execute", work_units);

    // TODO: https://github.com/googlefonts/fontmake-rs/pull/26 style execution w/task-specific contexts
    let errors: Vec<WorkError> = work
        .into_par_iter()
        .filter_map(|work| work.exec(context).err())
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

struct ChangeDetector {
    paths: Paths,
    ir_source: Box<dyn Source>,
    prev_inputs: Input,
    current_inputs: Input,
}

impl ChangeDetector {
    fn new(config: Config, paths: Paths, prev_inputs: Input) -> Result<ChangeDetector, Error> {
        // What sources are we dealing with?
        let mut ir_source = ir_source(&config.args.source)?;
        let current_inputs = ir_source.inputs().map_err(Error::FontIrError)?;

        Ok(ChangeDetector {
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
    let build_dir = require_dir(paths.build_dir())?;
    require_dir(paths.glyph_ir_dir())?;
    let config = Config::new(Args::parse(), build_dir)?;
    let prev_inputs = init(&config).map_err(Error::IoError)?;

    let mut change_detector = ChangeDetector::new(config.clone(), paths.clone(), prev_inputs)?;
    let context = Context::new_root(
        config.args.emit_ir,
        paths,
        change_detector.current_inputs.clone(),
    );

    // TODO: consider making a more explicit model of a graph
    // TODO: Rayon focuses on CPU-bound tasks but we are doing IO too
    // perhaps we should do the IO first, e.g. read (but not parse) inputs, etc?

    // TODO: enter metadata work onto threadpool, proper dependencies
    // Pending proper graph processing just run it first
    let mut work: Vec<Box<dyn Work>> = Vec::new();
    add_static_metadata_work(&mut change_detector, &context, &mut work)?;
    do_work(&context, work)?;

    // Accumulate work for the parts of IR that trivially parallelize
    let mut work: Vec<Box<dyn Work>> = Vec::new();

    add_glyph_work(&config, &mut change_detector, &context, &mut work)?;

    // Try to do the work
    do_work(&context, work)?;

    // Goodness, it all seems to have worked
    finish_successfully(change_detector)?;
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
    use fontir::{orchestration::Context, source::Paths, stateset::StateSet};
    use tempfile::tempdir;

    use crate::{
        add_glyph_work, add_static_metadata_work, finish_successfully, glyphs_changed,
        glyphs_deleted, init, require_dir, Args, ChangeDetector, Config,
    };

    fn testdata_dir() -> PathBuf {
        let path = PathBuf::from("../resources/testdata")
            .canonicalize()
            .unwrap();
        assert!(path.is_dir(), "{:#?} isn't a dir", path);
        path
    }

    fn test_args(source: &str) -> Args {
        Args {
            source: testdata_dir().join(source),
            emit_ir: true,
        }
    }

    struct TestCompile {
        glyphs_changed: HashSet<String>,
        glyphs_deleted: HashSet<String>,
    }

    impl TestCompile {
        fn new(context: &ChangeDetector) -> TestCompile {
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
        let args = test_args("wght_var.designspace");
        let config = Config::new(args.clone(), build_dir.to_path_buf()).unwrap();

        init(&config).unwrap();
        let config_file = config.file();
        let paths = Paths::new(build_dir);
        let ir_input_file = paths.ir_input_file();

        assert!(config_file.exists(), "Should exist: {:#?}", config_file);
        assert!(
            !ir_input_file.exists(),
            "Should not exist: {:#?}",
            ir_input_file
        );
        assert!(!Config::new(args, build_dir.to_path_buf())
            .unwrap()
            .has_changed(&config_file));
    }

    #[test]
    fn detect_compiler_change() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let args = test_args("wght_var.designspace");
        let config = Config::new(args.clone(), build_dir.to_path_buf()).unwrap();

        let compiler_location = std::env::current_exe().unwrap();
        let metadata = compiler_location.metadata().unwrap();
        let mut compiler = StateSet::new();
        // size +1, I'd give it all up for just a little more
        compiler.set_file_state(
            &compiler_location,
            FileTime::from_system_time(metadata.modified().unwrap()),
            metadata.len() + 1,
        );
        assert!(Config {
            args,
            compiler,
            build_dir: build_dir.to_path_buf()
        }
        .has_changed(&config.file()));
    }

    fn compile(build_dir: &Path, source: &str) -> TestCompile {
        let args = test_args(source);
        let paths = Paths::new(build_dir);
        require_dir(paths.glyph_ir_dir()).unwrap();
        let config = Config::new(args, build_dir.to_path_buf()).unwrap();

        let prev_inputs = init(&config).unwrap();

        let mut change_detector =
            ChangeDetector::new(config.clone(), paths.clone(), prev_inputs).unwrap();
        let work_context = Context::new_root(
            config.args.emit_ir,
            paths,
            change_detector.current_inputs.clone(),
        );
        let result = TestCompile::new(&change_detector);

        let mut work = Vec::new();

        add_static_metadata_work(&mut change_detector, &work_context, &mut work).unwrap();
        add_glyph_work(&config, &mut change_detector, &work_context, &mut work).unwrap();

        // Try to do the work
        for work_item in work {
            work_item.exec(&work_context).unwrap();
        }

        finish_successfully(change_detector).unwrap();
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

        let bar_ir = build_dir.join("glyph_ir/bar.yml");
        assert!(bar_ir.is_file(), "no file {:#?}", bar_ir);
        fs::remove_file(bar_ir).unwrap();

        let result = compile(build_dir, "wght_var.designspace");
        assert_eq!(HashSet::from(["bar".to_string()]), result.glyphs_changed);
        assert_eq!(HashSet::from([]), result.glyphs_deleted);
    }
}
