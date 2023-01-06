use std::{
    collections::{HashMap, HashSet},
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
use log::{error, info, log_enabled, trace, warn};
use serde::{Deserialize, Serialize};
use std::io::Write;
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

fn add_static_metadata_ir_job(
    change_detector: &mut ChangeDetector,
    jobs: &mut HashMap<WorkIdentifier, Job>,
) -> Result<(), Error> {
    // TODO don't recreate static metadata if we don't have to
    jobs.insert(
        WorkIdentifier::StaticMetadata,
        Job {
            work: change_detector
                .ir_source
                .create_static_metadata_work(&change_detector.current_inputs)?,
            happens_after: HashSet::new(),
        },
    );
    Ok(())
}

fn add_glyph_ir_jobs(
    change_detector: &mut ChangeDetector,
    jobs: &mut HashMap<WorkIdentifier, Job>,
) -> Result<(), Error> {
    // Destroy IR for deleted glyphs. No dependencies.
    for glyph_name in glyphs_deleted(
        &change_detector.current_inputs,
        &change_detector.prev_inputs,
    )
    .iter()
    {
        jobs.insert(
            WorkIdentifier::GlyphIrDelete(glyph_name.to_string()),
            Job {
                work: DeleteWork::create(change_detector.paths.glyph_ir_file(glyph_name)),
                happens_after: HashSet::new(),
            },
        );
    }

    // Generate IR for changed glyphs
    let glyphs_changed = glyphs_changed(change_detector);
    let glyph_work = change_detector
        .ir_source
        .create_glyph_ir_work(&glyphs_changed, &change_detector.current_inputs)?;
    for (glyph_name, work) in glyphs_changed.iter().zip(glyph_work) {
        let id = WorkIdentifier::GlyphIr(glyph_name.to_string());
        let happens_after = HashSet::from([WorkIdentifier::StaticMetadata]);

        jobs.insert(
            id,
            Job {
                work,
                happens_after,
            },
        );
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

/// A unit of executable work plus the identifiers of work that it depends on
struct Job {
    // The actual task
    work: Box<dyn Work + Send>,
    // Things that must happen before us, e.g. our dependencies
    happens_after: HashSet<WorkIdentifier>,
}

fn create_jobs(
    change_detector: &mut ChangeDetector,
) -> Result<HashMap<WorkIdentifier, Job>, Error> {
    let mut jobs = HashMap::new();

    // FE: f(source) => IR
    add_static_metadata_ir_job(change_detector, &mut jobs)?;
    add_glyph_ir_jobs(change_detector, &mut jobs)?;

    // BE: f(IR) => binary
    // TODO :)

    Ok(jobs)
}

fn launchable(
    pending_jobs: &HashMap<WorkIdentifier, Job>,
    success: &HashSet<WorkIdentifier>,
) -> Vec<WorkIdentifier> {
    pending_jobs
        .iter()
        .filter_map(|(id, job)| {
            let can_start = job.happens_after.is_subset(success);
            if !can_start && log_enabled!(log::Level::Trace) {
                let mut unfulfilled_deps: Vec<_> =
                    job.happens_after.difference(success).into_iter().collect();
                unfulfilled_deps.sort();
                trace!("Cannot start {:?}, blocked on {:?}", id, unfulfilled_deps);
            };
            can_start.then(|| id.clone())
        })
        .collect()
}

fn main() -> Result<(), Error> {
    env_logger::builder()
        .format(|buf, record| {
            let ts = buf.timestamp_micros();
            writeln!(
                buf,
                "{}: {:?}: {}: {}",
                ts,
                std::thread::current().id(),
                buf.default_level_style(record.level())
                    .value(record.level()),
                record.args()
            )
        })
        .init();

    let paths = Paths::new(Path::new("build"));
    let build_dir = require_dir(paths.build_dir())?;
    require_dir(paths.glyph_ir_dir())?;
    let config = Config::new(Args::parse(), build_dir)?;
    let prev_inputs = init(&config).map_err(Error::IoError)?;

    let mut change_detector = ChangeDetector::new(config.clone(), paths.clone(), prev_inputs)?;
    let mut pending_jobs = create_jobs(&mut change_detector)?;
    let job_count = pending_jobs.len();

    // Async work will send us it's ID on completion
    let (send, recv) = crossbeam_channel::unbounded::<(WorkIdentifier, Result<(), WorkError>)>();
    let mut success = HashSet::new();
    let mut error = HashSet::new();
    rayon::in_place_scope(|scope| {
        let root_context = Context::new_root(
            config.args.emit_ir,
            paths,
            change_detector.current_inputs.clone(),
        );

        // Whenever a task completes see if it was the last incomplete dependency of other task(s)
        // and spawn them if it was
        // TODO timeout and die it if takes too long to make forward progress or we're spinning w/o progress
        while success.len() < job_count && error.is_empty() {
            // Spawn anything that is currently executable (has no unfulfilled dependencies)
            for id in launchable(&pending_jobs, &success) {
                info!("Start {:?}", id);
                let job = pending_jobs.remove(&id).unwrap();
                let work = job.work;
                let work_context = root_context.copy_for_work(id.clone(), Some(job.happens_after));
                let send = send.clone();
                scope.spawn(move |_| {
                    let result = work.exec(&work_context);
                    if let Err(e) = send.send((id.clone(), result)) {
                        error!("Unable to write {:?} to completion channel: {}", id, e);
                    }
                })
            }

            // Block for things to phone home to say they are done
            // Then complete everything that has reported since our last check
            let mut opt_complete = Some(recv.recv().unwrap()); // blocks
            while let Some((completed_id, result)) = opt_complete.take() {
                if !match result {
                    Ok(..) => success.insert(completed_id.clone()),
                    Err(..) => error.insert(completed_id.clone()),
                } {
                    panic!("Repeat signals for completion of {:#?}", completed_id);
                }
                info!(
                    "{}/{} complete, most recently {:?}",
                    error.len() + success.len(),
                    pending_jobs.len(),
                    completed_id
                );

                // See if anything else is complete in case things come in waves
                if let Ok(completed_id) = recv.try_recv() {
                    opt_complete = Some(completed_id);
                }
            }
        }
    });

    // Did we win?
    if !error.is_empty() {
        return Err(Error::TasksFailed);
    }
    if success.len() != job_count {
        panic!("No error but not everything succeeded?!");
    }
    finish_successfully(change_detector)?;
    Ok(())
}

#[cfg(test)]
mod tests {

    use std::{
        collections::{HashMap, HashSet},
        fs,
        path::{Path, PathBuf},
    };

    use filetime::FileTime;
    use fontir::{orchestration::Context, source::Paths, stateset::StateSet};
    use tempfile::tempdir;

    use crate::{
        add_glyph_ir_jobs, add_static_metadata_ir_job, finish_successfully, glyphs_changed,
        glyphs_deleted, init, launchable, require_dir, Args, ChangeDetector, Config,
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
        let result = TestCompile::new(&change_detector);

        let mut jobs = HashMap::new();

        add_static_metadata_ir_job(&mut change_detector, &mut jobs).unwrap();
        add_glyph_ir_jobs(&mut change_detector, &mut jobs).unwrap();

        // Try to do the work
        // As we currently don't stress dependencies just run one by one
        // This will likely need to change when we start doing things like glyphs with components
        let root_context = Context::new_root(
            config.args.emit_ir,
            paths,
            change_detector.current_inputs.clone(),
        );
        let mut complete = HashSet::new();
        while !jobs.is_empty() {
            let launchable = launchable(&jobs, &complete);
            assert!(
                !launchable.is_empty(),
                "Unable to make forward progress, bad graph?"
            );
            let id = &launchable[0];
            let job = jobs.remove(id).unwrap();
            job.work.exec(&root_context).unwrap();
            assert!(
                complete.insert(id.clone()),
                "We just did {:?} a second time?",
                id
            );
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
