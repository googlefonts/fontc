use std::{
    collections::{HashMap, HashSet},
    ffi::OsStr,
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
};

use clap::Parser;
use crossbeam_channel::{Receiver, TryRecvError};
use fontc::{be, Error};
use fontir::{
    error::WorkError,
    orchestration::{Context, WorkIdentifier},
    paths::Paths as IrPaths,
    source::{DeleteWork, Input, Source, Work},
    stateset::StateSet,
};
use glyphs2fontir::source::GlyphsIrSource;
use indexmap::IndexSet;
use log::{debug, error, info, log_enabled, trace, warn};
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

    /// Working directory for the build process. If emit-ir is on, written here.
    #[arg(short, long)]
    #[clap(default_value = "build")]
    build_dir: PathBuf,
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

    let ir_paths = IrPaths::new(&config.build_dir);
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

impl ChangeDetector {
    fn static_metadata_ir_change(&self) -> bool {
        self.current_inputs.static_metadata != self.prev_inputs.static_metadata
<<<<<<< HEAD
            || !self
                .paths
                .target_file(&WorkIdentifier::StaticMetadata)
                .is_file()
    }

    fn feature_change(&self) -> bool {
        self.static_metadata_change()
            || self.current_inputs.features != self.prev_inputs.features
            || !self.paths.target_file(&WorkIdentifier::FeatureIr).is_file()
=======
            || !self.paths.static_metadata_ir_file().is_file()
    }

    fn feature_ir_change(&self) -> bool {
        self.static_metadata_ir_change()
            || self.current_inputs.features != self.prev_inputs.features
            || !self.paths.feature_ir_file().is_file()
    }

    fn feature_be_change(&self) -> bool {
        self.feature_ir_change() || !self.paths.feature_be_file().is_file()
>>>>>>> 951ffab (Run the fea-rs parser)
    }

    fn glyphs_changed(&self) -> HashSet<&str> {
        if self.static_metadata_ir_change() {
            return self
                .current_inputs
                .glyphs
                .keys()
                .map(|e| e.as_str())
                .collect();
        }
        self.current_inputs
            .glyphs
            .iter()
            .filter(
                |(glyph_name, curr_state)| match self.prev_inputs.glyphs.get(*glyph_name) {
                    Some(prev_state) => {
                        // If the input changed or the output doesn't exist a rebuild is probably in order
                        prev_state != *curr_state
                            || !self
                                .paths
                                .target_file(&WorkIdentifier::GlyphIr(glyph_name.to_string()))
                                .exists()
                    }
                    None => true,
                },
            )
            .map(|(glyph_name, _)| glyph_name.as_str())
            .collect()
    }

    fn glyphs_deleted(&self) -> HashSet<&str> {
        self.prev_inputs
            .glyphs
            .keys()
            .filter(|glyph_name| !self.current_inputs.glyphs.contains_key(glyph_name.as_str()))
            .map(|e| e.as_str())
            .collect()
    }
}

fn add_static_metadata_ir_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.static_metadata_ir_change() {
        workload.insert(
            WorkIdentifier::StaticMetadata,
            Job {
                work: change_detector
                    .ir_source
                    .create_static_metadata_work(&change_detector.current_inputs)?,
                happens_after: HashSet::new(),
            },
        );
    } else {
        workload.mark_success(WorkIdentifier::StaticMetadata);
    }
    Ok(())
}

fn add_feature_ir_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.feature_ir_change() {
        workload.insert(
            WorkIdentifier::FeatureIr,
            Job {
                work: change_detector
                    .ir_source
                    .create_feature_ir_work(&change_detector.current_inputs)?,
                happens_after: HashSet::new(),
            },
        );
    } else {
        workload.mark_success(WorkIdentifier::FeatureIr);
    }
    Ok(())
}

fn add_feature_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.feature_be_change() {
        workload.insert(
            WorkIdentifier::FeatureBe,
            Job {
                work: be::create_feature_work(&change_detector.paths)?,
                happens_after: HashSet::from([
                    WorkIdentifier::StaticMetadata,
                    WorkIdentifier::FeatureIr,
                ]),
            },
        );
    } else {
        workload.mark_success(WorkIdentifier::FeatureBe);
    }
    Ok(())
}

fn add_glyph_ir_jobs(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    // Destroy IR for deleted glyphs. No dependencies.
    for glyph_name in change_detector.glyphs_deleted().iter() {
        let id = WorkIdentifier::GlyphIr(glyph_name.to_string());
        let path = change_detector.paths.target_file(&id);
        workload.insert(
            id,
            Job {
                work: DeleteWork::create(path),
                happens_after: HashSet::new(),
            },
        );
    }

    // Generate IR for changed glyphs
    let glyphs_changed = change_detector.glyphs_changed();
    let glyph_work = change_detector
        .ir_source
        .create_glyph_ir_work(&glyphs_changed, &change_detector.current_inputs)?;
    for (glyph_name, work) in glyphs_changed.iter().zip(glyph_work) {
        let id = WorkIdentifier::GlyphIr(glyph_name.to_string());
        let happens_after = HashSet::from([WorkIdentifier::StaticMetadata]);

        workload.insert(
            id,
            Job {
                work,
                dependencies: happens_after,
            },
        );
    }

    Ok(())
}

struct ChangeDetector {
    paths: IrPaths,
    ir_source: Box<dyn Source>,
    prev_inputs: Input,
    current_inputs: Input,
}

impl ChangeDetector {
    fn new(config: Config, paths: IrPaths, prev_inputs: Input) -> Result<ChangeDetector, Error> {
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

enum RecvType {
    Blocking,
    NonBlocking,
}

/// A set of interdependent jobs to execute.
struct Workload {
    pre_success: usize,
    job_count: usize,
    success: HashSet<WorkIdentifier>,
    error: Vec<(WorkIdentifier, String)>,
    jobs_pending: HashMap<WorkIdentifier, Job>,
}

impl Workload {
    fn new() -> Workload {
        Workload {
            pre_success: 0,
            job_count: 0,
            success: HashSet::new(),
            error: Vec::new(),
            jobs_pending: HashMap::new(),
        }
    }

    fn insert(&mut self, id: WorkIdentifier, job: Job) {
        self.jobs_pending.insert(id, job);
    }

    fn mark_success(&mut self, id: WorkIdentifier) {
        if self.success.insert(id) {
            self.pre_success += 1;
        }
    }
}

impl Workload {
    fn launchable(&self) -> Vec<WorkIdentifier> {
        self.jobs_pending
            .iter()
            .filter_map(|(id, job)| {
                let can_start = job.happens_after.is_subset(&self.success);
                if !can_start && log_enabled!(log::Level::Trace) {
                    let mut unfulfilled_deps: Vec<_> = job
                        .happens_after
                        .difference(&self.success)
                        .into_iter()
                        .collect();
                    unfulfilled_deps.sort();
                    trace!("Cannot start {:?}, blocked on {:?}", id, unfulfilled_deps);
                };
                can_start.then(|| id.clone())
            })
            .collect()
    }

    fn exec(mut self, root_context: Context) -> Result<(), Error> {
        self.job_count = self.jobs_pending.len();

        // Async work will send us it's ID on completion
        let (send, recv) =
            crossbeam_channel::unbounded::<(WorkIdentifier, Result<(), WorkError>)>();

        rayon::in_place_scope(|scope| {
            // Whenever a task completes see if it was the last incomplete dependency of other task(s)
            // and spawn them if it was
            // TODO timeout and die it if takes too long to make forward progress or we're spinning w/o progress
            while self.success.len() < self.job_count + self.pre_success {
                // Spawn anything that is currently executable (has no unfulfilled dependencies)
                for id in self.launchable() {
                    trace!("Start {:?}", id);
                    let job = self.jobs_pending.remove(&id).unwrap();
                    let work = job.work;
                    let work_context =
                        root_context.copy_for_work(id.clone(), Some(job.happens_after));
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
                self.read_completions(&recv, RecvType::Blocking)?;
            }
            Ok::<(), Error>(())
        })?;

        // If ^ exited due to error the scope awaited any live tasks; capture their results
        self.read_completions(&recv, RecvType::NonBlocking)?;

        Ok(())
    }

    fn read_completions(
        &mut self,
        recv: &Receiver<(WorkIdentifier, Result<(), WorkError>)>,
        initial_read: RecvType,
    ) -> Result<(), Error> {
        let mut opt_complete = match initial_read {
            RecvType::Blocking => match recv.recv() {
                Ok(completed) => Some(completed),
                Err(e) => panic!("Blocking read failed: {}", e),
            },
            RecvType::NonBlocking => match recv.try_recv() {
                Ok(completed) => Some(completed),
                Err(TryRecvError::Empty) => None,
                Err(TryRecvError::Disconnected) => {
                    panic!("Channel closed before reading completed")
                }
            },
        };
        while let Some((completed_id, result)) = opt_complete.take() {
            if !match result {
                Ok(..) => self.success.insert(completed_id.clone()),
                Err(e) => {
                    self.error.push((completed_id.clone(), format!("{}", e)));
                    true
                }
            } {
                panic!("Repeat signals for completion of {:#?}", completed_id);
            }
            debug!(
                "{}/{} complete, most recently {:?}",
                self.error.len() + self.success.len() - self.pre_success,
                self.job_count,
                completed_id
            );

            // See if anything else is complete in case things come in waves
            if let Ok(completed_id) = recv.try_recv() {
                opt_complete = Some(completed_id);
            }
        }
        if !self.error.is_empty() {
            return Err(Error::TasksFailed(self.error.clone()));
        }
        Ok(())
    }
}

/// A unit of executable work plus the identifiers of work that it depends on
struct Job {
    // The actual task
    work: Box<dyn Work + Send>,
    // Things that must happen before we execute, probably because we use their output
    dependencies: HashSet<WorkIdentifier>,
}

fn create_workload(change_detector: &mut ChangeDetector) -> Result<Workload, Error> {
    let mut workload = Workload::new();

    // FE: f(source) => IR
    add_static_metadata_ir_job(change_detector, &mut workload)?;
    add_feature_ir_job(change_detector, &mut workload)?;
    add_glyph_ir_jobs(change_detector, &mut workload)?;

    // BE: f(IR) => binary
    add_feature_be_job(change_detector, &mut workload)?;

    Ok(workload)
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

    let args = Args::parse();
    let paths = IrPaths::new(&args.build_dir);
    let build_dir = require_dir(paths.build_dir())?;
    require_dir(paths.glyph_ir_dir())?;
    let config = Config::new(args, build_dir)?;
    let prev_inputs = init(&config).map_err(Error::IoError)?;

    let mut change_detector = ChangeDetector::new(config.clone(), paths.clone(), prev_inputs)?;
    let workload = create_workload(&mut change_detector)?;

    let root_context = Context::new_root(
        config.args.emit_ir,
        paths,
        change_detector.current_inputs.clone(),
    );
    workload.exec(root_context)?;

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
    use fontir::{
        orchestration::{Context, WorkIdentifier},
        paths::Paths as IrPaths,
        stateset::StateSet,
    };
    use tempfile::tempdir;

    use crate::{
        add_feature_ir_job, add_glyph_ir_jobs, add_static_metadata_ir_job, finish_successfully,
        init, require_dir, Args, ChangeDetector, Config, Workload,
    };

    fn testdata_dir() -> PathBuf {
        let path = PathBuf::from("../resources/testdata")
            .canonicalize()
            .unwrap();
        assert!(path.is_dir(), "{:#?} isn't a dir", path);
        path
    }

    fn test_args(source: &str, build_dir: &Path) -> Args {
        Args {
            source: testdata_dir().join(source),
            emit_ir: true,
            build_dir: build_dir.to_path_buf(),
        }
    }

    struct TestCompile {
        work_completed: HashSet<WorkIdentifier>,
        glyphs_changed: HashSet<String>,
        glyphs_deleted: HashSet<String>,
    }

    impl TestCompile {
        fn new(change_detector: &ChangeDetector) -> TestCompile {
            TestCompile {
                work_completed: HashSet::new(),
                glyphs_changed: change_detector
                    .glyphs_changed()
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                glyphs_deleted: change_detector
                    .glyphs_deleted()
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
        let args = test_args("wght_var.designspace", build_dir);
        let config = Config::new(args.clone(), build_dir.to_path_buf()).unwrap();

        init(&config).unwrap();
        let config_file = config.file();
        let paths = IrPaths::new(build_dir);
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
        let args = test_args("wght_var.designspace", build_dir);
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
        let args = test_args(source, build_dir);
        let paths = IrPaths::new(build_dir);
        require_dir(paths.glyph_ir_dir()).unwrap();
        let config = Config::new(args, build_dir.to_path_buf()).unwrap();

        let prev_inputs = init(&config).unwrap();

        let mut change_detector =
            ChangeDetector::new(config.clone(), paths.clone(), prev_inputs).unwrap();
        let mut result = TestCompile::new(&change_detector);

        let mut workload = Workload::new();

        add_static_metadata_ir_job(&mut change_detector, &mut workload).unwrap();
        add_glyph_ir_jobs(&mut change_detector, &mut workload).unwrap();
        add_feature_ir_job(&mut change_detector, &mut workload).unwrap();

        // Try to do the work
        // As we currently don't stress dependencies just run one by one
        // This will likely need to change when we start doing things like glyphs with components
        let root_context = Context::new_root(
            config.args.emit_ir,
            paths,
            change_detector.current_inputs.clone(),
        );
        let pre_success = workload.success.clone();
        while !workload.jobs_pending.is_empty() {
            let launchable = workload.launchable();
            if launchable.is_empty() {
                eprintln!("Completed:");
                for id in workload.success.iter() {
                    eprintln!("  {:?}", id);
                }
                eprintln!("Unable to proceed with:");
                for (id, job) in workload.jobs_pending.iter() {
                    eprintln!("  {:?}, happens-after {:?}", id, job.happens_after);
                }
                assert!(
                    !launchable.is_empty(),
                    "Unable to make forward progress, bad graph?"
                );
            }

            let id = &launchable[0];
            let job = workload.jobs_pending.remove(id).unwrap();
            let context = root_context.copy_for_work(id.clone(), Some(job.happens_after));
            job.work.exec(&context).unwrap();
            assert!(
                workload.success.insert(id.clone()),
                "We just did {:?} a second time?",
                id
            );
        }
        finish_successfully(change_detector).unwrap();
        result.work_completed = workload.success.difference(&pre_success).cloned().collect();
        result
    }

    #[test]
    fn compile_work() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(build_dir, "wght_var.designspace");
        assert_eq!(
            HashSet::from([
                WorkIdentifier::StaticMetadata,
                WorkIdentifier::FeatureIr,
                WorkIdentifier::GlyphIr(String::from("bar")),
                WorkIdentifier::GlyphIr(String::from("plus")),
            ]),
            result.work_completed
        );
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
        assert_eq!(HashSet::new(), result.work_completed);
        assert_eq!(HashSet::from([]), result.glyphs_changed);
        assert_eq!(HashSet::from([]), result.glyphs_deleted);
    }

    #[test]
    fn second_compile_only_glyph() {
        // glyph depends on static metadata, which isn't going to run
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(build_dir, "wght_var.designspace");
        assert!(result.work_completed.len() > 1);

        fs::remove_file(build_dir.join("glyph_ir/bar.yml")).unwrap();

        let result = compile(build_dir, "wght_var.designspace");
        assert_eq!(
            HashSet::from([WorkIdentifier::GlyphIr(String::from("bar")),]),
            result.work_completed
        );
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
