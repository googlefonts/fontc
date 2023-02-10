use std::{
    collections::{HashMap, HashSet},
    ffi::OsStr,
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
    sync::Arc,
};

use clap::Parser;
use crossbeam_channel::{Receiver, TryRecvError};
use fontbe::{
    features::FeatureWork,
    glyphs::{create_glyph_merge_work, create_glyph_work},
    orchestration::{AnyWorkId, Context as BeContext, WorkId as BeWorkIdentifier},
    paths::Paths as BePaths,
};
use fontc::{
    work::{AnyContext, AnyWork, AnyWorkError},
    Error,
};
use fontdrasil::{
    orchestration::{access_none, access_one, AccessFn},
    types::GlyphName,
};
use fontir::{
    glyph::create_finalize_static_metadata_work,
    orchestration::{Context as FeContext, Flags, WorkId as FeWorkIdentifier},
    paths::Paths as IrPaths,
    source::{DeleteWork, Input, Source},
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

    /// Whether to write additional debug files to disk.
    #[arg(long)]
    #[clap(default_value = "false")]
    emit_debug: bool,

    /// Whether to Try Hard(tm) to match fontmake (Python) behavior in cases where there are other options.
    ///
    /// See https://github.com/googlefonts/fontmake-rs/pull/123 for an example of
    /// where this matters.
    #[arg(long)]
    #[clap(default_value = "true")]
    match_legacy: bool,

    /// Working directory for the build process. If emit-ir is on, written here.
    #[arg(short, long)]
    #[clap(default_value = "build")]
    build_dir: PathBuf,
}

impl Args {
    fn flags(&self) -> Flags {
        let mut flags = Flags::default();

        flags.set(Flags::EMIT_IR, self.emit_ir);
        flags.set(Flags::EMIT_DEBUG, self.emit_debug);
        flags.set(Flags::MATCH_LEGACY, self.match_legacy);

        flags
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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

    fn file(&self) -> PathBuf {
        self.args.build_dir.join("fontc.yml")
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
        panic!("{dir:#?} is taken by something that isn't a directory");
    }
    if !dir.exists() {
        fs::create_dir(dir)?
    }
    debug!("require_dir {:?}", dir);
    Ok(dir.to_path_buf())
}

fn init(config: &Config) -> Result<Input, io::Error> {
    let config_file = config.file();

    let ir_paths = IrPaths::new(&config.args.build_dir);
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
        "designspace" => Ok(Box::new(DesignSpaceIrSource::new(source.to_path_buf()))),
        "glyphs" => Ok(Box::new(GlyphsIrSource::new(source.to_path_buf()))),
        _ => Err(Error::UnrecognizedSource(source.to_path_buf())),
    }
}

fn finish_successfully(context: ChangeDetector) -> Result<(), Error> {
    let current_sources =
        serde_yaml::to_string(&context.current_inputs).map_err(Error::YamlSerError)?;
    fs::write(context.ir_paths.ir_input_file(), current_sources).map_err(Error::IoError)
}

impl ChangeDetector {
    fn static_metadata_ir_change(&self) -> bool {
        self.current_inputs.static_metadata != self.prev_inputs.static_metadata
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::InitStaticMetadata)
                .is_file()
    }

    fn feature_ir_change(&self) -> bool {
        self.static_metadata_ir_change()
            || self.current_inputs.features != self.prev_inputs.features
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::Features)
                .is_file()
    }

    fn feature_be_change(&self) -> bool {
        self.feature_ir_change()
            || !self
                .be_paths
                .target_file(&BeWorkIdentifier::Features)
                .is_file()
    }

    fn glyphs_changed(&self) -> IndexSet<GlyphName> {
        if self.static_metadata_ir_change() {
            return self.current_inputs.glyphs.keys().cloned().collect();
        }
        self.current_inputs
            .glyphs
            .iter()
            .filter_map(
                |(glyph_name, curr_state)| match self.prev_inputs.glyphs.get(glyph_name) {
                    Some(prev_state) => {
                        // If the input changed or the output doesn't exist a rebuild is probably in order
                        (prev_state != curr_state
                            || !self
                                .ir_paths
                                .target_file(&FeWorkIdentifier::Glyph(glyph_name.clone()))
                                .exists())
                        .then_some(glyph_name)
                    }
                    None => Some(glyph_name),
                },
            )
            .cloned()
            .collect()
    }

    fn glyphs_deleted(&self) -> IndexSet<GlyphName> {
        self.prev_inputs
            .glyphs
            .keys()
            .filter(|glyph_name| !self.current_inputs.glyphs.contains_key(glyph_name))
            .cloned()
            .collect()
    }
}

fn add_init_static_metadata_ir_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.static_metadata_ir_change() {
        let id: AnyWorkId = FeWorkIdentifier::InitStaticMetadata.into();
        let write_access = access_one(id.clone());
        workload.insert(
            id,
            Job {
                work: change_detector
                    .ir_source
                    .create_static_metadata_work(&change_detector.current_inputs)?
                    .into(),
                dependencies: HashSet::new(),
                write_access,
            },
        );
    } else {
        workload.mark_success(FeWorkIdentifier::InitStaticMetadata);
    }
    Ok(())
}

fn add_finalize_static_metadata_ir_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.static_metadata_ir_change() {
        let mut dependencies: HashSet<_> = change_detector
            .glyphs_changed()
            .iter()
            .map(|gn| FeWorkIdentifier::Glyph(gn.clone()).into())
            .collect();
        dependencies.insert(FeWorkIdentifier::InitStaticMetadata.into());

        // Grant write to any glyph including ones we've never seen before so job can create them
        let write_access = Arc::new(|an_id: &AnyWorkId| {
            matches!(
                an_id,
                AnyWorkId::Fe(FeWorkIdentifier::Glyph(..))
                    | AnyWorkId::Fe(FeWorkIdentifier::FinalizeStaticMetadata)
            )
        });
        workload.insert(
            FeWorkIdentifier::FinalizeStaticMetadata.into(),
            Job {
                work: create_finalize_static_metadata_work().into(),
                dependencies,
                write_access,
            },
        );
    } else {
        workload.mark_success(FeWorkIdentifier::FinalizeStaticMetadata);
    }
    Ok(())
}

fn add_feature_ir_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.feature_ir_change() {
        let id: AnyWorkId = FeWorkIdentifier::Features.into();
        let write_access = access_one(id.clone());
        workload.insert(
            id,
            Job {
                work: change_detector
                    .ir_source
                    .create_feature_ir_work(&change_detector.current_inputs)?
                    .into(),
                dependencies: HashSet::new(),
                write_access,
            },
        );
    } else {
        workload.mark_success(FeWorkIdentifier::Features);
    }
    Ok(())
}

fn add_feature_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.feature_be_change() {
        let id: AnyWorkId = BeWorkIdentifier::Features.into();
        let write_access = access_one(id.clone());
        workload.insert(
            id,
            Job {
                work: FeatureWork::create().into(),
                dependencies: HashSet::from([
                    FeWorkIdentifier::FinalizeStaticMetadata.into(),
                    FeWorkIdentifier::Features.into(),
                ]),
                write_access,
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Features);
    }
    Ok(())
}

fn add_glyph_ir_jobs(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    // Destroy IR for deleted glyphs. No dependencies.
    for glyph_name in change_detector.glyphs_deleted().iter() {
        let id = FeWorkIdentifier::Glyph(glyph_name.clone());
        let path = change_detector.ir_paths.target_file(&id);
        workload.insert(
            id.into(),
            Job {
                work: DeleteWork::create(path).into(),
                dependencies: HashSet::new(),
                write_access: access_none(),
            },
        );
    }

    // Generate IR for changed glyphs
    let glyphs_changed = change_detector.glyphs_changed();
    let glyph_work = change_detector
        .ir_source
        .create_glyph_ir_work(&glyphs_changed, &change_detector.current_inputs)?;
    for (glyph_name, work) in glyphs_changed.iter().zip(glyph_work) {
        let id = FeWorkIdentifier::Glyph(glyph_name.clone());
        let work = work.into();
        let dependencies = HashSet::from([FeWorkIdentifier::InitStaticMetadata.into()]);

        let id: AnyWorkId = id.into();
        let write_access = access_one(id.clone());
        workload.insert(
            id,
            Job {
                work,
                dependencies,
                write_access,
            },
        );
    }

    Ok(())
}

fn add_glyph_merge_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    let glyphs_changed = change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot of merging to do
    if !glyphs_changed.is_empty() {
        let mut dependencies: HashSet<_> = glyphs_changed
            .iter()
            .map(|gn| BeWorkIdentifier::Glyph(gn.clone()).into())
            .collect();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::GlyphMerge.into();
        let write_access = access_one(id.clone());
        workload.insert(
            id,
            Job {
                work: create_glyph_merge_work().into(),
                dependencies,
                write_access,
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::GlyphMerge.into());
    }
    Ok(())
}

fn add_glyph_be_job(workload: &mut Workload, fe_root: &FeContext, glyph_name: GlyphName) {
    let glyph_ir = fe_root.get_glyph_ir(&glyph_name);

    // To build a glyph we need it's components, plus static metadata
    let mut dependencies: HashSet<_> = glyph_ir
        .sources
        .values()
        .flat_map(|s| &s.components)
        .map(|c| AnyWorkId::Fe(FeWorkIdentifier::Glyph(c.base.clone())))
        .collect();
    dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

    let id = AnyWorkId::Be(BeWorkIdentifier::Glyph(glyph_name.clone()));

    // this job should already be a dependency of the glyph merge; if not terrible things will happen
    if !workload.is_dependency(&BeWorkIdentifier::GlyphMerge.into(), &id) {
        panic!("BE glyph '{glyph_name}' is being built but not participating in glyph merge",);
    }

    let write_access = access_one(id.clone());
    workload.insert(
        id,
        Job {
            work: create_glyph_work(glyph_name).into(),
            dependencies,
            write_access,
        },
    );
}

struct ChangeDetector {
    ir_paths: IrPaths,
    ir_source: Box<dyn Source>,
    prev_inputs: Input,
    current_inputs: Input,
    be_paths: BePaths,
}

impl ChangeDetector {
    fn new(config: Config, ir_paths: IrPaths, prev_inputs: Input) -> Result<ChangeDetector, Error> {
        // What sources are we dealing with?
        let mut ir_source = ir_source(&config.args.source)?;
        let current_inputs = ir_source.inputs().map_err(Error::FontIrError)?;
        let be_paths = BePaths::new(ir_paths.build_dir());

        Ok(ChangeDetector {
            ir_paths,
            ir_source,
            prev_inputs,
            current_inputs,
            be_paths,
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
    success: HashSet<AnyWorkId>,
    error: Vec<(AnyWorkId, String)>,
    jobs_pending: HashMap<AnyWorkId, Job>,
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

    fn insert(&mut self, id: AnyWorkId, job: Job) {
        self.jobs_pending.insert(id, job);
        self.job_count += 1;
    }

    fn is_dependency(&mut self, id: &AnyWorkId, dep: &AnyWorkId) -> bool {
        self.jobs_pending
            .get(id)
            .map(|job| job.dependencies.contains(dep))
            .unwrap_or(false)
    }

    fn mark_success(&mut self, id: impl Into<AnyWorkId>) {
        if self.success.insert(id.into()) {
            self.pre_success += 1;
        }
    }
}

impl Workload {
    fn launchable(&self) -> Vec<AnyWorkId> {
        self.jobs_pending
            .iter()
            .filter_map(|(id, job)| {
                let can_start = job.dependencies.is_subset(&self.success);
                if !can_start && log_enabled!(log::Level::Trace) {
                    let mut unfulfilled_deps: Vec<_> = job
                        .dependencies
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

    fn exec(mut self, fe_root: FeContext, be_root: BeContext) -> Result<(), Error> {
        // Async work will send us it's ID on completion
        let (send, recv) = crossbeam_channel::unbounded::<(AnyWorkId, Result<(), AnyWorkError>)>();

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
                    let work_context = AnyContext::for_work(
                        &fe_root,
                        &be_root,
                        &id,
                        job.dependencies,
                        job.write_access,
                    );

                    let send = send.clone();
                    scope.spawn(move |_| {
                        let result = work.exec(work_context);
                        if let Err(e) = send.send((id.clone(), result)) {
                            error!("Unable to write {:?} to completion channel: {}", id, e);
                        }
                    })
                }

                // Block for things to phone home to say they are done
                // Then complete everything that has reported since our last check
                let successes = self.read_completions(&recv, RecvType::Blocking)?;

                // When a glyph finishes register BE work for it
                for success in successes {
                    if let AnyWorkId::Fe(FeWorkIdentifier::Glyph(glyph_name)) = success {
                        add_glyph_be_job(&mut self, &fe_root, glyph_name);
                    }
                }
            }
            Ok::<(), Error>(())
        })?;

        // If ^ exited due to error the scope awaited any live tasks; capture their results
        self.read_completions(&recv, RecvType::NonBlocking)?;

        if self.error.is_empty() && self.success.len() != self.job_count + self.pre_success {
            panic!(
                "No errors but only {}/{} succeeded?!",
                self.success.len(),
                self.job_count + self.pre_success
            );
        }

        Ok(())
    }

    fn read_completions(
        &mut self,
        recv: &Receiver<(AnyWorkId, Result<(), AnyWorkError>)>,
        initial_read: RecvType,
    ) -> Result<Vec<AnyWorkId>, Error> {
        let mut successes = Vec::new();
        let mut opt_complete = match initial_read {
            RecvType::Blocking => match recv.recv() {
                Ok(completed) => Some(completed),
                Err(e) => panic!("Blocking read failed: {e}"),
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
                Ok(..) => {
                    let inserted = self.success.insert(completed_id.clone());
                    if inserted {
                        successes.push(completed_id.clone());
                    }
                    inserted
                }
                Err(e) => {
                    error!("{:?} failed {}", completed_id, e);
                    self.error.push((completed_id.clone(), format!("{e}")));
                    true
                }
            } {
                panic!("Repeat signals for completion of {completed_id:#?}");
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
        Ok(successes)
    }
}

/// A unit of executable work plus the identifiers of work that it depends on
struct Job {
    // The actual task
    work: AnyWork,
    // Things that must happen before we execute. Our  task can read these things.
    dependencies: HashSet<AnyWorkId>,
    // Things our job needs write access to
    write_access: AccessFn<AnyWorkId>,
}

fn create_workload(change_detector: &mut ChangeDetector) -> Result<Workload, Error> {
    let mut workload = Workload::new();

    // FE: f(source) => IR
    add_init_static_metadata_ir_job(change_detector, &mut workload)?;
    add_feature_ir_job(change_detector, &mut workload)?;
    add_glyph_ir_jobs(change_detector, &mut workload)?;
    add_finalize_static_metadata_ir_job(change_detector, &mut workload)?;

    // BE: f(IR) => binary
    add_feature_be_job(change_detector, &mut workload)?;
    add_glyph_merge_be_job(change_detector, &mut workload)?;

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
    let ir_paths = IrPaths::new(&args.build_dir);
    let be_paths = BePaths::new(&args.build_dir);
    require_dir(ir_paths.build_dir())?;
    require_dir(ir_paths.glyph_ir_dir())?;
    // It's confusing to have leftover debug files
    if be_paths.debug_dir().is_dir() {
        fs::remove_dir_all(be_paths.debug_dir()).map_err(Error::IoError)?;
    }
    require_dir(be_paths.debug_dir())?;
    require_dir(be_paths.glyph_dir())?;
    let config = Config::new(args)?;
    let prev_inputs = init(&config).map_err(Error::IoError)?;

    let mut change_detector = ChangeDetector::new(config.clone(), ir_paths.clone(), prev_inputs)?;
    let workload = create_workload(&mut change_detector)?;

    let fe_root = FeContext::new_root(
        config.args.flags(),
        ir_paths,
        change_detector.current_inputs.clone(),
    );
    let be_root = BeContext::new_root(config.args.flags(), be_paths, &fe_root);
    workload.exec(fe_root, be_root)?;

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
    use fontbe::{
        orchestration::{AnyWorkId, Context as BeContext, WorkId as BeWorkIdentifier},
        paths::Paths as BePaths,
    };
    use fontc::work::AnyContext;
    use fontdrasil::types::GlyphName;
    use fontir::{
        ir::GlyphInstance,
        orchestration::{Context as FeContext, WorkId as FeWorkIdentifier},
        paths::Paths as IrPaths,
        stateset::StateSet,
    };
    use indexmap::IndexSet;
    use tempfile::tempdir;

    use crate::{
        add_feature_be_job, add_feature_ir_job, add_finalize_static_metadata_ir_job,
        add_glyph_be_job, add_glyph_ir_jobs, add_glyph_merge_be_job,
        add_init_static_metadata_ir_job, finish_successfully, init, require_dir, Args,
        ChangeDetector, Config, Workload,
    };

    fn testdata_dir() -> PathBuf {
        let path = PathBuf::from("../resources/testdata")
            .canonicalize()
            .unwrap();
        assert!(path.is_dir(), "{path:#?} isn't a dir");
        path
    }

    fn test_args(build_dir: &Path, source: &str) -> Args {
        Args {
            source: testdata_dir().join(source),
            emit_ir: true,
            emit_debug: false,
            build_dir: build_dir.to_path_buf(),
            match_legacy: true,
        }
    }

    struct TestCompile {
        work_completed: HashSet<AnyWorkId>,
        glyphs_changed: IndexSet<GlyphName>,
        glyphs_deleted: IndexSet<GlyphName>,
        fe_context: FeContext,
        _be_context: BeContext,
    }

    impl TestCompile {
        fn new(
            change_detector: &ChangeDetector,
            fe_context: FeContext,
            be_context: BeContext,
        ) -> TestCompile {
            TestCompile {
                work_completed: HashSet::new(),
                glyphs_changed: change_detector.glyphs_changed(),
                glyphs_deleted: change_detector.glyphs_deleted(),
                fe_context,
                _be_context: be_context,
            }
        }
    }

    #[test]
    fn init_captures_state() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let args = test_args(build_dir, "wght_var.designspace");
        let config = Config::new(args.clone()).unwrap();

        init(&config).unwrap();
        let config_file = config.file();
        let paths = IrPaths::new(build_dir);
        let ir_input_file = paths.ir_input_file();

        assert!(config_file.exists(), "Should exist: {config_file:#?}");
        assert!(
            !ir_input_file.exists(),
            "Should not exist: {ir_input_file:#?}"
        );
        assert!(!Config::new(args).unwrap().has_changed(&config_file));
    }

    #[test]
    fn detect_compiler_change() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let args = test_args(build_dir, "wght_var.designspace");
        let config = Config::new(args.clone()).unwrap();

        let compiler_location = std::env::current_exe().unwrap();
        let metadata = compiler_location.metadata().unwrap();
        let mut compiler = StateSet::new();
        // size +1, I'd give it all up for just a little more
        compiler.set_file_state(
            &compiler_location,
            FileTime::from_system_time(metadata.modified().unwrap()),
            metadata.len() + 1,
        );
        assert!(Config { args, compiler }.has_changed(&config.file()));
    }

    fn compile(args: Args) -> TestCompile {
        let _ = env_logger::builder().is_test(true).try_init();

        let ir_paths = IrPaths::new(&args.build_dir);
        let be_paths = BePaths::new(&args.build_dir);
        require_dir(ir_paths.glyph_ir_dir()).unwrap();
        require_dir(be_paths.glyph_dir()).unwrap();
        let config = Config::new(args).unwrap();

        let prev_inputs = init(&config).unwrap();

        let mut change_detector =
            ChangeDetector::new(config.clone(), ir_paths.clone(), prev_inputs).unwrap();

        let mut workload = Workload::new();

        add_init_static_metadata_ir_job(&mut change_detector, &mut workload).unwrap();
        add_finalize_static_metadata_ir_job(&mut change_detector, &mut workload).unwrap();
        add_glyph_ir_jobs(&mut change_detector, &mut workload).unwrap();
        add_feature_ir_job(&mut change_detector, &mut workload).unwrap();
        add_feature_be_job(&mut change_detector, &mut workload).unwrap();

        add_glyph_merge_be_job(&mut change_detector, &mut workload).unwrap();

        // Try to do the work
        // As we currently don't stress dependencies just run one by one
        // This will likely need to change when we start doing things like glyphs with components

        let fe_root = FeContext::new_root(
            config.args.flags(),
            ir_paths,
            change_detector.current_inputs.clone(),
        );
        let be_root = BeContext::new_root(config.args.flags(), be_paths, &fe_root.read_only());
        let mut result = TestCompile::new(
            &change_detector,
            fe_root.read_only(),
            be_root.copy_read_only(),
        );

        let pre_success = workload.success.clone();
        while !workload.jobs_pending.is_empty() {
            let launchable = workload.launchable();
            if launchable.is_empty() {
                eprintln!("Completed:");
                for id in workload.success.iter() {
                    eprintln!("  {id:?}");
                }
                eprintln!("Unable to proceed with:");
                for (id, job) in workload.jobs_pending.iter() {
                    eprintln!("  {:?}, happens-after {:?}", id, job.dependencies);
                }
                assert!(
                    !launchable.is_empty(),
                    "Unable to make forward progress, bad graph?"
                );
            }

            let id = &launchable[0];
            let job = workload.jobs_pending.remove(id).unwrap();
            let context =
                AnyContext::for_work(&fe_root, &be_root, id, job.dependencies, job.write_access);
            job.work.exec(context).unwrap();
            assert!(
                workload.success.insert(id.clone()),
                "We just did {id:?} a second time?"
            );

            if let AnyWorkId::Fe(FeWorkIdentifier::Glyph(glyph_name)) = id {
                add_glyph_be_job(&mut workload, &fe_root, glyph_name.clone());
            }
        }
        finish_successfully(change_detector).unwrap();
        result.work_completed = workload.success.difference(&pre_success).cloned().collect();
        result
    }

    #[test]
    fn compile_work() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(test_args(build_dir, "wght_var.designspace"));
        assert_eq!(
            HashSet::from([
                FeWorkIdentifier::InitStaticMetadata.into(),
                FeWorkIdentifier::FinalizeStaticMetadata.into(),
                FeWorkIdentifier::Features.into(),
                FeWorkIdentifier::Glyph("bar".into()).into(),
                FeWorkIdentifier::Glyph("plus".into()).into(),
                BeWorkIdentifier::Features.into(),
                BeWorkIdentifier::Glyph("bar".into()).into(),
                BeWorkIdentifier::Glyph("plus".into()).into(),
                BeWorkIdentifier::GlyphMerge.into(),
            ]),
            result.work_completed
        );
    }

    #[test]
    fn second_compile_is_nop() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(test_args(build_dir, "wght_var.designspace"));
        assert_eq!(
            IndexSet::from(["bar".into(), "plus".into()]),
            result.glyphs_changed
        );
        assert_eq!(IndexSet::new(), result.glyphs_deleted);

        let result = compile(test_args(build_dir, "wght_var.designspace"));
        assert_eq!(HashSet::new(), result.work_completed);
        assert_eq!(IndexSet::new(), result.glyphs_changed);
        assert_eq!(IndexSet::new(), result.glyphs_deleted);
    }

    #[test]
    fn second_compile_only_glyph() {
        // glyph depends on static metadata, which isn't going to run
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(test_args(build_dir, "wght_var.designspace"));
        assert!(result.work_completed.len() > 1);

        fs::remove_file(build_dir.join("glyph_ir/bar.yml")).unwrap();

        let result = compile(test_args(build_dir, "wght_var.designspace"));
        assert_eq!(
            HashSet::from([
                FeWorkIdentifier::Glyph("bar".into()).into(),
                BeWorkIdentifier::Glyph("bar".into()).into(),
                BeWorkIdentifier::GlyphMerge.into(),
            ]),
            result.work_completed
        );
    }

    #[test]
    fn deleted_ir_recreated() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(test_args(build_dir, "wght_var.designspace"));
        assert_eq!(
            IndexSet::from(["bar".into(), "plus".into()]),
            result.glyphs_changed
        );
        assert_eq!(IndexSet::new(), result.glyphs_deleted);

        let bar_ir = build_dir.join("glyph_ir/bar.yml");
        assert!(bar_ir.is_file(), "no file {bar_ir:#?}");
        fs::remove_file(bar_ir).unwrap();

        let result = compile(test_args(build_dir, "wght_var.designspace"));
        assert_eq!(IndexSet::from(["bar".into()]), result.glyphs_changed);
        assert_eq!(IndexSet::new(), result.glyphs_deleted);
    }

    #[test]
    fn compile_fea() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(test_args(build_dir, "static.designspace"));
        assert!(
            result
                .work_completed
                .contains(&BeWorkIdentifier::Features.into()),
            "Missing BE feature work in {:?}",
            result.work_completed
        );

        let feature_ttf = build_dir.join("features.ttf");
        assert!(feature_ttf.is_file(), "Should have written {feature_ttf:?}");
    }

    fn build_contour_and_composite_glyph(match_legacy: bool) -> GlyphInstance {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let mut args = test_args(build_dir, "glyphs2/MixedContourComponent.glyphs");
        args.match_legacy = match_legacy; // <-- important :)
        let result = compile(args);

        let glyph = result
            .fe_context
            .get_glyph_ir(&"contour_and_component".into());

        assert_eq!(1, glyph.sources.len());
        glyph.sources.values().next().unwrap().clone()
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_non_legacy_mode() {
        let inst = build_contour_and_composite_glyph(false);
        assert!(inst.contours.is_empty(), "{inst:?}");
        assert_eq!(2, inst.components.len(), "{inst:?}");
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_legacy_mode() {
        let inst = build_contour_and_composite_glyph(true);
        assert!(inst.components.is_empty(), "{inst:?}");
        assert_eq!(2, inst.contours.len(), "{inst:?}");
    }
}
