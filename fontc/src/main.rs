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
    cmap::create_cmap_work,
    features::FeatureWork,
    font::create_font_work,
    glyphs::{create_glyf_loca_work, create_glyph_work},
    head::create_head_work,
    hmetrics::create_hmetric_work,
    maxp::create_maxp_work,
    orchestration::{AnyWorkId, Context as BeContext, WorkId as BeWorkIdentifier},
    paths::Paths as BePaths,
    post::create_post_work,
};

use fontc::{
    work::{AnyContext, AnyWork, AnyWorkError, ReadAccess},
    Args, Config, Error,
};
use fontdrasil::{
    orchestration::{access_none, access_one, AccessFn},
    types::GlyphName,
};
use fontir::{
    glyph::create_finalize_static_metadata_work,
    orchestration::{Context as FeContext, WorkId as FeWorkIdentifier},
    paths::Paths as IrPaths,
    source::{DeleteWork, Input, Source},
};
use glyphs2fontir::source::GlyphsIrSource;
use indexmap::IndexSet;
use log::{debug, error, log_enabled, trace, warn};
use regex::Regex;

use ufo2fontir::source::DesignSpaceIrSource;

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
    fn init_static_metadata_ir_change(&self) -> bool {
        self.current_inputs.static_metadata != self.prev_inputs.static_metadata
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::InitStaticMetadata)
                .is_file()
    }

    fn final_static_metadata_ir_change(&self) -> bool {
        self.current_inputs.static_metadata != self.prev_inputs.static_metadata
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::FinalizeStaticMetadata)
                .is_file()
    }

    fn feature_ir_change(&self) -> bool {
        self.final_static_metadata_ir_change()
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

    fn post_be_change(&self) -> bool {
        self.final_static_metadata_ir_change()
            || !self.be_paths.target_file(&BeWorkIdentifier::Post).is_file()
    }

    fn glyphs_changed(&self) -> IndexSet<GlyphName> {
        let glyph_iter = self.current_inputs.glyphs.iter();

        if self.init_static_metadata_ir_change() {
            return glyph_iter.map(|(name, _)| name).cloned().collect();
        }
        glyph_iter
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
    if change_detector.init_static_metadata_ir_change() {
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
                read_access: ReadAccess::Dependencies,
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
    if change_detector.final_static_metadata_ir_change() {
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
                read_access: ReadAccess::Dependencies,
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
                read_access: ReadAccess::Dependencies,
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
    if change_detector.feature_be_change() && change_detector.glyph_name_filter.is_none() {
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
                read_access: ReadAccess::Dependencies,
                write_access,
            },
        );
    } else {
        // Features are extremely prone to not making sense when glyphs are filtered
        if change_detector.glyph_name_filter.is_some() {
            warn!("Not processing BE Features because a glyph name filter is active");
        }
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
                read_access: ReadAccess::Dependencies,
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
                read_access: ReadAccess::Dependencies,
                write_access,
            },
        );
    }

    Ok(())
}

fn add_glyf_loca_be_job(
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

        // Write the merged glyphs and write individual glyphs that are updated, such as composites with bboxes
        let write_access: AccessFn<_> = Arc::new(|id| {
            matches!(
                id,
                AnyWorkId::Be(BeWorkIdentifier::Glyf)
                    | AnyWorkId::Be(BeWorkIdentifier::Loca)
                    | AnyWorkId::Be(BeWorkIdentifier::Glyph(..))
            )
        });
        let id: AnyWorkId = BeWorkIdentifier::Glyf.into();
        workload.insert(
            id,
            Job {
                work: create_glyf_loca_work().into(),
                dependencies,
                // We need to read all glyphs, even unchanged ones, plus static metadata
                read_access: ReadAccess::Custom(Arc::new(|id| {
                    matches!(
                        id,
                        AnyWorkId::Be(BeWorkIdentifier::Glyf)
                            | AnyWorkId::Be(BeWorkIdentifier::Loca)
                            | AnyWorkId::Be(BeWorkIdentifier::Glyph(..))
                    )
                })),
                write_access,
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Glyf);
        workload.mark_success(BeWorkIdentifier::Loca);
    }
    Ok(())
}

fn add_cmap_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    let glyphs_changed = change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot of merging to do
    if !glyphs_changed.is_empty() {
        let mut dependencies: HashSet<_> = glyphs_changed
            .iter()
            .map(|gn| FeWorkIdentifier::Glyph(gn.clone()).into())
            .collect();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Cmap.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_cmap_work().into(),
                dependencies,
                // We need to read all glyph IR, even unchanged ones, plus static metadata
                read_access: ReadAccess::Custom(Arc::new(|id| {
                    matches!(id, AnyWorkId::Fe(FeWorkIdentifier::Glyph(..)))
                })),
                write_access: access_one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Cmap);
    }
    Ok(())
}

fn add_post_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.post_be_change() {
        let dependencies = HashSet::new();
        // No deps for now because it doesn't actually *do* anything :)

        let id: AnyWorkId = BeWorkIdentifier::Post.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_post_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: access_one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Post);
    }
    Ok(())
}

fn add_head_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.final_static_metadata_ir_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Head.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_head_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: access_one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Head);
    }
    Ok(())
}

fn add_maxp_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    let glyphs_changed = change_detector.glyphs_changed();
    if !glyphs_changed.is_empty() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Maxp.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_maxp_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: access_one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Maxp);
    }
    Ok(())
}

fn add_hmetrics_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    let glyphs_changed = change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot to do
    if !glyphs_changed.is_empty() {
        let mut dependencies: HashSet<_> = glyphs_changed
            .iter()
            .flat_map(|gn| {
                [
                    FeWorkIdentifier::Glyph(gn.clone()).into(),
                    BeWorkIdentifier::Glyph(gn.clone()).into(),
                ]
            })
            .collect();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Hmtx.into();
        workload.insert(
            id,
            Job {
                work: create_hmetric_work().into(),
                dependencies,
                // We need to read all FE and BE glyphs, even unchanged ones, plus static metadata
                read_access: ReadAccess::Custom(Arc::new(|id| {
                    matches!(
                        id,
                        AnyWorkId::Fe(FeWorkIdentifier::Glyph(..))
                            | AnyWorkId::Be(BeWorkIdentifier::Glyph(..))
                    )
                })),
                write_access: Arc::new(|id| {
                    matches!(
                        id,
                        AnyWorkId::Be(BeWorkIdentifier::Hmtx)
                            | AnyWorkId::Be(BeWorkIdentifier::Hhea)
                    )
                }),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Hmtx);
    }
    Ok(())
}

fn add_font_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    let glyphs_changed = change_detector.glyphs_changed();

    // If glyphs or features changed we better do the thing
    if !glyphs_changed.is_empty() || change_detector.feature_be_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());
        dependencies.insert(BeWorkIdentifier::Features.into());
        dependencies.insert(BeWorkIdentifier::Cmap.into());
        dependencies.insert(BeWorkIdentifier::Glyf.into());
        dependencies.insert(BeWorkIdentifier::Head.into());
        dependencies.insert(BeWorkIdentifier::Hhea.into());
        dependencies.insert(BeWorkIdentifier::Hmtx.into());
        dependencies.insert(BeWorkIdentifier::Loca.into());
        dependencies.insert(BeWorkIdentifier::Maxp.into());

        let id: AnyWorkId = BeWorkIdentifier::Font.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_font_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: access_one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Font);
    }
    Ok(())
}

fn add_glyph_be_job(workload: &mut Workload, fe_root: &FeContext, glyph_name: GlyphName) {
    let glyph_ir = fe_root.get_glyph_ir(&glyph_name);

    // To build a glyph we need it's components, plus static metadata
    let mut dependencies: HashSet<_> = glyph_ir
        .sources()
        .values()
        .flat_map(|s| &s.components)
        .map(|c| AnyWorkId::Fe(FeWorkIdentifier::Glyph(c.base.clone())))
        .collect();
    dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

    let id = AnyWorkId::Be(BeWorkIdentifier::Glyph(glyph_name.clone()));

    // this job should already be a dependency of glyf and hmtx; if not terrible things will happen
    if !workload.is_dependency(&BeWorkIdentifier::Glyf.into(), &id) {
        panic!("BE glyph '{glyph_name}' is being built but not participating in glyf",);
    }
    if !workload.is_dependency(&BeWorkIdentifier::Hmtx.into(), &id) {
        panic!("BE glyph '{glyph_name}' is being built but not participating in hmtx",);
    }

    let write_access = access_one(id.clone());
    workload.insert(
        id,
        Job {
            work: create_glyph_work(glyph_name).into(),
            dependencies,
            read_access: ReadAccess::Dependencies,
            write_access,
        },
    );
}

struct ChangeDetector {
    glyph_name_filter: Option<Regex>,
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
        let mut current_inputs = ir_source.inputs().map_err(Error::FontIrError)?;
        let be_paths = BePaths::new(ir_paths.build_dir());

        let glyph_name_filter = config
            .args
            .glyph_name_filter
            .map(|raw_filter| Regex::new(&raw_filter))
            .transpose()
            .map_err(Error::BadRegex)?;

        if let Some(regex) = &glyph_name_filter {
            current_inputs.glyphs.retain(|glyph_name, _| {
                let result = regex.is_match(glyph_name.as_str());
                if !result {
                    trace!("'{glyph_name}' does not match --glyph_name_filter");
                }
                result
            });
        }

        Ok(ChangeDetector {
            glyph_name_filter,
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
    fn handle_success(&mut self, fe_root: &FeContext, success: AnyWorkId) {
        debug!("{success:?} successful");
        match success {
            // When a glyph finishes IR, register BE work for it
            AnyWorkId::Fe(FeWorkIdentifier::Glyph(glyph_name)) => {
                add_glyph_be_job(self, fe_root, glyph_name)
            }

            // When static metadata finalizes, add BE work for any new glyphs
            // If anything progresses before we've done this we have a graph bug
            AnyWorkId::Fe(FeWorkIdentifier::FinalizeStaticMetadata) => {
                for glyph_name in fe_root.get_final_static_metadata().glyph_order.iter() {
                    let id = AnyWorkId::Be(BeWorkIdentifier::Glyph(glyph_name.clone()));
                    if self.jobs_pending.contains_key(&id) {
                        continue;
                    }

                    debug!("Updating graph for new glyph {glyph_name}");

                    // It would be lovely if our new glyph was in glyf and hmtx
                    // loca hides with glyf
                    for merge_id in [BeWorkIdentifier::Glyf, BeWorkIdentifier::Hmtx] {
                        self.jobs_pending
                            .get_mut(&AnyWorkId::Be(merge_id))
                            .unwrap()
                            .dependencies
                            .insert(id.clone());
                    }

                    add_glyph_be_job(self, fe_root, glyph_name.clone());
                }
            }

            // Glyf carries Loca along for the ride
            AnyWorkId::Be(BeWorkIdentifier::Glyf) => {
                self.mark_success(AnyWorkId::Be(BeWorkIdentifier::Loca))
            }

            // Hmtx carries hhea along for the ride
            AnyWorkId::Be(BeWorkIdentifier::Hmtx) => {
                self.mark_success(AnyWorkId::Be(BeWorkIdentifier::Hhea))
            }
            _ => (),
        }
    }

    fn launchable(&self) -> Vec<AnyWorkId> {
        self.jobs_pending
            .iter()
            .filter_map(|(id, job)| {
                let can_start = job.dependencies.is_subset(&self.success);
                if !can_start && log_enabled!(log::Level::Trace) {
                    let mut unfulfilled_deps: Vec<_> =
                        job.dependencies.difference(&self.success).collect();
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
                        job.read_access,
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
                successes
                    .into_iter()
                    .for_each(|s| self.handle_success(&fe_root, s));
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
                    error!("{:?} failed {:?}", completed_id, e);
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
    // Things our job needs read access to. Usually all our dependencies.
    read_access: ReadAccess,
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
    add_glyf_loca_be_job(change_detector, &mut workload)?;
    add_cmap_be_job(change_detector, &mut workload)?;
    add_head_be_job(change_detector, &mut workload)?;
    add_hmetrics_job(change_detector, &mut workload)?;
    add_maxp_be_job(change_detector, &mut workload)?;
    add_post_be_job(change_detector, &mut workload)?;

    // Make a damn font
    add_font_be_job(change_detector, &mut workload)?;

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
    let prev_inputs = config.init()?;

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
        fs::{self, File},
        io::Read,
        path::{Path, PathBuf},
    };

    use fontbe::{
        orchestration::{AnyWorkId, Context as BeContext, WorkId as BeWorkIdentifier},
        paths::Paths as BePaths,
    };
    use fontc::work::AnyContext;
    use fontdrasil::types::GlyphName;
    use fontir::{
        ir,
        orchestration::{Context as FeContext, WorkId as FeWorkIdentifier},
        paths::Paths as IrPaths,
    };
    use indexmap::IndexSet;
    use log::debug;
    use read_fonts::{
        tables::{
            cmap::{Cmap, CmapSubtable},
            glyf::{self, CompositeGlyph, Glyf, SimpleGlyph},
            hmtx::Hmtx,
            loca::Loca,
        },
        types::{F2Dot14, GlyphId, Tag},
        FontData, FontRead, FontReadWithArgs, FontRef,
    };
    use tempfile::{tempdir, TempDir};
    use write_fonts::dump_table;

    use crate::{
        add_cmap_be_job, add_feature_be_job, add_feature_ir_job,
        add_finalize_static_metadata_ir_job, add_font_be_job, add_glyf_loca_be_job,
        add_glyph_ir_jobs, add_head_be_job, add_hmetrics_job, add_init_static_metadata_ir_job,
        add_maxp_be_job, add_post_be_job, finish_successfully, require_dir, Args, ChangeDetector,
        Config, Workload,
    };

    struct TestCompile {
        build_dir: PathBuf,
        work_completed: HashSet<AnyWorkId>,
        glyphs_changed: IndexSet<GlyphName>,
        glyphs_deleted: IndexSet<GlyphName>,
        fe_context: FeContext,
        be_context: BeContext,
    }

    impl TestCompile {
        fn new(
            change_detector: &ChangeDetector,
            fe_context: FeContext,
            be_context: BeContext,
        ) -> TestCompile {
            TestCompile {
                build_dir: change_detector.be_paths.build_dir().to_path_buf(),
                work_completed: HashSet::new(),
                glyphs_changed: change_detector.glyphs_changed(),
                glyphs_deleted: change_detector.glyphs_deleted(),
                fe_context,
                be_context,
            }
        }

        fn get_glyph_index(&self, name: &str) -> u32 {
            self.fe_context
                .get_final_static_metadata()
                .glyph_id(&name.into())
                .unwrap()
        }

        fn raw_glyf_loca(&self) -> (Vec<u8>, Vec<u8>) {
            (
                read_file(&self.build_dir.join("glyf.table")),
                read_file(&self.build_dir.join("loca.table")),
            )
        }
    }

    fn compile(args: Args) -> TestCompile {
        let _ = env_logger::builder().is_test(true).try_init();

        let ir_paths = IrPaths::new(&args.build_dir);
        let be_paths = BePaths::new(&args.build_dir);
        require_dir(ir_paths.glyph_ir_dir()).unwrap();
        require_dir(be_paths.glyph_dir()).unwrap();
        let config = Config::new(args).unwrap();

        let prev_inputs = config.init().unwrap();

        let mut change_detector =
            ChangeDetector::new(config.clone(), ir_paths.clone(), prev_inputs).unwrap();

        let mut workload = Workload::new();

        add_init_static_metadata_ir_job(&mut change_detector, &mut workload).unwrap();
        add_finalize_static_metadata_ir_job(&mut change_detector, &mut workload).unwrap();
        add_glyph_ir_jobs(&mut change_detector, &mut workload).unwrap();
        add_feature_ir_job(&mut change_detector, &mut workload).unwrap();
        add_feature_be_job(&mut change_detector, &mut workload).unwrap();

        add_glyf_loca_be_job(&mut change_detector, &mut workload).unwrap();
        add_cmap_be_job(&mut change_detector, &mut workload).unwrap();
        add_head_be_job(&mut change_detector, &mut workload).unwrap();
        add_hmetrics_job(&mut change_detector, &mut workload).unwrap();
        add_maxp_be_job(&mut change_detector, &mut workload).unwrap();
        add_post_be_job(&mut change_detector, &mut workload).unwrap();

        add_font_be_job(&mut change_detector, &mut workload).unwrap();

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
            let context = AnyContext::for_work(
                &fe_root,
                &be_root,
                id,
                job.dependencies,
                job.read_access,
                job.write_access,
            );
            debug!("Exec {:?}", id);
            job.work.exec(context).unwrap();
            assert!(
                workload.success.insert(id.clone()),
                "We just did {id:?} a second time?"
            );

            workload.handle_success(&fe_root, id.clone());
        }
        finish_successfully(change_detector).unwrap();
        result.work_completed = workload.success.difference(&pre_success).cloned().collect();
        result
    }

    #[test]
    fn compile_work() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
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
                BeWorkIdentifier::Cmap.into(),
                BeWorkIdentifier::Glyf.into(),
                BeWorkIdentifier::Head.into(),
                BeWorkIdentifier::Hhea.into(),
                BeWorkIdentifier::Hmtx.into(),
                BeWorkIdentifier::Loca.into(),
                BeWorkIdentifier::Maxp.into(),
                BeWorkIdentifier::Post.into(),
                BeWorkIdentifier::Font.into(),
            ]),
            result.work_completed
        );
    }

    #[test]
    fn second_compile_is_nop() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert_eq!(
            IndexSet::from(["bar".into(), "plus".into()]),
            result.glyphs_changed
        );
        assert_eq!(IndexSet::new(), result.glyphs_deleted);

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert_eq!(HashSet::new(), result.work_completed);
        assert_eq!(IndexSet::new(), result.glyphs_changed);
        assert_eq!(IndexSet::new(), result.glyphs_deleted);
    }

    #[test]
    fn second_compile_only_glyph() {
        // glyph depends on static metadata, which isn't going to run
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert!(result.work_completed.len() > 1);

        fs::remove_file(build_dir.join("glyph_ir/bar.yml")).unwrap();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert_eq!(
            HashSet::from([
                FeWorkIdentifier::Glyph("bar".into()).into(),
                BeWorkIdentifier::Glyph("bar".into()).into(),
                BeWorkIdentifier::Cmap.into(),
                BeWorkIdentifier::Glyf.into(),
                BeWorkIdentifier::Hhea.into(),
                BeWorkIdentifier::Hmtx.into(),
                BeWorkIdentifier::Loca.into(),
                BeWorkIdentifier::Maxp.into(),
                BeWorkIdentifier::Font.into(),
            ]),
            result.work_completed
        );
    }

    #[test]
    fn deleted_ir_recreated() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert_eq!(
            IndexSet::from(["bar".into(), "plus".into()]),
            result.glyphs_changed
        );
        assert_eq!(IndexSet::new(), result.glyphs_deleted);

        let bar_ir = build_dir.join("glyph_ir/bar.yml");
        assert!(bar_ir.is_file(), "no file {bar_ir:#?}");
        fs::remove_file(bar_ir).unwrap();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert_eq!(IndexSet::from(["bar".into()]), result.glyphs_changed);
        assert_eq!(IndexSet::new(), result.glyphs_deleted);
    }

    #[test]
    fn compile_fea() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "static.designspace"));
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

    fn build_contour_and_composite_glyph(temp_dir: &TempDir, match_legacy: bool) -> ir::Glyph {
        let build_dir = temp_dir.path();

        let mut args = Args::for_test(build_dir, "glyphs2/MixedContourComponent.glyphs");
        args.match_legacy = match_legacy; // <-- important :)
        let result = compile(args);

        let glyph = result
            .fe_context
            .get_glyph_ir(&"contour_and_component".into());

        assert_eq!(1, glyph.sources().len());
        (*glyph).clone()
    }

    fn read_file(path: &Path) -> Vec<u8> {
        assert!(path.exists(), "{path:?} not found");
        let mut buf = Vec::new();
        File::open(path).unwrap().read_to_end(&mut buf).unwrap();
        buf
    }

    fn glyph_bytes(build_dir: &Path, name: &str) -> Vec<u8> {
        read_file(&build_dir.join(format!("glyphs/{name}.glyph")))
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_non_legacy_mode() {
        let temp_dir = tempdir().unwrap();
        let glyph = build_contour_and_composite_glyph(&temp_dir, false);
        assert!(glyph.default_instance().contours.is_empty(), "{glyph:?}");
        assert_eq!(2, glyph.default_instance().components.len(), "{glyph:?}");

        let raw_glyph = glyph_bytes(temp_dir.path(), glyph.name.as_str());
        let glyph = CompositeGlyph::read(FontData::new(&raw_glyph)).unwrap();
        // -1: composite, per https://learn.microsoft.com/en-us/typography/opentype/spec/glyf
        assert_eq!(-1, glyph.number_of_contours());
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_legacy_mode() {
        let temp_dir = tempdir().unwrap();
        let glyph = build_contour_and_composite_glyph(&temp_dir, true);
        assert!(glyph.default_instance().components.is_empty(), "{glyph:?}");
        assert_eq!(2, glyph.default_instance().contours.len(), "{glyph:?}");

        let raw_glyph = glyph_bytes(temp_dir.path(), glyph.name.as_str());
        let glyph = SimpleGlyph::read(FontData::new(&raw_glyph)).unwrap();
        assert_eq!(2, glyph.number_of_contours());
    }

    #[test]
    fn compile_simple_binary_glyph() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "static.designspace"));

        let raw_glyph = glyph_bytes(build_dir, "bar");
        let glyph = SimpleGlyph::read(FontData::new(&raw_glyph)).unwrap();
        assert_eq!(1, glyph.number_of_contours());
        assert_eq!(4, glyph.num_points());
        assert_eq!(
            [222, -241, 295, 760],
            [glyph.x_min(), glyph.y_min(), glyph.x_max(), glyph.y_max()]
        );
    }

    fn glyphs<'a>(raw_glyf: &'a [u8], raw_loca: &'a [u8]) -> Vec<glyf::Glyph<'a>> {
        let glyf = Glyf::read(FontData::new(raw_glyf)).unwrap();
        let loca = Loca::read_with_args(FontData::new(raw_loca), &true).unwrap();

        (0..loca.len())
            .map(|gid| loca.get_glyf(GlyphId::new(gid as u16), &glyf))
            .map(|r| r.unwrap().unwrap())
            .collect()
    }

    #[test]
    fn compile_simple_glyphs_to_glyf_loca() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "static.designspace"));

        let raw_glyf = read_file(&build_dir.join("glyf.table"));
        let raw_loca = read_file(&build_dir.join("loca.table"));

        // See resources/testdata/Static-Regular.ufo/glyphs
        // bar, 4 points, 1 contour
        // plus, 12 points, 1 contour
        assert_eq!(
            vec![(4, 1), (12, 1)],
            glyphs(&raw_glyf, &raw_loca)
                .iter()
                .map(|g| match g {
                    glyf::Glyph::Simple(glyph) => (glyph.num_points(), glyph.number_of_contours()),
                    glyf::Glyph::Composite(glyph) => (0, glyph.number_of_contours()),
                })
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_composite_glyphs_has_expected_glyph_types() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Component.glyphs"));
        let (raw_glyf, raw_loca) = result.raw_glyf_loca();

        // Per source, glyphs should be period, comma, non_uniform_scale
        // Period is simple, the other two use it as a component
        let glyphs = glyphs(&raw_glyf, &raw_loca);
        assert!(glyphs.len() > 1, "{glyphs:#?}");
        let period_idx = result.get_glyph_index("period");
        assert!(matches!(glyphs[0], glyf::Glyph::Simple(..)), "{glyphs:#?}");
        for (idx, glyph) in glyphs.iter().enumerate() {
            if idx == period_idx.try_into().unwrap() {
                assert!(
                    matches!(glyphs[idx], glyf::Glyph::Simple(..)),
                    "glyphs[{idx}] should be simple\n{glyph:#?}\nAll:\n{glyphs:#?}"
                );
            } else {
                assert!(
                    matches!(glyphs[idx], glyf::Glyph::Composite(..)),
                    "glyphs[{idx}] should be composite\n{glyph:#?}\nAll:\n{glyphs:#?}"
                );
            }
        }
    }

    #[test]
    fn compile_composite_glyphs_to_glyf_loca() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Component.glyphs"));
        let (raw_glyf, raw_loca) = result.raw_glyf_loca();

        // non-uniform scaling of period
        let period_idx = result.get_glyph_index("period");
        let non_uniform_scale_idx = result.get_glyph_index("non_uniform_scale");
        let glyphs = glyphs(&raw_glyf, &raw_loca);
        let glyf::Glyph::Composite(glyph) = &glyphs[non_uniform_scale_idx as usize] else {
            panic!("Expected a composite\n{glyphs:#?}");
        };
        let component = glyph.components().next().unwrap();
        assert_eq!(period_idx, component.glyph.to_u16() as u32);

        // If we all work together we're a real transform!
        assert_component_transform(
            &component,
            [0.84519, 0.58921, -1.16109, 1.66553, -233.0, -129.0],
        );
    }

    fn assert_component_transform(component: &glyf::Component, expected_transform: [f32; 6]) {
        let [xx, yx, xy, yy, dx, dy] = expected_transform;
        assert_eq!(
            glyf::Anchor::Offset {
                x: dx as i16,
                y: dy as i16
            },
            component.anchor
        );
        assert_eq!(
            glyf::Transform {
                xx: F2Dot14::from_f32(xx),
                yx: F2Dot14::from_f32(yx),
                xy: F2Dot14::from_f32(xy),
                yy: F2Dot14::from_f32(yy),
            },
            component.transform
        );
    }

    #[test]
    fn compile_composite_glyphs_to_glyf_loca_applies_transforms() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Component.glyphs"));
        let (raw_glyf, raw_loca) = result.raw_glyf_loca();

        let gid = result.get_glyph_index("simple_transform_again");
        let glyphs = glyphs(&raw_glyf, &raw_loca);
        let glyf::Glyph::Composite(glyph) = &glyphs[gid as usize] else {
            panic!("Expected a composite\n{glyphs:#?}");
        };

        let components: Vec<_> = glyph.components().collect();
        assert_eq!(2, components.len(), "{components:#?}");
        assert_component_transform(&components[0], [2.0, 0.0, 0.0, 1.5, 50.0, 50.0]);
        assert_component_transform(&components[1], [1.5, 0.0, 0.0, 2.0, 50.0, 50.0]);

        // Have ye bbox?
        assert_eq!(
            [425, 125, 800, 250],
            [glyph.x_min(), glyph.y_min(), glyph.x_max(), glyph.y_max()]
        );
    }

    #[test]
    fn writes_cmap() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Component.glyphs"));

        let raw_cmap = dump_table(&*result.be_context.get_cmap()).unwrap();
        let font_data = FontData::new(&raw_cmap);
        let cmap = Cmap::read(font_data).unwrap();

        assert_eq!(1, cmap.encoding_records().len());
        let encoding_record = cmap.encoding_records().first().unwrap();
        let CmapSubtable::Format4(cmap4) = encoding_record.subtable(font_data).unwrap() else {
            panic!("Wrong subtable {encoding_record:#?}");
        };

        // Hopefully we find the codepoints in the .glyphs file, with glyph ids based on order in that file
        let cp_and_gid: Vec<(u16, u16)> = cmap4
            .start_code()
            .iter()
            .zip(cmap4.end_code())
            .zip(cmap4.id_delta())
            .filter_map(|((start, end), id_delta)| {
                let start = start.get();
                let end = end.get();
                if (start, end) == (0xFFFF, 0xFFFF) {
                    return None;
                }
                Some((start, end, id_delta.get()))
            })
            .flat_map(|(start, end, id_delta)| {
                (start..=end).map(move |cp| (cp, (cp as i32 + id_delta as i32).try_into().unwrap()))
            })
            .collect();
        assert_eq!(
            vec![
                (0x002E, 0),
                (0x002C, 1),
                (0x0030, 2),
                (0x031, 3),
                (0x032, 4)
            ],
            cp_and_gid,
            "start {:?}\nend {:?}id_delta {:?}",
            cmap4.start_code(),
            cmap4.end_code(),
            cmap4.id_delta()
        );
    }

    #[test]
    fn hmtx_of_one() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/NotDef.glyphs"));

        let raw_hmtx = result.be_context.get_hmtx();
        let hmtx = Hmtx::read_with_args(FontData::new(raw_hmtx.get()), &(1, 1)).unwrap();
        assert_eq!(
            vec![(600, 250)],
            hmtx.h_metrics()
                .iter()
                .map(|m| (m.advance.get(), m.side_bearing.get()))
                .collect::<Vec<_>>()
        );
        assert!(hmtx.left_side_bearings().is_empty());
    }

    #[test]
    fn hmetrics_of_mono() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Mono.glyphs"));

        let hhea = result.be_context.get_hhea();
        assert_eq!(1, hhea.number_of_long_metrics);
        assert_eq!(175, hhea.min_left_side_bearing.to_i16());
        assert_eq!(50, hhea.min_right_side_bearing.to_i16());
        assert_eq!(375, hhea.x_max_extent.to_i16());
        assert_eq!(425, hhea.advance_width_max.to_u16());

        let maxp = result.be_context.get_maxp();
        assert_eq!(3, maxp.num_glyphs);

        let raw_hmtx = result.be_context.get_hmtx();
        let hmtx = Hmtx::read_with_args(
            FontData::new(raw_hmtx.get()),
            &(hhea.number_of_long_metrics, maxp.num_glyphs),
        )
        .unwrap();
        assert_eq!(
            vec![(425, 175)],
            hmtx.h_metrics()
                .iter()
                .map(|m| (m.advance.get(), m.side_bearing.get()))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            vec![200, 225],
            hmtx.left_side_bearings()
                .iter()
                .map(|m| m.get())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn wght_var_has_expected_tables() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "wght_var.designspace"));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());

        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

        assert_eq!(
            vec![
                Tag::new(b"cmap"),
                Tag::new(b"glyf"),
                Tag::new(b"head"),
                Tag::new(b"hhea"),
                Tag::new(b"hmtx"),
                Tag::new(b"loca"),
                Tag::new(b"maxp"),
            ],
            font.table_directory
                .table_records()
                .iter()
                .map(|tr| tr.tag())
                .collect::<Vec<_>>()
        );
    }
}
