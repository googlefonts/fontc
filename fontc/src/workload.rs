//! Tracking jobs to run

use std::{
    collections::{HashMap, HashSet},
    panic::AssertUnwindSafe,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

use crossbeam_channel::{Receiver, TryRecvError};
use fontbe::{
    avar::create_avar_work,
    cmap::create_cmap_work,
    colr::create_colr_work,
    cpal::create_cpal_work,
    features::{
        create_gather_ir_kerning_work, create_kern_segment_work, create_kerns_work,
        create_mark_work, FeatureCompilationWork, FeatureFirstPassWork,
    },
    font::create_font_work,
    fvar::create_fvar_work,
    gasp::create_gasp_work,
    glyphs::{create_glyf_loca_work, create_glyf_work},
    gvar::create_gvar_work,
    head::create_head_work,
    hvar::create_hvar_work,
    meta::create_meta_work,
    metrics_and_limits::create_metric_and_limit_work,
    mvar::create_mvar_work,
    name::create_name_work,
    orchestration::{AnyWorkId, Context as BeContext, WorkId as BeWorkIdentifier},
    os2::create_os2_work,
    post::create_post_work,
    stat::create_stat_work,
    vertical_metrics::create_vertical_metrics_work,
};
use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, AccessType, Identifier, IdentifierDiscriminant},
    types::GlyphName,
};
use fontir::{
    glyph::create_glyph_order_work,
    orchestration::{Context as FeContext, WorkId as FeWorkIdentifier},
    source::Source,
};
use log::{debug, trace, warn};

use crate::{
    timing::{JobTime, JobTimer},
    work::{AnyAccess, AnyContext, AnyWork},
    Error, Input,
};

/// A set of interdependent jobs to execute.
pub struct Workload {
    source: Box<dyn Source>,
    job_count: usize,
    success: HashSet<AnyWorkId>,
    error: Option<Error>,
    skip_features: bool,
    // we count the number of errors encountered but only store the first we see
    n_failures: usize,

    // When K completes also mark all entries in V complete
    also_completes: HashMap<AnyWorkId, Vec<AnyWorkId>>,
    pub(crate) jobs_pending: HashMap<AnyWorkId, Job>,
    pub(crate) count_pending: HashMap<IdentifierDiscriminant, Arc<AtomicUsize>>,

    pub(crate) timer: JobTimer,
}

/// A unit of executable work plus the identifiers of work that it depends on
///
/// Exists to allow us to modify dependencies, such as adding new ones.
#[derive(Debug)]
pub(crate) struct Job {
    pub(crate) id: AnyWorkId,
    // The actual task. Exec takes work and sets the running flag.
    pub(crate) work: AnyWork,
    // Things our job needs read access to. Job won't run if anything it can read is pending.
    pub(crate) read_access: AnyAccess,
    // Things our job needs write access to
    pub(crate) write_access: AnyAccess,
    // is this job running right now?
    pub(crate) running: bool,
}

enum RecvType {
    Blocking,
    NonBlocking,
}

/// Higher is better
///
/// We basically want things that block the glyph order => kern => fea sequence to go asap
fn priority(id: &AnyWorkId) -> u32 {
    match id {
        AnyWorkId::Fe(FeWorkIdentifier::Features) => 99,
        AnyWorkId::Fe(FeWorkIdentifier::KerningGroups) => 99,
        AnyWorkId::Fe(FeWorkIdentifier::KernInstance(..)) => 99,
        AnyWorkId::Fe(FeWorkIdentifier::GlyphOrder) => 99,
        AnyWorkId::Fe(FeWorkIdentifier::PreliminaryGlyphOrder) => 99,
        AnyWorkId::Fe(FeWorkIdentifier::StaticMetadata) => 99,
        AnyWorkId::Fe(FeWorkIdentifier::GlobalMetrics) => 99,
        AnyWorkId::Be(BeWorkIdentifier::GatherIrKerning) => 99,
        AnyWorkId::Be(BeWorkIdentifier::Features) => 99,
        AnyWorkId::Be(BeWorkIdentifier::GlyfFragment(..)) => 0,
        AnyWorkId::Be(BeWorkIdentifier::GvarFragment(..)) => 0,
        AnyWorkId::Fe(FeWorkIdentifier::Glyph(..)) => 1,
        _ => 32,
    }
}

impl Workload {
    // Pass in timer to enable t0 to be as early as possible
    pub fn new(input: &Input, mut timer: JobTimer, skip_features: bool) -> Result<Self, Error> {
        let time = timer
            .create_timer(AnyWorkId::InternalTiming("create_source"), 0)
            .run();

        let source = input.create_source()?;

        timer.add(time.complete());
        let time = timer
            .create_timer(AnyWorkId::InternalTiming("Create workload"), 0)
            .run();

        let mut workload = Self {
            source,
            job_count: 0,
            success: Default::default(),
            error: Default::default(),
            n_failures: 0,
            also_completes: Default::default(),
            jobs_pending: Default::default(),
            count_pending: Default::default(),
            skip_features,
            timer,
        };

        // Create work roughly in the order it would typically occur
        // Work is eligible to run as soon as all dependencies are complete
        // so this is NOT the definitive execution order

        // FE: f(source) => IR
        workload.add(workload.source.create_static_metadata_work()?);
        workload.add(workload.source.create_global_metric_work()?);
        workload.add(workload.source.create_feature_ir_work()?);
        workload.add_skippable_feature_work(workload.source.create_kerning_group_ir_work()?);
        workload
            .source
            .create_glyph_ir_work()?
            .into_iter()
            .for_each(|w| workload.add(w));
        workload.add(create_glyph_order_work());
        workload.add(workload.source.create_color_palette_work()?);
        workload.add(workload.source.create_paint_graph_work()?);

        // BE: f(IR, maybe other BE work) => binary
        workload.add_skippable_feature_work(FeatureFirstPassWork::create());
        workload.add_skippable_feature_work(FeatureCompilationWork::create());
        workload.add(create_gasp_work());
        let ir_glyphs = workload
            .jobs_pending
            .keys()
            .filter_map(|id| match id {
                AnyWorkId::Fe(FeWorkIdentifier::Glyph(name)) => Some(name.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        for glyph_name in ir_glyphs {
            workload.add(create_glyf_work(glyph_name))
        }
        workload.add(create_glyf_loca_work());
        workload.add(create_avar_work());
        workload.add(create_stat_work());
        workload.add(create_meta_work());
        workload.add(create_cmap_work());
        workload.add(create_colr_work());
        workload.add(create_cpal_work());
        workload.add(create_fvar_work());
        workload.add(create_gvar_work());
        workload.add(create_head_work());
        workload.add_skippable_feature_work(create_gather_ir_kerning_work());
        workload.add_skippable_feature_work(create_kerns_work());
        workload.add_skippable_feature_work(create_mark_work());
        workload.add(create_metric_and_limit_work());
        workload.add(create_hvar_work());
        workload.add(create_vertical_metrics_work());
        workload.add(create_mvar_work());
        workload.add(create_name_work());
        workload.add(create_os2_work());
        workload.add(create_post_work());

        // Make a damn font
        workload.add(create_font_work());

        workload.timer.add(time.complete());

        Ok(workload)
    }

    fn add_skippable_feature_work(&mut self, work: impl Into<AnyWork>) {
        if !self.skip_features {
            self.add(work);
        } else {
            self.skip(work);
        }
    }

    /// True if job might read what other produces
    #[cfg(test)]
    fn might_read(&self, job: &Job, other: &Job) -> bool {
        let result = job.read_access.check(&other.id)
            || self
                .also_completes
                .get(&other.id)
                .map(|also| also.iter().any(|other_id| job.read_access.check(other_id)))
                .unwrap_or_default();
        result
    }

    pub(crate) fn add(&mut self, work: impl Into<AnyWork>) {
        let work = work.into();
        let id = work.id();
        let read_access = work.read_access();
        let write_access = work.write_access();
        self.insert(Job {
            id,
            work,
            read_access,
            write_access,
            running: false,
        });
    }

    /// Do all the task dependency bookkeeping but don't actually run the wokr
    pub(crate) fn skip(&mut self, work: impl Into<AnyWork>) {
        let work: AnyWork = work.into();
        let id = work.id();
        self.insert_nop(id, work.read_access());
    }

    fn insert_with_bookkeeping(&mut self, job: Job) {
        trace!(
            "insert_job {}{:?} dependencies {:?}",
            matches!(job.work, AnyWork::AlsoComplete(..))
                .then_some("(nop) ")
                .unwrap_or(""),
            job.id,
            job.read_access
        );

        self.job_count += 1;
        self.count_pending
            .entry(job.id.discriminant())
            .or_default()
            .fetch_add(1, Ordering::AcqRel);
        self.jobs_pending.insert(job.id.clone(), job);
    }

    fn insert_nop(&mut self, id: AnyWorkId, read_access: AnyAccess) {
        self.insert_with_bookkeeping(Job {
            id: id.clone(),
            work: AnyWork::Nop(id.clone(), read_access.clone()),
            read_access, // We don't want to be deemed runnable prematurely
            write_access: AnyAccess::Fe(Access::None),
            running: false,
        });
    }

    pub(crate) fn insert(&mut self, job: Job) {
        let also_completes = job.work.also_completes();

        // We need pending entries for also-completes items so dependencies on them work
        for id in also_completes.iter() {
            self.insert_with_bookkeeping(Job {
                id: id.clone(),
                work: AnyWork::AlsoComplete(id.clone(), job.read_access.clone()),
                read_access: job.read_access.clone(), // We don't want to be deemed runnable prematurely
                write_access: AnyAccess::Fe(Access::None),
                running: false,
            });
        }
        if !also_completes.is_empty() {
            self.also_completes.insert(job.id.clone(), also_completes);
        }

        self.insert_with_bookkeeping(job);
    }

    fn complete_one(&mut self, id: AnyWorkId) {
        trace!("complete_one {id:?}");
        if self.jobs_pending.remove(&id).is_none() {
            panic!("{id:?} completed but isn't pending!");
        }
        if !self.success.insert(id.clone()) {
            panic!("Multiple completions of {id:?}");
        }
    }

    fn mark_also_completed(&mut self, success: &AnyWorkId) {
        let Some(also_completed) = self.also_completes.get(success).cloned() else {
            return;
        };
        for id in also_completed {
            self.complete_one(id);
        }
    }

    /// When BE glyph jobs are initially created they don't know enough to set fine grained dependencies
    /// so they depend on *all* IR glyphs. Once IR for a glyph completes we can refine that:
    ///
    /// * If the glyph doesn't emit to binary we don't need to do the BE work at all
    /// * If the glyph has no components the BE for it doesn't use glyph order and needn't block on it
    /// * If the glyph does have components we need to block on glyph order because that might alter them
    ///    * For example, flatten
    ///
    /// By minimizing dependencies we allow jobs to start earlier and execute with greater concurrency.
    fn update_be_glyph_work(&mut self, fe_root: &FeContext, glyph_name: GlyphName) {
        let glyph = fe_root
            .glyphs
            .get(&FeWorkIdentifier::Glyph(glyph_name.clone()));
        let be_id = AnyWorkId::Be(BeWorkIdentifier::GlyfFragment(glyph_name));

        // If the inputs to the BE glyph didn't change it won't be pending
        let Some(be_job) = self.jobs_pending.get_mut(&be_id) else {
            return;
        };

        if !glyph.emit_to_binary {
            trace!("Skipping execution of {be_id:?}; it does not emit to binary");
            for counter in self.counters(&be_id) {
                counter.fetch_sub(1, Ordering::AcqRel);
            }
            self.complete_one(be_id.clone());
            self.mark_also_completed(&be_id);
            return;
        }

        let mut deps = AccessBuilder::<AnyWorkId>::new().variant(FeWorkIdentifier::StaticMetadata);

        let mut has_components = false;
        for inst in glyph.sources().values() {
            for component in inst.components.iter() {
                has_components = true;
                deps = deps.specific_instance(FeWorkIdentifier::Glyph(component.base.clone()));
            }
        }

        // We don't *have* to wait on glyph order, but if we don't it delays the critical path
        if has_components {
            deps = deps.variant(FeWorkIdentifier::GlyphOrder);
        }

        let deps = deps.build().into();
        trace!(
            "Updating {be_id:?} deps from {:?} to {deps:?}",
            be_job.read_access
        );
        be_job.read_access = deps
    }

    fn handle_success(
        &mut self,
        fe_root: &FeContext,
        be_root: &BeContext,
        success: AnyWorkId,
        timing: JobTime,
    ) -> Result<(), Error> {
        log::debug!("{success:?} successful");

        self.timer.add(timing);

        self.complete_one(success.clone());
        self.mark_also_completed(&success);

        // When glyph order finalizes, add BE work for any new glyphs
        if let AnyWorkId::Fe(FeWorkIdentifier::GlyphOrder) = success {
            let preliminary_glyph_order = fe_root.preliminary_glyph_order.get();
            for glyph_name in fe_root
                .glyph_order
                .get()
                .difference(&preliminary_glyph_order)
            {
                debug!("Generating a BE job for {glyph_name}");
                self.add(create_glyf_work(glyph_name.clone()));

                // Glyph order is done so all IR must be done. Copy dependencies from the IR for the same name.
                self.update_be_glyph_work(fe_root, glyph_name.clone());
            }
        }

        if let AnyWorkId::Fe(FeWorkIdentifier::KerningGroups) = success {
            if let Some(groups) = fe_root.kerning_groups.try_get() {
                for location in groups.locations.iter() {
                    self.add(
                        self.source
                            .create_kerning_instance_ir_work(location.clone())?,
                    );
                }
            }

            // https://github.com/googlefonts/fontc/pull/655: don't set read access on GatherIrKerning until we spawn kern instance tasks
            self.jobs_pending
                .get_mut(&AnyWorkId::Be(BeWorkIdentifier::GatherIrKerning))
                .expect("Gather IR Kerning has to be pending")
                .read_access = AccessBuilder::<AnyWorkId>::new()
                .variant(FeWorkIdentifier::GlyphOrder)
                .variant(FeWorkIdentifier::KerningGroups)
                .variant(FeWorkIdentifier::KernInstance(NormalizedLocation::default()))
                .build()
                .into();
        }

        if let AnyWorkId::Be(BeWorkIdentifier::GatherIrKerning) = success {
            // When we skip features there are no kern pairs at all
            if let Some(kern_pairs) = be_root.all_kerning_pairs.try_get() {
                for work in create_kern_segment_work(&kern_pairs) {
                    self.add(work);
                }
            }
            // https://github.com/googlefonts/fontc/issues/647: it is now safe to set read access on segment gathering
            self.jobs_pending
                .get_mut(&AnyWorkId::Be(BeWorkIdentifier::GatherBeKerning))
                .expect("Gather BE Kerning has to be pending")
                .read_access = AccessBuilder::<AnyWorkId>::new()
                .variant(BeWorkIdentifier::KernFragment(0))
                .variant(BeWorkIdentifier::FeaturesAst)
                .variant(FeWorkIdentifier::Glyph(GlyphName::NOTDEF))
                .variant(FeWorkIdentifier::StaticMetadata)
                .build()
                .into();
        }

        if let AnyWorkId::Fe(FeWorkIdentifier::Glyph(glyph_name)) = success {
            self.update_be_glyph_work(fe_root, glyph_name);
        }

        Ok(())
    }

    /// Check if a dependency is fulfilled, that is it's output is available
    fn is_dep_fulfilled(&self, dep: &AccessType<AnyWorkId>) -> bool {
        // If the dependency is on the variant we can simply check the count
        // otherwise we have to check the specific job for completion. Checking the
        // count will return true even if the "I'm done" message hasn't been received yet.
        match dep {
            AccessType::SpecificInstanceOfVariant(id) => {
                !self.jobs_pending.contains_key(&id.clone())
            }
            AccessType::Variant(exemplar) => {
                self.count_pending
                    .get(&exemplar.discriminant())
                    .map(|a| a.load(Ordering::Acquire))
                    .unwrap_or_default()
                    == 0
            }
        }
    }

    fn anything_else_pending(&self, id: &AnyWorkId) -> bool {
        if self.jobs_pending.len() > 1 {
            return true;
        }
        let Some(the_pending_job) = self.jobs_pending.keys().next() else {
            return false;
        };
        id != the_pending_job
    }

    fn can_run(&self, job: &Job) -> bool {
        match &job.read_access {
            AnyAccess::Fe(access) => match access {
                Access::None => true,
                Access::Unknown => false,
                Access::SpecificInstanceOfVariant(id) => {
                    self.is_dep_fulfilled(&AccessType::SpecificInstanceOfVariant(id.clone().into()))
                }
                Access::Variant(id) => {
                    self.is_dep_fulfilled(&AccessType::Variant(id.clone().into()))
                }
                Access::Set(ids) => ids.iter().all(|id| match id {
                    AccessType::SpecificInstanceOfVariant(id) => self.is_dep_fulfilled(
                        &AccessType::SpecificInstanceOfVariant(id.clone().into()),
                    ),
                    AccessType::Variant(exemplar) => {
                        self.is_dep_fulfilled(&AccessType::Variant(exemplar.clone().into()))
                    }
                }),
                Access::All => !self.anything_else_pending(&job.id),
            },
            AnyAccess::Be(access) => match access {
                Access::None => true,
                Access::Unknown => false,
                Access::SpecificInstanceOfVariant(id) => {
                    self.is_dep_fulfilled(&AccessType::SpecificInstanceOfVariant(id.clone()))
                }
                Access::Variant(id) => self.is_dep_fulfilled(&AccessType::Variant(id.clone())),
                Access::Set(ids) => ids.iter().all(|id| self.is_dep_fulfilled(id)),
                Access::All => !self.anything_else_pending(&job.id),
            },
        }
    }

    /// Populate launchable with jobs ready to run from highest to lowest priority
    pub fn update_launchable(&mut self, launchable: &mut Vec<AnyWorkId>) {
        let timing = self
            .timer
            .create_timer(AnyWorkId::InternalTiming("Launchable"), 0)
            .run();

        launchable.clear();
        for id in self.jobs_pending.iter().filter_map(|(id, job)| {
            (!matches!(job.work, AnyWork::AlsoComplete(..)) && !job.running && self.can_run(job))
                .then_some(id)
        }) {
            launchable.push(id.clone());
        }

        self.timer.add(timing.complete());
    }

    fn counters(&self, id: &AnyWorkId) -> Vec<Arc<AtomicUsize>> {
        let also_completes = self.also_completes.get(id);
        let mut counters =
            Vec::with_capacity(1 + also_completes.map(|v| v.len()).unwrap_or_default());
        counters.push(
            self.count_pending
                .get(id.discriminant())
                .unwrap_or_else(|| panic!("No count of type for runnable {id:?}"))
                .clone(),
        );
        if let Some(also_completes) = also_completes {
            for id in also_completes {
                counters.push(
                    self.count_pending
                        .get(id.discriminant())
                        .unwrap_or_else(|| panic!("No count of type for runnable {id:?}"))
                        .clone(),
                );
            }
        }
        counters
    }

    pub fn exec(mut self, fe_root: &FeContext, be_root: &BeContext) -> Result<JobTimer, Error> {
        // Async work will send us it's ID on completion
        let (send, recv) =
            crossbeam_channel::unbounded::<(AnyWorkId, Result<(), Error>, JobTime)>();

        // a flag we set if we panic
        let abort_queued_jobs = Arc::new(AtomicBool::new(false));

        let run_queue = Arc::new(Mutex::new(Vec::<(
            AnyWork,
            JobTime,
            AnyContext,
            Vec<Arc<AtomicUsize>>,
        )>::with_capacity(512)));

        let runner = |scope: &rayon::Scope| {
            // Whenever a task completes see if it was the last incomplete dependency of other task(s)
            // and spawn them if it was
            // TODO timeout and die it if takes too long to make forward progress or we're spinning w/o progress

            // To avoid allocation every poll for work
            let mut launchable = Vec::with_capacity(512.min(self.job_count));
            let mut successes: Vec<(AnyWorkId, JobTime)> = Vec::with_capacity(64);
            let mut nth_wave = 0;

            while self.success.len() < self.job_count {
                // Spawn anything that is currently executable (has no unfulfilled dependencies)
                self.update_launchable(&mut launchable);
                if launchable.is_empty() && !self.jobs_pending.values().any(|j| j.running) {
                    if log::log_enabled!(log::Level::Warn) {
                        warn!("{}/{} jobs have succeeded, nothing is running, and nothing is launchable", self.success.len(), self.job_count);
                        for pending in self.jobs_pending.keys() {
                            warn!("  blocked: {pending:?}");
                        }
                    }
                    return Err(Error::UnableToProceed(self.jobs_pending.len()));
                }
                successes.clear();

                // Get launchables ready to run
                if !launchable.is_empty() {
                    nth_wave += 1;
                    let timing = self
                        .timer
                        .create_timer(AnyWorkId::InternalTiming("run_q"), nth_wave)
                        .run();

                    {
                        let mut run_queue = run_queue.lock().unwrap();

                        for id in launchable.iter() {
                            let timing = self.timer.create_timer(id.clone(), nth_wave);

                            let job = self.jobs_pending.get_mut(id).unwrap();
                            log::trace!("Start {id:?}");
                            job.running = true;

                            let mut work =
                                AnyWork::AlsoComplete(id.clone(), job.read_access.clone());
                            std::mem::swap(&mut job.work, &mut work);
                            let work_context = AnyContext::for_work(
                                fe_root,
                                be_root,
                                id,
                                job.read_access.clone(),
                                job.write_access.clone(),
                            );

                            let counters = self.counters(id);
                            let timing = timing.queued();
                            run_queue.push((work, timing, work_context, counters));
                        }

                        // Try to prioritize the critical path based on --emit-timing observation
                        // <https://github.com/googlefonts/fontc/issues/456>, <https://github.com/googlefonts/fontc/pull/565>
                        run_queue.sort_by_cached_key(|(work, ..)| priority(&work.id()));
                    }
                    self.timer.add(timing.complete());

                    // Spawn for every job that's executable. Each spawn will pull one item from the run queue.
                    let timing = self
                        .timer
                        .create_timer(AnyWorkId::InternalTiming("spawn"), nth_wave)
                        .run();
                    for _ in 0..launchable.len() {
                        let send = send.clone();
                        let run_queue = run_queue.clone();
                        let abort = abort_queued_jobs.clone();

                        scope.spawn(move |_| {
                            let runnable = { run_queue.lock().unwrap().pop() };
                            let Some((work, timing, work_context, counters)) = runnable else {
                                panic!("Spawned more jobs than items available to run");
                            };
                            let id = work.id();
                            let timing = timing.run();
                            if abort.load(Ordering::Relaxed) {
                                log::trace!("Aborting {id:?}");
                                return;
                            }
                            // # Unwind Safety
                            //
                            // 'unwind safety' does not impact memory safety, but
                            // it may impact program correctness; the thread may have
                            // left shared memory in an inconsistent state.
                            //
                            // I believe this is not a concern for us, as we cancel any
                            // pending jobs after seeing a panic and jobs that depend
                            // on state produced by the panicking job must be scheduled
                            // after it. Unless we have jobs that are mutating
                            // shared resources then I think this is fine.
                            //
                            // references:
                            // <https://doc.rust-lang.org/nomicon/exception-safety.html#exception-safety>
                            // <https://doc.rust-lang.org/std/panic/trait.UnwindSafe.html>
                            let result = match std::panic::catch_unwind(AssertUnwindSafe(|| {
                                work.exec(work_context)
                            })) {
                                Ok(result) => result,
                                Err(err) => {
                                    let msg = get_panic_message(err);
                                    abort.store(true, Ordering::Relaxed);
                                    Err(Error::Panic(msg))
                                }
                            };
                            // Decrement counters immediately so all-of detection checks true
                            // before our success result has passed through the channel
                            // At peak times, such as completion of tons of glyphs, the channel seems
                            // to have tens of ms of delay.
                            if result.is_ok() {
                                for counter in counters {
                                    counter.fetch_sub(1, Ordering::AcqRel);
                                }
                            }
                            let timing = timing.complete();

                            if let Err(e) = send.send((id.clone(), result, timing)) {
                                log::error!("Unable to write {id:?} to completion channel: {e}");
                            }
                        })
                    }
                    self.timer.add(timing.complete());
                }

                // Complete everything that has reported since our last check
                if successes.is_empty() {
                    let timing = self
                        .timer
                        .create_timer(AnyWorkId::InternalTiming("rc"), nth_wave)
                        .run();
                    self.read_completions(&mut successes, &recv, RecvType::Blocking)?;
                    self.timer.add(timing.complete());
                    let timing = self
                        .timer
                        .create_timer(AnyWorkId::InternalTiming("hs"), nth_wave)
                        .run();
                    for (success, timing) in successes.iter() {
                        self.handle_success(fe_root, be_root, success.clone(), timing.clone())?;
                    }
                    self.timer.add(timing.complete());
                }

                if launchable.is_empty() && successes.is_empty() {
                    // We didn't do anything
                }
            }
            Ok::<(), Error>(())
        };

        // use an explicit threadpool to avoid possible congestion if another
        // library we use is using the global threadpool
        #[cfg(not(target_family = "wasm"))]
        {
            let tp = rayon::ThreadPoolBuilder::new()
                .build()
                .expect("couldn't build threadpool");
            tp.in_place_scope(runner)?;
        }
        // WASM rayon uses a fall-back single threaded implementation
        #[cfg(target_family = "wasm")]
        rayon::in_place_scope(runner)?;

        // If ^ exited due to error the scope awaited any live tasks; capture their results
        self.read_completions(&mut Vec::new(), &recv, RecvType::NonBlocking)?;

        if self.error.is_none() {
            if self.success.len() != self.job_count {
                panic!(
                    "No errors but only {}/{} succeeded?!",
                    self.success.len(),
                    self.job_count
                );
            }
            if self
                .count_pending
                .iter()
                .any(|(_, c)| c.load(Ordering::Acquire) != 0)
            {
                panic!(
                    "Not all counts by discriminant are 0\n{:#?}",
                    self.count_pending
                );
            }
        }

        Ok(self.timer)
    }

    fn read_completions(
        &mut self,
        successes: &mut Vec<(AnyWorkId, JobTime)>,
        recv: &Receiver<(AnyWorkId, Result<(), Error>, JobTime)>,
        initial_read: RecvType,
    ) -> Result<(), Error> {
        successes.clear();
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
        while let Some((completed_id, result, timing)) = opt_complete.take() {
            if !match result {
                Ok(..) => {
                    if !self.success.contains(&completed_id) {
                        successes.push((completed_id.clone(), timing));
                        true
                    } else {
                        false
                    }
                }
                Err(e) => {
                    self.n_failures += 1;
                    if self.error.is_none() {
                        self.error = Some(e);
                    } else {
                        // the first error will be reported on exit, log the rest:
                        log::error!("task '{completed_id:?}' failed: '{e}'");
                    }
                    true
                }
            } {
                panic!("Repeat signals for completion of {completed_id:#?}");
            }

            log::debug!(
                "{}/{} complete, most recently {:?}",
                self.n_failures + self.success.len() + successes.len(),
                self.job_count,
                completed_id
            );

            // See if anything else is complete in case things come in waves
            if let Ok(completed_id) = recv.try_recv() {
                opt_complete = Some(completed_id);
            }
        }
        match self.error.take() {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }

    #[cfg(test)]
    fn unfulfilled_deps(&self, job: &Job) -> Vec<AnyWorkId> {
        let mut unfulfilled = self
            .jobs_pending
            .iter()
            .filter_map(|(id, other_job)| {
                if self.might_read(job, other_job) {
                    Some(id)
                } else {
                    None
                }
            })
            .cloned()
            .collect::<Vec<_>>();
        unfulfilled.sort();
        unfulfilled
    }

    /// Run all pending jobs.
    ///
    /// Returns the set of ids for tasks that executed as a result of this run.
    #[cfg(test)]
    pub fn run_for_test(&mut self, fe_root: &FeContext, be_root: &BeContext) -> HashSet<AnyWorkId> {
        let pre_success = self.success.clone();
        let mut sorted_pre_success = pre_success.iter().collect::<Vec<_>>();
        sorted_pre_success.sort();
        debug!("pre-success {sorted_pre_success:?}");

        let mut launchable = Vec::new();
        while !self.jobs_pending.is_empty() {
            self.update_launchable(&mut launchable);
            if launchable.is_empty() {
                log::error!("Completed:");
                let mut success: Vec<_> = self.success.iter().collect();
                success.sort();
                for id in success {
                    log::error!("  {id:?}");
                }
                log::error!("Unable to proceed with:");
                for (id, job) in self.jobs_pending.iter() {
                    let unfulfilled = self.unfulfilled_deps(job);
                    log::error!("  {id:?}, has unfulfilled dependencies: {unfulfilled:?}");
                }
                log::error!("Count by discriminant:");
                let mut counts = self
                    .count_pending
                    .iter()
                    .filter_map(|(k, c)| {
                        let value = c.load(Ordering::Acquire);
                        (value > 0).then_some((k, value))
                    })
                    .collect::<Vec<_>>();
                counts.sort();
                for (key, count) in counts {
                    log::error!("  {key} {count}");
                }

                assert!(
                    !launchable.is_empty(),
                    "Unable to make forward progress, bad graph?"
                );
            }

            let id = &launchable[0];
            let timing = self.timer.create_timer(id.clone(), 0);
            let job = self.jobs_pending.get(id).unwrap();

            let timing = timing.queued();
            let context = AnyContext::for_work(
                fe_root,
                be_root,
                id,
                job.read_access.clone(),
                job.write_access.clone(),
            );
            log::debug!("Exec {id:?}");
            let timing = timing.run();
            job.work
                .exec(context)
                .unwrap_or_else(|e| panic!("{id:?} failed: {e:?}"));

            for counter in self.counters(id) {
                counter.fetch_sub(1, Ordering::AcqRel);
            }

            let timing = timing.complete();
            self.handle_success(fe_root, be_root, id.clone(), timing)
                .unwrap_or_else(|e| panic!("Failed to handle success for {id:?}: {e}"));
        }
        self.success.difference(&pre_success).cloned().collect()
    }
}

// taken from std:
// <https://github.com/rust-lang/rust/blob/d5a82bbd26e1ad8b7401f6a718a9c57c96905483/library/std/src/panicking.rs#L247-L253>
fn get_panic_message(msg: Box<dyn std::any::Any + Send + 'static>) -> String {
    match msg.downcast_ref::<&'static str>() {
        Some(s) => s.to_string(),
        None => match msg.downcast_ref::<String>() {
            Some(s) => s.to_owned(),
            None => "Box<dyn Any>".to_owned(),
        },
    }
}
