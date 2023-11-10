//! Tracking jobs to run

use std::{
    collections::{HashMap, HashSet, VecDeque},
    panic::AssertUnwindSafe,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
    time::Instant,
};

use crossbeam_channel::{Receiver, Sender, TryRecvError};
use fontbe::orchestration::{AnyWorkId, Context as BeContext, WorkId as BeWorkIdentifier};
use fontdrasil::{
    orchestration::{Access, Priority},
    types::GlyphName,
};
use fontir::{
    orchestration::{Context as FeContext, WorkId as FeWorkIdentifier},
    source::Input,
};
use log::{debug, trace};
use rayon::Scope;

use crate::{
    timing::{create_timer, JobTime, JobTimeQueued, JobTimer},
    work::{AnyAccess, AnyContext, AnyWork, AnyWorkError},
    ChangeDetector, Error,
};

/// A piece of work that is ready to execute as soon as a thread becomes available
#[derive(Debug)]
pub(crate) struct PendingWork {
    abort: Arc<AtomicBool>,
    timing: JobTimeQueued,
    work: AnyWork,
    context: AnyContext,
}

/// Manages work that is ready to execute (has no unfulfilled dependencies).
///
/// Keeps the work in prioritized order so a threadpool job can grab the highest priority
/// work.
#[derive(Default, Debug, Clone)]
pub(crate) struct PrioritizedWork(Arc<Mutex<VecDeque<PendingWork>>>);

impl PrioritizedWork {
    #[inline]
    fn insert(
        &mut self,
        abort: Arc<AtomicBool>,
        timer: JobTimeQueued,
        work: AnyWork,
        context: AnyContext,
    ) {
        let id = work.id();
        let entry = PendingWork {
            abort,
            timing: timer,
            work,
            context,
        };
        {
            let mut queue = self.0.lock().unwrap();
            match id.priority() {
                Priority::High => queue.push_front(entry),
                Priority::Low => queue.push_back(entry),
            }
        }
    }

    /// Reserve space, such as when you anticipate adding a bunch of items to the queue
    #[inline]
    fn reserve(&mut self, additional: usize) {
        let mut queue = self.0.lock().unwrap();
        queue.reserve(additional);
    }

    #[inline]
    fn pop_front(&self) -> Option<PendingWork> {
        let mut queue = self.0.lock().unwrap();
        queue.pop_front()
    }
}

/// A set of interdependent jobs to execute.
#[derive(Debug)]
pub struct Workload<'a> {
    pub(crate) change_detector: &'a ChangeDetector,
    job_count: usize,
    success: HashSet<AnyWorkId>,
    error: Vec<(AnyWorkId, String)>,

    // When K completes also mark all entries in V complete
    also_completes: HashMap<AnyWorkId, Vec<AnyWorkId>>,
    pub(crate) jobs_pending: HashMap<AnyWorkId, Job>,

    timing: JobTimer,

    pub(crate) pending_execution: PrioritizedWork,
}

/// A unit of executable work plus the identifiers of work that it depends on
///
/// Exists to allow us to modify dependencies, such as adding new ones.
#[derive(Debug)]
pub(crate) struct Job {
    pub(crate) id: AnyWorkId,
    // The actual task. Exec takes work and sets the running flag.
    pub(crate) work: Option<AnyWork>,
    // Things our job needs read access to. Job won't run if anything it can read is pending.
    pub(crate) read_access: AnyAccess,
    // Things our job needs write access to
    pub(crate) write_access: AnyAccess,
    // Does this job actually need to execute?
    pub(crate) run: bool,
    // is this job running right now?
    pub(crate) running: bool,
}

enum RecvType {
    Blocking,
    NonBlocking,
}

impl<'a> Workload<'a> {
    pub fn new(change_detector: &'a ChangeDetector, t0: Instant) -> Workload {
        Workload {
            change_detector,
            job_count: 0,
            success: Default::default(),
            error: Default::default(),
            also_completes: Default::default(),
            jobs_pending: Default::default(),
            timing: JobTimer::new(t0),
            pending_execution: Default::default(),
        }
    }

    /// True if job might read what other produces
    fn might_read(&self, job: &Job, other: &Job) -> bool {
        let result = job.read_access.check(&other.id)
            || self
                .also_completes
                .get(&other.id)
                .map(|also| also.iter().any(|other_id| job.read_access.check(other_id)))
                .unwrap_or_default();
        result
    }

    pub fn current_inputs(&self) -> &Input {
        self.change_detector.current_inputs()
    }

    // TODO flags would be clearer than multiple bools
    pub(crate) fn add(&mut self, work: AnyWork, should_run: bool) {
        let id = work.id();
        let read_access = work.read_access();
        let write_access = work.write_access();
        self.insert(
            id.clone(),
            Job {
                id,
                work: Some(work),
                read_access,
                write_access,
                run: should_run,
                running: false,
            },
        );
    }

    pub(crate) fn insert(&mut self, id: AnyWorkId, job: Job) {
        let also_completes = job
            .work
            .as_ref()
            .expect("{id:?} submitted without work")
            .also_completes();

        self.job_count += 1 + also_completes.len();
        self.jobs_pending.insert(id.clone(), job);

        if !also_completes.is_empty() {
            self.also_completes.insert(id, also_completes);
        }
    }

    fn mark_also_completed(&mut self, success: &AnyWorkId) {
        let Some(also_completed) = self.also_completes.get(success) else {
            return;
        };
        for id in also_completed {
            if !self.success.insert(id.clone()) {
                panic!("Multiple completions of {id:?}");
            }
        }
    }

    fn update_be_glyph_dependencies(&mut self, fe_root: &FeContext, glyph_name: GlyphName) {
        let glyph = fe_root
            .glyphs
            .get(&FeWorkIdentifier::Glyph(glyph_name.clone()));
        let be_id = AnyWorkId::Be(BeWorkIdentifier::GlyfFragment(glyph_name));
        let be_job = self
            .jobs_pending
            .get_mut(&be_id)
            .expect("{be_id} should exist");

        let mut deps = HashSet::from([
            AnyWorkId::Fe(FeWorkIdentifier::StaticMetadata),
            FeWorkIdentifier::GlyphOrder.into(),
        ]);

        for inst in glyph.sources().values() {
            for component in inst.components.iter() {
                deps.insert(FeWorkIdentifier::Glyph(component.base.clone()).into());
            }
        }

        trace!(
            "Updating {be_id:?} deps from {:?} to {deps:?}",
            be_job.read_access
        );
        be_job.read_access = Access::Set(deps).into();
    }

    fn handle_success(&mut self, fe_root: &FeContext, success: AnyWorkId, timing: JobTime) {
        log::debug!("{success:?} successful");

        self.timing.add(timing);

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
                super::add_glyph_be_job(self, glyph_name.clone());

                // Glyph order is done so all IR must be done. Copy dependencies from the IR for the same name.
                self.update_be_glyph_dependencies(fe_root, glyph_name.clone());
            }
        }

        // When BE glyph jobs are initially created they don't know enough to set fine grained dependencies
        // so they depend on *all* IR glyphs. Once IR for a glyph completes we know enough to refine that
        // to just the glyphs our glyphs uses as components. This allows BE glyph jobs to run concurrently with
        // unrelated IR jobs.
        if let AnyWorkId::Fe(FeWorkIdentifier::Glyph(glyph_name)) = success {
            self.update_be_glyph_dependencies(fe_root, glyph_name);
        }
    }

    fn can_run_scan(&self, job: &Job) -> bool {
        // Job can only run if *no* incomplete jobs exist whose output it could read
        !self
            .jobs_pending
            .iter()
            .filter(|(other_id, _)| !self.success.contains(other_id))
            .any(|(_, other_job)| self.might_read(job, other_job))
    }

    fn can_run(&self, job: &Job) -> bool {
        // Custom access requires a scan across pending jobs, hence avoidance is nice
        match &job.read_access {
            AnyAccess::Fe(access) => match access {
                Access::None => true,
                Access::Unknown => false,
                Access::One(id) => !self.jobs_pending.contains_key(&id.clone().into()),
                Access::Set(ids) => !ids
                    .iter()
                    .any(|id| self.jobs_pending.contains_key(&id.clone().into())),
                Access::Custom(..) => self.can_run_scan(job),
                Access::All => self.can_run_scan(job),
            },
            AnyAccess::Be(access) => match access {
                Access::None => true,
                Access::Unknown => false,
                Access::One(id) => !self.jobs_pending.contains_key(id),
                Access::Set(ids) => !ids.iter().any(|id| self.jobs_pending.contains_key(id)),
                Access::Custom(..) => self.can_run_scan(job),
                Access::All => self.can_run_scan(job),
            },
        }
    }

    pub fn launchable(&mut self) -> Vec<AnyWorkId> {
        let timing = create_timer(AnyWorkId::Fe(FeWorkIdentifier::Overhead))
            .queued()
            .run();

        let mut launchable: Vec<_> = self
            .jobs_pending
            .iter()
            .filter_map(|(id, job)| {
                if !job.running && self.can_run(job) {
                    Some(id.clone())
                } else {
                    None
                }
            })
            .collect();
        launchable.sort_by_key(|id| id.priority());
        trace!("Launchable: {launchable:?}");

        self.timing.add(timing.complete());
        launchable
    }

    fn launch(
        &mut self,
        scope: &Scope,
        send: &Sender<(AnyWorkId, Result<(), AnyWorkError>, JobTime)>,
        abort: Arc<AtomicBool>,
        fe_root: &FeContext,
        be_root: &BeContext,
        work_ids: &[AnyWorkId],
    ) {
        // Place anything that needs launching on the run queue
        for id in work_ids.iter() {
            log::trace!("Start {:?}", id);
            let timing = create_timer(id.clone());
            let job = self.jobs_pending.get_mut(id).unwrap();
            job.running = true;
            let work = job
                .work
                .take()
                .unwrap_or_else(|| panic!("{id:?} ready to run but has no work?!"));
            if !job.run {
                if let Err(e) = send.send((id.clone(), Ok(()), JobTime::nop(id.clone()))) {
                    log::error!("Unable to write nop {id:?} to completion channel: {e}");
                    //FIXME: if we can't send messages it means the receiver has dropped,
                    //which means we should... return? abort?
                }
                continue;
            }
            let work_context = AnyContext::for_work(
                fe_root,
                be_root,
                id,
                job.read_access.clone(),
                job.write_access.clone(),
            );

            self.pending_execution
                .insert(abort.clone(), timing.queued(), work, work_context)
        }

        // Push 1 job onto the threadpool for each thing that became runnable.
        // Each job picks the highest priority work pending execution to run when it executes.
        for _ in 0..work_ids.len() {
            let send = send.clone();
            let pending_execution = self.pending_execution.clone(); // cheap arc copy
            scope.spawn(move |_| {
                let Some(pending) = pending_execution.pop_front() else {
                    log::error!("Nothing pending?!");
                    return;
                };
                if pending.abort.load(Ordering::Relaxed) {
                    log::trace!("Aborting {:?}", pending.work.id());
                    return;
                }
                let timing = pending.timing.run();
                let work = pending.work;
                let context = pending.context;
                let id = work.id();
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
                let result = match std::panic::catch_unwind(AssertUnwindSafe(|| work.exec(context)))
                {
                    Ok(result) => result,
                    Err(err) => {
                        let msg = get_panic_message(err);
                        pending.abort.store(true, Ordering::Relaxed);
                        Err(AnyWorkError::Panic(msg))
                    }
                };
                let timing = timing.complete();
                if let Err(e) = send.send((id.clone(), result, timing)) {
                    log::error!("Unable to write {id:?} to completion channel: {e}");
                }
            })
        }
    }

    pub fn exec(mut self, fe_root: &FeContext, be_root: &BeContext) -> Result<JobTimer, Error> {
        // Async work will send us it's ID on completion
        let (send, recv) =
            crossbeam_channel::unbounded::<(AnyWorkId, Result<(), AnyWorkError>, JobTime)>();

        // a flag we set if we panic
        let abort_queued_jobs = Arc::new(AtomicBool::new(false));

        // Do NOT assign custom thread names because it makes flamegraph root each thread individually
        rayon::in_place_scope(|scope| {
            // Whenever a task completes see if it was the last incomplete dependency of other task(s)
            // and spawn them if it was
            // TODO timeout and die it if takes too long to make forward progress or we're spinning w/o progress
            while self.success.len() < self.job_count {
                // Spawn anything that is currently executable (has no unfulfilled dependencies)
                let launchable = self.launchable();
                if launchable.is_empty() && !self.jobs_pending.values().any(|j| j.running) {
                    return Err(Error::UnableToProceed);
                }

                self.pending_execution.reserve(launchable.len());

                // Launch by priority.
                let low_idx = launchable
                    .iter()
                    .position(|l| l.priority() == Priority::Low)
                    .unwrap_or(launchable.len());

                let (high, low) = launchable.split_at(low_idx);

                self.launch(
                    scope,
                    &send,
                    abort_queued_jobs.clone(),
                    fe_root,
                    be_root,
                    high,
                );
                self.launch(
                    scope,
                    &send,
                    abort_queued_jobs.clone(),
                    fe_root,
                    be_root,
                    low,
                );

                // Block for things to phone home to say they are done
                // Then complete everything that has reported since our last check
                let successes = self.read_completions(&recv, RecvType::Blocking)?;
                successes
                    .into_iter()
                    .for_each(|(s, t)| self.handle_success(fe_root, s, t));
            }
            Ok::<(), Error>(())
        })?;

        // If ^ exited due to error the scope awaited any live tasks; capture their results
        self.read_completions(&recv, RecvType::NonBlocking)?;

        if self.error.is_empty() && self.success.len() != self.job_count {
            panic!(
                "No errors but only {}/{} succeeded?!",
                self.success.len(),
                self.job_count
            );
        }

        Ok(self.timing)
    }

    fn read_completions(
        &mut self,
        recv: &Receiver<(AnyWorkId, Result<(), AnyWorkError>, JobTime)>,
        initial_read: RecvType,
    ) -> Result<Vec<(AnyWorkId, JobTime)>, Error> {
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
        while let Some((completed_id, result, timing)) = opt_complete.take() {
            if !match result {
                Ok(..) => {
                    let inserted = self.success.insert(completed_id.clone());
                    if inserted {
                        successes.push((completed_id.clone(), timing));
                    }
                    inserted
                }
                Err(e) => {
                    log::error!("{completed_id:?} failed {e}");
                    self.error.push((completed_id.clone(), format!("{e}")));
                    true
                }
            } {
                panic!("Repeat signals for completion of {completed_id:#?}");
            }

            // When a job marks another as also completed the original may never have been in pending
            self.jobs_pending.remove(&completed_id);

            log::debug!(
                "{}/{} complete, most recently {:?}",
                self.error.len() + self.success.len(),
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
        while !self.jobs_pending.is_empty() {
            let launchable = self.launchable();
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
                    for id in unfulfilled.iter() {
                        log::error!(
                            "    {id:?}, has unfulfilled dependencies: {:?}",
                            self.unfulfilled_deps(self.jobs_pending.get(id).unwrap())
                        );
                    }
                }
                assert!(
                    !launchable.is_empty(),
                    "Unable to make forward progress, bad graph?"
                );
            }

            let id = &launchable[0];
            let timing = create_timer(id.clone());
            let job = self.jobs_pending.remove(id).unwrap();
            if job.run {
                let timing = timing.queued();
                let context =
                    AnyContext::for_work(fe_root, be_root, id, job.read_access, job.write_access);
                log::debug!("Exec {:?}", id);
                let timing = timing.run();
                job.work
                    .expect("{id:?} should have work!")
                    .exec(context)
                    .unwrap_or_else(|e| panic!("{id:?} failed: {e:?}"));
                let timing = timing.complete();
                assert!(
                    self.success.insert(id.clone()),
                    "We just did {id:?} a second time?"
                );
                self.handle_success(fe_root, id.clone(), timing);
            }
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
