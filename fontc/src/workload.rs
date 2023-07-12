//! Tracking jobs to run

use std::{
    collections::{HashMap, HashSet},
    time::Duration,
};

use crossbeam_channel::{Receiver, RecvTimeoutError, TryRecvError};
use fontbe::orchestration::{AnyWorkId, Context as BeContext};
use fontdrasil::orchestration::Access;
use fontir::{
    orchestration::{Context as FeContext, WorkId as FeWorkIdentifier},
    source::Input,
};
use log::{debug, trace};

use crate::{
    work::{AnyAccess, AnyContext, AnyWork, AnyWorkError},
    ChangeDetector, Error,
};

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
    pub fn new(change_detector: &'a ChangeDetector) -> Workload {
        Workload {
            change_detector,
            job_count: 0,
            success: Default::default(),
            error: Default::default(),
            also_completes: Default::default(),
            jobs_pending: Default::default(),
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
        let Some(also_completed) = self.also_completes.get(success) else { return };
        for id in also_completed {
            if !self.success.insert(id.clone()) {
                panic!("Multiple completions of {id:?}");
            }
        }
    }

    fn handle_success(&mut self, fe_root: &FeContext, success: AnyWorkId) {
        log::debug!("{success:?} successful");

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
            }
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
                Access::One(id) => !self.jobs_pending.contains_key(&id.into()),
                Access::Set(ids) => !ids
                    .iter()
                    .any(|id| self.jobs_pending.contains_key(&id.into())),
                Access::Custom(..) => self.can_run_scan(job),
                Access::All => self.can_run_scan(job),
            },
            AnyAccess::Be(access) => match access {
                Access::None => true,
                Access::One(id) => !self.jobs_pending.contains_key(id),
                Access::Set(ids) => !ids.iter().any(|id| self.jobs_pending.contains_key(id)),
                Access::Custom(..) => self.can_run_scan(job),
                Access::All => self.can_run_scan(job),
            },
        }
    }

    pub fn launchable(&self) -> Vec<AnyWorkId> {
        let launchable = self
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
        trace!("Launchable: {launchable:?}");
        launchable
    }

    pub fn exec(mut self, fe_root: &FeContext, be_root: &BeContext) -> Result<(), Error> {
        // Async work will send us it's ID on completion
        let (send, recv) = crossbeam_channel::unbounded::<(AnyWorkId, Result<(), AnyWorkError>)>();

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

                // Launch anything that needs launching
                for id in launchable {
                    let job = self.jobs_pending.get_mut(&id).unwrap();
                    log::trace!("Start {:?}", id);
                    let send = send.clone();
                    job.running = true;
                    let work = job
                        .work
                        .take()
                        .expect("{id:?} ready to run but has no work?!");
                    if job.run {
                        let work_context = AnyContext::for_work(
                            fe_root,
                            be_root,
                            &id,
                            job.read_access.clone(),
                            job.write_access.clone(),
                        );

                        scope.spawn(move |_| {
                            let result = work.exec(work_context);
                            if let Err(e) = send.send((id.clone(), result)) {
                                log::error!("Unable to write {id:?} to completion channel: {e}",);
                            }
                        })
                    } else if let Err(e) = send.send((id.clone(), Ok(()))) {
                        log::error!("Unable to write nop {id:?} to completion channel: {e}");
                    }
                }

                // Block for things to phone home to say they are done
                // Then complete everything that has reported since our last check
                let successes = self.read_completions(&recv, RecvType::Blocking)?;
                successes
                    .into_iter()
                    .for_each(|s| self.handle_success(fe_root, s));
            }
            Ok(())
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

        Ok(())
    }

    fn read_completions(
        &mut self,
        recv: &Receiver<(AnyWorkId, Result<(), AnyWorkError>)>,
        initial_read: RecvType,
    ) -> Result<Vec<AnyWorkId>, Error> {
        // read in a loop, timing out if no progress is made
        fn spin_read(
            recv: &Receiver<(AnyWorkId, Result<(), AnyWorkError>)>,
        ) -> Result<(AnyWorkId, Result<(), AnyWorkError>), Error> {
            const ONE_SECOND: Duration = Duration::from_secs(1);
            const N_SPINS_BEFORE_ERROR: usize = 10;
            let mut spins = 0;

            while spins < N_SPINS_BEFORE_ERROR {
                match recv.recv_timeout(ONE_SECOND) {
                    Ok(completed) => return Ok(completed),
                    Err(RecvTimeoutError::Timeout) => {
                        spins += 1;
                        log::info!("read loop spinning ({spins}/{N_SPINS_BEFORE_ERROR})");
                    }
                    Err(RecvTimeoutError::Disconnected) => return Err(Error::InternalError),
                }
            }
            log::error!("timed out while waiting for work");
            Err(Error::InternalError)
        }

        let mut successes = Vec::new();
        let mut opt_complete = match initial_read {
            RecvType::Blocking => Some(spin_read(recv)?),
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
            let job = self.jobs_pending.remove(id).unwrap();
            if job.run {
                let context =
                    AnyContext::for_work(fe_root, be_root, id, job.read_access, job.write_access);
                log::debug!("Exec {:?}", id);
                job.work
                    .expect("{id:?} should have work!")
                    .exec(context)
                    .unwrap_or_else(|e| panic!("{id:?} failed: {e:?}"));
                assert!(
                    self.success.insert(id.clone()),
                    "We just did {id:?} a second time?"
                );
                self.handle_success(fe_root, id.clone());
            }
        }
        self.success.difference(&pre_success).cloned().collect()
    }
}
