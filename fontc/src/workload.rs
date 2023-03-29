//! Tracking jobs to run

use std::collections::{HashMap, HashSet};

use crossbeam_channel::{Receiver, TryRecvError};
use fontbe::orchestration::{AnyWorkId, Context as BeContext, WorkId as BeWorkIdentifier};
use fontdrasil::orchestration::Access;
use fontir::orchestration::{Context as FeContext, WorkId as FeWorkIdentifier};

use crate::{
    work::{AnyContext, AnyWork, AnyWorkError, ReadAccess},
    Error,
};

/// A set of interdependent jobs to execute.
#[derive(Default)]
pub struct Workload {
    pre_success: usize,
    job_count: usize,
    success: HashSet<AnyWorkId>,
    error: Vec<(AnyWorkId, String)>,
    pub(crate) jobs_pending: HashMap<AnyWorkId, Job>,
}

/// A unit of executable work plus the identifiers of work that it depends on
pub(crate) struct Job {
    // The actual task
    pub(crate) work: AnyWork,
    // Things that must happen before we execute. Our  task can read these things.
    pub(crate) dependencies: HashSet<AnyWorkId>,
    // Things our job needs read access to. Usually all our dependencies.
    pub(crate) read_access: ReadAccess,
    // Things our job needs write access to
    pub(crate) write_access: Access<AnyWorkId>,
}

enum RecvType {
    Blocking,
    NonBlocking,
}

impl Workload {
    pub fn new() -> Workload {
        Self::default()
    }

    pub(crate) fn insert(&mut self, id: AnyWorkId, job: Job) {
        self.jobs_pending.insert(id, job);
        self.job_count += 1;
    }

    pub(crate) fn is_dependency(&mut self, id: &AnyWorkId, dep: &AnyWorkId) -> bool {
        self.jobs_pending
            .get(id)
            .map(|job| job.dependencies.contains(dep))
            .unwrap_or(false)
    }

    pub(crate) fn mark_success(&mut self, id: impl Into<AnyWorkId>) {
        if self.success.insert(id.into()) {
            self.pre_success += 1;
        }
    }

    fn handle_success(&mut self, fe_root: &FeContext, success: AnyWorkId) {
        log::debug!("{success:?} successful");
        match success {
            // When a glyph finishes IR, register BE work for it
            AnyWorkId::Fe(FeWorkIdentifier::Glyph(glyph_name)) => {
                super::add_glyph_be_job(self, fe_root, glyph_name)
            }

            // When static metadata finalizes, add BE work for any new glyphs
            // If anything progresses before we've done this we have a graph bug
            AnyWorkId::Fe(FeWorkIdentifier::FinalizeStaticMetadata) => {
                for glyph_name in fe_root.get_final_static_metadata().glyph_order.iter() {
                    let id = AnyWorkId::Be(BeWorkIdentifier::Glyph(glyph_name.clone()));
                    if self.jobs_pending.contains_key(&id) {
                        continue;
                    }

                    log::debug!("Updating graph for new glyph {glyph_name}");

                    // It would be lovely if our new glyph was in glyf and hmtx
                    // loca hides with glyf
                    for merge_id in [BeWorkIdentifier::Glyf, BeWorkIdentifier::Hmtx] {
                        self.jobs_pending
                            .get_mut(&AnyWorkId::Be(merge_id))
                            .unwrap()
                            .dependencies
                            .insert(id.clone());
                    }

                    super::add_glyph_be_job(self, fe_root, glyph_name.clone());
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

    pub fn launchable(&self) -> Vec<AnyWorkId> {
        self.jobs_pending
            .iter()
            .filter_map(|(id, job)| {
                let can_start = job.dependencies.is_subset(&self.success);
                if !can_start && log::log_enabled!(log::Level::Trace) {
                    let mut unfulfilled_deps: Vec<_> =
                        job.dependencies.difference(&self.success).collect();
                    unfulfilled_deps.sort();
                };
                can_start.then(|| id.clone())
            })
            .collect()
    }

    pub fn exec(mut self, fe_root: FeContext, be_root: BeContext) -> Result<(), Error> {
        // Async work will send us it's ID on completion
        let (send, recv) = crossbeam_channel::unbounded::<(AnyWorkId, Result<(), AnyWorkError>)>();

        rayon::in_place_scope(|scope| {
            // Whenever a task completes see if it was the last incomplete dependency of other task(s)
            // and spawn them if it was
            // TODO timeout and die it if takes too long to make forward progress or we're spinning w/o progress
            while self.success.len() < self.job_count + self.pre_success {
                // Spawn anything that is currently executable (has no unfulfilled dependencies)
                for id in self.launchable() {
                    log::trace!("Start {:?}", id);
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
                            log::error!("Unable to write {:?} to completion channel: {}", id, e);
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
                    log::error!("{:?} failed {:?}", completed_id, e);
                    self.error.push((completed_id.clone(), format!("{e}")));
                    true
                }
            } {
                panic!("Repeat signals for completion of {completed_id:#?}");
            }
            log::debug!(
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

    /// Run all pending jobs.
    ///
    /// Returns the set of ids for tasks that are finished as a result of this run.
    #[cfg(test)]
    pub fn run_for_test(&mut self, fe_root: &FeContext, be_root: &BeContext) -> HashSet<AnyWorkId> {
        let pre_success = self.success.clone();
        while !self.jobs_pending.is_empty() {
            let launchable = self.launchable();
            if launchable.is_empty() {
                log::error!("Completed:");
                for id in self.success.iter() {
                    log::error!("  {id:?}");
                }
                log::error!("Unable to proceed with:");
                for (id, job) in self.jobs_pending.iter() {
                    log::error!("  {:?}, happens-after {:?}", id, job.dependencies);
                }
                assert!(
                    !launchable.is_empty(),
                    "Unable to make forward progress, bad graph?"
                );
            }

            let id = &launchable[0];
            let job = self.jobs_pending.remove(id).unwrap();
            let context = AnyContext::for_work(
                fe_root,
                be_root,
                id,
                job.dependencies,
                job.read_access,
                job.write_access,
            );
            log::debug!("Exec {:?}", id);
            job.work.exec(context).unwrap();
            assert!(
                self.success.insert(id.clone()),
                "We just did {id:?} a second time?"
            );

            self.handle_success(fe_root, id.clone());
        }
        self.success.difference(&pre_success).cloned().collect()
    }
}
