use std::{
    collections::VecDeque,
    sync::{Condvar, Mutex},
};

pub struct ThreadPool {
    n_threads: usize,
}

struct State<'env> {
    jobs: VecDeque<Box<dyn FnOnce() + Send + 'env>>,
    closed: bool,
}

pub struct Scope<'scope, 'env> {
    state: &'scope Mutex<State<'env>>,
    condvar: &'scope Condvar,
}

impl<'scope, 'env> Scope<'scope, 'env> {
    /// Submit a job to the threadpool.
    ///
    /// Jobs will be run in the order they are submitted.
    pub fn execute<F: FnOnce() + Send + 'env>(&self, f: F) {
        self.state.lock().unwrap().jobs.push_back(Box::new(f));
        self.condvar.notify_one();
    }
}

impl ThreadPool {
    pub fn new() -> Self {
        let n = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(4);
        Self { n_threads: n }
    }

    /// Run work in parallel. `f` receives a [`Scope`] and may call
    /// [`Scope::execute`] to queue closures for execution on the pool's
    /// worker threads. All submitted work is guaranteed to have completed
    /// before this method returns.
    pub fn run<'env, F>(&self, f: F)
    where
        F: FnOnce(&Scope<'_, 'env>),
    {
        let state: Mutex<State<'env>> = Mutex::new(State {
            jobs: VecDeque::new(),
            closed: false,
        });
        let condvar = Condvar::new();

        std::thread::scope(|s| {
            for _ in 0..self.n_threads {
                s.spawn(|| {
                    loop {
                        let mut guard = state.lock().unwrap();
                        loop {
                            if let Some(job) = guard.jobs.pop_front() {
                                drop(guard);
                                if let Err(e) =
                                    std::panic::catch_unwind(std::panic::AssertUnwindSafe(job))
                                {
                                    let msg = e
                                        .downcast_ref::<&str>()
                                        .copied()
                                        .or_else(|| e.downcast_ref::<String>().map(String::as_str))
                                        .unwrap_or("(unknown)");
                                    log::error!("worker thread caught panic: {msg}");
                                }
                                break;
                            } else if guard.closed {
                                return;
                            } else {
                                guard = condvar.wait(guard).unwrap();
                            }
                        }
                    }
                });
            }

            let submitter = Scope {
                state: &state,
                condvar: &condvar,
            };
            f(&submitter);

            state.lock().unwrap().closed = true;
            condvar.notify_all();
        });
    }
}
