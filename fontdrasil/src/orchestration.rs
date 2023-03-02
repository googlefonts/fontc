//! Common parts of work orchestration.

use std::{collections::HashSet, fmt::Debug, hash::Hash, sync::Arc};

pub const MISSING_DATA: &str = "Missing data, dependency management failed us?";

/// A function that indicates whether access to something is permitted.
pub type AccessFn<I> = Arc<dyn Fn(&I) -> bool + Send + Sync>;

/// A unit of work safe to run in parallel
///
/// Naively you'd think we'd just return FnOnce + Send but that didn't want to compile
/// See <https://github.com/rust-lang/rust/issues/29625>.
///
/// Data produced by work is written into a Context (type parameter C).
pub trait Work<C, E> {
    fn exec(&self, context: &C) -> Result<(), E>;
}

/// Access control for context
///
/// Not meant to prevent malicious access, merely to detect mistakes
/// because the result of mistaken concurrent access can be confusing to track down.
pub struct AccessControlList<I>
where
    I: Eq + Hash + Debug,
{
    // Returns true if you can write to the provided id
    write_access: AccessFn<I>,

    // If present, what you can access through this context
    // Intent is root has None, task-specific Context only allows access to dependencies
    read_mask: Option<HashSet<I>>,
}

impl<I: Eq + Hash + Debug> AccessControlList<I> {
    pub fn read_only() -> AccessControlList<I> {
        AccessControlList {
            write_access: Arc::new(|_| false),
            read_mask: None,
        }
    }

    pub fn read_write(
        read: HashSet<I>,
        write_access: Arc<dyn Fn(&I) -> bool + Send + Sync>,
    ) -> AccessControlList<I> {
        AccessControlList {
            write_access,
            read_mask: Some(read),
        }
    }
}

pub fn access_one<I: Eq + Send + Sync + 'static>(id: I) -> AccessFn<I> {
    Arc::new(move |id2| id == *id2)
}

pub fn access_none<I: Eq + Send + Sync + 'static>() -> AccessFn<I> {
    Arc::new(move |_| false)
}

impl<I: Eq + Hash + Debug> AccessControlList<I> {
    pub fn assert_read_access_to_any(&self, ids: &[I]) {
        if self.read_mask.is_none() {
            return;
        }
        let read_mask = self.read_mask.as_ref().unwrap();

        if !ids.iter().any(|id| read_mask.contains(id)) {
            panic!("Illegal access to {ids:?}");
        }
    }

    pub fn assert_read_access(&self, id: &I) {
        if !self
            .read_mask
            .as_ref()
            .map(|mask| mask.contains(id))
            .unwrap_or(true)
        {
            panic!("Illegal access to {id:?}");
        }
    }

    pub fn assert_write_access_to_any(&self, ids: &[I]) {
        if !ids.iter().any(|id| (self.write_access)(id)) {
            panic!("Illegal access to {ids:?}");
        }
    }

    pub fn assert_write_access(&self, id: &I) {
        if !(self.write_access)(id) {
            panic!("Illegal access to {id:?}");
        }
    }
}
