//! Common parts of work orchestration.

use std::{collections::HashSet, fmt::Debug, hash::Hash};

pub const MISSING_DATA: &str = "Missing data, dependency management failed us?";

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
    I: Copy + Eq + Hash + Debug,
{
    // If present, the one and only key you are allowed to write to
    // None means we're read-only
    write_mask: Option<I>,

    // If present, what you can access through this context
    // Intent is root has None, task-specific Context only allows access to dependencies
    read_mask: Option<HashSet<I>>,
}

impl<I: Copy + Eq + Hash + Debug> AccessControlList<I> {
    pub fn read_only() -> AccessControlList<I> {
        AccessControlList {
            write_mask: None,
            read_mask: None,
        }
    }

    pub fn read_write(read: HashSet<I>, write: I) -> AccessControlList<I> {
        AccessControlList {
            write_mask: Some(write),
            read_mask: Some(read),
        }
    }
}

impl<I: Copy + Eq + Hash + Debug> AccessControlList<I> {
    pub fn check_read_access(&self, id: I) {
        if !self
            .read_mask
            .as_ref()
            .map(|mask| mask.contains(&id))
            .unwrap_or(true)
        {
            panic!("Illegal access");
        }
    }

    pub fn check_write_access(&self, id: I) {
        if self.write_mask.as_ref() != Some(&id) {
            panic!("Illegal access to {:?}", id);
        }
    }
}
