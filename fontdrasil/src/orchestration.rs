//! Common parts of work orchestration.

use std::{
    collections::HashSet,
    fmt::Debug,
    hash::Hash,
    sync::{Arc, RwLock},
};

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

#[derive(Clone)]
pub struct Cache<T: Default> {
    pub item: Arc<RwLock<T>>,
}

impl<T: Default> Cache<T> {
    pub fn new() -> Cache<T> {
        Cache {
            item: Default::default(),
        }
    }
}

impl<T: Default> Default for Cache<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Tracks what parts of a context we can read/write.
///
/// Not meant to prevent malicious access, merely to detect mistakes
/// as the result of mistaken concurrent access can be confusing to track down.
pub struct Acl<I>
where
    I: Eq + Hash + Debug,
{
    // If present, the one and only key you are allowed to write to
    // None means we're read-only
    write_mask: Option<I>,

    // If present, what you can access through this context
    // Intent is root has None, task-specific Context only allows access to dependencies
    read_mask: Option<HashSet<I>>,
}

impl<I: Eq + Hash + Debug> Acl<I> {
    pub fn read_only() -> Acl<I> {
        Acl {
            write_mask: None,
            read_mask: None,
        }
    }

    pub fn read_write(read: HashSet<I>, write: I) -> Acl<I> {
        Acl {
            write_mask: Some(write),
            read_mask: Some(read),
        }
    }
}

impl<I: Eq + Hash + Debug> Acl<I> {
    pub fn check_read_access(&self, id: &I) {
        if !self
            .read_mask
            .as_ref()
            .map(|mask| mask.contains(id))
            .unwrap_or(true)
        {
            panic!("Illegal access");
        }
    }

    pub fn check_write_access(&self, id: &I) {
        if !self
            .write_mask
            .as_ref()
            .map(|mask| mask == id)
            .unwrap_or(false)
        {
            panic!("Illegal access to {:?}", id);
        }
    }
}

pub fn glyph_file(glyph_name: &str, suffix: &str) -> String {
    // TODO handle names that are invalid for the filesystem
    // Ref https://github.com/unified-font-object/ufo-spec/issues/164
    glyph_name.to_owned() + suffix
}
