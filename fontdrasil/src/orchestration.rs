//! Common parts of work orchestration.

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::Arc,
};

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

    // Returns true if you can read the provided id
    read_access: AccessFn<I>,
}

impl<I: Eq + Hash + Debug + Send + Sync + 'static> AccessControlList<I> {
    pub fn read_only() -> AccessControlList<I> {
        AccessControlList {
            write_access: access_none(),
            read_access: access_all(),
        }
    }

    pub fn read_write(read_access: AccessFn<I>, write_access: AccessFn<I>) -> AccessControlList<I> {
        AccessControlList {
            write_access,
            read_access,
        }
    }
}

pub fn access_all<I: Eq + Send + Sync + 'static>() -> AccessFn<I> {
    Arc::new(move |_| true)
}

pub fn access_one<I: Eq + Send + Sync + 'static>(id: I) -> AccessFn<I> {
    Arc::new(move |id2| id == *id2)
}

pub fn access_none<I: Eq + Send + Sync + 'static>() -> AccessFn<I> {
    Arc::new(move |_| false)
}

enum AccessCheck {
    Any,
    All,
}

impl Display for AccessCheck {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AccessCheck::All => write!(f, "all"),
            AccessCheck::Any => write!(f, "any"),
        }
    }
}

fn assert_access_many<I: Eq + Hash + Debug>(
    demand: AccessCheck,
    access_fn: &AccessFn<I>,
    ids: &[I],
    desc: &str,
) {
    let allow = match demand {
        AccessCheck::All => ids.iter().all(|id| access_fn(id)),
        AccessCheck::Any => ids.iter().any(|id| access_fn(id)),
    };
    if !allow {
        panic!("Illegal {desc} of {demand} {ids:?}");
    }
}

fn assert_access_one<I: Eq + Hash + Debug>(access_fn: &AccessFn<I>, id: &I, desc: &str) {
    let allow = access_fn(id);
    if !allow {
        panic!("Illegal {desc} of {id:?}");
    }
}

impl<I: Eq + Hash + Debug> AccessControlList<I> {
    pub fn assert_read_access_to_all(&self, ids: &[I]) {
        assert_access_many(AccessCheck::All, &self.read_access, ids, "read");
    }

    pub fn assert_read_access_to_any(&self, ids: &[I]) {
        assert_access_many(AccessCheck::Any, &self.read_access, ids, "read");
    }

    pub fn assert_read_access(&self, id: &I) {
        assert_access_one(&self.read_access, id, "read");
    }

    pub fn assert_write_access_to_all(&self, ids: &[I]) {
        assert_access_many(AccessCheck::All, &self.write_access, ids, "write");
    }

    pub fn assert_write_access_to_any(&self, ids: &[I]) {
        assert_access_many(AccessCheck::Any, &self.write_access, ids, "write");
    }

    pub fn assert_write_access(&self, id: &I) {
        assert_access_one(&self.write_access, id, "write");
    }
}
