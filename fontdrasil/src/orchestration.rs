//! Common parts of work orchestration.

use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::Arc,
};

pub const MISSING_DATA: &str = "Missing data, dependency management failed us?";

/// A type that represents whether access to something is permitted.
#[derive(Clone)]
pub struct AccessFn<I>(Arc<dyn Fn(&I) -> bool + Send + Sync>);

// deref to a function so that you can use fn call syntax:
impl<I> std::ops::Deref for AccessFn<I> {
    type Target = dyn Fn(&I) -> bool + Send + Sync;
    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

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
            write_access: AccessFn::none(),
            read_access: AccessFn::all(),
        }
    }

    pub fn read_write(read_access: AccessFn<I>, write_access: AccessFn<I>) -> AccessControlList<I> {
        AccessControlList {
            write_access,
            read_access,
        }
    }
}

impl<I: Eq> AccessFn<I> {
    /// Create a new `AccessFn` from the provided closer
    pub fn new<F: Fn(&I) -> bool + Send + Sync + 'static>(func: F) -> Self {
        AccessFn(Arc::new(func))
    }

    pub fn all() -> Self {
        Self::new(|_| true)
    }

    pub fn none() -> Self {
        Self::new(|_| false)
    }

    pub fn one(allow_id: I) -> Self
    where
        I: Eq + Send + Sync + 'static,
    {
        Self::new(move |id| id == &allow_id)
    }

    pub fn call(&self, id: &I) -> bool {
        self.0(id)
    }
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

#[allow(clippy::redundant_closure)] // a spurious warning
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
