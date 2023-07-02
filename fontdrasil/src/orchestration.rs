//! Common parts of work orchestration.

use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    hash::Hash,
    sync::Arc,
};

pub const MISSING_DATA: &str = "Missing data, dependency management failed us?";

/// A rule that represents whether access to something is permitted.
#[derive(Clone)]
pub enum Access<I> {
    /// No access is permitted
    None,
    /// Any access is permitted
    All,
    /// Access to one specific resource is permitted
    One(I),
    /// Access to multiple resources is permitted
    Set(HashSet<I>),
    /// A closure is used to determine access
    Custom(Arc<dyn Fn(&I) -> bool + Send + Sync>),
}

/// A unit of work safe to run in parallel
///
/// Naively you'd think we'd just return FnOnce + Send but that didn't want to compile
/// See <https://github.com/rust-lang/rust/issues/29625>.
///
/// Data produced by work is written into a Context (type parameter C). The identifier
/// type for work, I, is used to identify the task itself as well as the slots this work
/// might wish to access in the Context.
pub trait Work<C, I, E> {
    /// The identifier for this work
    fn id(&self) -> I;

    /// The identifier(s) for any work beyond ourself that is completed when this work completes.
    ///
    /// By default an empty vector, indicating only our own id completes.
    ///
    /// For example, Glyf work might write Loca at the same time and would then
    /// return the id for Loca here.
    fn also_completes(&self) -> Vec<I> {
        Vec::new()
    }

    fn exec(&self, context: &C) -> Result<(), E>;
}

/// Access control for context
///
/// Not meant to prevent malicious access, merely to detect mistakes
/// because the result of mistaken concurrent access can be confusing to track down.
pub struct AccessControlList<I> {
    // Returns true if you can write to the provided id
    write_access: Access<I>,

    // Returns true if you can read the provided id
    read_access: Access<I>,
}

impl<I: Eq + Hash + Debug + Send + Sync + 'static> AccessControlList<I> {
    pub fn read_only() -> AccessControlList<I> {
        AccessControlList {
            write_access: Access::none(),
            read_access: Access::all(),
        }
    }

    pub fn read_write(read_access: Access<I>, write_access: Access<I>) -> AccessControlList<I> {
        AccessControlList {
            write_access,
            read_access,
        }
    }
}

impl<I: Eq + Hash> Access<I> {
    /// Create a new access rule with custom logic
    pub fn custom<F: Fn(&I) -> bool + Send + Sync + 'static>(func: F) -> Self {
        Access::Custom(Arc::new(func))
    }

    pub fn all() -> Self {
        Self::All
    }

    pub fn none() -> Self {
        Self::None
    }

    pub fn one(allow_id: I) -> Self
    where
        I: Eq + Send + Sync + 'static,
    {
        Self::One(allow_id)
    }

    /// Check whether a given id is allowed per this rule.
    pub fn check(&self, id: &I) -> bool {
        match self {
            Access::None => false,
            Access::All => true,
            Access::One(allow) => id == allow,
            Access::Set(ids) => ids.contains(id),
            Access::Custom(f) => f(id),
        }
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

fn assert_access_many<I: Eq + Hash + Debug>(
    demand: AccessCheck,
    access: &Access<I>,
    ids: &[I],
    desc: &str,
) {
    let allow = match demand {
        AccessCheck::All => ids.iter().all(|id| access.check(id)),
        AccessCheck::Any => ids.iter().any(|id| access.check(id)),
    };
    if !allow {
        panic!("Illegal {desc} of {demand} {ids:?}");
    }
}

fn assert_access_one<I: Eq + Hash + Debug>(access: &Access<I>, id: &I, desc: &str) {
    let allow = access.check(id);
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
