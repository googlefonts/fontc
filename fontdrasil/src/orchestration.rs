//! Common parts of work orchestration.

use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    hash::Hash,
};

pub const MISSING_DATA: &str = "Missing data, dependency management failed us?";

/// Identifies all items of the same group, typically enum variant, of an identifer
///
/// String because it's enormously easier to understand debug output that way
pub type IdentifierDiscriminant = &'static str;

/// A type that affords identity.
///
/// Frequently copied, used in hashmap/sets and printed to logs, hence the trait list.
pub trait Identifier: Debug + Clone + Eq + Hash {
    /// Return a value that is consistent across all instances in the same group, e.g. enum variant.
    fn discriminant(&self) -> IdentifierDiscriminant;
}

pub struct AccessBuilder<I: Identifier> {
    access: Access<I>,
}

impl<I: Identifier> Default for AccessBuilder<I> {
    fn default() -> Self {
        Self {
            access: Access::None,
        }
    }
}

impl<I: Identifier> AccessBuilder<I> {
    pub fn new() -> Self {
        Self::default()
    }

    fn add_access(mut self, id: AccessType<I>) -> Self {
        self.access = match self.access {
            Access::All => self.access,
            Access::None => match id {
                AccessType::SpecificInstanceOfVariant(id) => Access::SpecificInstanceOfVariant(id),
                AccessType::Variant(id) => Access::Variant(id),
            },
            Access::SpecificInstanceOfVariant(prior_id) => Access::Set(HashSet::from([
                AccessType::SpecificInstanceOfVariant(prior_id),
                id,
            ])),
            Access::Variant(prior_id) => {
                Access::Set(HashSet::from([AccessType::Variant(prior_id), id]))
            }
            Access::Set(mut ids) => {
                ids.insert(id);
                Access::Set(ids)
            }
            Access::Unknown => panic!("Cannot combine {:?} and {id:?}", self.access),
        };
        self
    }

    /// Access to all examples of an identifier is required.
    ///
    /// For example, an identifier where there can only be a single example
    /// such as for a unit-variant of an enum or where access to all examples
    /// is required, such as access to IR for any glyph.
    pub fn variant(self, id: impl Into<I>) -> Self {
        self.add_access(AccessType::Variant(id.into()))
    }

    /// Access to a specific example of a general class is required.
    ///
    /// For example, the glyph variations of a specific glyph. If there can only
    /// be one instance of the class, such as a unit-variant of an enum, using
    /// this method results in less efficient processing than
    pub fn specific_instance(self, id: impl Into<I>) -> Self {
        self.add_access(AccessType::SpecificInstanceOfVariant(id.into()))
    }

    pub fn build(self) -> Access<I> {
        self.access
    }
}

/// A rule that represents whether access to something is permitted.
#[derive(Clone)]
pub enum Access<I: Identifier> {
    /// Nothing is accessed
    None,
    /// Access requirements not yet known. Intended to be used when what access is needed
    /// isn't initially known. Once known, access will change to some other option.
    Unknown,
    /// Any access is permitted
    All,
    /// Access to a specific example of a general class is required.
    ///
    /// For example, the glyph variations of a specific glyph. If there can only
    /// be one instance of the class, such as a unit-variant of an enum, using
    /// this method results in less efficient processing than [`Access::Variant`]
    SpecificInstanceOfVariant(I),
    /// Access to all examples of an identifier is required.
    ///
    /// For example, an identifier where there can only be a single example
    /// such as for a unit-variant of an enum or where access to all examples
    /// is required, such as access to IR for any glyph.
    Variant(I),
    /// Access to multiple resources or classes of resource is permitted
    Set(HashSet<AccessType<I>>),
}

impl<I: Identifier> Debug for Access<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "None"),
            Self::Unknown => write!(f, "Unknown"),
            Self::All => write!(f, "All"),
            Self::SpecificInstanceOfVariant(id) => f.debug_tuple("Specific").field(id).finish(),
            Self::Variant(id) => f.debug_tuple("Any").field(id).finish(),
            Self::Set(ids) => f.debug_tuple("Set").field(ids).finish(),
        }
    }
}

/// A unit of work safe to run in parallel
///
/// Naively you'd think we'd just return FnOnce + Send but that didn't want to compile
/// See <https://github.com/rust-lang/rust/issues/29625>.
///
/// Data produced by work is written into a Context (type parameter C). The identifier
/// type for work, I, is used to identify the task itself as well as the slots this work
/// might wish to access in the Context.
pub trait Work<C, I, E>: Debug
where
    I: Identifier,
{
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

    /// What this work needs to be able to read; our dependencies
    ///
    /// Anything we can read should be completed before we execute.
    ///
    /// The default is no access.
    fn read_access(&self) -> Access<I> {
        Access::None
    }

    /// What this work needs to be able to write.
    ///
    /// Defaults to our own id plus anything we also complete.
    fn write_access(&self) -> Access<I> {
        let mut also = self.also_completes();
        if also.is_empty() {
            return Access::SpecificInstanceOfVariant(self.id());
        }
        also.push(self.id());
        Access::Set(
            also.into_iter()
                .map(|id| AccessType::SpecificInstanceOfVariant(id))
                .collect(),
        )
    }

    fn exec(&self, context: &C) -> Result<(), E>;
}

/// Access control for context
///
/// Not meant to prevent malicious access, merely to detect mistakes
/// because the result of mistaken concurrent access can be confusing to track down.
#[derive(Clone)]
pub struct AccessControlList<I: Identifier> {
    // Returns true if you can write to the provided id
    write_access: Access<I>,

    // Returns true if you can read the provided id
    read_access: Access<I>,
}

impl<I: Identifier> Default for AccessControlList<I> {
    fn default() -> Self {
        Self {
            write_access: Access::None,
            read_access: Access::None,
        }
    }
}

impl<I: Identifier + Send + Sync + 'static> AccessControlList<I> {
    pub fn read_only() -> AccessControlList<I> {
        AccessControlList {
            write_access: Access::None,
            read_access: Access::All,
        }
    }

    pub fn read_write(read_access: Access<I>, write_access: Access<I>) -> AccessControlList<I> {
        AccessControlList {
            write_access,
            read_access,
        }
    }
}

impl<I: Identifier> Access<I> {
    /// Check whether a given id is allowed per this rule.
    pub fn check(&self, id: &I) -> bool {
        match self {
            Access::None => false,
            Access::Unknown => false,
            Access::All => true,
            Access::SpecificInstanceOfVariant(all) => all == id,
            Access::Variant(all) => all.discriminant() == id.discriminant(),
            Access::Set(ids) => ids.iter().any(|allow| allow.check(id)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AccessType<I: Identifier> {
    Variant(I),
    SpecificInstanceOfVariant(I),
}

impl<I: Identifier> AccessType<I> {
    fn check(&self, id: &I) -> bool {
        match self {
            AccessType::Variant(d) => d.discriminant() == id.discriminant(),
            AccessType::SpecificInstanceOfVariant(one) => one == id,
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

fn assert_access_many<I: Identifier>(
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
        panic!("Illegal {desc} of {demand} {ids:?}. {desc} access: {access:?}");
    }
}

fn assert_access_one<I: Identifier>(access: &Access<I>, id: &I, desc: &str) {
    let allow = access.check(id);
    if !allow {
        panic!("Illegal {desc} of {id:?}. {desc} access: {access:?}");
    }
}

impl<I: Identifier> AccessControlList<I> {
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
