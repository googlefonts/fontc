//! Type erased storage for... stuff

use std::{
    any::{Any, TypeId},
    collections::HashMap,
    sync::Arc,
};

/// Stores all the intermediate state
pub(crate) struct ResourceStore {
    storage: HashMap<ResourceIdentifier, AnyResource>,
}

pub(crate) struct AnyResource {
    inner: Arc<dyn Any>,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ResourceIdentifier {
    /// The type of the generated resource
    type_id: TypeId,
    /// For resources with multiple instances (glyphs, e.g.) some extra token
    /// that uniquely identifies a specific instance
    instance_id: (), //TODO
}

pub trait ResourceRequest {
    type Output;
    fn identifier(&self) -> ResourceIdentifier;
}

impl ResourceStore {
    pub fn get(&self, ident: &ResourceIdentifier) -> Option<&AnyResource> {
        self.storage.get(ident)
    }

    pub fn contains(&self, ident: &ResourceIdentifier) -> bool {
        self.storage.contains_key(ident)
    }
}

impl AnyResource {
    pub fn downcast<T: Any>(&self) -> Option<&T> {
        self.inner.downcast_ref()
    }
}
