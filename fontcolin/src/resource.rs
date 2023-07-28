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
    // just used for debugging
    type_name: &'static str,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ResourceIdentifier {
    /// The type of the generated resource
    type_id: TypeId,
    /// For resources with multiple instances (glyphs, e.g.) some extra token
    /// that uniquely identifies a specific instance
    instance_id: (), //TODO
}

/// A type that identifies s specific resource.
///
/// In the simple/common case (where there is only a single representation of a
/// given resource) this is implemented by the resource itself. In cases where
/// a given resource type may have different instances (such as with glyphs) it
/// is implemented for some marker type.
///
/// # NOTE:
///
/// is this going to be pulling its weight? this is unclear.
pub trait ResourceRequest {
    type Output: 'static;
    fn identifier(&self) -> ResourceIdentifier;
}

enum ResourceError {
    Missing(&'static str),
    WrongType {
        expected: &'static str,
        found: &'static str,
    },
}

#[macro_export]
macro_rules! resource {
    ($ident:ident => $ty:ty) => {
        pub struct $ident;
        impl $crate::resource::ResourceRequest for $ident {
            type Output = $ty;
            fn identifier(&self) -> $crate::resource::ResourceIdentifier {
                ResourceIdentifier {
                    type_id: std::any::TypeId::of::<$ty>(),
                    instance_id: (),
                }
            }
        }
    };
}

impl ResourceIdentifier {
    pub fn matches_type<T: 'static>(&self) -> bool {
        TypeId::of::<T>() == self.type_id
    }
}

impl ResourceStore {
    pub fn get_any(&self, ident: &ResourceIdentifier) -> Option<&AnyResource> {
        self.storage.get(ident)
    }

    pub fn get<T: ResourceRequest>(&self, res: &T) -> Result<&T::Output, ResourceError> {
        self.get_any(&res.identifier())
            .ok_or_else(|| ResourceError::Missing(std::any::type_name::<T>()))
            .and_then(AnyResource::downcast)
    }

    pub fn contains(&self, ident: &ResourceIdentifier) -> bool {
        self.storage.contains_key(ident)
    }
}

impl AnyResource {
    pub fn new<T: Any>(inner: T) -> Self {
        Self {
            inner: Arc::new(inner),
            type_name: std::any::type_name::<T>(),
        }
    }

    pub fn downcast<T: Any>(&self) -> Result<&T, ResourceError> {
        self.inner
            .downcast_ref()
            .ok_or_else(|| ResourceError::WrongType {
                expected: std::any::type_name::<T>(),
                found: self.type_name,
            })
    }
}
