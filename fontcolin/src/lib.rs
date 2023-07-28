use std::{error::Error as StdError, path::PathBuf};

use graph::WorkGraph;
use resource::{AnyResource, ResourceIdentifier, ResourceRequest, ResourceStore};

mod graph;
mod providers;
mod resource;

struct Fontc {
    resources: ResourceStore,
    providers: ProviderSet,
}

/// Identifies a particular task, and describes its dependencies.
///
/// A given `TaskDescription` may have multiple possible implementors; for instance
/// tasks that involve producing IR will have an implementation for each
/// supported input format.
trait Task {
    ///// The identifier for the resource produced by this task.
    //fn identifier(&self) -> ResourceIdentifier;
    /// Identifiers for resources that must be available to run this task.
    fn dependencies(&self) -> Dependencies;
    /// Run the task.
    fn exec(&self, ctx: &CompilationContext) -> Result<AnyResource, Error>;
}

// how do we represent deps? idk, but let's name a type now and we can add
// variants later
#[derive(Default)]
pub enum Dependencies {
    #[default]
    None,
    Slice(&'static [ResourceIdentifier]),
}

struct AnyTask(Box<dyn Task>);

/// A type that is capable of creating tasks that produce resources.
trait Provider {
    fn provide_task(&self, for_resource: &ResourceIdentifier) -> Option<AnyTask>;
    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

struct ProviderSet {
    providers: Vec<Box<dyn Provider>>,
}

pub(crate) struct CompilationContext<'a> {
    resources: &'a ResourceStore,
    args: &'a Args,
}

struct Args {
    source_path: PathBuf,
}

impl Fontc {
    fn run<T: ResourceRequest>(&mut self, resource: T) -> Result<&T::Output, Error> {
        if let Ok(existing) = self.resources.get(&resource) {
            return Ok(existing);
        }
        let ident = resource.identifier();
        let _graph = WorkGraph::new(ident, &self.resources, &self.providers)?;
        Err(Error::DidMistake)
    }
}

impl ProviderSet {
    /// Find a task capable of producing the specified resource.
    ///
    /// There should always be exactly one provider for any task; if this is not
    /// the case it indicates a bug.
    ///
    /// TODO: should this even be an error?
    fn find_task(&self, for_resource: &ResourceIdentifier) -> Result<AnyTask, ProviderError> {
        let mut result: Option<(AnyTask, &'static str)> = None;
        for provider in &self.providers {
            result = match (result.take(), provider.provide_task(for_resource)) {
                (None, None) => None,
                (None, Some(task)) => Some((task, provider.type_name())),
                (Some((_prev, first)), Some(_)) => {
                    return Err(ProviderError::DuplicateProvider {
                        resource: for_resource.to_owned(),
                        first,
                        second: provider.type_name(),
                    });
                }
                (some, None) => some,
            };
        }
        result
            .map(|(task, _)| task)
            .ok_or_else(|| ProviderError::NoProvider(for_resource.to_owned()))
    }
}

impl<T: Task + 'static> From<T> for AnyTask {
    fn from(value: T) -> Self {
        AnyTask(Box::new(value))
    }
}

#[derive(thiserror::Error, Debug)]
enum Error {
    #[error("oh no")]
    DidMistake,
    #[error("{0}")]
    ProviderError(#[from] ProviderError),
    #[error("failed to read {path}: '{inner}'")]
    CantReadFile {
        path: PathBuf,
        #[source]
        inner: Box<dyn StdError>,
    },
}

#[derive(thiserror::Error, Debug)]
enum ProviderError {
    #[error("No provider for resource {0:?}")]
    NoProvider(ResourceIdentifier),
    #[error("Multiple providers for resource {resource:?}: '{first}', '{second}'")]
    DuplicateProvider {
        resource: ResourceIdentifier,
        first: &'static str,
        second: &'static str,
    },
}
