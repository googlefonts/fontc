use std::borrow::Cow;

use graph::{Node, WorkGraph};
use resource::{AnyResource, ResourceIdentifier, ResourceRequest, ResourceStore};

mod graph;
mod resource;

struct Fontc {
    resources: ResourceStore,
}

/// Identifies a particular task, and describes its dependencies.
///
/// A given `TaskDescription` may have multiple possible implementors; for instance
/// tasks that involve producing IR will have an implementation for each
/// supported input format.
trait Task {
    /// The identifier for the resource produced by this task.
    fn identifier(&self) -> ResourceIdentifier;
    /// Identifiers for resources that must be available to run this task.
    fn dependencies(&self) -> Cow<[ResourceIdentifier]>;
    /// Run the task.
    fn exec(&self) -> Result<AnyResource, Error>;
}

struct AnyTask(Box<dyn Task>);

trait Provider {
    fn make_task(&self, for_resource: &ResourceIdentifier) -> Option<AnyTask>;
    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

struct ProviderSet {
    providers: Vec<Box<dyn Provider>>,
}

enum Error {
    DidMistake,
    MissingResource(ResourceIdentifier),
}

impl Fontc {
    fn run<T: ResourceRequest>(&mut self, resource: T) -> Result<&T::Output, Error> {
        let ident = resource.identifier();
        if let Some(existing) = self.resources.get(&ident) {
            return Ok(existing.downcast::<T::Output>().unwrap());
        }
        let graph = self.build_work_graph(ident);
        Err(Error::DidMistake)
        //
    }

    ///// A graph of dependencies required to complete a task
    //fn build_work_graph(&self, for_resource: ResourceIdentifier) -> Result<WorkGraph, Error> {
    //let mut graph = WorkGraph::default();
    //let mut queue = VecDeque::from([(&for_resource, graph.next_id())]);
    //while let Some((ident, id)) = queue.pop_front() {
    //let task = self.find_task(ident)?;
    //let mut node = Node::new(id);

    //for resource in task.0.dependencies().iter() {
    //// this work has already been done, so we can skip
    //if self.resources.contains(resource) {
    //continue;
    //}

    //let next_id = graph.next_id();
    ////node.add_dep(next_id);
    ////queue.push_back((resource));
    ////next_id += 1;
    //}
    ////
    //}
    //graph
    //}

    //fn produce
}

enum ProviderError {
    NoProvider(ResourceIdentifier),
    DuplicateProvider {
        resource: ResourceIdentifier,
        first: &'static str,
        second: &'static str,
    },
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
            result = match (result.take(), provider.make_task(for_resource)) {
                (None, None) => None,
                (None, Some(task)) => Some((task, provider.type_name())),
                (Some((prev, first)), Some(task)) => {
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
