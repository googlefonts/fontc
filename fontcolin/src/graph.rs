use std::collections::{HashMap, VecDeque};

use crate::{
    resource::{ResourceIdentifier, ResourceStore},
    AnyTask, ProviderSet,
};

type TaskId = u64;

#[derive(Default)]
pub(crate) struct WorkGraph {
    //tasks: Vec<AnyTask>,
    // stuff
}

struct GraphBuilder<'a> {
    storage: &'a ResourceStore,
    providers: &'a ProviderSet,
    resource_ids: HashMap<ResourceIdentifier, (TaskId)>,
    deps: Vec<Vec<TaskId>>,
    //tasks: HashMap<ResourceIdentifier, (AnyTask, Vec<ResourceIdentifier>)>,
    //deps: HashM
    next_id: TaskId,
}

impl<'a> GraphBuilder<'a> {
    fn new(storage: &'a ResourceStore, providers: &'a ProviderSet) -> Self {
        Self {
            storage,
            providers,
            //next_id: 0,
            //tasks: Default::default(),
            resource_ids: Default::default(),
            deps: Default::default(),
            next_id: 0,
        }
    }

    fn build_for_root(self, ident: ResourceIdentifier) -> Result<WorkGraph, Error> {
        // if the root resource exists, there's nothing to do:
        if self.storage.contains(&ident) {
            return Ok(Default::default());
        }

        let mut queue = VecDeque::from([ident]);
        while let Some(resource) = queue.pop_front() {
            if self.storage.contains(&resource) {
                continue;
            }
            // if this already exists, or if we've already seen it, skip
            //if self.existing_task_or_resource(&resource) {
            //continue;
            //}

            let task = self.providers.find_task(&resource)?;
            for dep in task.0.dependencies() {}
        }

        //
    }

    //pub(crate) fn next_id(&mut self) -> u64 {
    //let next = self.next_id;
    //self.next_id += 1;
    //next
    //}

    fn existing_task_or_resource(&self, resource: &ResourceIdentifier) -> bool {
        self.storage.contains(resource) || self.tasks.contains_key(resource)
    }
}

pub(crate) struct Node {
    id: u64,
    deps: Vec<u64>,
}

impl Node {
    pub(crate) fn new(id: u64) -> Self {
        Self {
            id,
            deps: Vec::new(),
        }
    }

    pub(crate) fn add_dep(&mut self, dep_id: u64) {
        self.deps.push(dep_id)
    }
}
