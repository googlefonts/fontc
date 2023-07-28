use std::collections::{hash_map::Entry, HashMap, VecDeque};

use crate::{
    resource::{ResourceIdentifier, ResourceStore},
    AnyTask, ProviderError, ProviderSet,
};

type TaskId = usize;

#[derive(Default)]
pub(crate) struct WorkGraph {
    tasks: HashMap<TaskId, AnyTask>,
    /// a map from each task to all of its parents
    parents: HashMap<TaskId, Vec<TaskId>>,
    /// the number of outstanding depedencies for each task
    outstanding_deps: HashMap<TaskId, usize>,
}

impl WorkGraph {
    pub(crate) fn new(
        root: ResourceIdentifier,
        storage: &ResourceStore,
        providers: &ProviderSet,
    ) -> Result<Self, ProviderError> {
        GraphBuilder::new(storage, providers).build_for_root(root)
    }
}

struct GraphBuilder<'a> {
    storage: &'a ResourceStore,
    providers: &'a ProviderSet,
    tasks: HashMap<ResourceIdentifier, (AnyTask, TaskId)>,
    deps: HashMap<TaskId, Vec<TaskId>>,
    next_id: TaskId,
}

impl<'a> GraphBuilder<'a> {
    fn new(storage: &'a ResourceStore, providers: &'a ProviderSet) -> Self {
        Self {
            storage,
            providers,
            tasks: Default::default(),
            deps: Default::default(),
            next_id: 0,
        }
    }

    fn build_for_root(mut self, ident: ResourceIdentifier) -> Result<WorkGraph, ProviderError> {
        // if the root resource exists, there's nothing to do:
        if self.storage.contains(&ident) {
            return Ok(Default::default());
        }

        let mut queue = VecDeque::from([ident]);
        while let Some(resource) = queue.pop_front() {
            if self.storage.contains(&resource) {
                continue;
            }

            let (task, id) = self.get_task(resource)?;
            let id = *id;
            //debug_assert_eq!(self.deps.len(), *id);
            let mut dep_ids = Vec::new();
            for dep in task.0.dependencies().iter() {
                if !self.storage.contains(&dep) {
                    let (_, dep_id) = self.get_task(dep.clone())?;
                    dep_ids.push(*dep_id);
                    queue.push_back(dep.clone());
                }
            }
            self.deps.insert(id, dep_ids);
        }

        let tasks = self
            .tasks
            .into_values()
            .map(|(task, id)| (id, task))
            .collect();

        let mut outstanding_deps = HashMap::new();
        let mut parents = HashMap::new();
        for (id, children) in self.deps {
            outstanding_deps.insert(id, children.len());
            for child in children {
                parents.entry(child).or_insert_with(Vec::new).push(id);
            }
        }

        Ok(WorkGraph {
            tasks,
            parents,
            outstanding_deps,
        })
    }

    fn next_id(&mut self) -> TaskId {
        let next = self.next_id;
        self.next_id += 1;
        next
    }

    fn get_task(
        &mut self,
        resource: ResourceIdentifier,
    ) -> Result<&(AnyTask, TaskId), ProviderError> {
        match self.tasks.entry(resource) {
            Entry::Occupied(entry) => Ok(entry.get()),
            Entry::Vacant(entry) => {
                let task = self.providers.find_task(entry.key())?;
                let id = self.next_id();
                Ok(entry.insert((task, id)))
            }
        }
    }
}
