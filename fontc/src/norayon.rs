// Simple scope for sequential execution (replaces rayon::Scope)
pub(crate) struct SequentialScope;

impl SequentialScope {
    pub(crate) fn spawn<F>(&self, func: F)
    where
        F: FnOnce(&SequentialScope) + Send,
    {
        // Execute immediately instead of spawning a thread
        func(self);
    }
}
