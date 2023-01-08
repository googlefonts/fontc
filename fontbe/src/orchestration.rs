//! Helps coordinate the graph execution for BE

use std::{collections::HashSet, sync::Arc};

use crate::paths::Paths;

// Unique identifier of work. If there are no fields work is unique.
// Meant to be small and cheap to copy around.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WorkIdentifier {
    Glyph(String),
    GlyphMerge,
    FinalMerge,
}

/// Read/write access to data for async work.
///
/// Intent is a root orchestrator creates a context and share copies with restricted
/// access with spawned tasks. Copies with access control are created to detect bad
/// execution order / mistakes, not to block actual bad actors.
#[allow(dead_code)] // TEMPORARY
pub struct Context {
    paths: Arc<Paths>,

    // The input we're working on. Note that change detection may mean we only process
    // a subset of the full input.
    pub input: Arc<fontir::orchestration::Context>,

    // If present, the one and only key you are allowed to write to
    // Otherwise you totally get to write whatever you like
    write_mask: Option<WorkIdentifier>,

    // If present, what you can access through this context
    // Intent is root has None, task-specific Context only allows access to dependencies
    read_mask: Option<HashSet<WorkIdentifier>>,
    // work results we've completed or restored from disk
    // We create individual caches so we can return typed results from get fns
}
