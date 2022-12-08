//! Helps coordinate the graph execution
//!
//! Eventually has to move outside fontir to let us use for BE work

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use parking_lot::RwLock;

use crate::ir::{GlyphIr, StaticMetadata};

// Unique identifier of work. If there are no fields work is unique.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum WorkIdentifier {
    GlobalMetadata,
    GlyphIr(u32),
    FinishIr,
}

const MISSING_DATA: &str = "Missing data, dependency management failed us?";

/// Read/write access to data for async work.
///
/// Intent is a root orchestrator creates a context and share copies with restricted
/// access with spawned tasks. Copies with access control are created to detect bad
/// execution order / mistakes, not to block actual bad actors.
pub struct Context {
    // If present, the one and only key you are allowed to write to
    // Otherwise you totally get to write whatever you like
    write_mask: Option<WorkIdentifier>,

    // If present, what you can access through this context
    // Intent is root has None, task-specific Context only allows access to dependencies
    read_mask: Option<HashSet<WorkIdentifier>>,

    // work results we've completed or restored from disk
    // We create individual caches so we can return typed results from get fns
    static_metadata: Cache<Option<Arc<StaticMetadata>>>,
    glyph_ir: Cache<HashMap<u32, Arc<GlyphIr>>>,
}

#[derive(Clone)]
struct Cache<T: Default> {
    item: Arc<RwLock<T>>,
}

impl<T: Default> Cache<T> {
    fn new() -> Cache<T> {
        Cache {
            item: Default::default(),
        }
    }
}

impl Context {
    pub fn new_root() -> Context {
        Context {
            write_mask: None,
            read_mask: None,
            static_metadata: Cache::new(),
            glyph_ir: Cache::new(),
        }
    }
}

impl Context {
    pub fn copy_for_work(
        &self,
        work_id: WorkIdentifier,
        dependencies: HashSet<WorkIdentifier>,
    ) -> Context {
        Context {
            write_mask: Some(work_id),
            read_mask: Some(dependencies),
            static_metadata: self.static_metadata.clone(),
            glyph_ir: self.glyph_ir.clone(),
        }
    }

    fn check_read_access(&self, id: &WorkIdentifier) {
        if !self
            .read_mask
            .as_ref()
            .map(|mask| mask.contains(id))
            .unwrap_or(true)
        {
            panic!("Illegal access");
        }
    }

    fn check_write_access(&self, id: &WorkIdentifier) {
        if !self.write_mask.map(|mask| mask == *id).unwrap_or(true) {
            panic!("Illegal access");
        }
    }

    pub fn get_static_metadata(&self) -> Arc<StaticMetadata> {
        self.check_read_access(&WorkIdentifier::GlobalMetadata);
        let rl = self.static_metadata.item.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_static_metadata(&self, global_metadata: StaticMetadata) {
        self.check_write_access(&WorkIdentifier::GlobalMetadata);
        let mut wl = self.static_metadata.item.write();
        *wl = Some(Arc::from(global_metadata));
    }

    pub fn get_glyph_ir(&self, glyph_order: u32) -> Arc<GlyphIr> {
        self.check_read_access(&WorkIdentifier::GlyphIr(glyph_order));
        let rl = self.glyph_ir.item.read();
        rl.get(&glyph_order).expect(MISSING_DATA).clone()
    }

    pub fn set_glyph_ir(&self, glyph_order: u32, ir: GlyphIr) {
        self.check_write_access(&WorkIdentifier::GlyphIr(glyph_order));
        let mut wl = self.glyph_ir.item.write();
        wl.insert(glyph_order, Arc::from(ir));
    }
}
