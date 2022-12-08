// Helps coordinate the graph execution
// Eventually has to move outside fontir to let us use for BE work

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use parking_lot::RwLock;

use crate::ir::{GlobalMetadata, GlyphIr};

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
    global_metadata_cache: Arc<RwLock<Option<Arc<GlobalMetadata>>>>,
    glyph_ir_cache: Arc<RwLock<HashMap<u32, Arc<GlyphIr>>>>,
}

impl Context {
    pub fn new_root() -> Context {
        Context {
            write_mask: None,
            read_mask: None,
            global_metadata_cache: Arc::from(RwLock::new(None)),
            glyph_ir_cache: Arc::from(RwLock::new(HashMap::new())),
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
            global_metadata_cache: self.global_metadata_cache.clone(),
            glyph_ir_cache: self.glyph_ir_cache.clone(),
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

    pub fn get_global_metadata(&self) -> Arc<GlobalMetadata> {
        self.check_read_access(&WorkIdentifier::GlobalMetadata);
        let rl = self.global_metadata_cache.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_global_metadata(&self, global_metadata: GlobalMetadata) {
        self.check_write_access(&WorkIdentifier::GlobalMetadata);
        let mut wl = self.global_metadata_cache.write();
        *wl = Some(Arc::from(global_metadata));
    }

    pub fn get_glyph_ir(&self, glyph_order: u32) -> Arc<GlyphIr> {
        self.check_read_access(&WorkIdentifier::GlyphIr(glyph_order));
        let rl = self.glyph_ir_cache.read();
        rl.get(&glyph_order).expect(MISSING_DATA).clone()
    }

    pub fn set_glyph_ir(&self, glyph_order: u32, ir: GlyphIr) {
        self.check_write_access(&WorkIdentifier::GlyphIr(glyph_order));
        let mut wl = self.glyph_ir_cache.write();
        wl.insert(glyph_order, Arc::from(ir));
    }
}
