//! Helps coordinate the graph execution for IR

use std::{
    collections::HashMap,
    fmt::Debug,
    fs::File,
    io::{BufReader, BufWriter},
    path::Path,
    sync::Arc,
};

use bitflags::bitflags;
use fontdrasil::{
    orchestration::{AccessControlList, Work, MISSING_DATA},
    types::GlyphName,
};
use parking_lot::RwLock;
use serde::{de::DeserializeOwned, Serialize};

use crate::{error::WorkError, ir, paths::Paths, source::Input};

bitflags! {
    pub struct Flags: u32 {
        // If set IR will be emitted to disk when written into Context
        const EMIT_IR = 0b00000001;
        // If set additional debug files will be emitted to disk
        const EMIT_DEBUG = 0b00000010;
        // If set seek to match fontmake (python) behavior even at cost of abandoning optimizations
        const MATCH_LEGACY = 0b00000100;
    }
}

impl Default for Flags {
    fn default() -> Self {
        Flags::MATCH_LEGACY
    }
}

// Unique identifier of work. If there are no fields work is unique.
// Meant to be small and cheap to copy around.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WorkId {
    /// Build the initial static metadata.
    InitStaticMetadata,
    Glyph(GlyphName),
    GlyphIrDelete,
    /// Update static metadata based on what we learned from IR
    ///
    /// Notably, IR glyphs with both components and paths may split into multiple
    /// BE glyphs.
    FinalizeStaticMetadata,
    Features,
}

pub type IrWork = dyn Work<Context, WorkError> + Send;

/// Read/write access to data for async work.
///
/// Intent is a root orchestrator creates a context and share copies with restricted
/// access with spawned tasks. Copies with access control are created to detect bad
/// execution order / mistakes, not to block actual bad actors.
pub struct Context {
    pub flags: Flags,

    paths: Arc<Paths>,

    // The input we're working on. Note that change detection may mean we only process
    // a subset of the full input.
    pub input: Arc<Input>,

    acl: AccessControlList<WorkId>,

    // work results we've completed or restored from disk
    // We create individual caches so we can return typed results from get fns
    static_metadata: Arc<RwLock<Option<Arc<ir::StaticMetadata>>>>,
    glyph_ir: Arc<RwLock<HashMap<GlyphName, Arc<ir::Glyph>>>>,
    feature_ir: Arc<RwLock<Option<Arc<ir::Features>>>>,
}

impl Context {
    fn copy(&self, acl: AccessControlList<WorkId>) -> Context {
        Context {
            flags: self.flags,
            paths: self.paths.clone(),
            input: self.input.clone(),
            acl,
            static_metadata: self.static_metadata.clone(),
            glyph_ir: self.glyph_ir.clone(),
            feature_ir: self.feature_ir.clone(),
        }
    }

    pub fn new_root(flags: Flags, paths: Paths, input: Input) -> Context {
        Context {
            flags,
            paths: Arc::from(paths),
            input: Arc::from(input),
            acl: AccessControlList::read_only(),
            static_metadata: Arc::from(RwLock::new(None)),
            glyph_ir: Arc::from(RwLock::new(HashMap::new())),
            feature_ir: Arc::from(RwLock::new(None)),
        }
    }

    pub fn copy_for_work(
        &self,
        read_access: Arc<dyn Fn(&WorkId) -> bool + Send + Sync>,
        write_access: Arc<dyn Fn(&WorkId) -> bool + Send + Sync>,
    ) -> Context {
        self.copy(AccessControlList::read_write(read_access, write_access))
    }

    pub fn read_only(&self) -> Context {
        self.copy(AccessControlList::read_only())
    }
}

impl Context {
    fn maybe_persist<V>(&self, file: &Path, content: &V)
    where
        V: ?Sized + Serialize + Debug,
    {
        if !self.flags.contains(Flags::EMIT_IR) {
            return;
        }
        let raw_file = File::create(file)
            .map_err(|e| panic!("Unable to write {file:?} {e}"))
            .unwrap();
        let buf_io = BufWriter::new(raw_file);
        serde_yaml::to_writer(buf_io, &content)
            .map_err(|e| panic!("Unable to serialize\n{content:#?}\nto {file:?}: {e}"))
            .unwrap();
    }

    fn restore<V>(&self, file: &Path) -> V
    where
        V: ?Sized + DeserializeOwned,
    {
        let raw_file = File::open(file)
            .map_err(|e| panic!("Unable to read {file:?} {e}"))
            .unwrap();
        let buf_io = BufReader::new(raw_file);
        match serde_yaml::from_reader(buf_io) {
            Ok(v) => v,
            Err(e) => panic!("Unable to deserialize {file:?} {e}"),
        }
    }

    fn set_cached_static_metadata(&self, ir: ir::StaticMetadata) {
        let mut wl = self.static_metadata.write();
        *wl = Some(Arc::from(ir));
    }

    pub fn get_static_metadata(&self) -> Arc<ir::StaticMetadata> {
        let ids = [WorkId::InitStaticMetadata, WorkId::FinalizeStaticMetadata];
        self.acl.assert_read_access_to_any(&ids);
        {
            let rl = self.static_metadata.read();
            if rl.is_some() {
                return rl.as_ref().unwrap().clone();
            }
        }
        self.set_cached_static_metadata(self.restore(&self.paths.target_file(&ids[0])));
        let rl = self.static_metadata.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_static_metadata(&self, ir: ir::StaticMetadata) {
        let ids = [WorkId::InitStaticMetadata, WorkId::FinalizeStaticMetadata];
        self.acl.assert_write_access_to_any(&ids);
        self.maybe_persist(&self.paths.target_file(&ids[0]), &ir);
        self.set_cached_static_metadata(ir);
    }

    pub fn get_glyph_ir(&self, glyph_name: &GlyphName) -> Arc<ir::Glyph> {
        let id = WorkId::Glyph(glyph_name.clone());
        self.acl.assert_read_access(&id);
        let rl = self.glyph_ir.read();
        rl.get(glyph_name).expect(MISSING_DATA).clone()
    }

    pub fn set_glyph_ir(&self, ir: ir::Glyph) {
        let id = WorkId::Glyph(ir.name.clone());
        self.acl.assert_write_access(&id);
        self.maybe_persist(&self.paths.target_file(&id), &ir);
        let mut wl = self.glyph_ir.write();
        wl.insert(ir.name.clone(), Arc::from(ir));
    }

    fn set_cached_features(&self, ir: ir::Features) {
        let mut wl = self.feature_ir.write();
        *wl = Some(Arc::from(ir));
    }

    pub fn get_features(&self) -> Arc<ir::Features> {
        let id = WorkId::Features;
        self.acl.assert_read_access(&id);
        {
            let rl = self.feature_ir.read();
            if rl.is_some() {
                return rl.as_ref().unwrap().clone();
            }
        }
        self.set_cached_features(self.restore(&self.paths.target_file(&id)));
        let rl = self.feature_ir.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_features(&self, ir: ir::Features) {
        let id = WorkId::Features;
        self.acl.assert_write_access(&id);
        self.maybe_persist(&self.paths.target_file(&id), &ir);
        self.set_cached_features(ir);
    }
}
