//! Helps coordinate the graph execution for IR

use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufReader, BufWriter},
    path::Path,
    sync::Arc,
};

use fontdrasil::orchestration::{Acl, Cache, Work, MISSING_DATA};
use serde::{de::DeserializeOwned, Serialize};

use crate::{error::WorkError, ir, paths::Paths, source::Input};

// Unique identifier of work. If there are no fields work is unique.
// Meant to be small and cheap to copy around.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WorkIdentifier {
    StaticMetadata,
    Glyph(String),
    GlyphIrDelete(String),
    Features,
}

pub type IrWork = dyn Work<Context, WorkError> + Send;

/// Read/write access to data for async work.
///
/// Intent is a root orchestrator creates a context and share copies with restricted
/// access with spawned tasks. Copies with access control are created to detect bad
/// execution order / mistakes, not to block actual bad actors.
pub struct Context {
    // If set IR will be emitted to disk when written into Context
    emit_ir: bool,

    paths: Arc<Paths>,

    // The input we're working on. Note that change detection may mean we only process
    // a subset of the full input.
    pub input: Arc<Input>,

    acl: Acl<WorkIdentifier>,

    // work results we've completed or restored from disk
    // We create individual caches so we can return typed results from get fns
    static_metadata: Cache<Option<Arc<ir::StaticMetadata>>>,
    glyph_ir: Cache<HashMap<String, Arc<ir::Glyph>>>,
    feature_ir: Cache<Option<Arc<ir::Features>>>,
}

impl Context {
    fn copy(&self, acl: Acl<WorkIdentifier>) -> Context {
        Context {
            emit_ir: self.emit_ir,
            paths: self.paths.clone(),
            input: self.input.clone(),
            acl,
            static_metadata: self.static_metadata.clone(),
            glyph_ir: self.glyph_ir.clone(),
            feature_ir: self.feature_ir.clone(),
        }
    }

    pub fn new_root(emit_ir: bool, paths: Paths, input: Input) -> Context {
        Context {
            emit_ir,
            paths: Arc::from(paths),
            input: Arc::from(input),
            acl: Acl::read_only(),
            static_metadata: Cache::new(),
            glyph_ir: Cache::new(),
            feature_ir: Cache::new(),
        }
    }

    pub fn copy_for_work(
        &self,
        work_id: WorkIdentifier,
        dependencies: Option<HashSet<WorkIdentifier>>,
    ) -> Context {
        self.copy(Acl::read_write(dependencies.unwrap_or_default(), work_id))
    }

    pub fn read_only(&self) -> Context {
        self.copy(Acl::read_only())
    }
}

impl Context {
    fn maybe_persist<V>(&self, file: &Path, content: &V)
    where
        V: ?Sized + Serialize,
    {
        if !self.emit_ir {
            return;
        }
        let raw_file = File::create(file)
            .map_err(|e| panic!("Unable to write {:?} {}", file, e))
            .unwrap();
        let buf_io = BufWriter::new(raw_file);
        serde_yaml::to_writer(buf_io, &content)
            .map_err(|e| panic!("Unable to serialize to {:?}: {}", file, e))
            .unwrap();
    }

    fn restore<V>(&self, file: &Path) -> V
    where
        V: ?Sized + DeserializeOwned,
    {
        let raw_file = File::open(file)
            .map_err(|e| panic!("Unable to read {:?} {}", file, e))
            .unwrap();
        let buf_io = BufReader::new(raw_file);
        match serde_yaml::from_reader(buf_io) {
            Ok(v) => v,
            Err(e) => panic!("Unable to deserialize {:?} {}", file, e),
        }
    }

    fn set_cached_static_metadata(&self, ir: ir::StaticMetadata) {
        let mut wl = self.static_metadata.item.write().unwrap();
        *wl = Some(Arc::from(ir));
    }

    pub fn get_static_metadata(&self) -> Arc<ir::StaticMetadata> {
        let id = WorkIdentifier::StaticMetadata;
        self.acl.check_read_access(&id);
        {
            let rl = self.static_metadata.item.read().unwrap();
            if rl.is_some() {
                return rl.as_ref().unwrap().clone();
            }
        }
        self.set_cached_static_metadata(self.restore(&self.paths.target_file(&id)));
        let rl = self.static_metadata.item.read().unwrap();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_static_metadata(&self, ir: ir::StaticMetadata) {
        let id = WorkIdentifier::StaticMetadata;
        self.acl.check_write_access(&id);
        self.maybe_persist(&&self.paths.target_file(&id), &ir);
        self.set_cached_static_metadata(ir);
    }

    pub fn get_glyph_ir(&self, glyph_name: &str) -> Arc<ir::Glyph> {
        let id = WorkIdentifier::Glyph(glyph_name.to_string());
        self.acl.check_read_access(&id);
        let rl = self.glyph_ir.item.read().unwrap();
        rl.get(glyph_name).expect(MISSING_DATA).clone()
    }

    pub fn set_glyph_ir(&self, glyph_name: &str, ir: ir::Glyph) {
        let id = WorkIdentifier::Glyph(glyph_name.to_string());
        self.acl.check_write_access(&id);
        self.maybe_persist(&self.paths.target_file(&id), &ir);
        let mut wl = self.glyph_ir.item.write().unwrap();
        wl.insert(glyph_name.to_string(), Arc::from(ir));
    }

    fn set_cached_features(&self, ir: ir::Features) {
        let mut wl = self.feature_ir.item.write().unwrap();
        *wl = Some(Arc::from(ir));
    }

    pub fn get_features(&self) -> Arc<ir::Features> {
        let id = WorkIdentifier::Features;
        self.acl.check_read_access(&id);
        {
            let rl = self.feature_ir.item.read().unwrap();
            if rl.is_some() {
                return rl.as_ref().unwrap().clone();
            }
        }
        self.set_cached_features(self.restore(&&self.paths.target_file(&id)));
        let rl = self.feature_ir.item.read().unwrap();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_features(&self, ir: ir::Features) {
        let id = WorkIdentifier::Features;
        self.acl.check_write_access(&id);
        self.maybe_persist(&self.paths.target_file(&id), &ir);
        self.set_cached_features(ir);
    }
}
