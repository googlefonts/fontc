//! Helps coordinate the graph execution for IR

use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    fs::File,
    io::{BufReader, BufWriter},
    path::Path,
    sync::Arc,
};

use fontdrasil::{
    orchestration::{AccessControlList, Work, MISSING_DATA},
    types::GlyphName,
};
use parking_lot::RwLock;
use serde::{de::DeserializeOwned, Serialize};

use crate::{error::WorkError, ir, paths::Paths, source::Input};

// Unique identifier of work. If there are no fields work is unique.
// Meant to be small and cheap to copy around.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WorkIdentifier {
    StaticMetadata,
    Glyph(GlyphName),
    GlyphIrDelete(),
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

    acl: AccessControlList<WorkIdentifier>,

    // work results we've completed or restored from disk
    // We create individual caches so we can return typed results from get fns
    static_metadata: Arc<RwLock<Option<Arc<ir::StaticMetadata>>>>,
    glyph_ir: Arc<RwLock<HashMap<GlyphName, Arc<ir::Glyph>>>>,
    feature_ir: Arc<RwLock<Option<Arc<ir::Features>>>>,
}

impl Context {
    fn copy(&self, acl: AccessControlList<WorkIdentifier>) -> Context {
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
            acl: AccessControlList::read_only(),
            static_metadata: Arc::from(RwLock::new(None)),
            glyph_ir: Arc::from(RwLock::new(HashMap::new())),
            feature_ir: Arc::from(RwLock::new(None)),
        }
    }

    pub fn copy_for_work(
        &self,
        work_id: WorkIdentifier,
        dependencies: Option<HashSet<WorkIdentifier>>,
    ) -> Context {
        self.copy(AccessControlList::read_write(
            dependencies.unwrap_or_default(),
            work_id,
        ))
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
        if !self.emit_ir {
            return;
        }
        let raw_file = File::create(file)
            .map_err(|e| panic!("Unable to write {:?} {}", file, e))
            .unwrap();
        let buf_io = BufWriter::new(raw_file);
        serde_yaml::to_writer(buf_io, &content)
            .map_err(|e| panic!("Unable to serialize\n{:#?}\nto {:?}: {}", content, file, e))
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
        let mut wl = self.static_metadata.write();
        *wl = Some(Arc::from(ir));
    }

    pub fn get_static_metadata(&self) -> Arc<ir::StaticMetadata> {
        let id = WorkIdentifier::StaticMetadata;
        self.acl.check_read_access(&id);
        {
            let rl = self.static_metadata.read();
            if rl.is_some() {
                return rl.as_ref().unwrap().clone();
            }
        }
        self.set_cached_static_metadata(self.restore(&self.paths.target_file(&id)));
        let rl = self.static_metadata.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_static_metadata(&self, ir: ir::StaticMetadata) {
        let id = WorkIdentifier::StaticMetadata;
        self.acl.check_write_access(&id);
        self.maybe_persist(&self.paths.target_file(&id), &ir);
        self.set_cached_static_metadata(ir);
    }

    pub fn get_glyph_ir(&self, glyph_name: &GlyphName) -> Arc<ir::Glyph> {
        let id = WorkIdentifier::Glyph(glyph_name.clone());
        self.acl.check_read_access(&id);
        let rl = self.glyph_ir.read();
        rl.get(glyph_name).expect(MISSING_DATA).clone()
    }

    pub fn set_glyph_ir(&self, glyph_name: GlyphName, ir: ir::Glyph) {
        let id = WorkIdentifier::Glyph(glyph_name.clone());
        self.acl.check_write_access(&id);
        self.maybe_persist(&self.paths.target_file(&id), &ir);
        let mut wl = self.glyph_ir.write();
        wl.insert(glyph_name, Arc::from(ir));
    }

    fn set_cached_features(&self, ir: ir::Features) {
        let mut wl = self.feature_ir.write();
        *wl = Some(Arc::from(ir));
    }

    pub fn get_features(&self) -> Arc<ir::Features> {
        let id = WorkIdentifier::Features;
        self.acl.check_read_access(&id);
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
        let id = WorkIdentifier::Features;
        self.acl.check_write_access(&id);
        self.maybe_persist(&self.paths.target_file(&id), &ir);
        self.set_cached_features(ir);
    }
}
