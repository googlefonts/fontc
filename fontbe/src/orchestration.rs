//! Helps coordinate the graph execution for BE

use std::{collections::HashSet, fs, path::Path, sync::Arc};

use fontdrasil::{
    orchestration::{AccessControlList, Work, MISSING_DATA},
    types::GlyphName,
};
use fontir::orchestration::{Context as FeContext, WorkId as FeWorkIdentifier};
use parking_lot::RwLock;
use write_fonts::FontBuilder;

use crate::{error::Error, paths::Paths};

/// Unique identifier of work.
///
/// If there are no fields work is unique.
/// Meant to be small and cheap to copy around.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WorkId {
    Features,
    Glyph(GlyphName),
    GlyphMerge,
    FinalMerge,
}

// Identifies work of any type, FE, BE, ... future optimization passes, w/e.
// Useful because BE work can very reasonably depend on FE work
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnyWorkId {
    Fe(FeWorkIdentifier),
    Be(WorkId),
}

impl AnyWorkId {
    pub fn unwrap_be(&self) -> &WorkId {
        match self {
            AnyWorkId::Fe(..) => panic!("Not a BE identifier"),
            AnyWorkId::Be(id) => id,
        }
    }

    pub fn unwrap_fe(&self) -> &FeWorkIdentifier {
        match self {
            AnyWorkId::Fe(id) => id,
            AnyWorkId::Be(..) => panic!("Not a FE identifier"),
        }
    }
}

impl From<FeWorkIdentifier> for AnyWorkId {
    fn from(id: FeWorkIdentifier) -> Self {
        AnyWorkId::Fe(id)
    }
}

impl From<WorkId> for AnyWorkId {
    fn from(id: WorkId) -> Self {
        AnyWorkId::Be(id)
    }
}

pub type BeWork = dyn Work<Context, Error> + Send;

/// Read/write access to data for async work.
///
/// Intent is a root orchestrator creates a context and share copies with restricted
/// access with spawned tasks. Copies with access control are created to detect bad
/// execution order / mistakes, not to block actual bad actors.
pub struct Context {
    // If set artifacts prior to final binary will be emitted to disk when written into Context
    pub emit_ir: bool,

    // If set additional debug files will be emitted to disk
    pub emit_debug: bool,

    paths: Arc<Paths>,

    // The final, fully populated, read-only FE context
    pub ir: Arc<FeContext>,

    acl: AccessControlList<AnyWorkId>,

    // work results we've completed or restored from disk
    // We create individual caches so we can return typed results from get fns
    features: Arc<RwLock<Option<Arc<Vec<u8>>>>>,
}

impl Context {
    pub fn new_root(
        emit_ir: bool,
        emit_debug: bool,
        paths: Paths,
        ir: &fontir::orchestration::Context,
    ) -> Context {
        Context {
            emit_ir,
            emit_debug,
            paths: Arc::from(paths),
            ir: Arc::from(ir.read_only()),
            acl: AccessControlList::read_only(),
            features: Arc::from(RwLock::new(None)),
        }
    }

    pub fn copy_for_work(
        &self,
        work_id: WorkId,
        dependencies: Option<HashSet<AnyWorkId>>,
    ) -> Context {
        Context {
            emit_ir: self.emit_ir,
            emit_debug: self.emit_debug,
            paths: self.paths.clone(),
            ir: self.ir.clone(),
            acl: AccessControlList::read_write(dependencies.unwrap_or_default(), work_id.into()),
            features: self.features.clone(),
        }
    }
}

impl Context {
    /// A reasonable place to write extra files to help someone debugging
    pub fn debug_dir(&self) -> &Path {
        self.paths.debug_dir()
    }

    fn maybe_persist(&self, file: &Path, content: &[u8]) {
        if !self.emit_ir {
            return;
        }
        fs::write(file, content)
            .map_err(|e| panic!("Unable to write {:?} {}", file, e))
            .unwrap();
    }

    fn restore(&self, file: &Path) -> Vec<u8> {
        fs::read(file)
            .map_err(|e| panic!("Unable to read {:?} {}", file, e))
            .unwrap()
    }

    fn set_cached_features(&self, font: Vec<u8>) {
        let mut wl = self.features.write();
        *wl = Some(Arc::from(font));
    }

    pub fn get_features(&self) -> Arc<Vec<u8>> {
        let id = WorkId::Features;
        self.acl.check_read_access(&id.clone().into());
        {
            let rl = self.features.read();
            if rl.is_some() {
                return rl.as_ref().unwrap().clone();
            }
        }
        let font = self.restore(&self.paths.target_file(&id));
        self.set_cached_features(font);
        let rl = self.features.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_features(&self, mut font: FontBuilder) {
        let id = WorkId::Features;
        self.acl.check_write_access(&id.clone().into());
        let font = font.build();
        self.maybe_persist(&self.paths.target_file(&id), &font);
        self.set_cached_features(font);
    }
}
