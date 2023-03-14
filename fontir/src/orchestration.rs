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

pub type ContextItem<T> = Arc<RwLock<Option<Arc<T>>>>;

/// Generates fn $getter_name(&self) -> Arc<$value_type>.
///
/// Assumes we are in an impl block for a Context and that
/// self.$lock_name is a ContextItem<$ir_type>
///
/// <https://veykril.github.io/tlborm/decl-macros/minutiae/fragment-specifiers.html>
#[macro_export]
macro_rules! context_accessors {
    ($getter_name:ident, $setter_name:ident, $lock_name:ident, $value_type:ty, $id:expr, $restore_fn:ident, $prepersist_fn:ident) => {
        pub fn $getter_name(&self) -> Arc<$value_type> {
            let id = $id;
            self.acl.assert_read_access(&id.clone().into());
            {
                let rl = self.$lock_name.read();
                if rl.is_some() {
                    return rl.as_ref().unwrap().clone();
                }
            }
            set_cached(&self.$lock_name, $restore_fn(&self.paths.target_file(&id)));
            let rl = self.$lock_name.read();
            rl.as_ref().expect(MISSING_DATA).clone()
        }

        pub fn $setter_name(&self, value: $value_type) {
            let id = $id;
            self.acl.assert_write_access(&id.clone().into());
            if self.flags.contains(Flags::EMIT_IR) {
                let buf = $prepersist_fn(&value);
                self.persist(&self.paths.target_file(&id), &buf);
            }
            set_cached(&self.$lock_name, value);
        }
    };
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
    init_static_metadata: ContextItem<ir::StaticMetadata>,
    final_static_metadata: ContextItem<ir::StaticMetadata>,
    glyph_ir: Arc<RwLock<HashMap<GlyphName, Arc<ir::Glyph>>>>,
    feature_ir: ContextItem<ir::Features>,
}

pub fn set_cached<T>(lock: &Arc<RwLock<Option<Arc<T>>>>, value: T) {
    let mut wl = lock.write();
    *wl = Some(Arc::from(value));
}

impl Context {
    fn copy(&self, acl: AccessControlList<WorkId>) -> Context {
        Context {
            flags: self.flags,
            paths: self.paths.clone(),
            input: self.input.clone(),
            acl,
            init_static_metadata: self.init_static_metadata.clone(),
            final_static_metadata: self.final_static_metadata.clone(),
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
            init_static_metadata: Arc::from(RwLock::new(None)),
            final_static_metadata: Arc::from(RwLock::new(None)),
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

fn nop<T>(v: &T) -> &T {
    v
}

fn restore<V>(file: &Path) -> V
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

impl Context {
    fn maybe_persist<V>(&self, file: &Path, content: &V)
    where
        V: ?Sized + Serialize + Debug,
    {
        if !self.flags.contains(Flags::EMIT_IR) {
            return;
        }
        self.persist(file, content);
    }

    fn persist<V>(&self, file: &Path, content: &V)
    where
        V: ?Sized + Serialize + Debug,
    {
        let raw_file = File::create(file)
            .map_err(|e| panic!("Unable to write {file:?} {e}"))
            .unwrap();
        let buf_io = BufWriter::new(raw_file);
        serde_yaml::to_writer(buf_io, &content)
            .map_err(|e| panic!("Unable to serialize\n{content:#?}\nto {file:?}: {e}"))
            .unwrap();
    }

    fn set_cached_glyph(&self, ir: ir::Glyph) {
        let mut wl = self.glyph_ir.write();
        wl.insert(ir.name.clone(), Arc::from(ir));
    }

    pub fn get_glyph_ir(&self, glyph_name: &GlyphName) -> Arc<ir::Glyph> {
        let id = WorkId::Glyph(glyph_name.clone());
        self.acl.assert_read_access(&id);
        {
            let rl = self.glyph_ir.read();
            if let Some(glyph) = rl.get(glyph_name) {
                return glyph.clone();
            }
        }
        self.set_cached_glyph(restore(&self.paths.target_file(&id)));
        let rl = self.glyph_ir.read();
        rl.get(glyph_name).expect(MISSING_DATA).clone()
    }

    pub fn set_glyph_ir(&self, ir: ir::Glyph) {
        let id = WorkId::Glyph(ir.name.clone());
        self.acl.assert_write_access(&id);
        self.maybe_persist(&self.paths.target_file(&id), &ir);
        self.set_cached_glyph(ir);
    }

    context_accessors! { get_init_static_metadata, set_init_static_metadata, init_static_metadata, ir::StaticMetadata, WorkId::InitStaticMetadata, restore, nop }
    context_accessors! { get_final_static_metadata, set_final_static_metadata, final_static_metadata, ir::StaticMetadata, WorkId::FinalizeStaticMetadata, restore, nop }
    context_accessors! { get_features, set_features, feature_ir, ir::Features, WorkId::Features, restore, nop }
}
