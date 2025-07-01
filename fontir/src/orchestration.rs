//! Helps coordinate the graph execution for IR

use std::{
    collections::HashMap,
    fmt::Debug,
    fs::File,
    hash::Hash,
    io::{self, BufReader, BufWriter, Read, Write},
    sync::Arc,
};

use crate::{error::Error, ir, paths::Paths};
use bitflags::bitflags;
use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessControlList, Identifier, IdentifierDiscriminant, Work},
    types::GlyphName,
};
use parking_lot::RwLock;
use write_fonts::{read::FontRead, validate::Validate, FontWrite};

bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct Flags: u32 {
        // If set IR will be emitted to disk when written into Context
        const EMIT_IR = 0b00000001;
        // If set additional debug files will be emitted to disk
        const EMIT_DEBUG = 0b00000010;
        // If set, a glyph with contours and components will be converted to a simple (contour) glyph
        const PREFER_SIMPLE_GLYPHS = 0b00000100;
        // If set, a composite that references another composite will replace that composite with the
        // glyph(s) it references until only simple (contour) glyphs are referenced
        const FLATTEN_COMPONENTS = 0b00001000;
        const DECOMPOSE_TRANSFORMED_COMPONENTS = 0b00010000;
        // If set a files reporting on timing will be emitted to disk
        const EMIT_TIMING = 0b00100000;
        // If set, the direction of contours will NOT be reversed
        const KEEP_DIRECTION = 0b01000000;
        // If set, production names are read & used
        const PRODUCTION_NAMES = 0b10000000;
        // If set, all the composite glyphs will be decomposed to simple glyphs
        const DECOMPOSE_COMPONENTS = 0b100000000;
    }
}

impl Default for Flags {
    fn default() -> Self {
        Flags::PREFER_SIMPLE_GLYPHS | Flags::PRODUCTION_NAMES
    }
}

/// Clones are cheap and reference the same wrapped item courtesy of Arc
///
/// Courtesy of Arc this is Clone even if T isn't
#[derive(Default)]
pub struct ContextItem<I, T, P>
where
    I: Identifier,
{
    id: I,
    acl: Arc<AccessControlList<I>>,
    persistent_storage: Arc<P>,
    value: Arc<RwLock<Option<Arc<T>>>>,
}

impl<I, T, P> ContextItem<I, T, P>
where
    I: Identifier,
    P: PersistentStorage<I>,
    T: Persistable,
{
    pub fn new(id: I, acl: Arc<AccessControlList<I>>, persistent_storage: Arc<P>) -> Self {
        ContextItem {
            id,
            acl,
            persistent_storage,
            value: Default::default(),
        }
    }

    pub fn clone_with_acl(&self, acl: Arc<AccessControlList<I>>) -> Self {
        ContextItem {
            id: self.id.clone(),
            acl,
            persistent_storage: self.persistent_storage.clone(),
            value: self.value.clone(),
        }
    }

    /// Read item that you are sure must exist. Panic if not.
    ///
    /// Intended for use in [Work] to access items present in
    /// [Work::read_access]. If these are missing something is horribly
    /// wrong and we should kerplode.
    pub fn get(&self) -> Arc<T> {
        if let Some(in_memory) = self.try_get() {
            return in_memory;
        }

        // it's *not* in memory but perhaps it's written down?
        if self.persistent_storage.active() {
            if let Some(mut reader) = self.persistent_storage.reader(&self.id) {
                let restored = T::read(&mut reader);
                *self.value.write() = Some(Arc::from(restored));
            }
        }

        // if we still don't have an answer just give up
        self.try_get()
            .unwrap_or_else(|| panic!("{:?} is not available", self.id))
    }

    /// Read an item that might not exist
    pub fn try_get(&self) -> Option<Arc<T>> {
        self.acl.assert_read_access(&self.id);
        self.value.read().as_ref().cloned()
    }
}

impl<I, T, P> ContextItem<I, T, P>
where
    I: Identifier,
    T: PartialEq + Persistable,
    P: PersistentStorage<I>,
{
    /// Update the value if it has changed.
    ///
    /// Change logging and dependent task execution will only fire if the value changed.
    pub fn set(&self, value: T) {
        self.acl.assert_write_access(&self.id);

        // nop?
        if self
            .value
            .read()
            .as_ref()
            .map(|arc| **arc == value)
            .unwrap_or(false)
        {
            return;
        }

        if self.persistent_storage.active() {
            let mut writer = self.persistent_storage.writer(&self.id);
            value.write(&mut writer);
        }

        *self.value.write() = Some(Arc::from(value));
    }
}

/// Clones are cheap and reference the same wrapped item courtesy of Arc
///
/// Courtesy of Arc this is Clone even if T isn't
#[derive(Default)]
pub struct ContextMap<I, T, P>
where
    I: Identifier,
    T: IdAware<I>,
    P: PersistentStorage<I>,
{
    acl: Arc<AccessControlList<I>>,
    persistent_storage: Arc<P>,
    value: Arc<RwLock<HashMap<I, Arc<T>>>>,
}

impl<I, T, P> ContextMap<I, T, P>
where
    I: Identifier,
    T: IdAware<I> + Persistable,
    P: PersistentStorage<I>,
{
    pub fn new(acl: Arc<AccessControlList<I>>, persistent_storage: Arc<P>) -> Self {
        ContextMap {
            acl,
            persistent_storage,
            value: Default::default(),
        }
    }

    pub fn clone_with_acl(&self, acl: Arc<AccessControlList<I>>) -> Self {
        ContextMap {
            acl,
            persistent_storage: self.persistent_storage.clone(),
            value: self.value.clone(),
        }
    }

    /// Read an item that might not exist
    pub fn try_get(&self, id: &I) -> Option<Arc<T>> {
        self.acl.assert_read_access(id);
        self.value.read().get(id).cloned()
    }

    /// A copy of all the entries in the map. Values are arc'd so they are cheap, though not free, copies.
    pub fn all(&self) -> Vec<(I, Arc<T>)> {
        self.value
            .read()
            .iter()
            .map(|(id, v)| {
                self.acl.assert_read_access(id);
                (id.clone(), v.clone())
            })
            .collect()
    }

    /// Read item that you are sure must exist. Panic if not.
    ///
    /// Intended for use in [Work] to access items present in
    /// [Work::read_access]. If these are missing something is horribly
    /// wrong and we should kerplode.
    pub fn get(&self, id: &I) -> Arc<T> {
        if let Some(in_memory) = self.try_get(id) {
            return in_memory;
        }

        // it's *not* in memory but perhaps it's written down?
        if self.persistent_storage.active() {
            if let Some(mut reader) = self.persistent_storage.reader(id) {
                let restored = T::read(&mut reader);
                self.value.write().insert(id.clone(), Arc::from(restored));
            }
        }

        // if we still don't have an answer just give up
        self.try_get(id)
            .unwrap_or_else(|| panic!("{id:?} is not available"))
    }
}

impl<I, T, Ir> ContextMap<I, T, Ir>
where
    I: Identifier,
    T: IdAware<I> + Persistable,
    Ir: PersistentStorage<I>,
{
    pub fn set_unconditionally(&self, value: T) {
        let key = value.id();
        self.acl.assert_write_access(&key);

        if self.persistent_storage.active() {
            let mut writer = self.persistent_storage.writer(&key);
            value.write(&mut writer);
        }

        self.value.write().insert(key, Arc::from(value));
    }
}

impl<I, T, Ir> ContextMap<I, T, Ir>
where
    I: Identifier,
    T: IdAware<I> + PartialEq + Persistable,
    Ir: PersistentStorage<I>,
{
    pub fn set(&self, value: T) {
        let key = value.id();
        self.acl.assert_write_access(&key);

        // nop?
        if self
            .value
            .read()
            .get(&key)
            .map(|arc| **arc == value)
            .unwrap_or(false)
        {
            return;
        }

        self.set_unconditionally(value);
    }
}

pub trait IdAware<I> {
    fn id(&self) -> I;
}

pub trait Persistable {
    fn read(from: &mut dyn Read) -> Self;
    fn write(&self, to: &mut dyn Write);
}

/// Reads and writes to somewhere that lives longer than processes.
///
/// This enables the compiler to restore state from prior executions which is
/// crucial to incremental operation.
pub trait PersistentStorage<I> {
    fn active(&self) -> bool;
    /// None if there is nothing written down for id
    fn reader(&self, id: &I) -> Option<Box<dyn Read>>;
    fn writer(&self, id: &I) -> Box<dyn Write>;
}

// Unique identifier of work. If there are no fields work is unique.
// Meant to be small and cheap to copy around.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WorkId {
    /// Build the initial static metadata.
    ///
    /// Contains all names, axes, etc. Does NOT contain the final glyph order.
    StaticMetadata,
    /// Build potentially variable font-wide metrics.
    GlobalMetrics,
    Glyph(GlyphName),
    /// Glyph order from source, prior to adjustment
    ///
    /// Typically whatever2ir would populate this and then
    /// it would be used to produce GlyphOrder.
    ///
    /// Most things should use GlyphOrder not this.
    PreliminaryGlyphOrder,
    /// The final glyph order. Most things that need glyph order should rely on this.
    GlyphOrder,
    Features,
    KerningGroups,
    KernInstance(NormalizedLocation),
    Anchor(GlyphName),
    /// CPAL data
    ColorPalettes,
    /// COLR data
    PaintGraph,
}

impl WorkId {
    /// An id representing access to all glyphs
    pub const ALL_GLYPHS: Self = WorkId::Glyph(GlyphName::NOTDEF);
    /// an id representing access to all anchors
    pub const ALL_ANCHORS: Self = WorkId::Anchor(GlyphName::NOTDEF);
}

impl Identifier for WorkId {
    fn discriminant(&self) -> IdentifierDiscriminant {
        match self {
            WorkId::StaticMetadata => "IrStaticMetadata",
            WorkId::GlobalMetrics => "IrGlobalMetrics",
            WorkId::Glyph(..) => "IrGlyph",
            WorkId::PreliminaryGlyphOrder => "IrPreliminaryGlyphOrder",
            WorkId::GlyphOrder => "IrGlyphOrder",
            WorkId::Features => "IrFeatures",
            WorkId::KerningGroups => "IrKerningGroups",
            WorkId::KernInstance(..) => "IrKernInstance",
            WorkId::Anchor(..) => "IrAnchor",
            WorkId::ColorPalettes => "IrPalettes",
            WorkId::PaintGraph => "IrPaints",
        }
    }
}

pub type IrWork = dyn Work<Context, WorkId, Error> + Send;

pub struct IrPersistentStorage {
    active: bool,
    pub(crate) paths: Paths,
}

impl PersistentStorage<WorkId> for IrPersistentStorage {
    fn active(&self) -> bool {
        self.active
    }

    fn reader(&self, id: &WorkId) -> Option<Box<dyn Read>> {
        let file = self.paths.target_file(id);
        if !file.exists() {
            return None;
        }
        let raw_file = File::open(file.clone())
            .map_err(|e| panic!("Unable to write {file:?} {e}"))
            .unwrap();
        Some(Box::from(BufReader::new(raw_file)))
    }

    fn writer(&self, id: &WorkId) -> Box<dyn Write> {
        let file = self.paths.target_file(id);
        let raw_file = File::create(file.clone())
            .map_err(|e| panic!("Unable to write {file:?} {e}"))
            .unwrap();
        Box::from(BufWriter::new(raw_file))
    }
}

impl<T> Persistable for T
where
    for<'a> T: FontRead<'a> + FontWrite + Validate,
{
    fn read(from: &mut dyn Read) -> Self {
        let mut buf = Vec::new();
        from.read_to_end(&mut buf).unwrap();
        T::read(buf.as_slice().into()).expect("if we wrote it we can read it")
    }

    fn write(&self, to: &mut dyn io::Write) {
        let bytes = write_fonts::dump_table(self).unwrap();
        to.write_all(&bytes).unwrap();
    }
}

type FeContextItem<T> = ContextItem<WorkId, T, IrPersistentStorage>;
type FeContextMap<T> = ContextMap<WorkId, T, IrPersistentStorage>;

/// Read/write access to data for async work.
///
/// Intent is a root orchestrator creates a context and share copies with restricted
/// access with spawned tasks. Copies with access control are created to detect bad
/// execution order / mistakes, not to block actual bad actors.
pub struct Context {
    pub flags: Flags,

    pub(crate) persistent_storage: Arc<IrPersistentStorage>,

    // work results we've completed or restored from disk
    // We create individual caches so we can return typed results from get fns
    pub static_metadata: FeContextItem<ir::StaticMetadata>,
    pub preliminary_glyph_order: FeContextItem<ir::GlyphOrder>,
    pub glyph_order: FeContextItem<ir::GlyphOrder>,
    pub global_metrics: FeContextItem<ir::GlobalMetrics>,
    pub glyphs: FeContextMap<ir::Glyph>,
    pub features: FeContextItem<ir::FeaturesSource>,
    pub kerning_groups: FeContextItem<ir::KerningGroups>,
    pub kerning_at: FeContextMap<ir::KerningInstance>,
    pub anchors: FeContextMap<ir::GlyphAnchors>,
    pub colors: FeContextItem<ir::ColorPalettes>,
    pub paint_graph: FeContextItem<ir::PaintGraph>,
}

pub fn set_cached<T>(lock: &Arc<RwLock<Option<Arc<T>>>>, value: T) {
    let mut wl = lock.write();
    *wl = Some(Arc::from(value));
}

impl Context {
    fn copy(&self, acl: AccessControlList<WorkId>) -> Context {
        let acl = Arc::from(acl);
        Context {
            flags: self.flags,
            persistent_storage: self.persistent_storage.clone(),
            static_metadata: self.static_metadata.clone_with_acl(acl.clone()),
            preliminary_glyph_order: self.preliminary_glyph_order.clone_with_acl(acl.clone()),
            glyph_order: self.glyph_order.clone_with_acl(acl.clone()),
            global_metrics: self.global_metrics.clone_with_acl(acl.clone()),
            glyphs: self.glyphs.clone_with_acl(acl.clone()),
            features: self.features.clone_with_acl(acl.clone()),
            kerning_groups: self.kerning_groups.clone_with_acl(acl.clone()),
            kerning_at: self.kerning_at.clone_with_acl(acl.clone()),
            anchors: self.anchors.clone_with_acl(acl.clone()),
            colors: self.colors.clone_with_acl(acl.clone()),
            paint_graph: self.paint_graph.clone_with_acl(acl),
        }
    }

    pub fn new_root(flags: Flags, paths: Paths) -> Context {
        let acl = Arc::from(AccessControlList::read_only());
        let persistent_storage = Arc::from(IrPersistentStorage {
            active: flags.contains(Flags::EMIT_IR),
            paths,
        });
        Context {
            flags,
            persistent_storage: persistent_storage.clone(),
            static_metadata: ContextItem::new(
                WorkId::StaticMetadata,
                acl.clone(),
                persistent_storage.clone(),
            ),
            preliminary_glyph_order: ContextItem::new(
                WorkId::PreliminaryGlyphOrder,
                acl.clone(),
                persistent_storage.clone(),
            ),
            glyph_order: ContextItem::new(
                WorkId::GlyphOrder,
                acl.clone(),
                persistent_storage.clone(),
            ),
            global_metrics: ContextItem::new(
                WorkId::GlobalMetrics,
                acl.clone(),
                persistent_storage.clone(),
            ),
            glyphs: ContextMap::new(acl.clone(), persistent_storage.clone()),
            features: ContextItem::new(WorkId::Features, acl.clone(), persistent_storage.clone()),
            kerning_groups: ContextItem::new(
                WorkId::KerningGroups,
                acl.clone(),
                persistent_storage.clone(),
            ),
            kerning_at: ContextMap::new(acl.clone(), persistent_storage.clone()),
            anchors: ContextMap::new(acl.clone(), persistent_storage.clone()),
            colors: ContextItem::new(
                WorkId::ColorPalettes,
                acl.clone(),
                persistent_storage.clone(),
            ),
            paint_graph: ContextItem::new(WorkId::PaintGraph, acl, persistent_storage),
        }
    }

    pub fn copy_for_work(
        &self,
        read_access: Access<WorkId>,
        write_access: Access<WorkId>,
    ) -> Context {
        self.copy(AccessControlList::read_write(read_access, write_access))
    }

    pub fn read_only(&self) -> Context {
        self.copy(AccessControlList::read_only())
    }

    pub fn get_glyph(&self, name: impl Into<GlyphName>) -> Arc<ir::Glyph> {
        let id = WorkId::Glyph(name.into());
        self.glyphs.get(&id)
    }

    pub fn get_anchor(&self, name: impl Into<GlyphName>) -> Arc<ir::GlyphAnchors> {
        let id = WorkId::Anchor(name.into());
        self.anchors.get(&id)
    }
}
