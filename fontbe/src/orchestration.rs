//! Helps coordinate the graph execution for BE

use std::{collections::HashMap, fs, io, path::Path, sync::Arc};

use fontdrasil::{
    orchestration::{Access, AccessControlList, Work, MISSING_DATA},
    types::GlyphName,
};
use fontir::{
    context_accessors,
    orchestration::{Context as FeContext, ContextItem, Flags, WorkId as FeWorkIdentifier},
};
use parking_lot::RwLock;
use read_fonts::{FontData, FontRead};
use write_fonts::{
    dump_table,
    tables::{
        cmap::Cmap,
        glyf::{Bbox, SimpleGlyph},
        head::Head,
        hhea::Hhea,
        maxp::Maxp,
        post::Post,
    },
    validate::Validate,
    FontBuilder, FontWrite,
};
use write_fonts::{from_obj::FromTableRef, tables::glyf::CompositeGlyph};

use crate::{error::Error, paths::Paths};

/// What exactly is being assembled from glyphs?
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum GlyphMerge {
    Glyf,
    Loca,
    Cmap,
}

/// Unique identifier of work.
///
/// If there are no fields work is unique.
/// Meant to be small and cheap to copy around.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WorkId {
    Features,
    Glyph(GlyphName),
    Cmap,
    Glyf,
    Head,
    Hhea,
    Hmtx,
    Loca,
    Maxp,
    Post,
    Font,
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

/// <https://learn.microsoft.com/en-us/typography/opentype/spec/glyf>
#[derive(Debug, Clone)]
pub enum Glyph {
    Simple(SimpleGlyph),
    Composite(CompositeGlyph),
}

impl From<SimpleGlyph> for Glyph {
    fn from(value: SimpleGlyph) -> Self {
        Glyph::Simple(value)
    }
}

impl From<CompositeGlyph> for Glyph {
    fn from(value: CompositeGlyph) -> Self {
        Glyph::Composite(value)
    }
}

impl Glyph {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            Glyph::Simple(table) => dump_table(table),
            Glyph::Composite(table) => dump_table(table),
        }
        .unwrap()
    }

    pub fn bbox(&self) -> Bbox {
        match self {
            Glyph::Simple(table) => table.bbox,
            Glyph::Composite(table) => table.bbox,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Glyph::Simple(table) => table.contours().is_empty(),
            Glyph::Composite(table) => table.components().is_empty(),
        }
    }
}

pub type BeWork = dyn Work<Context, Error> + Send;
type GlyfLoca = (Vec<u8>, Vec<u32>);

/// Read/write access to data for async work.
///
/// Intent is a root orchestrator creates a context and share copies with restricted
/// access with spawned tasks. Copies with access control are created to detect bad
/// execution order / mistakes, not to block actual bad actors.
pub struct Context {
    pub flags: Flags,

    paths: Arc<Paths>,

    // The final, fully populated, read-only FE context
    pub ir: Arc<FeContext>,

    acl: AccessControlList<AnyWorkId>,

    // work results we've completed or restored from disk
    // We create individual caches so we can return typed results from get fns
    features: Arc<RwLock<Option<Arc<Vec<u8>>>>>,

    // TODO: variations
    glyphs: Arc<RwLock<HashMap<GlyphName, Arc<Glyph>>>>,

    glyf_loca: ContextItem<GlyfLoca>,
    cmap: ContextItem<Cmap>,
    post: ContextItem<Post>,
    maxp: ContextItem<Maxp>,
    head: ContextItem<Head>,
    hhea: ContextItem<Hhea>,
    hmtx: ContextItem<Bytes>,
    font: ContextItem<Bytes>,
}

impl Context {
    fn copy(&self, acl: AccessControlList<AnyWorkId>) -> Context {
        Context {
            flags: self.flags,
            paths: self.paths.clone(),
            ir: self.ir.clone(),
            acl,
            features: self.features.clone(),
            glyphs: self.glyphs.clone(),
            glyf_loca: self.glyf_loca.clone(),
            cmap: self.cmap.clone(),
            post: self.post.clone(),
            maxp: self.maxp.clone(),
            head: self.head.clone(),
            hhea: self.hhea.clone(),
            hmtx: self.hmtx.clone(),
            font: self.font.clone(),
        }
    }

    pub fn new_root(flags: Flags, paths: Paths, ir: &fontir::orchestration::Context) -> Context {
        Context {
            flags,
            paths: Arc::from(paths),
            ir: Arc::from(ir.read_only()),
            acl: AccessControlList::read_only(),
            features: Arc::from(RwLock::new(None)),
            glyphs: Arc::from(RwLock::new(HashMap::new())),
            glyf_loca: Arc::from(RwLock::new(None)),
            cmap: Arc::from(RwLock::new(None)),
            post: Arc::from(RwLock::new(None)),
            maxp: Arc::from(RwLock::new(None)),
            head: Arc::from(RwLock::new(None)),
            hhea: Arc::from(RwLock::new(None)),
            hmtx: Arc::from(RwLock::new(None)),
            font: Arc::from(RwLock::new(None)),
        }
    }

    pub fn copy_for_work(
        &self,
        read_access: Access<AnyWorkId>,
        write_access: Access<AnyWorkId>,
    ) -> Context {
        self.copy(AccessControlList::read_write(read_access, write_access))
    }

    pub fn copy_read_only(&self) -> Context {
        self.copy(AccessControlList::read_only())
    }
}

fn set_cached<T>(lock: &Arc<RwLock<Option<Arc<T>>>>, value: T) {
    let mut wl = lock.write();
    *wl = Some(Arc::from(value));
}

fn from_file<T>(file: &Path) -> T
where
    for<'a> T: FontRead<'a>,
{
    let buf = read_entire_file(file);
    T::read(FontData::new(&buf)).unwrap()
}

pub struct Bytes {
    buf: Vec<u8>,
}

impl Bytes {
    pub(crate) fn new(buf: Vec<u8>) -> Bytes {
        Bytes { buf }
    }

    pub fn get(&self) -> &[u8] {
        &self.buf
    }
}

// Free fn because that lets it fit into the context_accessors macro.
fn raw_from_file(file: &Path) -> Bytes {
    let buf = read_entire_file(file);
    Bytes { buf }
}

fn raw_to_bytes(table: &Bytes) -> Vec<u8> {
    table.buf.clone()
}

fn to_bytes<T>(table: &T) -> Vec<u8>
where
    T: FontWrite + Validate,
{
    dump_table(table).unwrap()
}

fn read_entire_file(file: &Path) -> Vec<u8> {
    fs::read(file)
        .map_err(|e| panic!("Unable to read {file:?} {e}"))
        .unwrap()
}

impl Context {
    /// A reasonable place to write extra files to help someone debugging
    pub fn debug_dir(&self) -> &Path {
        self.paths.debug_dir()
    }

    fn maybe_persist(&self, file: &Path, content: &[u8]) {
        if !self.flags.contains(Flags::EMIT_IR) {
            return;
        }
        self.persist(file, content);
    }

    fn persist(&self, file: &Path, content: &[u8]) {
        fs::write(file, content)
            .map_err(|e| panic!("Unable to write {file:?} {e}"))
            .unwrap();
    }

    pub fn read_raw(&self, id: WorkId) -> Result<Vec<u8>, io::Error> {
        self.acl.assert_read_access(&id.clone().into());
        fs::read(self.paths.target_file(&id))
    }

    pub fn get_features(&self) -> Arc<Vec<u8>> {
        let id = WorkId::Features;
        self.acl.assert_read_access(&id.clone().into());
        {
            let rl = self.features.read();
            if rl.is_some() {
                return rl.as_ref().unwrap().clone();
            }
        }
        let font = read_entire_file(&self.paths.target_file(&id));
        set_cached(&self.features, font);
        let rl = self.features.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_features(&self, mut font: FontBuilder) {
        let id = WorkId::Features;
        self.acl.assert_write_access(&id.clone().into());
        let font = font.build();
        self.maybe_persist(&self.paths.target_file(&id), &font);
        set_cached(&self.features, font);
    }

    fn set_cached_glyph(&self, glyph_name: GlyphName, glyph: Glyph) {
        let mut wl = self.glyphs.write();
        wl.insert(glyph_name, Arc::from(glyph));
    }

    pub fn get_glyph(&self, glyph_name: &GlyphName) -> Arc<Glyph> {
        let id = WorkId::Glyph(glyph_name.clone());
        self.acl.assert_read_access(&id.clone().into());
        {
            let rl = self.glyphs.read();
            if let Some(glyph) = rl.get(glyph_name) {
                return glyph.clone();
            }
        }

        // Vec[u8] => read type => write type == all the right type
        let glyph = read_entire_file(&self.paths.target_file(&id));
        let glyph = read_fonts::tables::glyf::SimpleGlyph::read(FontData::new(&glyph)).unwrap();
        let glyph = SimpleGlyph::from_table_ref(&glyph);

        self.set_cached_glyph(glyph_name.clone(), glyph.into());
        let rl = self.glyphs.read();
        rl.get(glyph_name).expect(MISSING_DATA).clone()
    }

    pub fn set_glyph(&self, glyph_name: GlyphName, glyph: Glyph) {
        let id = WorkId::Glyph(glyph_name.clone());
        self.acl.assert_write_access(&id.clone().into());
        self.maybe_persist(&self.paths.target_file(&id), &glyph.to_bytes());
        self.set_cached_glyph(glyph_name, glyph);
    }

    pub fn get_glyf_loca(&self) -> Arc<GlyfLoca> {
        let ids = [WorkId::Glyf.into(), WorkId::Loca.into()];
        self.acl.assert_read_access_to_all(&ids);
        {
            let rl = self.glyf_loca.read();
            if rl.is_some() {
                return rl.as_ref().unwrap().clone();
            }
        }

        let loca = read_entire_file(&self.paths.target_file(&WorkId::Loca))
            .chunks_exact(std::mem::size_of::<u32>())
            .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
            .collect();
        let glyf = read_entire_file(&self.paths.target_file(&WorkId::Glyf));

        set_cached(&self.glyf_loca, (glyf, loca));
        let rl = self.glyf_loca.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_glyf_loca(&self, glyf_loca: GlyfLoca) {
        let ids = [WorkId::Glyf.into(), WorkId::Loca.into()];
        self.acl.assert_write_access_to_all(&ids);

        let (glyf, loca) = glyf_loca;
        self.maybe_persist(&self.paths.target_file(&WorkId::Glyf), &glyf);
        self.maybe_persist(
            &self.paths.target_file(&WorkId::Loca),
            &loca
                .iter()
                .flat_map(|v| v.to_be_bytes())
                .collect::<Vec<u8>>(),
        );

        set_cached(&self.glyf_loca, (glyf, loca));
    }

    // Lovely little typed accessors
    context_accessors! { get_cmap, set_cmap, cmap, Cmap, WorkId::Cmap, from_file, to_bytes }
    context_accessors! { get_maxp, set_maxp, maxp, Maxp, WorkId::Maxp, from_file, to_bytes }
    context_accessors! { get_post, set_post, post, Post, WorkId::Post, from_file, to_bytes }
    context_accessors! { get_head, set_head, head, Head, WorkId::Head, from_file, to_bytes }
    context_accessors! { get_hhea, set_hhea, hhea, Hhea, WorkId::Hhea, from_file, to_bytes }

    // Accessors where value is raw bytes
    context_accessors! { get_hmtx, set_hmtx, hmtx, Bytes, WorkId::Hmtx, raw_from_file, raw_to_bytes }
    context_accessors! { get_font, set_font, font, Bytes, WorkId::Font, raw_from_file, raw_to_bytes }
}
