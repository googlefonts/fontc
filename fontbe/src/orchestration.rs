//! Helps coordinate the graph execution for BE

use std::{collections::HashMap, fs, path::Path, sync::Arc};

use fontdrasil::{
    orchestration::{AccessControlList, Work, MISSING_DATA},
    types::GlyphName,
};
use fontir::orchestration::{Context as FeContext, Flags, WorkId as FeWorkIdentifier};
use parking_lot::RwLock;
use read_fonts::{FontData, FontRead};
use write_fonts::{dump_table, tables::glyf::SimpleGlyph, FontBuilder};
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
    Glyf,
    Loca,
    Cmap,
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

    glyf_loca: Arc<RwLock<Option<Arc<GlyfLoca>>>>,
    cmap: Arc<RwLock<Option<Arc<Vec<u8>>>>>,
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
        }
    }

    pub fn copy_for_work(
        &self,
        read_access: Arc<dyn Fn(&AnyWorkId) -> bool + Send + Sync>,
        write_access: Arc<dyn Fn(&AnyWorkId) -> bool + Send + Sync>,
    ) -> Context {
        self.copy(AccessControlList::read_write(read_access, write_access))
    }

    pub fn copy_read_only(&self) -> Context {
        self.copy(AccessControlList::read_only())
    }
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
        fs::write(file, content)
            .map_err(|e| panic!("Unable to write {file:?} {e}"))
            .unwrap();
    }

    fn restore(&self, file: &Path) -> Vec<u8> {
        fs::read(file)
            .map_err(|e| panic!("Unable to read {file:?} {e}"))
            .unwrap()
    }

    fn set_cached_features(&self, font: Vec<u8>) {
        let mut wl = self.features.write();
        *wl = Some(Arc::from(font));
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
        let font = self.restore(&self.paths.target_file(&id));
        self.set_cached_features(font);
        let rl = self.features.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_features(&self, mut font: FontBuilder) {
        let id = WorkId::Features;
        self.acl.assert_write_access(&id.clone().into());
        let font = font.build();
        self.maybe_persist(&self.paths.target_file(&id), &font);
        self.set_cached_features(font);
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
        let glyph = self.restore(&self.paths.target_file(&id));
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

    fn set_cached_glyf_loca(&self, glyf_loca: GlyfLoca) {
        let mut wl = self.glyf_loca.write();
        *wl = Some(Arc::from(glyf_loca));
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

        let loca = self
            .restore(&self.paths.target_file(&WorkId::Loca))
            .chunks_exact(std::mem::size_of::<u32>())
            .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
            .collect();
        let glyf = self.restore(&self.paths.target_file(&WorkId::Glyf));

        self.set_cached_glyf_loca((glyf, loca));
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

        self.set_cached_glyf_loca((glyf, loca));
    }

    fn set_cached_cmap(&self, cmap: Vec<u8>) {
        let mut wl = self.cmap.write();
        *wl = Some(Arc::from(cmap));
    }

    pub fn get_cmap(&self) -> Arc<Vec<u8>> {
        let id = WorkId::Cmap;
        self.acl.assert_read_access(&id.clone().into());
        {
            let rl = self.cmap.read();
            if rl.is_some() {
                return rl.as_ref().unwrap().clone();
            }
        }
        let font = self.restore(&self.paths.target_file(&id));
        self.set_cached_features(font);
        let rl = self.cmap.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_cmap(&self, cmap: Vec<u8>) {
        let id = WorkId::Cmap;
        self.acl.assert_write_access(&id.clone().into());
        self.maybe_persist(&self.paths.target_file(&id), &cmap);
        self.set_cached_cmap(cmap);
    }
}
