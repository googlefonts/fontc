//! Helps coordinate the graph execution for BE

use std::{collections::HashMap, fs, io, path::Path, sync::Arc};

use font_types::F2Dot14;
use fontdrasil::{
    orchestration::{Access, AccessControlList, Work, MISSING_DATA},
    types::GlyphName,
};
use fontir::{
    context_accessors,
    orchestration::{Context as FeContext, ContextItem, Flags, WorkId as FeWorkIdentifier},
    variations::VariationRegion,
};
use kurbo::Vec2;
use log::trace;
use parking_lot::RwLock;
use read_fonts::{FontData, FontRead};
use serde::{Deserialize, Serialize};

use write_fonts::{
    dump_table,
    tables::{
        avar::Avar,
        cmap::Cmap,
        fvar::Fvar,
        glyf::{Bbox, SimpleGlyph},
        gvar::GlyphDeltas,
        head::Head,
        hhea::Hhea,
        maxp::Maxp,
        name::Name,
        os2::Os2,
        post::Post,
        variations::Tuple,
    },
    validate::Validate,
    FontBuilder, FontWrite, OtRound,
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
    Avar,
    Cmap,
    Fvar,
    Glyf,
    GlyfFragment(GlyphName),
    Gvar,
    GvarFragment(GlyphName),
    Head,
    Hhea,
    Hmtx,
    Loca,
    LocaFormat,
    Maxp,
    Name,
    Os2,
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

/// Unusually we store something other than the binary gvar per glyph.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/gvar>
#[derive(Serialize, Deserialize, Debug)]
pub struct GvarFragment {
    /// None entries are safe to omit per IUP
    pub deltas: Vec<(VariationRegion, Vec<Option<Vec2>>)>,
}

impl GvarFragment {
    pub fn to_deltas(&self) -> Vec<GlyphDeltas> {
        self.deltas
            .iter()
            .filter_map(|(region, deltas)| {
                if region.is_default() {
                    return None;
                }

                // Variation of no point has limited entertainment value
                if deltas.is_empty() {
                    return None;
                }

                let deltas: Vec<_> = deltas
                    .iter()
                    .map(|v| {
                        v.map(|Vec2 { x, y }| {
                            let x: i16 = x.ot_round();
                            let y: i16 = y.ot_round();
                            (x, y)
                        })
                    })
                    .collect();

                let tuple_builder: TupleBuilder = region.into();
                let (min, peak, max) = tuple_builder.build();
                Some(GlyphDeltas::new(peak, deltas, Some((min, max))))
            })
            .collect()
    }
}

/// <https://learn.microsoft.com/en-us/typography/opentype/spec/otvaroverview#variation-data>
#[derive(Debug, Default)]
struct TupleBuilder {
    axis_names: Vec<String>,
    min: Vec<F2Dot14>,
    peak: Vec<F2Dot14>,
    max: Vec<F2Dot14>,
}

impl TupleBuilder {
    fn build(self) -> (Tuple, Tuple, Tuple) {
        (
            Tuple::new(self.min),
            Tuple::new(self.peak),
            Tuple::new(self.max),
        )
    }
}

impl From<&VariationRegion> for TupleBuilder {
    fn from(region: &VariationRegion) -> Self {
        let mut builder = TupleBuilder::default();
        for (axis_name, tent) in region.iter() {
            builder.axis_names.push(axis_name.clone());
            builder.min.push(F2Dot14::from_f32(tent.min.to_f32()));
            builder.peak.push(F2Dot14::from_f32(tent.peak.to_f32()));
            builder.max.push(F2Dot14::from_f32(tent.max.to_f32()));
        }
        trace!("{builder:?}");
        builder
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LocaFormat {
    Short = 0,
    Long = 1,
}

impl LocaFormat {
    pub fn new(loca: &[u32]) -> LocaFormat {
        // https://github.com/fonttools/fonttools/blob/1c283756a5e39d69459eea80ed12792adc4922dd/Lib/fontTools/ttLib/tables/_l_o_c_a.py#L37
        if loca.last().copied().unwrap_or_default() < 0x20000
            && loca.iter().all(|offset| offset % 2 == 0)
        {
            LocaFormat::Short
        } else {
            LocaFormat::Long
        }
    }
}

// Free function of specific form to fit macro
pub fn loca_format_from_file(file: &Path) -> LocaFormat {
    trace!("loca_format_from_file");
    let bytes = fs::read(file).unwrap();
    match bytes.first() {
        Some(0) => LocaFormat::Short,
        Some(1) => LocaFormat::Long,
        _ => {
            panic!("serialized LocaFormat is invalid")
        }
    }
}

// Free function of specific form to fit macro
fn loca_format_to_bytes(format: &LocaFormat) -> Vec<u8> {
    vec![*format as u8]
}

pub type BeWork = dyn Work<Context, Error> + Send;
pub struct GlyfLoca {
    pub glyf: Vec<u8>,
    pub raw_loca: Vec<u8>,
    pub loca: Vec<u32>,
}

fn raw_loca(loca: &[u32]) -> Vec<u8> {
    let format = LocaFormat::new(loca);
    if format == LocaFormat::Short {
        loca.iter()
            .flat_map(|offset| ((offset >> 1) as u16).to_be_bytes())
            .collect()
    } else {
        loca.iter()
            .flat_map(|offset| offset.to_be_bytes())
            .collect()
    }
}

impl GlyfLoca {
    pub fn new(glyf: Vec<u8>, loca: Vec<u32>) -> Self {
        Self {
            glyf,
            raw_loca: raw_loca(&loca),
            loca,
        }
    }

    pub fn read(format: LocaFormat, paths: &Paths) -> Self {
        let glyf = read_entire_file(&paths.target_file(&WorkId::Glyf));
        let raw_loca = read_entire_file(&paths.target_file(&WorkId::Loca));
        let loca = if format == LocaFormat::Short {
            raw_loca
                .chunks_exact(std::mem::size_of::<u16>())
                .map(|bytes| u16::from_be_bytes(bytes.try_into().unwrap()) as u32 * 2)
                .collect()
        } else {
            raw_loca
                .chunks_exact(std::mem::size_of::<u32>())
                .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
                .collect()
        };
        Self {
            glyf,
            raw_loca,
            loca,
        }
    }

    fn write(&self, paths: &Paths) {
        persist(&paths.target_file(&WorkId::Glyf), &self.glyf);
        persist(&paths.target_file(&WorkId::Loca), &self.raw_loca);
    }
}

fn persist(file: &Path, content: &[u8]) {
    fs::write(file, content)
        .map_err(|e| panic!("Unable to write {file:?} {e}"))
        .unwrap();
}

/// Read/write access to data for async work.
///
/// Intent is a root orchestrator creates a context and share copies with restricted
/// access with spawned tasks. Copies with access control are created to detect bad
/// execution order / mistakes, not to block actual bad actors.
pub struct Context {
    pub flags: Flags,

    pub paths: Arc<Paths>,

    // The final, fully populated, read-only FE context
    pub ir: Arc<FeContext>,

    acl: AccessControlList<AnyWorkId>,

    // work results we've completed or restored from disk
    // We create individual caches so we can return typed results from get fns
    features: Arc<RwLock<Option<Arc<Vec<u8>>>>>,

    glyphs: Arc<RwLock<HashMap<GlyphName, Arc<Glyph>>>>,
    gvar_fragments: Arc<RwLock<HashMap<GlyphName, Arc<GvarFragment>>>>,

    glyf_loca: ContextItem<GlyfLoca>,
    avar: ContextItem<Avar>,
    cmap: ContextItem<Cmap>,
    fvar: ContextItem<Fvar>,
    gvar: ContextItem<Bytes>,
    post: ContextItem<Post>,
    loca_format: ContextItem<LocaFormat>,
    maxp: ContextItem<Maxp>,
    name: ContextItem<Name>,
    os2: ContextItem<Os2>,
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
            gvar_fragments: self.gvar_fragments.clone(),
            glyf_loca: self.glyf_loca.clone(),
            avar: self.avar.clone(),
            cmap: self.cmap.clone(),
            fvar: self.fvar.clone(),
            gvar: self.gvar.clone(),
            post: self.post.clone(),
            loca_format: self.loca_format.clone(),
            maxp: self.maxp.clone(),
            name: self.name.clone(),
            os2: self.os2.clone(),
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
            gvar_fragments: Arc::from(RwLock::new(HashMap::new())),
            glyf_loca: Arc::from(RwLock::new(None)),
            avar: Arc::from(RwLock::new(None)),
            cmap: Arc::from(RwLock::new(None)),
            fvar: Arc::from(RwLock::new(None)),
            gvar: Arc::from(RwLock::new(None)),
            post: Arc::from(RwLock::new(None)),
            loca_format: Arc::from(RwLock::new(None)),
            maxp: Arc::from(RwLock::new(None)),
            name: Arc::from(RwLock::new(None)),
            os2: Arc::from(RwLock::new(None)),
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

    // we need a self.persist for macros
    fn persist(&self, file: &Path, content: &[u8]) {
        persist(file, content);
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
        let id = WorkId::GlyfFragment(glyph_name.clone());
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
        let id = WorkId::GlyfFragment(glyph_name.clone());
        self.acl.assert_write_access(&id.clone().into());
        if self.flags.contains(Flags::EMIT_IR) {
            self.persist(&self.paths.target_file(&id), &glyph.to_bytes());
        }
        self.set_cached_glyph(glyph_name, glyph);
    }

    fn set_cached_gvar_fragment(&self, glyph_name: GlyphName, variations: GvarFragment) {
        let mut wl = self.gvar_fragments.write();
        wl.insert(glyph_name, Arc::from(variations));
    }

    pub fn get_gvar_fragment(&self, glyph_name: &GlyphName) -> Arc<GvarFragment> {
        let id = WorkId::GvarFragment(glyph_name.clone());
        self.acl.assert_read_access(&id.clone().into());
        {
            let rl = self.gvar_fragments.read();
            if let Some(variations) = rl.get(glyph_name) {
                return variations.clone();
            }
        }

        let gvar_fragment = read_entire_file(&self.paths.target_file(&id));
        let variations: GvarFragment = bincode::deserialize(&gvar_fragment).unwrap();

        self.set_cached_gvar_fragment(glyph_name.clone(), variations);
        let rl = self.gvar_fragments.read();
        rl.get(glyph_name).expect(MISSING_DATA).clone()
    }

    pub fn set_gvar_fragment(&self, glyph_name: GlyphName, gvar_fragment: GvarFragment) {
        let id = WorkId::GvarFragment(glyph_name.clone());
        self.acl.assert_write_access(&id.clone().into());

        if self.flags.contains(Flags::EMIT_IR) {
            let bytes = bincode::serialize(&gvar_fragment).unwrap();
            self.persist(&self.paths.target_file(&id), &bytes);
        }
        self.set_cached_gvar_fragment(glyph_name, gvar_fragment);
    }

    pub fn get_glyf_loca(&self) -> Arc<GlyfLoca> {
        let ids = [
            WorkId::Glyf.into(),
            WorkId::Loca.into(),
            WorkId::LocaFormat.into(),
        ];
        self.acl.assert_read_access_to_all(&ids);
        {
            let rl = self.glyf_loca.read();
            if rl.is_some() {
                return rl.as_ref().unwrap().clone();
            }
        }

        let format = self.get_loca_format();
        set_cached(
            &self.glyf_loca,
            GlyfLoca::read(*format, self.paths.as_ref()),
        );
        let rl = self.glyf_loca.read();
        rl.as_ref().expect(MISSING_DATA).clone()
    }

    pub fn set_glyf_loca(&self, glyf_loca: GlyfLoca) {
        let ids = [
            WorkId::Glyf.into(),
            WorkId::Loca.into(),
            WorkId::LocaFormat.into(),
        ];
        self.acl.assert_write_access_to_all(&ids);

        let loca_format = LocaFormat::new(&glyf_loca.loca);
        if self.flags.contains(Flags::EMIT_IR) {
            glyf_loca.write(self.paths.as_ref());
        }

        self.set_loca_format(loca_format);
        set_cached(&self.glyf_loca, glyf_loca);
    }

    // Lovely little typed accessors
    context_accessors! { get_avar, set_avar, avar, Avar, WorkId::Avar, from_file, to_bytes }
    context_accessors! { get_cmap, set_cmap, cmap, Cmap, WorkId::Cmap, from_file, to_bytes }
    context_accessors! { get_fvar, set_fvar, fvar, Fvar, WorkId::Fvar, from_file, to_bytes }
    context_accessors! { get_loca_format, set_loca_format, loca_format, LocaFormat, WorkId::LocaFormat, loca_format_from_file, loca_format_to_bytes }
    context_accessors! { get_maxp, set_maxp, maxp, Maxp, WorkId::Maxp, from_file, to_bytes }
    context_accessors! { get_name, set_name, name, Name, WorkId::Name, from_file, to_bytes }
    context_accessors! { get_os2, set_os2, os2, Os2, WorkId::Os2, from_file, to_bytes }
    context_accessors! { get_post, set_post, post, Post, WorkId::Post, from_file, to_bytes }
    context_accessors! { get_head, set_head, head, Head, WorkId::Head, from_file, to_bytes }
    context_accessors! { get_hhea, set_hhea, hhea, Hhea, WorkId::Hhea, from_file, to_bytes }

    // Accessors where value is raw bytes
    context_accessors! { get_gvar, set_gvar, gvar, Bytes, WorkId::Gvar, raw_from_file, raw_to_bytes }
    context_accessors! { get_hmtx, set_hmtx, hmtx, Bytes, WorkId::Hmtx, raw_from_file, raw_to_bytes }
    context_accessors! { get_font, set_font, font, Bytes, WorkId::Font, raw_from_file, raw_to_bytes }
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

pub(crate) fn to_bytes<T>(table: &T) -> Vec<u8>
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

#[cfg(test)]
mod tests {
    use tempfile::tempdir;

    use crate::{orchestration::LocaFormat, paths::Paths};

    use super::GlyfLoca;

    #[test]
    fn no_glyphs_is_short() {
        assert_eq!(LocaFormat::Short, LocaFormat::new(&Vec::new()));
    }

    #[test]
    fn some_glyphs_is_short() {
        assert_eq!(LocaFormat::Short, LocaFormat::new(&[24, 48, 112]));
    }

    #[test]
    fn unpadded_glyphs_is_long() {
        assert_eq!(LocaFormat::Long, LocaFormat::new(&[24, 7, 112]));
    }

    #[test]
    fn big_glyphs_is_long() {
        assert_eq!(
            LocaFormat::Long,
            LocaFormat::new(&(0..=32).map(|i| i * 0x1000).collect::<Vec<_>>())
        );
    }

    #[test]
    fn round_trip_glyf_loca() {
        let glyf = (0..16_u8).collect::<Vec<_>>();
        let loca = vec![0, 4, 16];
        let gl = GlyfLoca::new(glyf.clone(), loca.clone());
        let tmp = tempdir().unwrap();
        let paths = Paths::new(tmp.path());
        gl.write(&paths);

        let gl = GlyfLoca::read(LocaFormat::Short, &paths);
        assert_eq!((glyf, loca), (gl.glyf, gl.loca));
    }
}
