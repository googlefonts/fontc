//! Helps coordinate the graph execution for BE

use std::{
    fs::File,
    io::{self, BufReader, BufWriter, Read, Write},
    path::{Path, PathBuf},
    sync::Arc, collections::BTreeMap,
};

use fea_rs::{
    compile::{PairPosBuilder, ValueRecord as ValueRecordBuilder},
    GlyphMap, GlyphSet,
};
use fontdrasil::{
    orchestration::{Access, AccessControlList, Identifier, IdentifierDiscriminant, Work},
    types::GlyphName,
};
use fontir::{
    ir::{Anchor, StaticMetadata},
    orchestration::{
        Context as FeContext, ContextItem, ContextMap, Flags, IdAware, Persistable,
        PersistentStorage, WorkId as FeWorkIdentifier,
    },
    variations::VariationRegion,
};
use log::trace;
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};

use smol_str::SmolStr;
use write_fonts::{
    dump_table,
    read::{FontData, FontRead},
    tables::{
        avar::Avar,
        cmap::Cmap,
        fvar::Fvar,
        gdef::Gdef,
        glyf::Glyph as RawGlyph,
        gpos::Gpos,
        gsub::Gsub,
        gvar::{GlyphDelta, GlyphDeltas},
        head::Head,
        hhea::Hhea,
        hvar::Hvar,
        loca::LocaFormat,
        maxp::Maxp,
        name::Name,
        os2::Os2,
        post::Post,
        stat::Stat,
        variations::{Tuple, VariationRegion as BeVariationRegion},
    },
    types::{F2Dot14, GlyphId, Tag},
    validate::Validate,
    FontWrite,
};

use crate::{error::Error, features::resolve_variable_metric, paths::Paths};

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
    PreliminaryFont,
    FinalFont,
    Fvar,
    Glyf,
    GlyfFragment(GlyphName),
    Gpos,
    Gsub,
    Gdef,
    Gvar,
    GvarFragment(GlyphName),
    Head,
    Hhea,
    Hmtx,
    Hvar,
    Kerning,
    Loca,
    LocaFormat,
    Marks,
    Maxp,
    Name,
    Os2,
    Post,
    Stat,
}

impl Identifier for WorkId {
    fn discriminant(&self) -> IdentifierDiscriminant {
        match self {
            WorkId::Features => "BeFeatures",
            WorkId::Avar => "BeAvar",
            WorkId::Cmap => "BeCmap",
            WorkId::PreliminaryFont => "BePreliminaryFont",
            WorkId::FinalFont => "BeFinalFont",
            WorkId::Fvar => "BeFvar",
            WorkId::Glyf => "BeGlyf",
            WorkId::GlyfFragment(..) => "BeGlyfFragment",
            WorkId::Gpos => "BeGpos",
            WorkId::Gsub => "BeGsub",
            WorkId::Gdef => "BeGdef",
            WorkId::Gvar => "BeGvar",
            WorkId::GvarFragment(..) => "BeGvarFragment",
            WorkId::Head => "BeHead",
            WorkId::Hhea => "BeHhea",
            WorkId::Hmtx => "BeHmtx",
            WorkId::Hvar => "BeHvar",
            WorkId::Kerning => "BeKerning",
            WorkId::Loca => "BeLoca",
            WorkId::LocaFormat => "BeLocaFormat",
            WorkId::Marks => "BeMarks",
            WorkId::Maxp => "BeMaxp",
            WorkId::Name => "BeName",
            WorkId::Os2 => "BeOs2",
            WorkId::Post => "BePost",
            WorkId::Stat => "BeStat",
        }
    }
}

// Identifies work of any type, FE, BE, ... future optimization passes, w/e.
// Useful because BE work can very reasonably depend on FE work
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnyWorkId {
    Fe(FeWorkIdentifier),
    Be(WorkId),
    /// Used to express access to all items of an Fe type
    AllOfFe(FeWorkIdentifier),
    /// Used to express access to all items of an Be type
    AllOfBe(WorkId),
    /// Used to capture timing not associated with work
    InternalTiming(&'static str),
}

impl Identifier for AnyWorkId {
    fn discriminant(&self) -> IdentifierDiscriminant {
        match self {
            AnyWorkId::Fe(id) | AnyWorkId::AllOfFe(id) => id.discriminant(),
            AnyWorkId::Be(id) | AnyWorkId::AllOfBe(id) => id.discriminant(),
            AnyWorkId::InternalTiming(..) => "InternalTiming",
        }
    }
}

impl AnyWorkId {
    pub fn unwrap_be(&self) -> &WorkId {
        match self {
            AnyWorkId::Be(id) => id,
            _ => panic!("Not a BE identifier"),
        }
    }

    pub fn unwrap_fe(&self) -> &FeWorkIdentifier {
        match self {
            AnyWorkId::Fe(id) => id,
            _ => panic!("Not a FE identifier"),
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

/// A glyph and its associated name
///
/// See <https://learn.microsoft.com/en-us/typography/opentype/spec/glyf>
#[derive(Debug, Clone)]
pub struct Glyph {
    pub name: GlyphName,
    pub data: RawGlyph,
}

impl Glyph {
    pub(crate) fn new(name: GlyphName, glyph: impl Into<RawGlyph>) -> Self {
        Self {
            name,
            data: glyph.into(),
        }
    }

    pub fn is_simple(&self) -> bool {
        matches!(&self.data, RawGlyph::Simple(_))
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        dump_table(&self.data).unwrap()
    }
}

impl IdAware<AnyWorkId> for Glyph {
    fn id(&self) -> AnyWorkId {
        AnyWorkId::Be(WorkId::GlyfFragment(self.name.clone()))
    }
}

impl Persistable for Glyph {
    fn read(from: &mut dyn Read) -> Self {
        let (name, bytes): (GlyphName, Vec<u8>) = bincode::deserialize_from(from).unwrap();
        let glyph = RawGlyph::read(bytes.as_slice().into()).unwrap();
        Glyph { name, data: glyph }
    }

    fn write(&self, to: &mut dyn Write) {
        let glyph_bytes = dump_table(&self.data).unwrap();
        let to_write = (&self.name, glyph_bytes);
        bincode::serialize_into(to, &to_write).unwrap();
    }
}

/// Unusually we store something other than the binary gvar per glyph.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/gvar>
#[derive(Serialize, Deserialize, Debug)]
pub struct GvarFragment {
    pub glyph_name: GlyphName,
    /// None entries are safe to omit per IUP
    pub deltas: Vec<(VariationRegion, Vec<GlyphDelta>)>,
}

impl GvarFragment {
    pub fn to_deltas(&self, axis_order: &[Tag]) -> Vec<GlyphDeltas> {
        self.deltas
            .iter()
            .filter_map(|(region, deltas)| {
                if region.is_default() {
                    return None;
                }

                // Variation of no point has limited entertainment value
                if deltas.is_empty() || deltas.iter().all(|d| !d.required) {
                    return None;
                }

                let tuple_builder = TupleBuilder::new(region, axis_order);
                let (min, peak, max) = tuple_builder.build();
                Some(GlyphDeltas::new(peak, deltas.clone(), Some((min, max))))
            })
            .collect()
    }
}

impl IdAware<AnyWorkId> for GvarFragment {
    fn id(&self) -> AnyWorkId {
        AnyWorkId::Be(WorkId::GvarFragment(self.glyph_name.clone()))
    }
}

impl Persistable for GvarFragment {
    fn read(from: &mut dyn Read) -> Self {
        bincode::deserialize_from(from).unwrap()
    }

    fn write(&self, to: &mut dyn io::Write) {
        bincode::serialize_into(to, &self).unwrap();
    }
}

/// <https://learn.microsoft.com/en-us/typography/opentype/spec/otvaroverview#variation-data>
#[derive(Debug, Default)]
struct TupleBuilder {
    axes: Vec<Tag>,
    min: Vec<F2Dot14>,
    peak: Vec<F2Dot14>,
    max: Vec<F2Dot14>,
}

impl TupleBuilder {
    fn new(region: &VariationRegion, axis_order: &[Tag]) -> Self {
        let mut builder = TupleBuilder::default();
        for tag in axis_order {
            let tent = region.get(tag).unwrap();
            builder.axes.push(*tag);
            builder.min.push(F2Dot14::from_f32(tent.min.to_f32()));
            builder.peak.push(F2Dot14::from_f32(tent.peak.to_f32()));
            builder.max.push(F2Dot14::from_f32(tent.max.to_f32()));
        }
        trace!("{builder:?}");
        builder
    }

    fn build(self) -> (Tuple, Tuple, Tuple) {
        (
            Tuple::new(self.min),
            Tuple::new(self.peak),
            Tuple::new(self.max),
        )
    }
}

/// Compiled binary font tables
#[derive(Default, Clone, Serialize, Deserialize, PartialEq)]
pub struct BinaryTables {
    pub tables: BTreeMap<Tag, Vec<u8>>,
}

impl Persistable for BinaryTables {
    fn read(from: &mut dyn Read) -> Self {
        bincode::deserialize_from(from).unwrap()
    }

    fn write(&self, to: &mut dyn io::Write) {
        bincode::serialize_into(to, &self).unwrap();
    }
}

/// Prebuilt kern to the extent we can without being able to assign deltas to a value record.
#[derive(Clone, Serialize, Deserialize, PartialEq)]
pub enum Kern {
    Pair {
        glyph0: GlyphId,
        glyph1: GlyphId,
        x_advance: ValueRecordBuilder,
    },
    Class {
        glyphs0: GlyphSet,
        glyphs1: GlyphSet,
        x_advance: ValueRecordBuilder,
    },
}

impl Kern {
    pub fn insert_into(&self, ppos_subtables: &mut PairPosBuilder) {
        match self {
            Kern::Pair {
                glyph0,
                glyph1,
                x_advance,
            } => {
                ppos_subtables.insert_pair(*glyph0, x_advance.clone(), *glyph1, Default::default())
            }
            Kern::Class {
                glyphs0,
                glyphs1,
                x_advance,
            } => ppos_subtables.insert_classes(
                glyphs0.clone(),
                x_advance.clone(),
                glyphs1.clone(),
                Default::default(),
            ),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct MarkGroupName(pub(crate) SmolStr);

#[derive(Default, Clone, Serialize, Deserialize, PartialEq)]
pub(crate) struct MarkGroup {
    pub(crate) bases: Vec<(GlyphName, Anchor)>,
    pub(crate) marks: Vec<(GlyphName, Anchor)>,
}

/// A mark or base entry, prepped for submission to the fea-rs builder
#[derive(Default, Clone, Serialize, Deserialize, PartialEq)]
pub(crate) struct MarkEntry {
    pub(crate) gid: GlyphId,
    x_default: i16,
    x_deltas: Vec<(BeVariationRegion, i16)>,
    y_default: i16,
    y_deltas: Vec<(BeVariationRegion, i16)>,
}

impl MarkEntry {
    pub(crate) fn new(
        static_metadata: &StaticMetadata,
        gid: GlyphId,
        anchor: &Anchor,
    ) -> Result<Self, Error> {
        // squish everything into the shape expected by `resolve_variable_metric`
        let (x_values, y_values) = anchor
            .positions
            .iter()
            .map(|(loc, pt)| {
                (
                    (loc.clone(), OrderedFloat::from(pt.x as f32)),
                    (loc.clone(), OrderedFloat::from(pt.y as f32)),
                )
            })
            .unzip();

        let (x_default, x_deltas) = resolve_variable_metric(static_metadata, &x_values)?;
        let (y_default, y_deltas) = resolve_variable_metric(static_metadata, &y_values)?;

        Ok(Self {
            gid,
            x_default,
            x_deltas,
            y_default,
            y_deltas,
        })
    }

    pub(crate) fn create_anchor_table(&self) -> fea_rs::compile::Anchor {
        fea_rs::compile::Anchor::new(self.x_default, self.y_default)
            .with_x_device(self.x_deltas.clone())
            .with_y_device(self.y_deltas.clone())
    }
}

#[derive(Default, Clone, Serialize, Deserialize, PartialEq)]
pub(crate) struct MarkBase {
    pub(crate) class: SmolStr,
    pub(crate) marks: Vec<MarkEntry>,
    pub(crate) bases: Vec<MarkEntry>,
}

impl MarkBase {
    pub(crate) fn new(class: SmolStr) -> Self {
        MarkBase {
            class,
            ..Default::default()
        }
    }

    pub(crate) fn insert_mark(&mut self, entry: MarkEntry) {
        self.marks.push(entry);
    }

    pub(crate) fn insert_base(&mut self, entry: MarkEntry) {
        self.bases.push(entry);
    }
}

#[derive(Default, Clone, Serialize, Deserialize, PartialEq)]
pub(crate) struct MarkMark {
    pub(crate) class: SmolStr,
    pub(crate) filter_set: Vec<GlyphId>,
    pub(crate) attaching_marks: Vec<MarkEntry>,
    pub(crate) base_marks: Vec<MarkEntry>,
}

impl MarkMark {
    pub(crate) fn new(class: SmolStr) -> Self {
        MarkMark {
            class,
            ..Default::default()
        }
    }

    pub(crate) fn insert_attaching_mark(&mut self, entry: MarkEntry) {
        self.attaching_marks.push(entry);
    }

    pub(crate) fn insert_base_mark(&mut self, entry: MarkEntry) {
        self.base_marks.push(entry)
    }
}

/// Precomputed marks, to the extent possible given that we cannot create temporary var indices in advance.
///
/// TODO: update once <https://github.com/googlefonts/fontc/issues/571> is fixed. Then we can build a
/// actual fea-rs structs in advance.
#[derive(Default, Clone, Serialize, Deserialize, PartialEq)]
pub struct Marks {
    pub(crate) glyphmap: GlyphMap,
    pub(crate) mark_base: Vec<MarkBase>,
    pub(crate) mark_mark: Vec<MarkMark>,
}

impl Persistable for Marks {
    fn read(from: &mut dyn Read) -> Self {
        bincode::deserialize_from(from).unwrap()
    }

    fn write(&self, to: &mut dyn io::Write) {
        bincode::serialize_into(to, self).unwrap()
    }
}

/// Precomputed kerning, to the extent possible given that we cannot create temporary var indices in advance.
///
/// TODO: update once <https://github.com/googlefonts/fontc/issues/571> is fixed. Then we can build a
/// [`fea_rs::compile::PairPosBuilder`] in advance.
#[derive(Default, Clone, Serialize, Deserialize, PartialEq)]
pub struct Kerning {
    deltas: Vec<Vec<(BeVariationRegion, i16)>>,
    kerns: Vec<Kern>,
}

impl Kerning {
    pub fn is_empty(&self) -> bool {
        self.kerns.is_empty()
    }

    pub fn deltas(&self) -> impl Iterator<Item = &Vec<(BeVariationRegion, i16)>> {
        self.deltas.iter()
    }

    pub fn kerns(&self) -> impl Iterator<Item = &Kern> {
        self.kerns.iter()
    }

    pub fn add_deltas(&mut self, deltas: Vec<(BeVariationRegion, i16)>) -> usize {
        self.deltas.push(deltas);
        self.deltas.len() - 1
    }

    pub fn add_pair(&mut self, glyph0: GlyphId, x_advance: ValueRecordBuilder, glyph1: GlyphId) {
        self.kerns.push(Kern::Pair {
            glyph0,
            glyph1,
            x_advance,
        })
    }

    pub fn add_class(
        &mut self,
        glyphs0: GlyphSet,
        x_advance: ValueRecordBuilder,
        glyphs1: GlyphSet,
    ) {
        self.kerns.push(Kern::Class {
            glyphs0,
            glyphs1,
            x_advance,
        })
    }
}

impl Persistable for Kerning {
    fn read(from: &mut dyn Read) -> Self {
        bincode::deserialize_from(from).unwrap()
    }

    fn write(&self, to: &mut dyn io::Write) {
        bincode::serialize_into(to, self).unwrap()
    }
}

// work around orphan rules.
//
// FIXME: Clarify if there's a good reason not to treat glyf/loca as a single
// entity, for the purpose of persistence? like a struct that contains both
// tables, and from which the format can be retrieved
//
// this whole thing needs a rethink, but this gets us working
#[derive(Clone, Copy, Serialize, Deserialize, PartialEq)]
pub struct LocaFormatWrapper(u8);

impl From<LocaFormat> for LocaFormatWrapper {
    fn from(value: LocaFormat) -> Self {
        LocaFormatWrapper(value as _)
    }
}

impl From<LocaFormatWrapper> for LocaFormat {
    fn from(value: LocaFormatWrapper) -> Self {
        if value.0 == 0 {
            LocaFormat::Short
        } else {
            LocaFormat::Long
        }
    }
}

impl Persistable for LocaFormatWrapper {
    fn read(from: &mut dyn Read) -> Self {
        bincode::deserialize_from(from).unwrap()
    }

    fn write(&self, to: &mut dyn io::Write) {
        bincode::serialize_into(to, self).unwrap()
    }
}

pub type BeWork = dyn Work<Context, AnyWorkId, Error> + Send;

/// Allows us to implement [Persistable] w/o violating the orphan rules
///
/// Other than that just kind of gets in the way
pub struct BeValue<T>(pub Option<T>);

impl<T> Persistable for BeValue<T>
where
    for<'a> T: FontRead<'a> + FontWrite + Validate,
{
    fn read(from: &mut dyn Read) -> Self {
        let mut buf = Vec::new();
        from.read_to_end(&mut buf).unwrap();
        (!buf.is_empty())
            .then(|| T::read(FontData::new(&buf)).unwrap())
            .into()
    }

    fn write(&self, to: &mut dyn io::Write) {
        let bytes = self
            .0
            .as_ref()
            .map(|t| dump_table(t).unwrap())
            .unwrap_or_default();
        to.write_all(&bytes).unwrap();
    }
}

impl<T> From<T> for BeValue<T>
where
    T: FontWrite + Validate,
{
    fn from(value: T) -> Self {
        BeValue(Some(value))
    }
}

impl<T> From<Option<T>> for BeValue<T>
where
    T: FontWrite + Validate,
{
    fn from(value: Option<T>) -> Self {
        BeValue(value)
    }
}

pub struct BePersistentStorage {
    active: bool,
    pub(crate) paths: Paths,
}

impl PersistentStorage<AnyWorkId> for BePersistentStorage {
    fn active(&self) -> bool {
        self.active
    }

    fn reader(&self, id: &AnyWorkId) -> Option<Box<dyn Read>> {
        let file = self.paths.target_file(id.unwrap_be());
        if !file.exists() {
            return None;
        }
        let raw_file = File::open(file.clone())
            .map_err(|e| panic!("Unable to write {file:?} {e}"))
            .unwrap();
        Some(Box::from(BufReader::new(raw_file)))
    }

    fn writer(&self, id: &AnyWorkId) -> Box<dyn io::Write> {
        let file = self.paths.target_file(id.unwrap_be());
        let raw_file = File::create(file.clone())
            .map_err(|e| panic!("Unable to write {file:?} {e}"))
            .unwrap();
        Box::from(BufWriter::new(raw_file))
    }
}

type BeContextItem<T> = ContextItem<AnyWorkId, T, BePersistentStorage>;
type BeContextMap<T> = ContextMap<AnyWorkId, T, BePersistentStorage>;

/// Read/write access to data for async work.
///
/// Intent is a root orchestrator creates a context and share copies with restricted
/// access with spawned tasks. Copies with access control are created to detect bad
/// execution order / mistakes, not to block actual bad actors.
pub struct Context {
    pub flags: Flags,

    pub persistent_storage: Arc<BePersistentStorage>,

    // The final, fully populated, read-only FE context
    pub ir: Arc<FeContext>,

    // work results we've completed or restored from disk
    pub gvar_fragments: BeContextMap<GvarFragment>,
    pub glyphs: BeContextMap<Glyph>,

    // Allow avar to be explicitly None to record a noop avar being generated
    pub avar: BeContextItem<BeValue<Avar>>,
    pub cmap: BeContextItem<BeValue<Cmap>>,
    pub fvar: BeContextItem<BeValue<Fvar>>,
    pub glyf: BeContextItem<Bytes>,
    pub gsub: BeContextItem<BeValue<Gsub>>,
    pub gpos: BeContextItem<BeValue<Gpos>>,
    pub gdef: BeContextItem<BeValue<Gdef>>,
    pub gvar: BeContextItem<Bytes>,
    pub post: BeContextItem<BeValue<Post>>,
    pub loca: BeContextItem<Bytes>,
    pub loca_format: BeContextItem<LocaFormatWrapper>,
    pub maxp: BeContextItem<BeValue<Maxp>>,
    pub name: BeContextItem<BeValue<Name>>,
    pub os2: BeContextItem<BeValue<Os2>>,
    pub head: BeContextItem<BeValue<Head>>,
    pub hhea: BeContextItem<BeValue<Hhea>>,
    pub hmtx: BeContextItem<Bytes>,
    pub hvar: BeContextItem<BeValue<Hvar>>,
    pub kerning: BeContextItem<Kerning>,
    pub marks: BeContextItem<Marks>,
    pub stat: BeContextItem<BeValue<Stat>>,
    pub preliminary_font: BeContextItem<BinaryTables>,
    pub font: BeContextItem<Bytes>,
}

impl Context {
    fn copy(&self, acl: AccessControlList<AnyWorkId>) -> Context {
        let acl = Arc::from(acl);
        Context {
            flags: self.flags,
            persistent_storage: self.persistent_storage.clone(),
            ir: self.ir.clone(),
            glyphs: self.glyphs.clone_with_acl(acl.clone()),
            gvar_fragments: self.gvar_fragments.clone_with_acl(acl.clone()),
            avar: self.avar.clone_with_acl(acl.clone()),
            cmap: self.cmap.clone_with_acl(acl.clone()),
            fvar: self.fvar.clone_with_acl(acl.clone()),
            glyf: self.glyf.clone_with_acl(acl.clone()),
            gsub: self.gsub.clone_with_acl(acl.clone()),
            gpos: self.gpos.clone_with_acl(acl.clone()),
            gdef: self.gdef.clone_with_acl(acl.clone()),
            gvar: self.gvar.clone_with_acl(acl.clone()),
            post: self.post.clone_with_acl(acl.clone()),
            loca: self.loca.clone_with_acl(acl.clone()),
            loca_format: self.loca_format.clone_with_acl(acl.clone()),
            maxp: self.maxp.clone_with_acl(acl.clone()),
            name: self.name.clone_with_acl(acl.clone()),
            os2: self.os2.clone_with_acl(acl.clone()),
            head: self.head.clone_with_acl(acl.clone()),
            hhea: self.hhea.clone_with_acl(acl.clone()),
            hmtx: self.hmtx.clone_with_acl(acl.clone()),
            hvar: self.hvar.clone_with_acl(acl.clone()),
            kerning: self.kerning.clone_with_acl(acl.clone()),
            marks: self.marks.clone_with_acl(acl.clone()),
            stat: self.stat.clone_with_acl(acl.clone()),
            preliminary_font: self.preliminary_font.clone_with_acl(acl.clone()),
            font: self.font.clone_with_acl(acl),
        }
    }

    pub fn new_root(flags: Flags, paths: Paths, ir: &fontir::orchestration::Context) -> Context {
        let acl = Arc::from(AccessControlList::read_only());
        let persistent_storage = Arc::from(BePersistentStorage {
            active: flags.contains(Flags::EMIT_IR),
            paths,
        });
        Context {
            flags,
            persistent_storage: persistent_storage.clone(),
            ir: Arc::from(ir.read_only()),
            glyphs: ContextMap::new(acl.clone(), persistent_storage.clone()),
            gvar_fragments: ContextMap::new(acl.clone(), persistent_storage.clone()),
            avar: ContextItem::new(WorkId::Avar.into(), acl.clone(), persistent_storage.clone()),
            cmap: ContextItem::new(WorkId::Cmap.into(), acl.clone(), persistent_storage.clone()),
            fvar: ContextItem::new(WorkId::Fvar.into(), acl.clone(), persistent_storage.clone()),
            glyf: ContextItem::new(WorkId::Glyf.into(), acl.clone(), persistent_storage.clone()),
            gpos: ContextItem::new(WorkId::Gpos.into(), acl.clone(), persistent_storage.clone()),
            gsub: ContextItem::new(WorkId::Gsub.into(), acl.clone(), persistent_storage.clone()),
            gdef: ContextItem::new(WorkId::Gdef.into(), acl.clone(), persistent_storage.clone()),
            gvar: ContextItem::new(WorkId::Gvar.into(), acl.clone(), persistent_storage.clone()),
            post: ContextItem::new(WorkId::Post.into(), acl.clone(), persistent_storage.clone()),
            loca: ContextItem::new(WorkId::Loca.into(), acl.clone(), persistent_storage.clone()),
            loca_format: ContextItem::new(
                WorkId::LocaFormat.into(),
                acl.clone(),
                persistent_storage.clone(),
            ),
            maxp: ContextItem::new(WorkId::Maxp.into(), acl.clone(), persistent_storage.clone()),
            name: ContextItem::new(WorkId::Name.into(), acl.clone(), persistent_storage.clone()),
            os2: ContextItem::new(WorkId::Os2.into(), acl.clone(), persistent_storage.clone()),
            head: ContextItem::new(WorkId::Head.into(), acl.clone(), persistent_storage.clone()),
            hhea: ContextItem::new(WorkId::Hhea.into(), acl.clone(), persistent_storage.clone()),
            hmtx: ContextItem::new(WorkId::Hmtx.into(), acl.clone(), persistent_storage.clone()),
            hvar: ContextItem::new(WorkId::Hvar.into(), acl.clone(), persistent_storage.clone()),
            kerning: ContextItem::new(
                WorkId::Kerning.into(),
                acl.clone(),
                persistent_storage.clone(),
            ),
            marks: ContextItem::new(
                WorkId::Marks.into(),
                acl.clone(),
                persistent_storage.clone(),
            ),
            stat: ContextItem::new(WorkId::Stat.into(), acl.clone(), persistent_storage.clone()),
            preliminary_font: ContextItem::new(WorkId::PreliminaryFont.into(), acl.clone(), persistent_storage.clone()),
            font: ContextItem::new(WorkId::FinalFont.into(), acl, persistent_storage),
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
        self.persistent_storage.paths.debug_dir()
    }

    pub fn font_file(&self) -> PathBuf {
        self.persistent_storage.paths.target_file(&WorkId::FinalFont)
    }
}

#[derive(PartialEq)]
pub struct Bytes {
    buf: Vec<u8>,
}

impl Bytes {
    pub fn get(&self) -> &[u8] {
        &self.buf
    }
}

impl From<Vec<u8>> for Bytes {
    fn from(buf: Vec<u8>) -> Self {
        Bytes { buf }
    }
}

impl Persistable for Bytes {
    fn read(from: &mut dyn Read) -> Self {
        let mut buf = Vec::new();
        from.read_to_end(&mut buf).unwrap();
        buf.into()
    }

    fn write(&self, to: &mut dyn io::Write) {
        to.write_all(&self.buf).unwrap();
    }
}

pub(crate) fn to_bytes<T>(table: &BeValue<T>) -> Option<Vec<u8>>
where
    T: FontWrite + Validate,
{
    table.0.as_ref().map(|t| dump_table(t).unwrap())
}

#[cfg(test)]
mod tests {
    use fontdrasil::coords::NormalizedCoord;
    use fontir::variations::{Tent, VariationRegion};

    use super::*;

    fn non_default_region() -> VariationRegion {
        let mut region = VariationRegion::default();
        region.insert(
            Tag::new(b"wght"),
            Tent::new(
                NormalizedCoord::new(0.0),
                NormalizedCoord::new(1.0),
                NormalizedCoord::new(1.0),
            ),
        );
        region
    }

    #[test]
    fn keeps_if_some_deltas() {
        let deltas = GvarFragment {
            glyph_name: "blah".into(),
            deltas: vec![(
                non_default_region(),
                vec![
                    GlyphDelta::optional(0, 0),
                    GlyphDelta::required(1, 0),
                    GlyphDelta::optional(1, 0),
                ],
            )],
        }
        .to_deltas(&[Tag::new(b"wght")]);
        assert!(!deltas.is_empty(), "{deltas:?}");
    }

    #[test]
    fn drops_nop_deltas() {
        let can_omit = GlyphDelta::optional(0, 0);
        let deltas = GvarFragment {
            glyph_name: "blah".into(),
            deltas: vec![(non_default_region(), vec![can_omit; 3])],
        }
        .to_deltas(&[Tag::new(b"wght")]);
        assert!(deltas.is_empty(), "{deltas:?}");
    }
}
