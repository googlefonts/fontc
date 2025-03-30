//! Helps coordinate the graph execution for BE

use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
    fs::File,
    io::{self, BufReader, BufWriter, Read, Write},
    path::{Path, PathBuf},
    sync::Arc,
};

use fea_rs::{
    compile::{Compilation, FeatureKey, PendingLookup},
    GlyphMap, GlyphSet, ParseTree,
};
use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessControlList, Identifier, IdentifierDiscriminant, Work},
    types::GlyphName,
};
use fontir::{
    ir::{self, GlyphOrder, KernGroup},
    orchestration::{
        Context as FeContext, ContextItem, ContextMap, Flags, IdAware, Persistable,
        PersistentStorage, WorkId as FeWorkIdentifier,
    },
    variations::VariationRegion,
};

use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};

use write_fonts::{
    dump_table,
    read::{collections::IntSet, tables::gsub::Gsub as ReadGsub, FontRead},
    tables::{
        base::Base,
        cmap::Cmap,
        colr::Colr,
        cpal::Cpal,
        fvar::Fvar,
        gasp::Gasp,
        gdef::{Gdef, GlyphClassDef},
        glyf::Glyph as RawGlyph,
        gpos::{
            builders::{
                CursivePosBuilder, MarkToBaseBuilder, MarkToLigBuilder, MarkToMarkBuilder,
                PairPosBuilder, ValueRecordBuilder,
            },
            Gpos,
        },
        gsub::Gsub,
        gvar::{GlyphDelta, GlyphDeltas},
        head::Head,
        hhea::Hhea,
        hvar::Hvar,
        layout::builders::CaretValueBuilder,
        loca::LocaFormat,
        maxp::Maxp,
        meta::Meta,
        mvar::Mvar,
        name::Name,
        os2::Os2,
        post::Post,
        stat::Stat,
        vhea::Vhea,
    },
    types::{GlyphId16, Tag},
    validate::Validate,
    FontWrite,
};

use crate::{avar::PossiblyEmptyAvar, error::Error, paths::Paths};

type KernBlock = usize;

/// Unique identifier of work.
///
/// If there are no fields work is unique.
/// Meant to be small and cheap to copy around.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum WorkId {
    Features,
    FeaturesAst,
    Avar,
    Cmap,
    Colr,
    Cpal,
    Font,
    Fvar,
    Gasp,
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
    Meta,
    Vhea,
    Vmtx,
    GatherIrKerning,
    KernFragment(KernBlock),
    GatherBeKerning,
    Loca,
    LocaFormat,
    Marks,
    Maxp,
    Mvar,
    Name,
    Os2,
    Post,
    Stat,
    ExtraFeaTables,
}

impl WorkId {
    /// An id representing access to all glyf fragments
    pub const ALL_GLYF_FRAGMENTS: WorkId = WorkId::GlyfFragment(GlyphName::NOTDEF);

    /// An id representing access to all gvar fragments
    pub const ALL_GVAR_FRAGMENTS: WorkId = WorkId::GvarFragment(GlyphName::NOTDEF);
}

impl Identifier for WorkId {
    fn discriminant(&self) -> IdentifierDiscriminant {
        match self {
            WorkId::Features => "BeFeatures",
            WorkId::Meta => "BeMeta",
            WorkId::FeaturesAst => "BeFeaturesAst",
            WorkId::Avar => "BeAvar",
            WorkId::Cmap => "BeCmap",
            WorkId::Colr => "BeColr",
            WorkId::Cpal => "BeCpal",
            WorkId::Font => "BeFont",
            WorkId::Fvar => "BeFvar",
            WorkId::Gasp => "BeGasp",
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
            WorkId::GatherIrKerning => "BeGatherIr",
            WorkId::KernFragment(..) => "BeKernFragment",
            WorkId::GatherBeKerning => "BeGatherKernFragments",
            WorkId::Loca => "BeLoca",
            WorkId::LocaFormat => "BeLocaFormat",
            WorkId::Marks => "BeMarks",
            WorkId::Maxp => "BeMaxp",
            WorkId::Mvar => "BeMvar",
            WorkId::Name => "BeName",
            WorkId::Os2 => "BeOs2",
            WorkId::Post => "BePost",
            WorkId::Stat => "BeStat",
            WorkId::Vhea => "BeVhea",
            WorkId::Vmtx => "BeVmtx",
            WorkId::ExtraFeaTables => "ExtraFeaTables",
        }
    }
}

// Identifies work of any type, FE, BE, ... future optimization passes, w/e.
// Useful because BE work can very reasonably depend on FE work
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnyWorkId {
    Fe(FeWorkIdentifier),
    Be(WorkId),
    /// Used to capture timing not associated with work
    InternalTiming(&'static str),
}

impl Identifier for AnyWorkId {
    fn discriminant(&self) -> IdentifierDiscriminant {
        match self {
            AnyWorkId::Fe(id) => id.discriminant(),
            AnyWorkId::Be(id) => id.discriminant(),
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

/// Tables other than GPOS/GSUB/GDEF generated from FEA
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct ExtraFeaTables {
    pub name: Option<Name>,
    // TODO: currently we only handle merging the name table
    pub head: Option<Head>,
    pub hhea: Option<Hhea>,
    pub vhea: Option<Vhea>,
    pub os2: Option<Os2>,
    pub base: Option<Base>,
    pub stat: Option<Stat>,
}

impl From<Compilation> for ExtraFeaTables {
    fn from(src: Compilation) -> ExtraFeaTables {
        let Compilation {
            head,
            hhea,
            vhea,
            os2,
            base,
            name,
            stat,
            ..
        } = src;
        ExtraFeaTables {
            head,
            hhea,
            vhea,
            os2,
            base,
            stat,
            name,
        }
    }
}

impl ExtraFeaTables {
    pub(crate) fn log_unhandled_extras(&self) {
        for (name, unhandled) in [
            ("head", self.head.is_some()),
            ("hhea", self.hhea.is_some()),
            ("vhea", self.vhea.is_some()),
            ("os2", self.os2.is_some()),
            ("base", self.base.is_some()),
            ("stat", self.stat.is_some()),
        ] {
            if unhandled {
                log::warn!("FEA generated unused table '{name}'");
            }
        }
    }
}

// we could use serde here but it produces really big outputs; so instead
// we can use fontwrite on each table, and then serialize an array of Option<Vec<u8>>
impl Persistable for ExtraFeaTables {
    fn read(from: &mut dyn Read) -> Self {
        fn read_table<'a, T: FontRead<'a>>(bytes: Option<&'a Vec<u8>>) -> Option<T> {
            let bytes = bytes?.as_slice();
            Some(T::read(bytes.into()).unwrap())
        }

        let [head, hhea, vhea, os2, base, stat, name]: [Option<Vec<u8>>; 7] =
            bincode::deserialize_from(from).unwrap();

        Self {
            head: read_table(head.as_ref()),
            hhea: read_table(hhea.as_ref()),
            vhea: read_table(vhea.as_ref()),
            os2: read_table(os2.as_ref()),
            base: read_table(base.as_ref()),
            stat: read_table(stat.as_ref()),
            name: read_table(name.as_ref()),
        }
    }

    fn write(&self, to: &mut dyn Write) {
        fn dump<T: Validate + FontWrite>(table: Option<&T>) -> Option<Vec<u8>> {
            table.map(write_fonts::dump_table).transpose().unwrap()
        }
        let ExtraFeaTables {
            head,
            hhea,
            vhea,
            os2,
            base,
            stat,
            name,
        } = self;

        let out = [
            dump(head.as_ref()),
            dump(hhea.as_ref()),
            dump(vhea.as_ref()),
            dump(os2.as_ref()),
            dump(base.as_ref()),
            dump(stat.as_ref()),
            dump(name.as_ref()),
        ];

        bincode::serialize_into(to, &out).unwrap()
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

    pub fn is_composite(&self) -> bool {
        matches!(&self.data, RawGlyph::Composite(_))
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
        let glyph = FontRead::read(bytes.as_slice().into()).unwrap();
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

                let coordinates = axis_order
                    .iter()
                    .map(|axis| *region.get(axis).unwrap())
                    .map(Into::into)
                    .collect();

                Some(GlyphDeltas::new(coordinates, deltas.clone()))
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

#[derive(Default, Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct MarkLookups {
    pub(crate) mark_base: Vec<PendingLookup<MarkToBaseBuilder>>,
    pub(crate) mark_mark: Vec<PendingLookup<MarkToMarkBuilder>>,
    pub(crate) mark_lig: Vec<PendingLookup<MarkToLigBuilder>>,
}
/// Marks, ready to feed to fea-rs in the form it expects
#[derive(Default, Clone, Serialize, Deserialize, PartialEq)]
pub struct FeaRsMarks {
    pub(crate) glyphmap: GlyphMap,
    pub(crate) mark_mkmk: MarkLookups,
    pub(crate) abvm: MarkLookups,
    pub(crate) blwm: MarkLookups,
    pub(crate) curs: Vec<PendingLookup<CursivePosBuilder>>,
    pub(crate) lig_carets: BTreeMap<GlyphId16, Vec<CaretValueBuilder>>,
}

impl Persistable for FeaRsMarks {
    fn read(from: &mut dyn Read) -> Self {
        bincode::deserialize_from(from).unwrap()
    }

    fn write(&self, to: &mut dyn io::Write) {
        bincode::serialize_into(to, self).unwrap()
    }
}

/// Kerns, ready to feed to fea-rs in the form it expects
///
/// The aggregation of all [KernFragment]s.
#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq)]
pub struct FeaRsKerns {
    /// ordered!
    pub lookups: Vec<PendingLookup<PairPosBuilder>>,
    /// each value is a set of lookups, referenced by their order in array above
    pub features: BTreeMap<FeatureKey, Vec<usize>>,
}

/// "Stage one" state from fea compilation, which is needed as input later.
///
/// To generate marks & kerns we need access to things like the AST and a
/// compiled GSUB table; to avoid needing to compile GSUB multiple times we
/// do it once up front, and then that gets shared by later jobs.
///
/// Before storing the AST, ensure that it has been validated (via [`fea_rs::compile::validate`]).
/// This does not include features that are generated, such as for kerning or marks.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct FeaFirstPassOutput {
    /// A validated abstract syntax tree.
    pub ast: ParseTree,
    // bytes of the compiled gsub table
    gsub_bytes: Option<Vec<u8>>,
    /// GDEF classes, if they were declared explicitly in the fea
    pub(crate) gdef_classes: Option<HashMap<GlyphId16, GlyphClassDef>>,
}

impl FeaFirstPassOutput {
    pub fn new(ast: ParseTree, compilation: Compilation) -> Result<Self, Error> {
        let gsub_bytes = compilation
            .gsub
            .as_ref()
            .map(write_fonts::dump_table)
            .transpose()
            .map_err(|e| Error::DumpTableError {
                e,
                context: "GSUB".into(),
            })?;
        Ok(Self {
            ast,
            gsub_bytes,
            gdef_classes: compilation.gdef_classes,
        })
    }

    #[cfg(test)]
    pub(crate) fn for_test(ast: ParseTree, glyph_map: &GlyphMap) -> Result<Self, Error> {
        use fea_rs::{
            compile::{NopFeatureProvider, NopVariationInfo},
            Opts,
        };

        match fea_rs::compile::compile::<NopVariationInfo, NopFeatureProvider>(
            &ast,
            glyph_map,
            None,
            None,
            Opts::new().compile_gpos(false),
        ) {
            Ok((compilation, _)) => Self::new(ast, compilation),
            Err(err) => panic!("{}", err.display()),
        }
    }

    /// Return a read-fonts GSUB table (for computing glyph closure for mark/kern)
    pub fn gsub(&self) -> Option<ReadGsub> {
        self.gsub_bytes
            .as_ref()
            .and_then(|bytes| ReadGsub::read(bytes.as_slice().into()).ok())
    }
}

impl FeaRsKerns {
    pub fn is_empty(&self) -> bool {
        self.lookups.is_empty()
    }

    #[cfg(test)]
    pub(crate) fn lookups_for_feature(
        &self,
        feature: &FeatureKey,
    ) -> Vec<&PendingLookup<PairPosBuilder>> {
        self.features
            .get(feature)
            .into_iter()
            .flatten()
            .map(|id| &self.lookups[*id])
            .collect()
    }
}

impl Persistable for FeaRsKerns {
    fn read(from: &mut dyn Read) -> Self {
        bincode::deserialize_from(from).unwrap()
    }

    fn write(&self, to: &mut dyn io::Write) {
        bincode::serialize_into(to, self).unwrap()
    }
}

impl Persistable for FeaFirstPassOutput {
    fn read(from: &mut dyn Read) -> Self {
        bincode::deserialize_from(from).unwrap()
    }

    fn write(&self, to: &mut dyn Write) {
        bincode::serialize_into(to, self).unwrap()
    }
}

/// Kerning adjustments at various locations
pub type KernAdjustments = BTreeMap<NormalizedLocation, OrderedFloat<f64>>;

/// Every kerning pair we have, taking from IR.
///
/// It is an invariant that every group referenced in an adjustment exists in the
/// groups mapping, and every glyph in a rule (including in all groups) is defined
/// in the glyph order.
#[derive(Default, Clone, Serialize, Deserialize, PartialEq)]
pub struct AllKerningPairs {
    /// A mapping from named kern groups to the appropriate set of glyphs
    pub groups: BTreeMap<KernGroup, GlyphSet>,
    pub adjustments: Vec<(ir::KernPair, KernAdjustments)>,
}

impl Persistable for AllKerningPairs {
    fn read(from: &mut dyn Read) -> Self {
        bincode::deserialize_from(from).unwrap()
    }

    fn write(&self, to: &mut dyn io::Write) {
        bincode::serialize_into(to, self).unwrap()
    }
}

/// One side of a kerning pair, represented as glyph ids
///
/// This parallels the [`ir::KernSide`] type, with glyph names resolved
/// to GIDs.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, PartialOrd, Eq, Ord)]
pub(crate) enum KernSide {
    /// A specific glyph
    Glyph(GlyphId16),
    /// A group of glyphs
    Group(GlyphSet),
}

/// A resolved user kern rule
///
/// This parallels the [`ir::KernPair`] type, but using glyph ids instead
/// of glyph names and a finalized value record instead of per-location positions.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, PartialOrd, Eq, Ord)]
pub(crate) struct KernPair {
    pub(crate) side1: KernSide,
    pub(crate) side2: KernSide,
    pub(crate) value: ValueRecordBuilder,
}

impl KernSide {
    pub(crate) const fn empty() -> Self {
        Self::Group(IntSet::empty())
    }

    pub(crate) fn is_empty(&self) -> bool {
        matches!(self, KernSide::Group(items) if items.is_empty())
    }

    pub(crate) fn is_group(&self) -> bool {
        matches!(self, KernSide::Group(_))
    }

    /// Convert from IR (which uses glyph names) to our representation (using ids)
    pub(crate) fn from_ir_side(
        ir: &ir::KernSide,
        glyphs: &GlyphOrder,
        groups: &BTreeMap<KernGroup, GlyphSet>,
    ) -> Option<Self> {
        match ir {
            ir::KernSide::Glyph(name) => glyphs.glyph_id(name).map(Self::Glyph),
            ir::KernSide::Group(name) => groups.get(name).cloned().map(Self::Group),
        }
    }

    fn contains(&self, gid: u16) -> bool {
        let gid = GlyphId16::new(gid);
        match self {
            KernSide::Glyph(glyph_id16) => glyph_id16 == &gid,
            KernSide::Group(glyph_set) => glyph_set.contains(gid),
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = GlyphId16> + '_ {
        let (first, second) = match self {
            Self::Glyph(gid) => (Some(*gid), None),
            Self::Group(group) => (None, Some(group)),
        };

        first
            .into_iter()
            .chain(second.into_iter().flat_map(|set| set.iter()))
    }
}

impl Display for KernSide {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KernSide::Glyph(gid) => write!(f, "{gid}"),
            KernSide::Group(group) => {
                let mut first = true;
                write!(f, "[")?;
                for gid in group.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{gid}")?;
                    first = false;
                }
                write!(f, "]")
            }
        }
    }
}

impl KernPair {
    /// if a rule is right-to-left, we need to set both x-advance AND x-position
    ///
    /// see <https://github.com/unified-font-object/ufo-spec/issues/16#issuecomment-119947719>
    /// for further details. The tl;dr is that the lookup itself does not have
    /// any knowledge of writing direction.
    pub(crate) fn make_rtl_compatible(&mut self) {
        self.value.make_rtl_compatible()
    }

    #[allow(dead_code)] // useful for debugging
    pub(crate) fn contains(&self, gid: u16) -> bool {
        self.side1.contains(gid) || self.side2.contains(gid)
    }

    pub(crate) fn add_to(self, builder: &mut PairPosBuilder) {
        match (self.side1, self.side2) {
            // these unwraps are all fine because we've already validated the input
            (KernSide::Glyph(side1), KernSide::Glyph(side2)) => {
                builder.insert_pair(side1, self.value, side2, Default::default());
            }
            (KernSide::Group(side1), KernSide::Group(side2)) => {
                builder.insert_classes(side1, self.value, side2, Default::default());
            }
            // if groups are mixed with glyphs then we enumerate the group
            (KernSide::Glyph(side1), KernSide::Group(side2)) => {
                for side2 in side2.iter() {
                    builder.insert_pair(side1, self.value.clone(), side2, Default::default());
                }
            }
            (KernSide::Group(side1), KernSide::Glyph(side2)) => {
                for side1 in side1.iter() {
                    builder.insert_pair(side1, self.value.clone(), side2, Default::default());
                }
            }
        }
    }

    /// Returns true if this entry has no glyphs in common with the provided set
    pub(crate) fn glyphs_are_disjoint(&self, glyphs: &IntSet<GlyphId16>) -> bool {
        self.first_glyphs()
            .chain(self.second_glyphs())
            .all(|gid| !glyphs.contains(gid))
    }

    /// a helper used when splitting kerns based on script direction
    pub(crate) fn with_new_glyphs(&self, side1: GlyphSet, side2: GlyphSet) -> Self {
        // for each of our sides, we want to keep groups groups and glyphs glyphs
        let (side1, side2) = match (&self.side1, &self.side2) {
            (KernSide::Glyph(_), KernSide::Glyph(_)) => (self.side1.clone(), self.side2.clone()),
            (KernSide::Glyph(_), KernSide::Group(_)) => {
                assert_eq!(side1.len(), 1);
                (self.side1.clone(), KernSide::Group(side2))
            }
            (KernSide::Group(_), KernSide::Glyph(_)) => {
                assert_eq!(side2.len(), 1);
                (KernSide::Group(side1), self.side2.clone())
            }
            _ => {
                assert!(!side1.is_empty() && !side2.is_empty());
                (KernSide::Group(side1), KernSide::Group(side2))
            }
        };
        KernPair {
            side1,
            side2,
            value: self.value.clone(),
        }
    }

    pub(crate) fn first_glyphs(&self) -> impl Iterator<Item = GlyphId16> + '_ {
        self.side1.iter()
    }

    pub(crate) fn second_glyphs(&self) -> impl Iterator<Item = GlyphId16> + '_ {
        self.side2.iter()
    }
}

/// A chunk of kerning that needs to be fed into a [PairPosBuilder]
///
/// Points to a slice of [AllKerningPairs].
#[derive(Default, Clone, Serialize, Deserialize, PartialEq)]
pub struct KernFragment {
    pub(crate) segment: usize,
    pub(crate) kerns: Vec<KernPair>,
}

impl IdAware<AnyWorkId> for KernFragment {
    fn id(&self) -> AnyWorkId {
        AnyWorkId::Be(WorkId::KernFragment(self.segment))
    }
}

impl Persistable for KernFragment {
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
    pub avar: BeContextItem<PossiblyEmptyAvar>,
    pub cmap: BeContextItem<Cmap>,
    pub colr: BeContextItem<Colr>,
    pub cpal: BeContextItem<Cpal>,
    pub fvar: BeContextItem<Fvar>,
    pub gasp: BeContextItem<Gasp>,
    pub glyf: BeContextItem<Bytes>,
    pub gsub: BeContextItem<Gsub>,
    pub gpos: BeContextItem<Gpos>,
    pub gdef: BeContextItem<Gdef>,
    pub gvar: BeContextItem<Bytes>,
    pub post: BeContextItem<Post>,
    pub meta: BeContextItem<Meta>,
    pub loca: BeContextItem<Bytes>,
    pub loca_format: BeContextItem<LocaFormatWrapper>,
    pub maxp: BeContextItem<Maxp>,
    pub name: BeContextItem<Name>,
    pub os2: BeContextItem<Os2>,
    pub head: BeContextItem<Head>,
    pub hhea: BeContextItem<Hhea>,
    pub hmtx: BeContextItem<Bytes>,
    pub hvar: BeContextItem<Hvar>,
    pub mvar: BeContextItem<Mvar>,
    pub vhea: BeContextItem<Vhea>,
    pub vmtx: BeContextItem<Bytes>,
    pub all_kerning_pairs: BeContextItem<AllKerningPairs>,
    pub kern_fragments: BeContextMap<KernFragment>,
    pub fea_ast: BeContextItem<FeaFirstPassOutput>,
    pub fea_rs_kerns: BeContextItem<FeaRsKerns>,
    pub fea_rs_marks: BeContextItem<FeaRsMarks>,
    pub extra_fea_tables: BeContextItem<ExtraFeaTables>,
    pub stat: BeContextItem<Stat>,
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
            colr: self.colr.clone_with_acl(acl.clone()),
            cpal: self.cpal.clone_with_acl(acl.clone()),
            fvar: self.fvar.clone_with_acl(acl.clone()),
            gasp: self.gasp.clone_with_acl(acl.clone()),
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
            mvar: self.mvar.clone_with_acl(acl.clone()),
            meta: self.meta.clone_with_acl(acl.clone()),
            vhea: self.vhea.clone_with_acl(acl.clone()),
            vmtx: self.vmtx.clone_with_acl(acl.clone()),
            all_kerning_pairs: self.all_kerning_pairs.clone_with_acl(acl.clone()),
            kern_fragments: self.kern_fragments.clone_with_acl(acl.clone()),
            fea_rs_kerns: self.fea_rs_kerns.clone_with_acl(acl.clone()),
            fea_rs_marks: self.fea_rs_marks.clone_with_acl(acl.clone()),
            stat: self.stat.clone_with_acl(acl.clone()),
            fea_ast: self.fea_ast.clone_with_acl(acl.clone()),
            extra_fea_tables: self.extra_fea_tables.clone_with_acl(acl.clone()),
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
            colr: ContextItem::new(WorkId::Colr.into(), acl.clone(), persistent_storage.clone()),
            cpal: ContextItem::new(WorkId::Cpal.into(), acl.clone(), persistent_storage.clone()),
            fvar: ContextItem::new(WorkId::Fvar.into(), acl.clone(), persistent_storage.clone()),
            gasp: ContextItem::new(WorkId::Gasp.into(), acl.clone(), persistent_storage.clone()),
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
            mvar: ContextItem::new(WorkId::Mvar.into(), acl.clone(), persistent_storage.clone()),
            meta: ContextItem::new(WorkId::Meta.into(), acl.clone(), persistent_storage.clone()),
            vhea: ContextItem::new(WorkId::Vhea.into(), acl.clone(), persistent_storage.clone()),
            vmtx: ContextItem::new(WorkId::Vmtx.into(), acl.clone(), persistent_storage.clone()),
            all_kerning_pairs: ContextItem::new(
                WorkId::GatherIrKerning.into(),
                acl.clone(),
                persistent_storage.clone(),
            ),
            kern_fragments: ContextMap::new(acl.clone(), persistent_storage.clone()),
            fea_rs_kerns: ContextItem::new(
                WorkId::GatherBeKerning.into(),
                acl.clone(),
                persistent_storage.clone(),
            ),
            fea_rs_marks: ContextItem::new(
                WorkId::Marks.into(),
                acl.clone(),
                persistent_storage.clone(),
            ),
            fea_ast: ContextItem::new(
                WorkId::FeaturesAst.into(),
                acl.clone(),
                persistent_storage.clone(),
            ),
            stat: ContextItem::new(WorkId::Stat.into(), acl.clone(), persistent_storage.clone()),
            extra_fea_tables: ContextItem::new(
                WorkId::ExtraFeaTables.into(),
                acl.clone(),
                persistent_storage.clone(),
            ),
            font: ContextItem::new(WorkId::Font.into(), acl, persistent_storage),
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
        self.persistent_storage.paths.target_file(&WorkId::Font)
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

pub(crate) fn to_bytes<T>(table: &T) -> Option<Vec<u8>>
where
    T: FontWrite + Validate,
{
    write_fonts::dump_table(table).ok()
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

    // kerning needs to be sort such that (glyph, glyph) comes first, then
    // (glyph, class), (class, glyph) and (class, class). This matches the
    // behaviour of the FEA syntax, and ensures that more specific rules are
    // applied first
    #[test]
    fn kern_pair_sort_order() {
        let glyph = KernSide::Glyph(GlyphId16::new(5));
        let class_ = KernSide::Group([1, 2, 3, 4].into_iter().map(GlyphId16::new).collect());
        let value = ValueRecordBuilder::new().with_x_advance(420);
        let glyph_glyph = KernPair {
            side1: glyph.clone(),
            side2: glyph.clone(),
            value: value.clone(),
        };

        let glyph_class = KernPair {
            side1: glyph.clone(),
            side2: class_.clone(),
            value: value.clone(),
        };

        let class_glyph = KernPair {
            side1: class_.clone(),
            side2: glyph.clone(),
            value: value.clone(),
        };

        let class_class = KernPair {
            side1: class_.clone(),
            side2: class_.clone(),
            value: value.clone(),
        };

        let mut unsorted = [&class_class, &glyph_class, &glyph_glyph, &class_glyph];
        unsorted.sort();
        assert_eq!(
            unsorted,
            [&glyph_glyph, &glyph_class, &class_glyph, &class_class]
        );
    }
}
