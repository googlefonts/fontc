use std::fmt::Debug;

use write_fonts::{
    read::{
        FontRead,
        tables::{gdef::Gdef, gpos::Gpos},
    },
    tables::{
        gdef::Gdef as WriteGdef,
        gpos::{
            Gpos as WriteGpos, MarkBasePosFormat1, PairPos, PositionLookup, SinglePos,
            builders::{
                AnchorBuilder as Anchor, MarkToBaseBuilder, PairPosBuilder, SinglePosBuilder,
                ValueRecordBuilder as ValueRecord,
            },
        },
        layout::{
            Feature, FeatureList, FeatureRecord, LangSys, LookupList, Script, ScriptList,
            ScriptRecord, builders::Builder,
        },
        variations::{common_builder::RemapVarStore, ivs_builder::VariationStoreBuilder},
    },
    types::{GlyphId, GlyphId16, Tag},
};

use crate::gpos::MarkAttachmentRule;

pub fn build_gpos_and_gdef(
    lookup: PositionLookup,
    store: VariationStoreBuilder,
) -> (Gpos<'static>, Gdef<'static>) {
    let mut gpos = WriteGpos::new(
        ScriptList::new(vec![ScriptRecord::new(
            Tag::new(b"DFLT"),
            Script::new(Some(LangSys::new(vec![0])), vec![]),
        )]),
        FeatureList::new(vec![FeatureRecord::new(
            Tag::new(b"dist"),
            Feature::new(None, vec![0]),
        )]),
        LookupList::new(vec![lookup]),
    );
    let (store, remapping) = store.build();
    gpos.remap_variation_indices(&remapping);
    let gdef = WriteGdef {
        item_var_store: Some(store).into(),
        ..Default::default()
    };
    // keep these small test tables alive for the returned refs
    let gpos_bytes: &'static [u8] = write_fonts::dump_table(&gpos).unwrap().leak();
    let gdef_bytes: &'static [u8] = write_fonts::dump_table(&gdef).unwrap().leak();
    (
        Gpos::read(gpos_bytes.into()).unwrap(),
        Gdef::read(gdef_bytes.into()).unwrap(),
    )
}

pub trait SimpleSinglePosBuilder {
    fn add_pos(&mut self, gid: u16, x_adv: i16);
    fn build_exactly_one_subtable(self) -> SinglePos;
}

impl SimpleSinglePosBuilder for SinglePosBuilder {
    fn add_pos(&mut self, gid: u16, x_adv: i16) {
        self.insert(
            GlyphId16::new(gid),
            ValueRecord::new().with_x_advance(x_adv),
        );
    }

    fn build_exactly_one_subtable(self) -> SinglePos {
        let mut varstore = VariationStoreBuilder::new(0);
        let subs = self.build(&mut varstore);
        assert_eq!(subs.len(), 1);
        subs.into_iter().next().unwrap()
    }
}

// a way to bolt a simpler API onto the PairPosBuilder from fea-rs
pub trait SimplePairPosBuilder {
    fn add_pair(&mut self, gid1: u16, gid2: u16, x_adv: i16);
    fn add_class(&mut self, class1: &[u16], class2: &[u16], x_adv: i16);
    fn build_exactly_one_subtable(self) -> PairPos;
}

impl SimplePairPosBuilder for PairPosBuilder {
    fn add_pair(&mut self, gid1: u16, gid2: u16, x_adv: i16) {
        self.insert_pair(
            GlyphId16::new(gid1),
            ValueRecord::new().with_x_advance(x_adv),
            GlyphId16::new(gid2),
            ValueRecord::new(),
        )
    }

    fn add_class(&mut self, class1: &[u16], class2: &[u16], x_adv: i16) {
        let class1 = class1.iter().copied().map(GlyphId16::new).collect();
        let class2 = class2.iter().copied().map(GlyphId16::new).collect();
        let record1 = ValueRecord::new().with_x_advance(x_adv);
        self.insert_classes(class1, record1, class2, ValueRecord::new())
    }

    fn build_exactly_one_subtable(self) -> PairPos {
        let mut varstore = VariationStoreBuilder::new(0);
        let subs = self.build(&mut varstore);
        assert_eq!(subs.len(), 1);
        subs.into_iter().next().unwrap()
    }
}

pub trait SimpleMarkBaseBuilder {
    fn add_mark(&mut self, gid: u16, class: &str, anchor: (i16, i16));
    fn add_base(&mut self, gid: u16, class: &str, anchor: (i16, i16));
    fn build_exactly_one_subtable(self) -> MarkBasePosFormat1;
}

impl SimpleMarkBaseBuilder for MarkToBaseBuilder {
    fn add_mark(&mut self, gid: u16, class: &str, anchor: (i16, i16)) {
        let anchor = Anchor::new(anchor.0, anchor.1);
        self.insert_mark(GlyphId16::new(gid), class, anchor)
            .unwrap();
    }

    fn add_base(&mut self, gid: u16, class: &str, anchor: (i16, i16)) {
        let anchor = Anchor::new(anchor.0, anchor.1);
        self.insert_base(GlyphId16::new(gid), class, anchor)
    }

    fn build_exactly_one_subtable(self) -> MarkBasePosFormat1 {
        let mut varstore = VariationStoreBuilder::new(0);
        let subs = self.build(&mut varstore);
        assert_eq!(subs.len(), 1);
        subs.into_iter().next().unwrap()
    }
}

// further decomposed for testing, so we just see one mark per entry
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SimpleAnchorRule {
    pub base_gid: GlyphId,
    pub mark_gid: GlyphId,
    pub base_anchor: (i16, i16),
    pub mark_anchor: (i16, i16),
}

pub type RawAnchor = (i16, i16);

impl PartialEq<(u16, RawAnchor, u16, RawAnchor)> for SimpleAnchorRule {
    fn eq(&self, other: &(u16, RawAnchor, u16, RawAnchor)) -> bool {
        let (base_id, base_anchor, mark_id, mark_anchor) = *other;
        self.base_gid == GlyphId::from(base_id)
            && self.base_anchor == base_anchor
            && self.mark_gid == GlyphId::from(mark_id)
            && self.mark_anchor == mark_anchor
    }
}

impl Debug for SimpleAnchorRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let base = self.base_gid.to_u32();
        let mark = self.mark_gid.to_u32();
        let (base_x, base_y) = self.base_anchor;
        let (mark_x, mark_y) = self.mark_anchor;
        write!(
            f,
            "({base}, ({base_x}, {base_y}), {mark}, ({mark_x}, {mark_y}))"
        )
    }
}

impl SimpleAnchorRule {
    // convert from the enum back to the specific pairpos type.
    pub fn from_mark_rules(rules: &[MarkAttachmentRule]) -> Vec<Self> {
        rules
            .iter()
            .flat_map(|rule| rule.iter_simple_rules())
            .collect()
    }
}
