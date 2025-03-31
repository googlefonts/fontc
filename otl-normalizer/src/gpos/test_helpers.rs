use std::fmt::Debug;

use write_fonts::{
    tables::{
        gpos::{
            builders::{
                AnchorBuilder as Anchor, MarkToBaseBuilder, PairPosBuilder,
                ValueRecordBuilder as ValueRecord,
            },
            MarkBasePosFormat1, PairPos,
        },
        layout::builders::Builder,
        variations::ivs_builder::VariationStoreBuilder,
    },
    types::GlyphId16,
};

use crate::gpos::MarkAttachmentRule;

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
    pub base_gid: GlyphId16,
    pub mark_gid: GlyphId16,
    pub base_anchor: (i16, i16),
    pub mark_anchor: (i16, i16),
}

pub type RawAnchor = (i16, i16);

impl PartialEq<(u16, RawAnchor, u16, RawAnchor)> for SimpleAnchorRule {
    fn eq(&self, other: &(u16, RawAnchor, u16, RawAnchor)) -> bool {
        let (base_id, base_anchor, mark_id, mark_anchor) = *other;
        self.base_gid.to_u16() == base_id
            && self.base_anchor == base_anchor
            && self.mark_gid.to_u16() == mark_id
            && self.mark_anchor == mark_anchor
    }
}

impl Debug for SimpleAnchorRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let base = self.base_gid.to_u16();
        let mark = self.mark_gid.to_u16();
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
