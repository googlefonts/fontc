use fea_rs::compile::{Anchor, Builder, MarkToBaseBuilder, PairPosBuilder, ValueRecord};
use write_fonts::{
    tables::{
        gpos::{MarkBasePosFormat1, PairPos},
        variations::ivs_builder::VariationStoreBuilder,
    },
    types::GlyphId,
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
            GlyphId::new(gid1),
            ValueRecord::new().with_x_advance(x_adv),
            GlyphId::new(gid2),
            ValueRecord::new(),
        )
    }

    fn add_class(&mut self, class1: &[u16], class2: &[u16], x_adv: i16) {
        let class1 = class1.iter().copied().map(GlyphId::new).collect();
        let class2 = class2.iter().copied().map(GlyphId::new).collect();
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
        self.insert_mark(GlyphId::new(gid), class.into(), anchor)
            .unwrap();
    }

    fn add_base(&mut self, gid: u16, class: &str, anchor: (i16, i16)) {
        let anchor = Anchor::new(anchor.0, anchor.1);
        self.insert_base(GlyphId::new(gid), &class.into(), anchor)
    }

    fn build_exactly_one_subtable(self) -> MarkBasePosFormat1 {
        let mut varstore = VariationStoreBuilder::new(0);
        let subs = self.build(&mut varstore);
        assert_eq!(subs.len(), 1);
        subs.into_iter().next().unwrap()
    }
}

// further decomposed for testing, so we just see one mark per entry
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
        self.base_gid.to_u16() == base_id
            && self.base_anchor == base_anchor
            && self.mark_gid.to_u16() == mark_id
            && self.mark_anchor == mark_anchor
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
