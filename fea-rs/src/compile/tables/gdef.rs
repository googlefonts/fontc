//! Building the GDEF table.
//!
//! The GDEF table may be [declared explicitly][gdef-spec] in the input FEA, or
//! it may be generated during compilation by tracking the declaration of various
//! types, such as mark classes.
//!
//! [gdef-spec]: http://adobe-type-tools.github.io/afdko/OpenTypeFeatureFileSpecification.html#9b-gdef-table

use std::collections::{BTreeMap, BTreeSet, HashMap};

use write_fonts::types::GlyphId;

use write_fonts::tables::{
    self,
    gdef::{
        AttachList, AttachPoint, CaretValue, GlyphClassDef, LigCaretList, LigGlyph, MarkGlyphSets,
    },
    layout::{ClassDef, ClassDefBuilder, CoverageTableBuilder},
};

use super::{VariationIndexRemapping, VariationStoreBuilder};
use crate::common::GlyphClass;

#[derive(Clone, Debug, Default)]
pub(crate) struct GdefBuilder {
    pub glyph_classes: HashMap<GlyphId, ClassId>,
    pub attach: BTreeMap<GlyphId, BTreeSet<u16>>,
    pub ligature_pos: BTreeMap<GlyphId, Vec<CaretValue>>,
    pub mark_attach_class: BTreeMap<GlyphId, u16>,
    pub mark_glyph_sets: Vec<GlyphClass>,
    pub var_store: Option<VariationStoreBuilder>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum ClassId {
    Base = 1,
    Ligature = 2,
    Mark = 3,
    Component = 4,
}

impl From<ClassId> for GlyphClassDef {
    fn from(src: ClassId) -> GlyphClassDef {
        match src {
            ClassId::Base => GlyphClassDef::Base,
            ClassId::Ligature => GlyphClassDef::Ligature,
            ClassId::Mark => GlyphClassDef::Mark,
            ClassId::Component => GlyphClassDef::Component,
        }
    }
}

impl GdefBuilder {
    pub fn build(&self) -> (tables::gdef::Gdef, Option<VariationIndexRemapping>) {
        let mut table = tables::gdef::Gdef::new(
            self.build_class_def(),
            self.build_attach_list(),
            self.build_lig_caret_list(),
            self.build_mark_attach_class_def(),
        );

        table.mark_glyph_sets_def = self.build_mark_glyph_sets().into();
        if let Some((var_store, key_map)) = self.var_store.clone().map(VariationStoreBuilder::build)
        {
            table.item_var_store.set(var_store);
            (table, Some(key_map))
        } else {
            (table, None)
        }
    }

    fn build_class_def(&self) -> Option<ClassDef> {
        (!self.glyph_classes.is_empty()).then(|| {
            self.glyph_classes
                .iter()
                .map(|(gid, cls)| (*gid, *cls as u16))
                .collect::<ClassDefBuilder>()
                .build()
        })
    }

    fn build_attach_list(&self) -> Option<AttachList> {
        let mut coverage = CoverageTableBuilder::default();
        let mut attach_points = Vec::new();
        for (glyph, points) in &self.attach {
            coverage.add(*glyph);
            attach_points.push(AttachPoint::new(points.iter().copied().collect()));
        }
        (!attach_points.is_empty()).then(|| AttachList::new(coverage.build(), attach_points))
    }

    fn build_lig_caret_list(&self) -> Option<LigCaretList> {
        let mut coverage = CoverageTableBuilder::default();
        let mut lig_glyphs = Vec::new();
        for (glyph, carets) in &self.ligature_pos {
            coverage.add(*glyph);
            lig_glyphs.push(LigGlyph::new(carets.clone()));
        }
        (!lig_glyphs.is_empty()).then(|| LigCaretList::new(coverage.build(), lig_glyphs))
    }

    fn build_mark_glyph_sets(&self) -> Option<MarkGlyphSets> {
        (!self.mark_glyph_sets.is_empty()).then(|| {
            MarkGlyphSets::new(
                self.mark_glyph_sets
                    .iter()
                    .map(|cls| cls.iter().collect::<CoverageTableBuilder>().build())
                    .collect(),
            )
        })
    }

    fn build_mark_attach_class_def(&self) -> Option<ClassDef> {
        (!self.mark_attach_class.is_empty()).then(|| {
            self.mark_attach_class
                .iter()
                .map(|(a, b)| (*a, *b))
                .collect::<ClassDefBuilder>()
                .build()
        })
    }

    /// Errors if the class contains a glyph that is already in an existing class.
    pub fn add_glyph_class(
        &mut self,
        glyphs: GlyphClass,
        class: ClassId,
    ) -> Result<(), (GlyphId, ClassId)> {
        for glyph in glyphs.iter() {
            if let Some(prev_class) = self.glyph_classes.insert(glyph, class) {
                if prev_class != class {
                    return Err((glyph, prev_class));
                }
            }
        }
        Ok(())
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.glyph_classes.is_empty()
            && self.attach.is_empty()
            && self.ligature_pos.is_empty()
            && self.mark_attach_class.is_empty()
            && self.mark_glyph_sets.is_empty()
    }
}

impl std::fmt::Display for ClassId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ClassId::Base => write!(f, "Base"),
            ClassId::Ligature => write!(f, "Ligature"),
            ClassId::Mark => write!(f, "Mark"),
            ClassId::Component => write!(f, "Component"),
        }
    }
}
