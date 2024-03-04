//! Building the GDEF table.
//!
//! The GDEF table may be [declared explicitly][gdef-spec] in the input FEA, or
//! it may be generated during compilation by tracking the declaration of various
//! types, such as mark classes.
//!
//! [gdef-spec]: http://adobe-type-tools.github.io/afdko/OpenTypeFeatureFileSpecification.html#9b-gdef-table

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Display;

use write_fonts::types::GlyphId;

use write_fonts::tables::{
    self,
    gdef::{
        AttachList, AttachPoint, CaretValue, GlyphClassDef, LigCaretList, LigGlyph, MarkGlyphSets,
    },
    layout::{ClassDef, ClassDefBuilder, CoverageTableBuilder},
};

use super::{VariationIndexRemapping, VariationStoreBuilder};
use crate::common::{GlyphClass, GlyphSet};

/// Data collected from a GDEF block.
#[derive(Clone, Debug, Default)]
pub struct GdefBuilder {
    pub glyph_classes: HashMap<GlyphId, GlyphClassDef>,
    /// if `true`, then glyph classes were not declared explicitly.
    ///
    /// we track this because it is an important distinction when using the
    /// glyph classes for manually generated kern/markpos lookups
    pub glyph_classes_were_inferred: bool,
    pub attach: BTreeMap<GlyphId, BTreeSet<u16>>,
    pub ligature_pos: BTreeMap<GlyphId, Vec<CaretValue>>,
    pub mark_attach_class: BTreeMap<GlyphId, u16>,
    pub mark_glyph_sets: Vec<GlyphSet>,
    pub var_store: Option<VariationStoreBuilder>,
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
    pub(crate) fn add_glyph_class(
        &mut self,
        // technically should be a GlyphSet, but we're storing it as individual
        // glyphs and this is private API, so :shrug:
        glyphs: GlyphClass,
        class: GlyphClassDef,
    ) -> Result<(), (GlyphId, GlyphClassDef)> {
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
            && self.var_store.is_none()
    }
}

pub(crate) trait GlyphClassDefExt {
    fn display(&self) -> impl Display;
}

impl GlyphClassDefExt for GlyphClassDef {
    fn display(&self) -> impl Display {
        match self {
            GlyphClassDef::Base => "Base",
            GlyphClassDef::Ligature => "Ligature",
            GlyphClassDef::Mark => "Mark",
            GlyphClassDef::Component => "Component",
            _ => "Invalid",
        }
    }
}
