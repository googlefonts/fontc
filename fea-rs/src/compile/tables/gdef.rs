//! Building the GDEF table.
//!
//! The GDEF table may be [declared explicitly][gdef-spec] in the input FEA, or
//! it may be generated during compilation by tracking the declaration of various
//! types, such as mark classes.
//!
//! [gdef-spec]: http://adobe-type-tools.github.io/afdko/OpenTypeFeatureFileSpecification.html#9b-gdef-table

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Display;

use write_fonts::types::GlyphId16;

use write_fonts::tables::{
    self,
    gdef::{
        AttachList, AttachPoint, CaretValue as RawCaretValue, GlyphClassDef, LigCaretList,
        LigGlyph, MarkGlyphSets,
    },
    layout::{
        builders::{CaretValueBuilder, CoverageTableBuilder},
        ClassDef,
    },
    variations::ivs_builder::RemapVariationIndices,
};

use super::{VariationIndexRemapping, VariationStoreBuilder};
use crate::common::{GlyphClass, GlyphSet};

/// Data collected from a GDEF block.
#[derive(Clone, Debug, Default)]
pub struct GdefBuilder {
    pub glyph_classes: HashMap<GlyphId16, GlyphClassDef>,
    /// if `true`, then glyph classes were not declared explicitly.
    ///
    /// we track this because it is an important distinction when using the
    /// glyph classes for manually generated kern/markpos lookups
    pub glyph_classes_were_inferred: bool,
    pub attach: BTreeMap<GlyphId16, BTreeSet<u16>>,
    pub ligature_pos: BTreeMap<GlyphId16, Vec<CaretValueBuilder>>,
    pub mark_attach_class: BTreeMap<GlyphId16, u16>,
    pub mark_glyph_sets: Vec<GlyphSet>,
    pub var_store: Option<VariationStoreBuilder>,
}

impl GdefBuilder {
    pub fn build(&self) -> (tables::gdef::Gdef, Option<VariationIndexRemapping>) {
        let mut var_store = self
            .var_store
            .clone()
            // we need to always have a varstore builder in order to build the lig_caret
            // table, but if we did not have one by now we aren't a variable font, so
            // we will throw it away after.
            .unwrap_or_else(|| VariationStoreBuilder::new(0));
        let mut table = tables::gdef::Gdef::new(
            self.build_class_def(),
            self.build_attach_list(),
            self.build_lig_caret_list(&mut var_store),
            self.build_mark_attach_class_def(),
        );

        table.mark_glyph_sets_def = self.build_mark_glyph_sets().into();
        if !var_store.is_empty() {
            let (var_store, key_map) = var_store.build();
            table.item_var_store.set(var_store);
            table.remap_variation_indices(&key_map);
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
                .collect()
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

    fn build_lig_caret_list(&self, var_store: &mut VariationStoreBuilder) -> Option<LigCaretList> {
        let mut coverage = CoverageTableBuilder::default();
        let mut lig_glyphs = Vec::new();
        for (glyph, carets) in &self.ligature_pos {
            coverage.add(*glyph);
            let mut carets = carets
                .iter()
                .map(|caret| caret.clone().build(var_store))
                .collect::<Vec<_>>();
            // TODO: I feel like we shouldn't be sorting these values if they are
            // point indices? Opened https://github.com/fonttools/fonttools/issues/3608 for
            // discussion
            carets.sort_by_key(|c| match c {
                RawCaretValue::Format1(table) => table.coordinate as i32,
                RawCaretValue::Format2(table) => table.caret_value_point_index as i32,
                RawCaretValue::Format3(table) => table.coordinate as i32,
            });

            lig_glyphs.push(LigGlyph::new(carets));
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
                .collect()
        })
    }

    /// Errors if the class contains a glyph that is already in an existing class.
    pub(crate) fn add_glyph_class(
        &mut self,
        // technically should be a GlyphSet, but we're storing it as individual
        // glyphs and this is private API, so :shrug:
        glyphs: GlyphClass,
        class: GlyphClassDef,
    ) -> Result<(), (GlyphId16, GlyphClassDef)> {
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
