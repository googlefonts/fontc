use fonttools::types::Tag;

use crate::types::{GlyphClass, GlyphId};

/// The explicit tables allowed in a fea file
#[allow(non_snake_case)]
#[derive(Clone, Debug, Default)]
pub(crate) struct Tables {
    pub head: Option<head>,
    pub hhea: Option<hhea>,
    pub vhea: Option<vhea>,
    pub GDEF: Option<GDEF>,
    pub BASE: Option<BASE>,
    //name: Option<tables::name>,
    //OS2: Option<tables::OS2>,
    //STAT: Option<tables::STAT>,
}
#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub struct head {
    font_revision: u16,
}

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub struct hhea {
    caret_offset: i32,
    ascender: i32,
    descender: i32,
    line_gap: i32,
}

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub struct vhea {
    vert_typo_ascender: i32,
    vert_typo_descender: i32,
    vert_typo_line_gap: i32,
}

#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct GDEF {
    pub base_glyphs: Option<GlyphClass>,
    pub ligature_glyphs: Option<GlyphClass>,
    pub mark_glyphs: Option<GlyphClass>,
    pub component_glyphs: Option<GlyphClass>,

    pub attach: Vec<(GlyphId, Vec<i16>)>,
    pub ligature_caret_pos: Vec<(GlyphId, Vec<i16>)>,
    pub ligature_caret_index: Vec<(GlyphId, Vec<i16>)>,
}

#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct BASE {
    pub horiz_tag_list: Vec<Tag>,
    pub horiz_script_list: Vec<ScriptRecord>,
    pub vert_tag_list: Vec<Tag>,
    pub vert_script_list: Vec<ScriptRecord>,
}

#[derive(Clone, Debug)]
pub struct ScriptRecord {
    pub script: Tag,
    pub default_baseline_tag: Tag,
    pub values: Vec<i16>,
}
