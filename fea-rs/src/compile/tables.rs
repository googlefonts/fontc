use fonttools::types::Tag;
use smol_str::SmolStr;

use crate::types::{GlyphClass, GlyphId};

/// The explicit tables allowed in a fea file
#[allow(non_snake_case)]
#[derive(Clone, Debug, Default)]
pub(crate) struct Tables {
    pub head: Option<head>,
    pub hhea: Option<hhea>,
    pub vhea: Option<vhea>,
    pub name: Option<name>,
    pub GDEF: Option<GDEF>,
    pub BASE: Option<BASE>,
    //name: Option<tables::name>,
    //OS2: Option<tables::OS2>,
    //STAT: Option<tables::STAT>,
}
#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct head {
    pub font_revision: f32,
}

#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct hhea {
    pub caret_offset: i16,
    pub ascender: i16,
    pub descender: i16,
    pub line_gap: i16,
}

#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct vhea {
    pub vert_typo_ascender: i16,
    pub vert_typo_descender: i16,
    pub vert_typo_line_gap: i16,
}

#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct name {
    pub records: Vec<NameRecord>,
}

#[derive(Clone, Debug, Default)]
pub struct NameRecord {
    pub platform_id: u16,
    pub encoding_id: u16,
    pub language_id: u16,
    pub name_id: u16,
    pub string: SmolStr,
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

impl head {
    pub(crate) fn build(&self) -> fonttools::tables::head::head {
        // match what python fonttools does
        let mut table = fonttools::tables::head::head::new(self.font_revision, 0, 0, 0, 0, 0);
        table.magicNumber = 0;
        table.flags = 0;
        table.lowestRecPPEM = 0;
        table.fontDirectionHint = 0;
        table.created = chrono::NaiveDate::from_ymd(2011, 12, 13).and_hms(11, 22, 33);
        table.modified = chrono::NaiveDate::from_ymd(2011, 12, 13).and_hms(11, 22, 33);
        //table.checksumAdjustment = 0;
        table
    }
}

impl hhea {
    pub fn build(&self) -> fonttools::tables::hhea::hhea {
        fonttools::tables::hhea::hhea {
            majorVersion: 1,
            minorVersion: 0,
            ascender: self.ascender,
            descender: self.descender,
            lineGap: self.line_gap,
            advanceWidthMax: 0,
            minLeftSideBearing: 0,
            minRightSideBearing: 0,
            xMaxExtent: 0,
            caretSlopeRun: 0,
            caretSlopeRise: 0,
            caretOffset: self.caret_offset,
            reserved0: 0,
            reserved1: 0,
            reserved2: 0,
            reserved3: 0,
            metricDataFormat: 0,
            numberOfHMetrics: 0,
        }
    }
}

impl name {
    pub fn build(&self) -> fonttools::tables::name::name {
        let records = self
            .records
            .iter()
            .filter(|record| !(1..=6).contains(&record.name_id))
            .map(|record| fonttools::tables::name::NameRecord {
                platformID: record.platform_id,
                encodingID: record.encoding_id,
                languageID: record.language_id,
                nameID: record.name_id,
                string: record.string.trim_matches('"').to_string(),
            })
            .collect();
        fonttools::tables::name::name { records }
    }
}
