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
    pub OS2: Option<OS2>,
    pub STAT: Option<STAT>,
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
    pub name_id: u16,
    pub spec: NameSpec,
}

#[derive(Clone, Debug, Default)]
pub struct NameSpec {
    pub platform_id: u16,
    pub encoding_id: u16,
    pub language_id: u16,
    pub string: SmolStr,
}

#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
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
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
pub struct BASE {
    pub horiz_tag_list: Vec<Tag>,
    pub horiz_script_list: Vec<ScriptRecord>,
    pub vert_tag_list: Vec<Tag>,
    pub vert_script_list: Vec<ScriptRecord>,
}

#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct OS2 {
    pub fs_type: u16,
    pub panose: [u8; 10],
    pub unicode_range: u128,
    pub code_page_range: u64,
    pub typo_ascender: i16,
    pub typo_descender: i16,
    pub typo_line_gap: i16,
    pub x_height: i16,
    pub cap_height: i16,
    pub win_ascent: u16,
    pub win_descent: u16,
    pub width_class: u16,
    pub weight_class: u16,
    pub vendor_id: SmolStr,
    pub lower_op_size: Option<u16>,
    pub upper_op_size: Option<u16>,
    pub horiz_script_list: Vec<ScriptRecord>,
    pub vert_tag_list: Vec<Tag>,
    pub vert_script_list: Vec<ScriptRecord>,
    pub family_class: i16,
}

#[derive(Clone, Debug)]
pub struct ScriptRecord {
    pub script: Tag,
    pub default_baseline_tag: Tag,
    pub values: Vec<i16>,
}

#[derive(Clone, Debug)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
pub struct STAT {
    pub name: StatFallbackName,
    pub records: Vec<AxisRecord>,
    pub values: Vec<AxisValue>,
}

#[derive(Clone, Debug)]
pub struct AxisRecord {
    pub tag: Tag,
    pub name: Vec<NameSpec>,
    pub ordering: u16,
}

#[derive(Clone, Debug)]
pub struct AxisValue {
    pub flags: u16,
    pub name: Vec<NameSpec>,
    pub location: AxisLocation,
}

#[derive(Clone, Debug)]
pub enum AxisLocation {
    One {
        tag: Tag,
        value: f32,
    },
    Two {
        tag: Tag,
        nominal: f32,
        min: f32,
        max: f32,
    },
    Three {
        tag: Tag,
        value: f32,
        linked: f32,
    },
    Four(Vec<(Tag, f32)>),
}

#[derive(Clone, Debug)]
pub enum StatFallbackName {
    Id(u16),
    Record(Vec<NameSpec>),
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
                platformID: record.spec.platform_id,
                encodingID: record.spec.encoding_id,
                languageID: record.spec.language_id,
                nameID: record.name_id,
                string: record.spec.string.trim_matches('"').to_string(),
            })
            .collect();
        fonttools::tables::name::name { records }
    }
}

impl OS2 {
    pub fn bit_for_code_page(val: u16) -> Option<u8> {
        CODEPAGE_TO_BIT
            .iter()
            .find_map(|(page, bit)| if *page == val { Some(*bit) } else { None })
    }

    pub fn build(&self) -> fonttools::tables::os2::os2 {
        const MASK_32: u32 = 0xffff_ffff;
        let panose = fonttools::tables::os2::Panose {
            panose0: self.panose[0],
            panose1: self.panose[1],
            panose2: self.panose[2],
            panose3: self.panose[3],
            panose4: self.panose[4],
            panose5: self.panose[5],
            panose6: self.panose[6],
            panose7: self.panose[7],
            panose8: self.panose[8],
            panose9: self.panose[9],
        };
        let mut result = fonttools::tables::os2::os2 {
            version: 2,
            xAvgCharWidth: 0,
            usWeightClass: self.weight_class,
            usWidthClass: self.width_class,
            fsType: self.fs_type,
            ySubscriptXSize: 0,
            ySubscriptYSize: 0,
            ySubscriptXOffset: 0,
            ySubscriptYOffset: 0,
            ySuperscriptXSize: 0,
            ySuperscriptYSize: 0,
            ySuperscriptYOffset: 0,
            ySuperscriptXOffset: 0,
            yStrikeoutSize: 0,
            yStrikeoutPosition: 0,
            sFamilyClass: self.family_class,
            panose,
            ulUnicodeRange1: (self.unicode_range & MASK_32 as u128) as u32,
            ulUnicodeRange2: (self.unicode_range >> 32 & MASK_32 as u128) as u32,
            ulUnicodeRange3: (self.unicode_range >> 64 & MASK_32 as u128) as u32,
            ulUnicodeRange4: (self.unicode_range >> 96 & MASK_32 as u128) as u32,
            achVendID: self.vendor_id.parse().unwrap(),
            fsSelection: 0,
            usFirstCharIndex: 0,
            usLastCharIndex: 0,
            sTypoAscender: self.typo_ascender,
            sTypoDescender: self.typo_descender,
            sTypoLineGap: self.typo_line_gap,
            usWinAscent: self.win_ascent,
            usWinDescent: self.win_descent,
            ulCodePageRange1: Some((self.code_page_range & MASK_32 as u64) as u32),
            ulCodePageRange2: Some((self.code_page_range >> 32 & MASK_32 as u64) as u32),
            sxHeight: Some(self.x_height),
            sCapHeight: Some(self.cap_height),
            usLowerOpticalPointSize: self.lower_op_size,
            usUpperOpticalPointSize: self.upper_op_size,
            usDefaultChar: None,
            usBreakChar: None,
            usMaxContext: None,
        };

        if result.usLowerOpticalPointSize.is_some() || result.usUpperOpticalPointSize.is_some() {
            result.version = 5;
        }
        result
    }
}

static CODEPAGE_TO_BIT: &[(u16, u8)] = &[
    (437, 63),
    (708, 61),
    (737, 60),
    (775, 59),
    (850, 62),
    (852, 58),
    (855, 57),
    (857, 56),
    (860, 55),
    (861, 54),
    (862, 53),
    (863, 52),
    (864, 51),
    (865, 50),
    (866, 49),
    (869, 48),
    (874, 16),
    (932, 17),
    (936, 18),
    (949, 19),
    (950, 20),
    (1250, 1),
    (1251, 2),
    (1252, 0),
    (1253, 3),
    (1254, 4),
    (1255, 5),
    (1256, 6),
    (1257, 7),
    (1258, 8),
    (1361, 21),
];
