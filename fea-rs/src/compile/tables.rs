use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    convert::TryInto,
};

use smol_str::SmolStr;
use write_fonts::{
    dump_table,
    from_obj::ToOwnedTable,
    read::{tables::name::Encoding, FontRef, TableProvider},
    tables::{
        self,
        gdef::{AttachList, AttachPoint, CaretValue, GlyphClassDef, LigCaretList, LigGlyph},
        layout::{ClassDef, ClassDefBuilder, CoverageTableBuilder},
    },
    types::{Fixed, LongDateTime, Tag, Uint24},
    validate::ValidationReport,
};

use crate::{
    compile::common::{MAC_PLATFORM_ID, WIN_PLATFORM_ID},
    types::{GlyphClass, GlyphId},
};

/// The explicit tables allowed in a fea file
#[allow(non_snake_case)]
#[derive(Clone, Debug, Default)]
pub(crate) struct Tables {
    pub head: Option<head>,
    pub hhea: Option<tables::hhea::Hhea>,
    pub vhea: Option<tables::vhea::Vhea>,
    pub vmtx: Option<vmtx>,
    pub name: NameBuilder,
    pub stylistic_sets: HashMap<Tag, Vec<NameSpec>>,
    pub character_variants: HashMap<Tag, CvParams>,
    pub GDEF: Option<Gdef>,
    pub BASE: Option<BASE>,
    pub OS2: Option<OS2>,
    pub stat: Option<StatBuilder>,
}
#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct head {
    pub font_revision: Fixed,
}

#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct vmtx {
    pub origins_y: Vec<(GlyphId, i16)>,
    pub advances_y: Vec<(GlyphId, i16)>,
}

#[derive(Clone, Debug, Default)]
pub struct CvParams {
    pub feat_ui_label_name: Vec<NameSpec>,
    pub feat_ui_tooltip_text_name: Vec<NameSpec>,
    pub samle_text_name: Vec<NameSpec>,
    pub param_ui_label_names: Vec<Vec<NameSpec>>,
    pub characters: Vec<char>,
}

#[derive(Clone, Debug, Default)]
pub struct NameSpec {
    pub platform_id: u16,
    pub encoding_id: u16,
    pub language_id: u16,
    pub string: SmolStr,
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

#[derive(Clone, Debug, Default)]
pub struct Gdef {
    pub glyph_classes: HashMap<GlyphId, ClassId>,
    pub attach: BTreeMap<GlyphId, BTreeSet<u16>>,
    pub ligature_pos: BTreeMap<GlyphId, Vec<CaretValue>>,
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
pub struct StatBuilder {
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
        value: Fixed,
    },
    Two {
        tag: Tag,
        nominal: Fixed,
        min: Fixed,
        max: Fixed,
    },
    Three {
        tag: Tag,
        value: Fixed,
        linked: Fixed,
    },
    Four(Vec<(Tag, Fixed)>),
}

#[derive(Clone, Debug)]
pub enum StatFallbackName {
    Id(u16),
    Record(Vec<NameSpec>),
}

impl StatBuilder {
    pub(crate) fn build(&self, name_builder: &mut NameBuilder) -> tables::stat::Stat {
        let elided_fallback_name_id = match &self.name {
            StatFallbackName::Id(id) if name_builder.contains_id(*id) => *id,
            StatFallbackName::Id(id) => {
                panic!("ElidedFallbackNameID '{}' does not exist in font", id)
            }
            StatFallbackName::Record(names) => name_builder.add_anon_group(names),
        };

        let mut design_axes = Vec::new();
        for record in &self.records {
            let name_id = name_builder.add_anon_group(&record.name);
            design_axes.push(tables::stat::AxisRecord {
                axis_tag: record.tag,
                axis_name_id: name_id,
                axis_ordering: record.ordering,
            });
        }
        design_axes.sort_unstable_by_key(|x| x.axis_tag);
        let axis_indices = design_axes
            .iter()
            .enumerate()
            .map(|(i, val)| (val.axis_tag, i as u16))
            .collect::<HashMap<_, _>>();

        let axis_values = self
            .values
            .iter()
            .map(|axis_value| {
                let flags = tables::stat::AxisValueTableFlags::from_bits(axis_value.flags).unwrap();
                let name_id = name_builder.add_anon_group(&axis_value.name);
                match &axis_value.location {
                    AxisLocation::One { tag, value } => tables::stat::AxisValue::format_1(
                        //TODO: validate that all referenced tags refer to existing axes
                        *axis_indices.get(tag).unwrap(),
                        flags,
                        name_id,
                        *value,
                    ),
                    AxisLocation::Two {
                        tag,
                        nominal,
                        min,
                        max,
                    } => {
                        let axis_index = *axis_indices.get(tag).unwrap();
                        tables::stat::AxisValue::format_2(
                            axis_index, flags, name_id, *nominal, *min, *max,
                        )
                    }
                    AxisLocation::Three { tag, value, linked } => {
                        let axis_index = *axis_indices.get(tag).unwrap();
                        tables::stat::AxisValue::format_3(
                            axis_index, flags, name_id, *value, *linked,
                        )
                    }
                    AxisLocation::Four(values) => {
                        let mapping = values
                            .iter()
                            .map(|(tag, value)| {
                                tables::stat::AxisValueRecord::new(
                                    *axis_indices.get(tag).unwrap(),
                                    *value,
                                )
                            })
                            .collect();
                        tables::stat::AxisValue::format_4(flags, name_id, mapping)
                    }
                }
            })
            .collect::<Vec<_>>();
        tables::stat::Stat::new(design_axes, axis_values, elided_fallback_name_id)
    }
}

#[derive(Clone, Debug)]
pub struct NameBuilder {
    records: Vec<(u16, NameSpec)>,
    last_anon_id: u16,
}

impl Default for NameBuilder {
    fn default() -> Self {
        NameBuilder {
            records: Vec::new(),
            last_anon_id: 255,
        }
    }
}

impl NameBuilder {
    pub(crate) fn add(&mut self, name_id: u16, name_spec: NameSpec) {
        if !(1..=6).contains(&name_id) {
            self.last_anon_id = self.last_anon_id.max(name_id);
            self.records.push((name_id, name_spec));
        }
    }

    pub(crate) fn add_anon_group(&mut self, entries: &[NameSpec]) -> u16 {
        let name_id = self.next_name_id();
        for name in entries {
            self.add(name_id, name.clone());
        }
        name_id
    }

    pub(crate) fn contains_id(&self, id: u16) -> bool {
        self.records.iter().any(|(name_id, _)| name_id == &id)
    }

    pub(crate) fn next_name_id(&self) -> u16 {
        self.last_anon_id + 1
    }

    pub(crate) fn build(&self) -> Option<write_fonts::tables::name::Name> {
        (!self.records.is_empty()).then(|| {
            write_fonts::tables::name::Name::new(
                self.records
                    .iter()
                    .filter_map(|(id, spec)| {
                        spec.is_implemented_in_fontations()
                            .then(|| spec.to_otf(*id))
                    })
                    .collect(),
            )
        })
    }
}

impl NameSpec {
    fn is_implemented_in_fontations(&self) -> bool {
        Encoding::new(self.platform_id, self.encoding_id) != Encoding::Unknown
    }

    //TODO: rename me to build
    pub fn to_otf(&self, name_id: u16) -> write_fonts::tables::name::NameRecord {
        let string = parse_string(self.platform_id, self.string.trim_matches('"'));
        write_fonts::tables::name::NameRecord::new(
            self.platform_id,
            self.encoding_id,
            self.language_id,
            name_id,
            string.into(),
        )
    }
}

impl CvParams {
    pub(crate) fn build(
        &self,
        names: &mut NameBuilder,
    ) -> write_fonts::tables::layout::CharacterVariantParams {
        let mut out = write_fonts::tables::layout::CharacterVariantParams::default();
        if !self.feat_ui_label_name.is_empty() {
            out.feat_ui_label_name_id = names.add_anon_group(&self.feat_ui_label_name);
        }
        if !self.feat_ui_tooltip_text_name.is_empty() {
            out.feat_ui_tooltip_text_name_id =
                names.add_anon_group(&self.feat_ui_tooltip_text_name);
        }

        if !self.samle_text_name.is_empty() {
            out.sample_text_name_id = names.add_anon_group(&self.samle_text_name);
        }

        if let Some((first, rest)) = self.param_ui_label_names.split_first() {
            out.first_param_ui_label_name_id = names.add_anon_group(first);
            for item in rest {
                names.add_anon_group(item);
            }
        }
        out.num_named_parameters = self.param_ui_label_names.len().try_into().unwrap();
        for c in &self.characters {
            out.character.push(Uint24::checked_new(*c as _).unwrap());
        }

        out
    }
}

fn parse_string(platform: u16, s: &str) -> String {
    debug_assert!(platform == WIN_PLATFORM_ID || platform == MAC_PLATFORM_ID);
    if !s.as_bytes().contains(&b'\\') {
        return s.to_string();
    }

    if platform == WIN_PLATFORM_ID {
        parse_win(s)
    } else {
        parse_mac(s)
    }
}

fn parse_win(s: &str) -> String {
    let mut out_u16 = Vec::with_capacity(s.len());
    let mut work = s;
    while !work.is_empty() {
        let pos = work.bytes().position(|b| b == b'\\');
        if let Some(pos) = pos {
            out_u16.extend(work[..pos].encode_utf16());
            let code = &work[pos + 1..pos + 5];
            let num = u16::from_str_radix(code, 16).unwrap();
            out_u16.push(num);
            work = &work[pos + 5..];
        } else {
            out_u16.extend(work.encode_utf16());
        }
    }
    String::from_utf16(&out_u16).unwrap()
}

fn parse_mac(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut work = s;
    while !work.is_empty() {
        let pos = work.bytes().position(|b| b == b'\\');
        if let Some(pos) = pos {
            out.push_str(&work[..pos]);
            let code = &work[pos + 1..pos + 3];
            let num = u8::from_str_radix(code, 16).unwrap();
            out.push(mac_roman_to_char(num));
            work = &work[pos + 3..];
        } else {
            out.push_str(work);
        }
    }
    out
}

// this is the value used in python fonttools when writing this table
const DATE_2011_12_13_H11_M22_S33: LongDateTime = LongDateTime::new(1323780153);

impl head {
    pub(crate) fn build(&self, font: Option<&FontRef>) -> write_fonts::tables::head::Head {
        // match what python fonttools does
        let mut head = font
            .and_then(|f| f.head().map(|x| x.to_owned_table()).ok())
            .unwrap_or_else(|| {
                let mut head = write_fonts::tables::head::Head {
                    created: DATE_2011_12_13_H11_M22_S33,
                    modified: DATE_2011_12_13_H11_M22_S33,
                    ..Default::default()
                };

                // I think we should still use the known default values but this matches
                // feaLib tests so :shrug:
                head.magic_number = 0;
                head.font_direction_hint = 0;
                head
            });
        head.font_revision = self.font_revision;
        head
    }
}

impl OS2 {
    pub fn bit_for_code_page(val: u16) -> Option<u8> {
        CODEPAGE_TO_BIT
            .iter()
            .find_map(|(page, bit)| if *page == val { Some(*bit) } else { None })
    }

    //pub fn build(&self) -> () {
    //pub fn build(&self) -> write_fonts::tables::os2::os2 {
    //todo!()
    //const MASK_32: u32 = 0xffff_ffff;
    //let panose = fonttools::tables::os2::Panose {
    //panose0: self.panose[0],
    //panose1: self.panose[1],
    //panose2: self.panose[2],
    //panose3: self.panose[3],
    //panose4: self.panose[4],
    //panose5: self.panose[5],
    //panose6: self.panose[6],
    //panose7: self.panose[7],
    //panose8: self.panose[8],
    //panose9: self.panose[9],
    //};
    //let mut result = fonttools::tables::os2::os2 {
    //version: 2,
    //xAvgCharWidth: 0,
    //usWeightClass: self.weight_class,
    //usWidthClass: self.width_class,
    //fsType: self.fs_type,
    //ySubscriptXSize: 0,
    //ySubscriptYSize: 0,
    //ySubscriptXOffset: 0,
    //ySubscriptYOffset: 0,
    //ySuperscriptXSize: 0,
    //ySuperscriptYSize: 0,
    //ySuperscriptYOffset: 0,
    //ySuperscriptXOffset: 0,
    //yStrikeoutSize: 0,
    //yStrikeoutPosition: 0,
    //sFamilyClass: self.family_class,
    //panose,
    //ulUnicodeRange1: (self.unicode_range & MASK_32 as u128) as u32,
    //ulUnicodeRange2: (self.unicode_range >> 32 & MASK_32 as u128) as u32,
    //ulUnicodeRange3: (self.unicode_range >> 64 & MASK_32 as u128) as u32,
    //ulUnicodeRange4: (self.unicode_range >> 96 & MASK_32 as u128) as u32,
    //achVendID: self.vendor_id.parse().unwrap(),
    //fsSelection: 0,
    //usFirstCharIndex: 0,
    //usLastCharIndex: 0,
    //sTypoAscender: self.typo_ascender,
    //sTypoDescender: self.typo_descender,
    //sTypoLineGap: self.typo_line_gap,
    //usWinAscent: self.win_ascent,
    //usWinDescent: self.win_descent,
    //ulCodePageRange1: Some((self.code_page_range & MASK_32 as u64) as u32),
    //ulCodePageRange2: Some((self.code_page_range >> 32 & MASK_32 as u64) as u32),
    //sxHeight: Some(self.x_height),
    //sCapHeight: Some(self.cap_height),
    //usLowerOpticalPointSize: self.lower_op_size,
    //usUpperOpticalPointSize: self.upper_op_size,
    //usDefaultChar: None,
    //usBreakChar: None,
    //usMaxContext: None,
    //};

    //if result.usLowerOpticalPointSize.is_some() || result.usUpperOpticalPointSize.is_some() {
    //result.version = 5;
    //}
    //result
    //}
}

impl Gdef {
    pub fn build(&self) -> Result<Vec<u8>, ValidationReport> {
        let table = tables::gdef::Gdef::new(
            self.build_class_def(),
            self.build_attach_list(),
            self.build_lig_caret_list(),
            None,
        );
        dump_table(&table)
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

    pub fn add_glyph_class(&mut self, glyphs: GlyphClass, class: ClassId) {
        for glyph in glyphs.iter() {
            self.glyph_classes.insert(glyph, class);
        }
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

fn mac_roman_to_char(inp: u8) -> char {
    if inp < 0x80 {
        inp as char
    } else {
        MAC_ROMAN_LOOKUP[inp as usize - 0x80]
    }
}

#[rustfmt::skip]
/// char equivalents of macroman values 0x80 - 0xFF
static MAC_ROMAN_LOOKUP: &[char] = &[
    'Ä', 'Å', 'Ç', 'É', 'Ñ', 'Ö', 'Ü', 'á',
    'à', 'â', 'ä', 'ã', 'å', 'ç', 'é', 'è',
    'ê', 'ë', 'í', 'ì', 'î', 'ï', 'ñ', 'ó',
    'ò', 'ô', 'ö', 'õ', 'ú', 'ù', 'û', 'ü',
    '†', '°', '¢', '£', '§', '•', '¶', 'ß',
    '®', '©', '™', '´', '¨', '≠', 'Æ', 'Ø',
    '∞', '±', '≤', '≥', '¥', 'µ', '∂', '∑',
    '∏', 'π', '∫', 'ª', 'º', 'Ω', 'æ', 'ø',
    '¿', '¡', '¬', '√', 'ƒ', '≈', '∆', '«',
    '»', '…', '\u{ca}', //nbsp
    'À', 'Ã', 'Õ', 'Œ', 'œ',
    '–', '—', '“', '”', '‘', '’', '÷', '◊',
    'ÿ', 'Ÿ', '⁄', '€', '‹', '›', 'ﬁ', 'ﬂ',
    '‡', '·', '‚', '„', '‰', 'Â', 'Ê', 'Á',
    'Ë', 'È', 'Í', 'Î', 'Ï', 'Ì', 'Ó', 'Ô',
    '\u{f8ff}', //
    'Ò', 'Ú', 'Û', 'Ù', 'ı', 'ˆ', '˜',
    '¯', '˘', '˙', '˚', '¸', '˝', '˛', 'ˇ',
];

#[test]
fn smoke_test_conversion() {
    assert_eq!(MAC_ROMAN_LOOKUP.len(), 128);
    assert_eq!(mac_roman_to_char(0x20), ' ');
    assert_eq!(mac_roman_to_char(0x7E), '~');
    assert_eq!(mac_roman_to_char(0x7F), 0x7f as char);
    assert_eq!(mac_roman_to_char(0x80), 'Ä');
    assert_eq!(mac_roman_to_char(0xFF), 'ˇ');
    assert_eq!(mac_roman_to_char(0x8e), 'é');
}
