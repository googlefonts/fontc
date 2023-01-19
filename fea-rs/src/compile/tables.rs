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
        gdef::{
            AttachList, AttachPoint, CaretValue, GlyphClassDef, LigCaretList, LigGlyph,
            MarkGlyphSets,
        },
        layout::{ClassDef, ClassDefBuilder, CoverageTableBuilder},
    },
    types::{Fixed, LongDateTime, Tag, Uint24},
    validate::ValidationReport,
};

use crate::{
    common::{GlyphClass, GlyphId},
    compile::tags::{MAC_PLATFORM_ID, WIN_PLATFORM_ID},
};

/// The explicit tables allowed in a fea file
#[derive(Clone, Debug, Default)]
pub(crate) struct Tables {
    pub head: Option<HeadBuilder>,
    pub hhea: Option<tables::hhea::Hhea>,
    pub vhea: Option<tables::vhea::Vhea>,
    pub vmtx: Option<VmtxBuilder>,
    pub name: NameBuilder,
    pub stylistic_sets: HashMap<Tag, Vec<NameSpec>>,
    pub character_variants: HashMap<Tag, CvParams>,
    pub gdef: Option<GdefBuilder>,
    pub base: Option<Base>,
    pub os2: Option<Os2Builder>,
    pub stat: Option<StatBuilder>,
}
#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types)]
pub struct HeadBuilder {
    pub font_revision: Fixed,
}

#[derive(Clone, Debug, Default)]
pub struct VmtxBuilder {
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
pub struct GdefBuilder {
    pub glyph_classes: HashMap<GlyphId, ClassId>,
    pub attach: BTreeMap<GlyphId, BTreeSet<u16>>,
    pub ligature_pos: BTreeMap<GlyphId, Vec<CaretValue>>,
    pub mark_attach_class: BTreeMap<GlyphId, u16>,
    pub mark_glyph_sets: Vec<GlyphClass>,
}

#[derive(Clone, Debug, Default)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
pub struct Base {
    pub horiz_tag_list: Vec<Tag>,
    pub horiz_script_list: Vec<ScriptRecord>,
    pub vert_tag_list: Vec<Tag>,
    pub vert_script_list: Vec<ScriptRecord>,
}

#[derive(Clone, Debug, Default)]
pub struct UnicodeRange([u32; 4]);

#[derive(Clone, Debug, Default)]
pub struct CodePageRange([u32; 2]);

#[derive(Clone, Debug, Default)]
pub struct Os2Builder {
    pub us_weight_class: u16,
    pub us_width_class: u16,
    pub fs_type: u16,
    pub s_family_class: i16,
    pub panose_10: [u8; 10],
    pub unicode_range: UnicodeRange,
    pub ach_vend_id: Tag,
    pub us_win_ascent: u16,
    pub us_win_descent: u16,
    pub code_page_range: CodePageRange,
    pub sx_height: i16,
    pub s_cap_height: i16,
    pub s_typo_ascender: i16,
    pub s_typo_descender: i16,
    pub s_typo_line_gap: i16,
    pub us_lower_optical_point_size: Option<u16>,
    pub us_upper_optical_point_size: Option<u16>,
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

        //HACK: we jump through a bunch of hoops to ensure our output matches
        //feaLib's; in particular we want to add our name table entries grouped by
        //axis.
        let mut sorted_values = HashMap::<Tag, Vec<_>>::new();
        let mut sorted_records = self.records.iter().collect::<Vec<_>>();
        sorted_records.sort_by_key(|x| x.ordering);

        for axis_value in &self.values {
            match axis_value.location {
                AxisLocation::One { tag, .. }
                | AxisLocation::Two { tag, .. }
                | AxisLocation::Three { tag, .. } => {
                    sorted_values.entry(tag).or_default().push(axis_value)
                }
                AxisLocation::Four(_) => sorted_values
                    .entry(Tag::default())
                    .or_default()
                    .push(axis_value),
            }
        }

        let mut design_axes = Vec::with_capacity(self.records.len());
        let mut axis_values = Vec::with_capacity(self.values.len());

        for (i, record) in self.records.iter().enumerate() {
            let name_id = name_builder.add_anon_group(&record.name);
            let record = tables::stat::AxisRecord {
                axis_tag: record.tag,
                axis_name_id: name_id,
                axis_ordering: record.ordering,
            };
            for axis_value in sorted_values
                .get(&record.axis_tag)
                .iter()
                .flat_map(|x| x.iter())
            {
                let flags = tables::stat::AxisValueTableFlags::from_bits(axis_value.flags).unwrap();
                let name_id = name_builder.add_anon_group(&axis_value.name);
                let value = match &axis_value.location {
                    AxisLocation::One { value, .. } => tables::stat::AxisValue::format_1(
                        //TODO: validate that all referenced tags refer to existing axes
                        i as u16, flags, name_id, *value,
                    ),
                    AxisLocation::Two {
                        nominal, min, max, ..
                    } => tables::stat::AxisValue::format_2(
                        i as _, flags, name_id, *nominal, *min, *max,
                    ),
                    AxisLocation::Three { value, linked, .. } => {
                        tables::stat::AxisValue::format_3(i as _, flags, name_id, *value, *linked)
                    }

                    AxisLocation::Four(_) => panic!("assigned to separate group"),
                };
                axis_values.push(value);
            }

            design_axes.push(record);
        }

        let format4 = sorted_values.remove(&Tag::default()).unwrap_or_default().into_iter().map(|format4| {
            let flags = tables::stat::AxisValueTableFlags::from_bits(format4.flags).unwrap();
            let name_id = name_builder.add_anon_group(&format4.name);
            let AxisLocation::Four(values) = &format4.location else { panic!("only format 4 in this group")};
            let mapping = values
                .iter()
                .map(|(tag, value)| {
                    let axis_index = design_axes
                        .iter()
                        .position(|rec| rec.axis_tag == *tag)
                        .expect("validated");
                    tables::stat::AxisValueRecord::new(axis_index as _, *value)
                })
                .collect();
            tables::stat::AxisValue::format_4(flags, name_id, mapping)
        });

        //feaLib puts format4 records first
        let axis_values = format4.chain(axis_values).collect();
        tables::stat::Stat::new(design_axes, axis_values, elided_fallback_name_id)
    }
}

impl Base {
    pub(crate) fn build(&self) -> tables::base::Base {
        let mut result = tables::base::Base::default();
        if !self.horiz_tag_list.is_empty() {
            assert!(!self.horiz_script_list.is_empty(), "validate this");
            let haxis = Base::build_axis(&self.horiz_tag_list, &self.horiz_script_list);
            result.horiz_axis = haxis.into();
        }
        if !self.vert_tag_list.is_empty() && !self.vert_script_list.is_empty() {
            assert!(!self.vert_script_list.is_empty(), "validate this");
            let vaxis = Base::build_axis(&self.vert_tag_list, &self.vert_script_list);
            result.vert_axis = vaxis.into();
        }
        result
    }

    fn build_axis(tag_list: &[Tag], script_list: &[ScriptRecord]) -> tables::base::Axis {
        let records = script_list.iter().map(|rec| {
            tables::base::BaseScriptRecord::new(
                rec.script,
                tables::base::BaseScript::new(
                    Some(tables::base::BaseValues::new(
                        tag_list
                            .iter()
                            .position(|x| *x == rec.default_baseline_tag)
                            .expect("validate this") as _,
                        rec.values
                            .iter()
                            .map(|coord| tables::base::BaseCoord::format_1(*coord))
                            .collect(),
                    )),
                    None,
                    Vec::new(),
                ),
            )
        });
        tables::base::Axis::new(
            Some(tables::base::BaseTagList::new(tag_list.to_owned())),
            tables::base::BaseScriptList::new(records.collect()),
        )
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
        self.last_anon_id = self.last_anon_id.max(name_id);
        self.records.push((name_id, name_spec));
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
            break;
        }
    }
    out
}

// this is the value used in python fonttools when writing this table
const DATE_2011_12_13_H11_M22_S33: LongDateTime = LongDateTime::new(1323780153);

impl HeadBuilder {
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

impl UnicodeRange {
    pub(crate) fn set_bit(&mut self, bit: u8) {
        set_bit_impl(&mut self.0, bit)
    }
}

impl CodePageRange {
    pub(crate) fn bit_for_code_page(val: u16) -> Option<u8> {
        CODEPAGE_TO_BIT
            .iter()
            .find_map(|(page, bit)| if *page == val { Some(*bit) } else { None })
    }

    pub(crate) fn add_code_page(&mut self, page: u16) {
        let bit = Self::bit_for_code_page(page).unwrap();
        set_bit_impl(&mut self.0, bit)
    }
}

// shared between the two types above
fn set_bit_impl<const N: usize>(array: &mut [u32; N], bit: u8) {
    assert!(bit < (32 * N) as u8);
    let idx = (bit / 32) as usize;
    let bit = bit % 32;
    array[idx] |= 1 << bit;
}

impl Os2Builder {
    pub fn build(&self) -> write_fonts::tables::os2::Os2 {
        let [ul_code_page_range_1, ul_code_page_range_2] = self.code_page_range.0;
        let [ul_unicode_range_1, ul_unicode_range_2, ul_unicode_range_3, ul_unicode_range_4] =
            self.unicode_range.0;

        write_fonts::tables::os2::Os2 {
            us_weight_class: self.us_weight_class,
            us_width_class: self.us_width_class,
            fs_type: self.fs_type,
            s_family_class: self.s_family_class,
            ul_unicode_range_1,
            ul_unicode_range_2,
            ul_unicode_range_3,
            ul_unicode_range_4,
            panose_10: self.panose_10,
            ach_vend_id: self.ach_vend_id,
            s_typo_ascender: self.s_typo_ascender,
            s_typo_descender: self.s_typo_descender,
            s_typo_line_gap: self.s_typo_line_gap,
            us_win_ascent: self.us_win_ascent,
            us_win_descent: self.us_win_descent,
            ul_code_page_range_1: Some(ul_code_page_range_1),
            ul_code_page_range_2: Some(ul_code_page_range_2),
            sx_height: Some(self.sx_height),
            s_cap_height: Some(self.s_cap_height),
            //TODO: these are defined in fea, but we want them to be present
            //since other v2 fields are? I assume they get overwritten anyway?
            us_default_char: Some(0),
            us_max_context: Some(0),
            us_break_char: Some(0),
            //TODO: ensure at validation that if one is present, the other is?
            us_lower_optical_point_size: self.us_lower_optical_point_size,
            us_upper_optical_point_size: self.us_upper_optical_point_size,
            ..Default::default()
        }
    }
}

impl GdefBuilder {
    pub fn build(&self) -> Result<Vec<u8>, ValidationReport> {
        let mut table = tables::gdef::Gdef::new(
            self.build_class_def(),
            self.build_attach_list(),
            self.build_lig_caret_list(),
            self.build_mark_attach_class_def(),
        );

        table.mark_glyph_sets_def = self.build_mark_glyph_sets().into();
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

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn parse_mac_str() {
        let inp = "M\\9fller";
        assert_eq!(parse_mac(inp), "Müller");
    }
}
