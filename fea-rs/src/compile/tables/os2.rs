//! The OS/2 table

use write_fonts::types::Tag;

/// [ulUnicodeRangeN](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#ulunicoderange1-bits-031ulunicoderange2-bits-3263ulunicoderange3-bits-6495ulunicoderange4-bits-96127)
#[derive(Clone, Debug, Default)]
pub(crate) struct UnicodeRange([u32; 4]);

/// [ulCodePageRangeN](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#ulcodepagerange1-bits-031ulcodepagerange2-bits-3263)
#[derive(Clone, Debug, Default)]
pub(crate) struct CodePageRange([u32; 2]);

#[derive(Clone, Debug, Default)]
pub(crate) struct Os2Builder {
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
