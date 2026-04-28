//! The OS/2 table

use write_fonts::types::Tag;

/// [ulUnicodeRangeN](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#ulunicoderange1-bits-031ulunicoderange2-bits-3263ulunicoderange3-bits-6495ulunicoderange4-bits-96127)
#[derive(Clone, Debug, Default, PartialEq)]
pub struct UnicodeRange([u32; 4]);

/// [ulCodePageRangeN](https://learn.microsoft.com/en-us/typography/opentype/spec/os2#ulcodepagerange1-bits-031ulcodepagerange2-bits-3263)
#[derive(Clone, Debug, Default, PartialEq)]
pub struct CodePageRange([u32; 2]);

/// Builder for the OS/2 table from FEA `table OS/2 { ... }` blocks.
///
/// Fields are `Option<T>` — `Some` means the field was explicitly set in FEA,
/// `None` means it was not mentioned and should not override computed values.
#[allow(missing_docs)]
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Os2Builder {
    pub us_weight_class: Option<u16>,
    pub us_width_class: Option<u16>,
    pub fs_type: Option<u16>,
    pub s_family_class: Option<i16>,
    pub panose_10: Option<[u8; 10]>,
    pub unicode_range: Option<UnicodeRange>,
    pub ach_vend_id: Option<Tag>,
    pub us_win_ascent: Option<u16>,
    pub us_win_descent: Option<u16>,
    pub code_page_range: Option<CodePageRange>,
    pub sx_height: Option<i16>,
    pub s_cap_height: Option<i16>,
    pub s_typo_ascender: Option<i16>,
    pub s_typo_descender: Option<i16>,
    pub s_typo_line_gap: Option<i16>,
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
    /// Build the write-fonts `Os2` table, using defaults for unset fields.
    pub fn build(&self) -> write_fonts::tables::os2::Os2 {
        let code_page_range = self.code_page_range.as_ref().map_or([0; 2], |r| r.0);
        let unicode_range = self.unicode_range.as_ref().map_or([0; 4], |r| r.0);

        write_fonts::tables::os2::Os2 {
            us_weight_class: self.us_weight_class.unwrap_or_default(),
            us_width_class: self.us_width_class.unwrap_or_default(),
            fs_type: self.fs_type.unwrap_or_default(),
            s_family_class: self.s_family_class.unwrap_or_default(),
            ul_unicode_range_1: unicode_range[0],
            ul_unicode_range_2: unicode_range[1],
            ul_unicode_range_3: unicode_range[2],
            ul_unicode_range_4: unicode_range[3],
            panose_10: self.panose_10.unwrap_or_default(),
            ach_vend_id: self.ach_vend_id.unwrap_or_default(),
            s_typo_ascender: self.s_typo_ascender.unwrap_or_default(),
            s_typo_descender: self.s_typo_descender.unwrap_or_default(),
            s_typo_line_gap: self.s_typo_line_gap.unwrap_or_default(),
            us_win_ascent: self.us_win_ascent.unwrap_or_default(),
            us_win_descent: self.us_win_descent.unwrap_or_default(),
            ul_code_page_range_1: Some(code_page_range[0]),
            ul_code_page_range_2: Some(code_page_range[1]),
            sx_height: Some(self.sx_height.unwrap_or_default()),
            s_cap_height: Some(self.s_cap_height.unwrap_or_default()),
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
