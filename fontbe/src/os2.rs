//! Generates a [OS/2](https://learn.microsoft.com/en-us/typography/opentype/spec/os2) table.

use std::{cmp::Ordering, collections::HashSet};

use fontdrasil::{
    orchestration::{Access, AccessBuilder, Work},
    types::WidthClass,
};
use fontir::{ir::GlobalMetricsInstance, orchestration::WorkId as FeWorkId};
use log::warn;
use write_fonts::{
    read::{tables::hmtx::Hmtx, FontData, TopLevelTable},
    tables::{
        gsub::{
            AlternateSubstFormat1, ExtensionSubtable, LigatureSubstFormat1, MultipleSubstFormat1,
            ReverseChainSingleSubstFormat1, SingleSubst, SubstitutionChainContext,
            SubstitutionLookup, SubstitutionSequenceContext,
        },
        layout::{
            ChainedClassSequenceRule, ChainedClassSequenceRuleSet, ChainedSequenceContext,
            ChainedSequenceContextFormat1, ChainedSequenceContextFormat2, ChainedSequenceRule,
            ChainedSequenceRuleSet, ClassSequenceRule, ClassSequenceRuleSet, SequenceContext,
            SequenceContextFormat1, SequenceContextFormat2, SequenceRule, SequenceRuleSet,
        },
        os2::Os2,
    },
    types::Tag,
    NullableOffsetMarker, OffsetMarker, OtRound,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

/// Used to build a bitfield.
///
/// Intended use: binary search for a codepoint against the range
/// formed by .0..=.1; if it matches bit .2 should be set.
///
/// Generated by resources/scripts/os2_unicode_ranges.py.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#ur>
/// <https://github.com/fonttools/fonttools/blob/47813b217c1f8bc343c094776020b4f32fc024b0/Lib/fontTools/ttLib/tables/O_S_2f_2.py#L351-L557>
const UNICODE_RANGES: &[(u32, u32, u32)] = &[
    (0x0000, 0x007F, 0),      // Basic Latin
    (0x0080, 0x00FF, 1),      // Latin-1 Supplement
    (0x0100, 0x017F, 2),      // Latin Extended-A
    (0x0180, 0x024F, 3),      // Latin Extended-B
    (0x0250, 0x02AF, 4),      // IPA Extensions
    (0x02B0, 0x02FF, 5),      // Spacing Modifier Letters
    (0x0300, 0x036F, 6),      // Combining Diacritical Marks
    (0x0370, 0x03FF, 7),      // Greek and Coptic
    (0x0400, 0x04FF, 9),      // Cyrillic
    (0x0500, 0x052F, 9),      // Cyrillic Supplement
    (0x0530, 0x058F, 10),     // Armenian
    (0x0590, 0x05FF, 11),     // Hebrew
    (0x0600, 0x06FF, 13),     // Arabic
    (0x0700, 0x074F, 71),     // Syriac
    (0x0750, 0x077F, 13),     // Arabic Supplement
    (0x0780, 0x07BF, 72),     // Thaana
    (0x07C0, 0x07FF, 14),     // NKo
    (0x0900, 0x097F, 15),     // Devanagari
    (0x0980, 0x09FF, 16),     // Bengali
    (0x0A00, 0x0A7F, 17),     // Gurmukhi
    (0x0A80, 0x0AFF, 18),     // Gujarati
    (0x0B00, 0x0B7F, 19),     // Oriya
    (0x0B80, 0x0BFF, 20),     // Tamil
    (0x0C00, 0x0C7F, 21),     // Telugu
    (0x0C80, 0x0CFF, 22),     // Kannada
    (0x0D00, 0x0D7F, 23),     // Malayalam
    (0x0D80, 0x0DFF, 73),     // Sinhala
    (0x0E00, 0x0E7F, 24),     // Thai
    (0x0E80, 0x0EFF, 25),     // Lao
    (0x0F00, 0x0FFF, 70),     // Tibetan
    (0x1000, 0x109F, 74),     // Myanmar
    (0x10A0, 0x10FF, 26),     // Georgian
    (0x1100, 0x11FF, 28),     // Hangul Jamo
    (0x1200, 0x137F, 75),     // Ethiopic
    (0x1380, 0x139F, 75),     // Ethiopic Supplement
    (0x13A0, 0x13FF, 76),     // Cherokee
    (0x1400, 0x167F, 77),     // Unified Canadian Aboriginal Syllabics
    (0x1680, 0x169F, 78),     // Ogham
    (0x16A0, 0x16FF, 79),     // Runic
    (0x1700, 0x171F, 84),     // Tagalog
    (0x1720, 0x173F, 84),     // Hanunoo
    (0x1740, 0x175F, 84),     // Buhid
    (0x1760, 0x177F, 84),     // Tagbanwa
    (0x1780, 0x17FF, 80),     // Khmer
    (0x1800, 0x18AF, 81),     // Mongolian
    (0x1900, 0x194F, 93),     // Limbu
    (0x1950, 0x197F, 94),     // Tai Le
    (0x1980, 0x19DF, 95),     // New Tai Lue
    (0x19E0, 0x19FF, 80),     // Khmer Symbols
    (0x1A00, 0x1A1F, 96),     // Buginese
    (0x1B00, 0x1B7F, 27),     // Balinese
    (0x1B80, 0x1BBF, 112),    // Sundanese
    (0x1C00, 0x1C4F, 113),    // Lepcha
    (0x1C50, 0x1C7F, 114),    // Ol Chiki
    (0x1D00, 0x1D7F, 4),      // Phonetic Extensions
    (0x1D80, 0x1DBF, 4),      // Phonetic Extensions Supplement
    (0x1DC0, 0x1DFF, 6),      // Combining Diacritical Marks Supplement
    (0x1E00, 0x1EFF, 29),     // Latin Extended Additional
    (0x1F00, 0x1FFF, 30),     // Greek Extended
    (0x2000, 0x206F, 31),     // General Punctuation
    (0x2070, 0x209F, 32),     // Superscripts And Subscripts
    (0x20A0, 0x20CF, 33),     // Currency Symbols
    (0x20D0, 0x20FF, 34),     // Combining Diacritical Marks For Symbols
    (0x2100, 0x214F, 35),     // Letterlike Symbols
    (0x2150, 0x218F, 36),     // Number Forms
    (0x2190, 0x21FF, 37),     // Arrows
    (0x2200, 0x22FF, 38),     // Mathematical Operators
    (0x2300, 0x23FF, 39),     // Miscellaneous Technical
    (0x2400, 0x243F, 40),     // Control Pictures
    (0x2440, 0x245F, 41),     // Optical Character Recognition
    (0x2460, 0x24FF, 42),     // Enclosed Alphanumerics
    (0x2500, 0x257F, 43),     // Box Drawing
    (0x2580, 0x259F, 44),     // Block Elements
    (0x25A0, 0x25FF, 45),     // Geometric Shapes
    (0x2600, 0x26FF, 46),     // Miscellaneous Symbols
    (0x2700, 0x27BF, 47),     // Dingbats
    (0x27C0, 0x27EF, 38),     // Miscellaneous Mathematical Symbols-A
    (0x27F0, 0x27FF, 37),     // Supplemental Arrows-A
    (0x2800, 0x28FF, 82),     // Braille Patterns
    (0x2900, 0x297F, 37),     // Supplemental Arrows-B
    (0x2980, 0x29FF, 38),     // Miscellaneous Mathematical Symbols-B
    (0x2A00, 0x2AFF, 38),     // Supplemental Mathematical Operators
    (0x2B00, 0x2BFF, 37),     // Miscellaneous Symbols and Arrows
    (0x2C00, 0x2C5F, 97),     // Glagolitic
    (0x2C60, 0x2C7F, 29),     // Latin Extended-C
    (0x2C80, 0x2CFF, 8),      // Coptic
    (0x2D00, 0x2D2F, 26),     // Georgian Supplement
    (0x2D30, 0x2D7F, 98),     // Tifinagh
    (0x2D80, 0x2DDF, 75),     // Ethiopic Extended
    (0x2DE0, 0x2DFF, 9),      // Cyrillic Extended-A
    (0x2E00, 0x2E7F, 31),     // Supplemental Punctuation
    (0x2E80, 0x2EFF, 59),     // CJK Radicals Supplement
    (0x2F00, 0x2FDF, 59),     // Kangxi Radicals
    (0x2FF0, 0x2FFF, 59),     // Ideographic Description Characters
    (0x3000, 0x303F, 48),     // CJK Symbols And Punctuation
    (0x3040, 0x309F, 49),     // Hiragana
    (0x30A0, 0x30FF, 50),     // Katakana
    (0x3100, 0x312F, 51),     // Bopomofo
    (0x3130, 0x318F, 52),     // Hangul Compatibility Jamo
    (0x3190, 0x319F, 59),     // Kanbun
    (0x31A0, 0x31BF, 51),     // Bopomofo Extended
    (0x31C0, 0x31EF, 61),     // CJK Strokes
    (0x31F0, 0x31FF, 50),     // Katakana Phonetic Extensions
    (0x3200, 0x32FF, 54),     // Enclosed CJK Letters And Months
    (0x3300, 0x33FF, 55),     // CJK Compatibility
    (0x3400, 0x4DBF, 59),     // CJK Unified Ideographs Extension A
    (0x4DC0, 0x4DFF, 99),     // Yijing Hexagram Symbols
    (0x4E00, 0x9FFF, 59),     // CJK Unified Ideographs
    (0xA000, 0xA48F, 83),     // Yi Syllables
    (0xA490, 0xA4CF, 83),     // Yi Radicals
    (0xA500, 0xA63F, 12),     // Vai
    (0xA640, 0xA69F, 9),      // Cyrillic Extended-B
    (0xA700, 0xA71F, 5),      // Modifier Tone Letters
    (0xA720, 0xA7FF, 29),     // Latin Extended-D
    (0xA800, 0xA82F, 100),    // Syloti Nagri
    (0xA840, 0xA87F, 53),     // Phags-pa
    (0xA880, 0xA8DF, 115),    // Saurashtra
    (0xA900, 0xA92F, 116),    // Kayah Li
    (0xA930, 0xA95F, 117),    // Rejang
    (0xAA00, 0xAA5F, 118),    // Cham
    (0xAC00, 0xD7AF, 56),     // Hangul Syllables
    (0xD800, 0xDFFF, 57),     // Non-Plane 0 *
    (0xE000, 0xF8FF, 60),     // Private Use Area (plane 0)
    (0xF900, 0xFAFF, 61),     // CJK Compatibility Ideographs
    (0xFB00, 0xFB4F, 62),     // Alphabetic Presentation Forms
    (0xFB50, 0xFDFF, 63),     // Arabic Presentation Forms-A
    (0xFE00, 0xFE0F, 91),     // Variation Selectors
    (0xFE10, 0xFE1F, 65),     // Vertical Forms
    (0xFE20, 0xFE2F, 64),     // Combining Half Marks
    (0xFE30, 0xFE4F, 65),     // CJK Compatibility Forms
    (0xFE50, 0xFE6F, 66),     // Small Form Variants
    (0xFE70, 0xFEFF, 67),     // Arabic Presentation Forms-B
    (0xFF00, 0xFFEF, 68),     // Halfwidth And Fullwidth Forms
    (0xFFF0, 0xFFFF, 69),     // Specials
    (0x10000, 0x1007F, 101),  // Linear B Syllabary
    (0x10080, 0x100FF, 101),  // Linear B Ideograms
    (0x10100, 0x1013F, 101),  // Aegean Numbers
    (0x10140, 0x1018F, 102),  // Ancient Greek Numbers
    (0x10190, 0x101CF, 119),  // Ancient Symbols
    (0x101D0, 0x101FF, 120),  // Phaistos Disc
    (0x10280, 0x1029F, 121),  // Lycian
    (0x102A0, 0x102DF, 121),  // Carian
    (0x10300, 0x1032F, 85),   // Old Italic
    (0x10330, 0x1034F, 86),   // Gothic
    (0x10380, 0x1039F, 103),  // Ugaritic
    (0x103A0, 0x103DF, 104),  // Old Persian
    (0x10400, 0x1044F, 87),   // Deseret
    (0x10450, 0x1047F, 105),  // Shavian
    (0x10480, 0x104AF, 106),  // Osmanya
    (0x10800, 0x1083F, 107),  // Cypriot Syllabary
    (0x10900, 0x1091F, 58),   // Phoenician
    (0x10920, 0x1093F, 121),  // Lydian
    (0x10A00, 0x10A5F, 108),  // Kharoshthi
    (0x12000, 0x123FF, 110),  // Cuneiform
    (0x12400, 0x1247F, 110),  // Cuneiform Numbers and Punctuation
    (0x1D000, 0x1D0FF, 88),   // Byzantine Musical Symbols
    (0x1D100, 0x1D1FF, 88),   // Musical Symbols
    (0x1D200, 0x1D24F, 88),   // Ancient Greek Musical Notation
    (0x1D300, 0x1D35F, 109),  // Tai Xuan Jing Symbols
    (0x1D360, 0x1D37F, 111),  // Counting Rod Numerals
    (0x1D400, 0x1D7FF, 89),   // Mathematical Alphanumeric Symbols
    (0x1F000, 0x1F02F, 122),  // Mahjong Tiles
    (0x1F030, 0x1F09F, 122),  // Domino Tiles
    (0x20000, 0x2A6DF, 59),   // CJK Unified Ideographs Extension B
    (0x2F800, 0x2FA1F, 61),   // CJK Compatibility Ideographs Supplement
    (0xE0000, 0xE007F, 92),   // Tags
    (0xE0100, 0xE01EF, 91),   // Variation Selectors Supplement
    (0xF0000, 0xFFFFD, 90),   // Private Use (plane 15)
    (0x100000, 0x10FFFD, 90), // Private Use (plane 16)
];

#[derive(Debug)]
struct Os2Work {}

pub fn create_os2_work() -> Box<BeWork> {
    Box::new(Os2Work {})
}

/// <https://github.com/fonttools/fonttools/blob/115275cbf429d91b75ac5536f5f0b2d6fe9d823a/Lib/fontTools/ttLib/tables/O_S_2f_2.py#L336-L348>
fn x_avg_char_width(context: &Context) -> Result<i16, Error> {
    let glyph_order = context.ir.glyph_order.get();
    let arc_hhea = context.hhea.get();
    let Some(hhea) = &arc_hhea.as_ref().0 else {
        return Err(Error::MissingTable(Tag::new(b"hhea")));
    };
    let raw_hmtx = context.hmtx.get();
    let num_glyphs = glyph_order.len() as u64;
    let hmtx = Hmtx::read(
        FontData::new(raw_hmtx.get()),
        hhea.number_of_long_metrics,
        num_glyphs as u16,
    )
    .map_err(|_| Error::InvalidTableBytes(Hmtx::TAG))?;

    // count width > 0 only, including adding tail only if > 0
    let (count, total) = hmtx
        .h_metrics()
        .iter()
        .filter_map(|metric| match metric.advance() {
            0 => None,
            v => Some(v as u64),
        })
        .fold((0_u64, 0_u64), |(count, total), value| {
            (count + 1, total + value)
        });
    // plus any copies of the final advance
    let last_advance = hmtx
        .h_metrics()
        .last()
        .map(|m| m.advance() as u64)
        .unwrap_or_default();
    let (count, total) = if last_advance > 0 {
        let num_short = num_glyphs - hhea.number_of_long_metrics as u64;
        (count + num_short, total + num_short * last_advance)
    } else {
        (count, total)
    };

    Ok((total as f32 / count as f32).ot_round())
}

fn apply_metrics(os2: &mut Os2, metrics: &GlobalMetricsInstance) {
    os2.s_cap_height = Some(metrics.cap_height.ot_round());
    os2.sx_height = Some(metrics.x_height.ot_round());

    os2.y_subscript_x_size = metrics.subscript_x_size.ot_round();
    os2.y_subscript_y_size = metrics.subscript_y_size.ot_round();
    os2.y_subscript_x_offset = metrics.subscript_x_offset.ot_round();
    os2.y_subscript_y_offset = metrics.subscript_y_offset.ot_round();

    os2.y_superscript_x_size = metrics.superscript_x_size.ot_round();
    os2.y_superscript_y_size = metrics.superscript_y_size.ot_round();
    os2.y_superscript_x_offset = metrics.superscript_x_offset.ot_round();
    os2.y_superscript_y_offset = metrics.superscript_y_offset.ot_round();

    os2.y_strikeout_size = metrics.strikeout_size.ot_round();
    os2.y_strikeout_position = metrics.strikeout_position.ot_round();

    os2.s_typo_ascender = metrics.os2_typo_ascender.ot_round();
    os2.s_typo_descender = metrics.os2_typo_descender.ot_round();
    os2.s_typo_line_gap = metrics.os2_typo_line_gap.ot_round();
    os2.us_win_ascent = metrics.os2_win_ascent.ot_round();
    os2.us_win_descent = metrics.os2_win_descent.ot_round();
}

fn add_unicode_range_bits(add_to: &mut HashSet<u32>, codepoint: u32) {
    let maybe_idx = UNICODE_RANGES
        .binary_search_by(|(from, to, _)| match codepoint {
            _ if codepoint < *from => Ordering::Greater,
            _ if codepoint > *to => Ordering::Less,
            _ => Ordering::Equal,
        })
        .ok();
    if let Some(idx) = maybe_idx {
        add_to.insert(UNICODE_RANGES[idx].2);
    }

    // The spec says that bit 57 ("Non Plane 0") is set for any codepoint beyond BMP so do that too
    // https://github.com/fonttools/fonttools/blob/47813b217c1f8bc343c094776020b4f32fc024b0/Lib/fontTools/ttLib/tables/O_S_2f_2.py#L599-L603
    if (0x10000..=0x10FFFF).contains(&codepoint) {
        add_to.insert(57);
    }
}

/// <https://github.com/fonttools/fonttools/blob/47813b217c1f8bc343c094776020b4f32fc024b0/Lib/fontTools/ttLib/tables/O_S_2f_2.py#L317-L334>
fn apply_unicode_range(os2: &mut Os2, codepoints: &HashSet<u32>) {
    let mut bits = HashSet::new();
    for codepoint in codepoints {
        add_unicode_range_bits(&mut bits, *codepoint);
    }

    let mut unicode_range = [0u32; 4];
    for bit in bits {
        let idx = bit / 32;
        let bit = bit - idx * 32;
        assert!(bit <= 32, "{bit}");
        unicode_range[idx as usize] |= 1 << bit;
    }
    os2.ul_unicode_range_1 = unicode_range[0];
    os2.ul_unicode_range_2 = unicode_range[1];
    os2.ul_unicode_range_3 = unicode_range[2];
    os2.ul_unicode_range_4 = unicode_range[3];
}

/// Given a set of Unicode codepoints (integers), calculate the
/// corresponding OS/2 CodePage range bits.
///
/// This is a direct translation from FontTools
///   <https://github.com/googlefonts/ufo2ft/blob/main/Lib/ufo2ft/util.py#L357-L449>
/// FontTools is in turn a translation of <https://github.com/fontforge/fontforge/blob/7b2c074/fontforge/tottf.c#L3158>
fn codepage_range_bits(codepoints: &HashSet<u32>) -> HashSet<usize> {
    let mut bits = HashSet::new();

    let chars = codepoints
        .iter()
        .filter_map(|cp| char::from_u32(*cp))
        .collect::<HashSet<_>>();
    let has_ascii = (0x20_u32..0x7E).all(|cp| codepoints.contains(&cp));
    let has_lineart = chars.contains(&'┤');

    for char in chars.iter() {
        match char {
            'Þ' if has_ascii => {
                bits.insert(0);
            } //Latin 1
            'Ľ' if has_ascii => {
                bits.insert(1); //Latin 2: Eastern Europe
                if has_lineart {
                    bits.insert(58); //Latin 2
                }
            }
            'Б' => {
                bits.insert(2); //Cyrillic
                if chars.contains(&'Ѕ') && has_lineart {
                    bits.insert(57); //IBM Cyrillic
                }
                if chars.contains(&'╜') && has_lineart {
                    bits.insert(49); //MS-DOS Russian
                }
            }
            'Ά' => {
                bits.insert(3); //Greek
                if has_lineart && chars.contains(&'½') {
                    bits.insert(48); //IBM Greek
                }
                if has_lineart && chars.contains(&'√') {
                    bits.insert(60); //Greek, former 437 G
                }
            }
            'İ' if has_ascii => {
                bits.insert(4); //Turkish
                if has_lineart {
                    bits.insert(56); //IBM turkish
                }
            }
            'א' => {
                bits.insert(5); //Hebrew
                if has_lineart && chars.contains(&'√') {
                    bits.insert(53); //Hebrew
                }
            }
            'ر' => {
                bits.insert(6); //Arabic
                if chars.contains(&'√') {
                    bits.insert(51); //Arabic
                }
                if has_lineart {
                    bits.insert(61); //Arabic; ASMO 708
                }
            }
            'ŗ' if has_ascii => {
                bits.insert(7); //Windows Baltic
                if has_lineart {
                    bits.insert(59); //MS-DOS Baltic
                }
            }
            '₫' if has_ascii => {
                bits.insert(8); //Vietnamese
            }
            'ๅ' => {
                bits.insert(16); //Thai
            }
            'エ' => {
                bits.insert(17); //JIS/Japan
            }
            'ㄅ' => {
                bits.insert(18); //Chinese: Simplified chars
            }
            'ㄱ' => {
                bits.insert(19); //Korean wansung
            }
            '央' => {
                bits.insert(20); //Chinese: Traditional chars
            }
            '곴' => {
                bits.insert(21); //Korean Johab
            }
            '♥' if has_ascii => {
                bits.insert(30); //OEM Character Set
            }
            //    //TODO: Symbol bit has a special meaning (check the spec), we need
            //    //to confirm if this is wanted by default.
            //    //elif  chr(0xF000) <= char <= chr(0xF0FF) {
            //    //   bits.insert(31)         //Symbol Character Set
            'þ' if has_ascii && has_lineart => {
                bits.insert(54); //MS-DOS Icelandic
            }
            '╚' if has_ascii => {
                bits.insert(62); //WE/Latin 1
                bits.insert(63); //US
            }
            'Å' if has_ascii && has_lineart && chars.contains(&'√') => {
                bits.insert(50); //MS-DOS Nordic
            }
            'é' if has_ascii && has_lineart && chars.contains(&'√') => {
                bits.insert(52); //MS-DOS Canadian French
            }
            'õ' if has_ascii && has_lineart && chars.contains(&'√') => {
                bits.insert(55); //MS-DOS Portuguese
            }
            _ => (),
        }
    }

    if has_ascii && chars.contains(&'‰') && chars.contains(&'∑') {
        bits.insert(29); // Macintosh Character Set (US Roman)
    }

    // when no codepage ranges can be enabled, fall back to enabling bit 0
    // (Latin 1) so that the font works in MS Word:
    // https://github.com/googlei18n/fontmake/issues/468
    if bits.is_empty() {
        bits.insert(0);
    }

    bits
}

fn apply_codepage_range(os2: &mut Os2, codepoints: &HashSet<u32>) {
    let bits = codepage_range_bits(codepoints);
    let mut codepage_range = [0u32; 2];
    for bit in bits {
        let idx = bit / 32;
        let bit = bit - idx * 32;
        assert!(bit <= 32, "{bit}");
        codepage_range[idx] |= 1 << bit;
    }
    os2.ul_code_page_range_1 = Some(codepage_range[0]);
    os2.ul_code_page_range_2 = Some(codepage_range[1]);
}

fn apply_min_max_char_index(os2: &mut Os2, codepoints: &HashSet<u32>) {
    let (min, max) = codepoints
        .iter()
        .fold((0xFFFF, 0), |(min, max), cp| (*cp.min(&min), *cp.max(&max)));
    os2.us_first_char_index = min.min(0xFFFF) as u16;
    os2.us_last_char_index = max.min(0xFFFF) as u16;
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L89-L96>
fn max_context_of_rule(input_glyph_count: usize, lookahead_glyph_count: usize) -> u16 {
    (input_glyph_count + lookahead_glyph_count) as u16
}

trait Rule {
    fn sequence_len(&self) -> usize;
    fn lookahead_len(&self) -> usize;

    fn context(&self) -> u16 {
        (self.sequence_len() + self.lookahead_len()) as u16
    }
}

trait RuleSet {
    type Rule;
    fn rules(&self) -> &[OffsetMarker<Self::Rule>];
}

/// A subtable with a set of nullable rules that contribute to max context
trait ContextualSubtable {
    type Rule: Rule;
    type RuleSet: RuleSet;
    fn rule_sets(&self) -> &[NullableOffsetMarker<Self::RuleSet>];
}

impl ContextualSubtable for &SequenceContextFormat1 {
    type Rule = SequenceRule;
    type RuleSet = SequenceRuleSet;
    fn rule_sets(&self) -> &[NullableOffsetMarker<Self::RuleSet>] {
        &self.seq_rule_sets
    }
}

impl RuleSet for SequenceRuleSet {
    type Rule = SequenceRule;
    fn rules(&self) -> &[OffsetMarker<Self::Rule>] {
        &self.seq_rules
    }
}

impl Rule for SequenceRule {
    fn sequence_len(&self) -> usize {
        self.input_sequence.len()
    }

    fn lookahead_len(&self) -> usize {
        self.seq_lookup_records.len()
    }
}

impl ContextualSubtable for &SequenceContextFormat2 {
    type Rule = ClassSequenceRule;
    type RuleSet = ClassSequenceRuleSet;
    fn rule_sets(&self) -> &[NullableOffsetMarker<Self::RuleSet>] {
        &self.class_seq_rule_sets
    }
}

impl RuleSet for ClassSequenceRuleSet {
    type Rule = ClassSequenceRule;
    fn rules(&self) -> &[OffsetMarker<Self::Rule>] {
        &self.class_seq_rules
    }
}

impl Rule for ClassSequenceRule {
    fn sequence_len(&self) -> usize {
        self.input_sequence.len()
    }

    fn lookahead_len(&self) -> usize {
        self.seq_lookup_records.len()
    }
}

impl ContextualSubtable for &ChainedSequenceContextFormat1 {
    type Rule = ChainedSequenceRule;
    type RuleSet = ChainedSequenceRuleSet;
    fn rule_sets(&self) -> &[NullableOffsetMarker<Self::RuleSet>] {
        &self.chained_seq_rule_sets
    }
}

impl RuleSet for ChainedSequenceRuleSet {
    type Rule = ChainedSequenceRule;
    fn rules(&self) -> &[OffsetMarker<Self::Rule>] {
        &self.chained_seq_rules
    }
}

impl Rule for ChainedSequenceRule {
    fn sequence_len(&self) -> usize {
        self.input_sequence.len()
    }

    fn lookahead_len(&self) -> usize {
        self.seq_lookup_records.len()
    }
}

impl ContextualSubtable for &ChainedSequenceContextFormat2 {
    type Rule = ChainedClassSequenceRule;
    type RuleSet = ChainedClassSequenceRuleSet;
    fn rule_sets(&self) -> &[NullableOffsetMarker<Self::RuleSet>] {
        &self.chained_class_seq_rule_sets
    }
}

impl RuleSet for ChainedClassSequenceRuleSet {
    type Rule = ChainedClassSequenceRule;
    fn rules(&self) -> &[OffsetMarker<Self::Rule>] {
        &self.chained_class_seq_rules
    }
}

impl Rule for ChainedClassSequenceRule {
    fn sequence_len(&self) -> usize {
        self.input_sequence.len()
    }

    fn lookahead_len(&self) -> usize {
        self.seq_lookup_records.len()
    }
}

fn max_context_of_contextual_subtable<C, RS, R>(subtable: &C) -> u16
where
    C: ContextualSubtable<Rule = R, RuleSet = RS>,
    RS: RuleSet<Rule = R>,
    R: Rule,
{
    subtable
        .rule_sets()
        .iter()
        .filter_map(|nullable| nullable.as_ref())
        .flat_map(|ruleset| ruleset.rules().iter())
        .map(|rule| rule.context())
        .max()
        .unwrap_or_default()
}

trait MaxContext {
    fn max_context(&self) -> u16;
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L27>
impl MaxContext for SingleSubst {
    fn max_context(&self) -> u16 {
        1
    }
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L27>
impl MaxContext for MultipleSubstFormat1 {
    fn max_context(&self) -> u16 {
        1
    }
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L27>
impl MaxContext for AlternateSubstFormat1 {
    fn max_context(&self) -> u16 {
        1
    }
}

/// <https://github.com/fonttools/fonttools/blob/bf77873d5a0ea7462664c8335c8cc7ea9e48ca18/Lib/fontTools/otlLib/maxContextCalc.py#L35-L39>
impl MaxContext for LigatureSubstFormat1 {
    fn max_context(&self) -> u16 {
        self.ligature_sets
            .iter()
            .flat_map(|l| l.ligatures.iter())
            // +1: components gives 2..N
            .fold(0, |max_ctx, ligature| {
                max_ctx.max((ligature.component_glyph_ids.len() + 1) as u16)
            })
    }
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L41-L43>
impl MaxContext for SubstitutionSequenceContext {
    fn max_context(&self) -> u16 {
        match self as &SequenceContext {
            SequenceContext::Format1(format1) => max_context_of_contextual_subtable(&format1),
            SequenceContext::Format2(format2) => max_context_of_contextual_subtable(&format2),
            SequenceContext::Format3(format3) => {
                max_context_of_rule(0, format3.seq_lookup_records.len())
            }
        }
    }
}

/// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L45-L49>
impl MaxContext for SubstitutionChainContext {
    fn max_context(&self) -> u16 {
        match self as &ChainedSequenceContext {
            ChainedSequenceContext::Format1(format1) => {
                max_context_of_contextual_subtable(&format1)
            }
            ChainedSequenceContext::Format2(format2) => {
                max_context_of_contextual_subtable(&format2)
            }
            ChainedSequenceContext::Format3(format3) => {
                max_context_of_rule(0, format3.seq_lookup_records.len())
            }
        }
    }
}

impl MaxContext for ExtensionSubtable {
    fn max_context(&self) -> u16 {
        match self {
            ExtensionSubtable::Single(inner) => inner.extension.max_context(),
            ExtensionSubtable::Multiple(inner) => inner.extension.max_context(),
            ExtensionSubtable::Alternate(inner) => inner.extension.max_context(),
            ExtensionSubtable::Ligature(inner) => inner.extension.max_context(),
            ExtensionSubtable::Contextual(inner) => inner.extension.max_context(),
            ExtensionSubtable::ChainContextual(inner) => inner.extension.max_context(),
            ExtensionSubtable::Reverse(inner) => inner.extension.max_context(),
        }
    }
}

// <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py#L55-L57>
impl MaxContext for ReverseChainSingleSubstFormat1 {
    fn max_context(&self) -> u16 {
        max_context_of_rule(
            self.backtrack_coverages.len(),
            self.lookahead_coverages.len(),
        )
    }
}

impl MaxContext for &SubstitutionLookup {
    fn max_context(&self) -> u16 {
        match self {
            SubstitutionLookup::Single(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Multiple(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Alternate(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Ligature(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Contextual(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::ChainContextual(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Extension(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
            SubstitutionLookup::Reverse(inner) => {
                inner.subtables.iter().map(|s| s.max_context()).max()
            }
        }
        .unwrap_or_default()
    }
}

/// See:
/// * <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/otlLib/maxContextCalc.py>
/// * <https://learn.microsoft.com/en-us/typography/opentype/spec/gsub#gsub-header>
/// * <https://learn.microsoft.com/en-us/typography/opentype/spec/gpos#gpos-header>
fn apply_max_context(os2: &mut Os2, context: &Context) {
    let mut max_context: u16 = 0;

    if let Some(arc_gsub) = context.gsub.try_get() {
        if let Some(gsub) = &arc_gsub.0 {
            let lookups = &gsub.lookup_list.lookups;
            max_context = max_context.max(lookups.iter().fold(0, |max_ctx, lookup| {
                max_ctx.max((lookup as &SubstitutionLookup).max_context())
            }));
        }
    }

    if let Some(arc_gpos) = context.gpos.try_get() {
        if let Some(gpos) = &arc_gpos.0 {
            let lookups = &gpos.lookup_list.lookups;
            warn!(
                "max context for gpos not implemented, {} position lookups being ignored",
                lookups.len()
            );
        }
    }

    os2.us_max_context = Some(max_context);
}

fn codepoints(context: &Context) -> HashSet<u32> {
    let glyph_order = context.ir.glyph_order.get();

    let mut codepoints = HashSet::new();
    for glyph_name in glyph_order.iter() {
        codepoints.extend(
            context
                .ir
                .glyphs
                .get(&FeWorkId::Glyph(glyph_name.clone()))
                .codepoints
                .iter(),
        );
    }
    codepoints
}

impl Work<Context, AnyWorkId, Error> for Os2Work {
    fn id(&self) -> AnyWorkId {
        WorkId::Os2.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::GlobalMetrics)
            .variant(WorkId::Hhea)
            .variant(WorkId::Hmtx)
            .variant(WorkId::Gpos)
            .variant(WorkId::Gsub)
            .variant(FeWorkId::ALL_GLYPHS)
            .build()
    }

    /// Generate [OS/2](https://learn.microsoft.com/en-us/typography/opentype/spec/os2)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();

        // We set the OS/2.us{Weight,Width}Class to the default value of 'wght'/'wdth' axes
        // in the same way fontmake does when building a VF:
        // https://github.com/fonttools/fonttools/blob/770917d8/Lib/fontTools/varLib/__init__.py#L1016-L1032
        let default_wght = static_metadata
            .axes
            .iter()
            .find(|axis| axis.tag == Tag::new(b"wght"))
            .map(|axis| axis.default.into_inner().0)
            .unwrap_or(400.0);
        let us_weight_class: u16 = default_wght.clamp(1.0, 1000.0).ot_round();

        let default_wdth = static_metadata
            .axes
            .iter()
            .find(|axis| axis.tag == Tag::new(b"wdth"))
            .map(|axis| axis.default.into_inner().0)
            .unwrap_or(100.0);
        let us_width_class = WidthClass::from_percent(default_wdth) as u16;

        let metrics = context
            .ir
            .global_metrics
            .get()
            .at(static_metadata.default_location());
        let codepoints = codepoints(context);

        let mut os2 = Os2 {
            us_weight_class,
            us_width_class,
            fs_type: static_metadata.misc.fs_type.unwrap_or_default(),
            ach_vend_id: static_metadata.misc.vendor_id,
            fs_selection: static_metadata.misc.selection_flags,
            x_avg_char_width: x_avg_char_width(context)?,

            // https://github.com/googlefonts/ufo2ft/blob/main/Lib/ufo2ft/outlineCompiler.py#L705
            us_break_char: Some(32),

            // Avoid "field must be present for version 2" caused by default to None
            us_default_char: Some(0),
            us_max_context: Some(0),

            ..Default::default()
        };
        apply_metrics(&mut os2, &metrics);
        apply_unicode_range(&mut os2, &codepoints);
        apply_codepage_range(&mut os2, &codepoints);
        apply_min_max_char_index(&mut os2, &codepoints);

        apply_max_context(&mut os2, context);

        context.os2.set_unconditionally(os2.into());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::coords::NormalizedLocation;
    use fontir::ir::{GlobalMetric, GlobalMetrics};
    use std::collections::HashSet;
    use write_fonts::tables::os2::Os2;

    use crate::os2::codepage_range_bits;

    use super::*;

    #[test]
    fn build_basic_os2() {
        let default_location = NormalizedLocation::new();
        let mut global_metrics =
            GlobalMetrics::new(default_location.clone(), 1000, None, None, None, 0.0);

        global_metrics.set(GlobalMetric::CapHeight, default_location.clone(), 37.5);
        global_metrics.set(GlobalMetric::XHeight, default_location.clone(), 112.2);

        let mut os2 = Os2 {
            ach_vend_id: Tag::new(b"DUCK"),
            x_avg_char_width: 42,
            ..Default::default()
        };
        apply_metrics(&mut os2, &global_metrics.at(&default_location));

        assert_eq!(Tag::new(b"DUCK"), os2.ach_vend_id);
        assert_eq!(42, os2.x_avg_char_width);
        assert_eq!(Some(38), os2.s_cap_height);
        assert_eq!(Some(112), os2.sx_height);
    }

    fn unicode_range_bits(codepoint: u32) -> HashSet<u32> {
        let mut bits = HashSet::new();
        add_unicode_range_bits(&mut bits, codepoint);
        bits
    }

    #[test]
    fn unicode_range_bit_lut_latin() {
        assert_eq!(HashSet::from([0]), unicode_range_bits(0x65));
    }

    #[test]
    fn unicode_range_bit_lut_large() {
        assert_eq!(HashSet::from([57]), unicode_range_bits(0x10FFFF));
    }

    #[test]
    fn unicode_range_bit_lut_mahjong() {
        assert_eq!(HashSet::from([57, 122]), unicode_range_bits(0x1F02F));
    }

    #[test]
    fn codepage_range_report_latin_1() {
        assert_eq!(
            HashSet::from([0]),
            codepage_range_bits(&(0x20..=0x7E).collect())
        );
    }

    #[test]
    fn codepage_range_fallback_to_latin_1() {
        let latin_1_sentinel = 'Þ' as u32;
        let latin_2_sentinel = 'Ľ' as u32;

        // No latin, but we'd rather report bit 0 than nothing
        let mut codepoints = (0x20..=0x7E).collect::<HashSet<_>>();
        codepoints.remove(&latin_1_sentinel);
        assert_eq!(HashSet::from([0]), codepage_range_bits(&codepoints));

        // No longer empty so we don't fallback to latin
        codepoints.insert(latin_2_sentinel);
        assert_eq!(HashSet::from([1]), codepage_range_bits(&codepoints));

        // We can report both, right?
        codepoints.insert(latin_1_sentinel);
        assert_eq!(HashSet::from([0, 1]), codepage_range_bits(&codepoints));
    }

    #[test]
    fn min_max_char_index_simple() {
        let codepoints = HashSet::from([5, 6, 99]);
        let mut os2 = Os2::default();
        apply_min_max_char_index(&mut os2, &codepoints);
        assert_eq!((5, 99), (os2.us_first_char_index, os2.us_last_char_index));
    }

    #[test]
    fn min_max_char_index_big_values() {
        // Mahjong font, notably entirely outside u16
        let codepoints = (0x1F000..=0x1F02B).collect();
        let mut os2 = Os2::default();
        apply_min_max_char_index(&mut os2, &codepoints);
        assert_eq!(
            (0xFFFF, 0xFFFF),
            (os2.us_first_char_index, os2.us_last_char_index)
        );
    }
}
