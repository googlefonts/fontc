//! Access data from the [Adobe Glyph List][agl-aglfn]
//!
//! [agl-aglfn]: https://github.com/adobe-type-tools/agl-aglfn

use std::{collections::HashMap, sync::OnceLock};

use smol_str::SmolStr;

// we generate a pair of static arrays in build.rs, AGLFN and LEGACY_AGL
include!(concat!(env!("OUT_DIR"), "/agl_codegen.rs"));

fn legacy_agl_to_uv() -> &'static HashMap<&'static str, &'static [char]> {
    static UV2AGL: OnceLock<HashMap<&str, &[char]>> = OnceLock::new();
    UV2AGL.get_or_init(|| LEGACY_AGL.iter().map(|(s, cps)| (*s, *cps)).collect())
}

/// Given a unicode value, return a glyph name if it exists in the AGLFN
pub fn agl_name_for_char(c: char) -> Option<&'static str> {
    static UV2AGL: OnceLock<HashMap<u32, &str>> = OnceLock::new();
    UV2AGL
        .get_or_init(|| AGLFN.iter().map(|(s, cp)| (*cp, *s)).collect())
        .get(&(c as u32))
        .copied()
}

/// Given a glyph name, return the unicode value if it exists in the AGLFN
pub fn char_for_agl_name(name: &str) -> Option<char> {
    static AGL2UV: OnceLock<HashMap<&str, u32>> = OnceLock::new();
    AGL2UV
        .get_or_init(|| AGLFN.iter().map(|(s, cp)| (*s, *cp)).collect())
        .get(name)
        .copied()
        .and_then(char::from_u32)
}

/// Convert a glyph name to a unicode string.
///
/// This attempts to handle ligatures and suffixes; so `f_f` becomes "ff",
/// and `longs_t.oldstyle` becomes "ſt".
///
/// The result is a `SmolStr`, but it should logically be thought of as
/// an array of `char`.
///
/// Unidentified glyphs or components will be omitted, so `f_UNKNOWNNAME` becomes "f".
///
/// See <https://github.com/fonttools/fonttools/blob/8697f91cdc3f/Lib/fontTools/agl.py#L5107>
/// (note that we don't bother handling zapf dingbats; yolo)
pub fn glyph_name_to_unicode(name: &str) -> SmolStr {
    // drop everything after the first '.', if it exists
    let trimmed = name.split_once('.').map(|(a, _)| a).unwrap_or(name);
    trimmed
        .split('_')
        .flat_map(glyph_component_to_unicode)
        .collect()
}

// https://github.com/fonttools/fonttools/blob/8697f91cd/Lib/fontTools/agl.py#L5131
fn glyph_component_to_unicode(component: &str) -> Vec<char> {
    // if component is in AGL, use that
    if let Some(chars) = legacy_agl_to_uv().get(component) {
        return chars.to_vec();
    }
    // now handle cases like uni2001 or uni40045123 (concatenated unicode literal)
    uni_to_unicode(component)
        // or the form u1234/u10FAFA
        .or_else(|| u_to_unicode(component).map(|uv| vec![uv]))
        .unwrap_or_default()
}

// https://github.com/fonttools/fonttools/blob/8697f91cdc/Lib/fontTools/agl.py#L5200
fn uni_to_unicode(component: &str) -> Option<Vec<char>> {
    let digits = component.strip_prefix("uni")?;
    if !digits.bytes().all(is_uppercase_hex) || digits.len() % 4 != 0 {
        return None;
    }
    let n_values = digits.len() / 4;

    (0..n_values)
        .map(|i| u32::from_str_radix(&digits[i * 4..i * 4 + 4], 16).expect("checked above"))
        .map(|uv| {
            if (0xD800..=0xDFFF).contains(&uv) {
                None
            } else {
                char::from_u32(uv)
            }
        })
        .collect()
}

fn is_uppercase_hex(b: u8) -> bool {
    b.is_ascii_digit() || (b'A'..=b'F').contains(&b)
}

// https://github.com/fonttools/fonttools/blob/8697f91cdc/Lib/fontTools/agl.py#L5219
fn u_to_unicode(component: &str) -> Option<char> {
    let value = component.strip_prefix('u')?;
    if !value.bytes().all(is_uppercase_hex) || !(4..=6).contains(&value.len()) {
        return None;
    }

    let uv = u32::from_str_radix(value, 16).ok()?;
    if (0..=0xD7FF).contains(&uv) || (0xE000..=0x10FFFF).contains(&uv) {
        char::from_u32(uv)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn name() {
        assert_eq!(LEGACY_AGL.len(), 4281);
        assert_eq!(AGLFN.len(), 586);
    }

    // https://github.com/fonttools/fonttools/blob/8697f91cdc/Tests/agl_test.py#L6
    #[test]
    fn test_spec_examples() {
        for (input, expected) in [
            ("Lcommaaccent", "Ļ"),
            ("uni20AC0308", "\u{20AC}\u{0308}"),
            ("u1040C", "\u{1040C}"),
            //out of bounds
            ("uniD801DC0C", ""),
            // lowercase is not okay!
            ("uni20ac", ""),
            (
                "Lcommaaccent_uni20AC0308_u1040C.alternate",
                "\u{013B}\u{20AC}\u{0308}\u{1040C}",
            ),
            ("Lcommaaccent_uni013B_u013B", "ĻĻĻ"),
            ("foo", ""),
            (".notdef", ""),
        ] {
            let result = glyph_name_to_unicode(input);
            assert_eq!(result, expected, "{result:?} != {expected:?} for '{input}'")
        }
    }

    // https://github.com/fonttools/fonttools/blob/8697f91cdc/Tests/agl_test.py#L20
    #[test]
    fn test_aglfn() {
        for (input, expected) in [("longs_t", "ſt"), ("f_f_i.alt123", "ffi")] {
            let result = glyph_name_to_unicode(input);
            assert_eq!(result, expected, "{result:?} != {expected:?} for '{input}'")
        }
    }

    // https://github.com/fonttools/fonttools/blob/8697f91cdc/Tests/agl_test.py#L24
    #[test]
    fn test_uni_abcd() {
        for (input, expected) in [
            ("uni0041", "A"),
            ("uni0041_uni0042_uni0043", "ABC"),
            ("uni004100420043", "ABC"),
            ("uni", ""),
            ("uni41", ""),
            ("uni004101", ""),
            ("uniDC00", ""),
        ] {
            let result = glyph_name_to_unicode(input);
            assert_eq!(result, expected, "{result:?} != {expected:?} for '{input}'")
        }
    }

    // https://github.com/fonttools/fonttools/blob/8697f91cdc/Tests/agl_test.py#L33
    #[test]
    fn test_u_abcd() {
        for (input, expected) in [
            ("u0041", "A"),
            ("u00041", "A"),
            ("u000041", "A"),
            ("u0000041", ""),
            ("u0041_uni0041_A.alt", "AAA"),
        ] {
            let result = glyph_name_to_unicode(input);
            assert_eq!(result, expected, "{result:?} != {expected:?} for '{input}'")
        }
    }

    // https://github.com/fonttools/fonttools/blob/8697f91cdc/Tests/agl_test.py#L33
    #[test]
    fn union() {
        // Interesting test case because "uni" is a prefix of "union".
        for (input, expected) in [
            ("union", "∪"),
            // U+222A U+FE00 is a Standardized Variant for UNION WITH SERIFS.
            ("union_uniFE00", "\u{222A}\u{FE00}"),
        ] {
            let result = glyph_name_to_unicode(input);
            assert_eq!(result, expected, "{result:?} != {expected:?} for '{input}'")
        }
    }
}
