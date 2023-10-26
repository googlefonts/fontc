//! Compiling the 'name' table
//!
//! The name table stores strings that are referenced by various other tables.

use smol_str::SmolStr;
use write_fonts::{read::tables::name::Encoding, types::NameId};

use crate::compile::tags::{MAC_PLATFORM_ID, WIN_PLATFORM_ID};

#[derive(Clone, Debug)]
pub(crate) struct NameBuilder {
    records: Vec<(NameId, NameSpec)>,
    last_anon_id: u16,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct NameSpec {
    pub platform_id: u16,
    pub encoding_id: u16,
    pub language_id: u16,
    pub string: SmolStr,
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
    pub(crate) fn add(&mut self, name_id: NameId, name_spec: NameSpec) {
        self.last_anon_id = self.last_anon_id.max(name_id.to_u16());
        self.records.push((name_id, name_spec));
    }

    pub(crate) fn add_anon_group(&mut self, entries: &[NameSpec]) -> NameId {
        let name_id = self.next_name_id();
        for name in entries {
            self.add(name_id, name.clone());
        }
        name_id
    }

    pub(crate) fn contains_id(&self, id: NameId) -> bool {
        self.records.iter().any(|(name_id, _)| name_id == &id)
    }

    pub(crate) fn next_name_id(&self) -> NameId {
        NameId::new(self.last_anon_id + 1)
    }

    pub(crate) fn build(&self) -> Option<write_fonts::tables::name::Name> {
        (!self.records.is_empty()).then(|| {
            write_fonts::tables::name::Name::new(
                self.records
                    .iter()
                    .filter(|(_, spec)| spec.is_implemented_in_fontations())
                    .map(|(id, spec)| spec.build(*id))
                    .collect(),
            )
        })
    }
}

impl NameSpec {
    fn is_implemented_in_fontations(&self) -> bool {
        Encoding::new(self.platform_id, self.encoding_id) != Encoding::Unknown
    }

    pub fn build(&self, name_id: NameId) -> write_fonts::tables::name::NameRecord {
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
