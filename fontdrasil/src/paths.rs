const SEPARATOR_CHAR: char = '^';

fn is_reserved_char(c: char) -> bool {
    matches!(
        c,
        '\0'..='\x1F'
            | '\x7F'
            | SEPARATOR_CHAR
            | '>'
            | '|'
            | '['
            | '?'
            | '+'
            | '\\'
            | '"'
            | ':'
            | '/'
            | '<'
            | '%'
            | ']'
            | '*'
    )
}

fn is_reserved_filename(name: &str) -> bool {
    matches!(
        name.to_ascii_uppercase().as_str(),
        "CON"
            | "PRN"
            | "AUX"
            | "CLOCK$"
            | "NUL"
            | "COM1"
            | "LPT1"
            | "LPT2"
            | "LPT3"
            | "COM2"
            | "COM3"
            | "COM4"
    )
}

const BASE_32_CHARS: [char; 32] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
    'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
];

#[inline]
fn ok_for_filenames(c: char) -> bool {
    match c as u32 {
        v if v < 32 => false,
        0x7F => false,
        // py repl: >>> for c in "\" * + / : < > ? [ \ ] |".split(" "): print(f"0x{ord(c):04x} => false, // {unicodedata.name(c).lower()}")
        0x0022 => false, // quotation mark
        0x002a => false, // asterisk
        0x002b => false, // plus sign
        0x002f => false, // solidus
        0x003a => false, // colon
        0x003c => false, // less-than sign
        0x003e => false, // greater-than sign
        0x003f => false, // question mark
        0x005b => false, // left square bracket
        0x005c => false, // reverse solidus
        0x005d => false, // right square bracket
        0x007c => false, // vertical line
        _ => true,
    }
}

/// Is this name part a poor choice on Windows?
///
/// Note that this applies to the portion of the name preceeding a . or as the documentation
/// puts it "NUL.txt and NUL.tar.gz are both equivalent to NUL"
///
/// "Do not use the following reserved names for the name of a file" from
/// <https://learn.microsoft.com/en-gb/windows/win32/fileio/naming-a-file#naming-conventions>
fn scary_for_windows(name: &str) -> bool {
    let name = if let Some(idx) = name.find('.') {
        &name[0..idx]
    } else {
        name
    };
    matches!(
        name.to_ascii_uppercase().as_str(),
        "CON"
            | "PRN"
            | "AUX"
            | "NUL"
            | "COM0"
            | "COM1"
            | "COM2"
            | "COM3"
            | "COM4"
            | "COM5"
            | "COM6"
            | "COM7"
            | "COM8"
            | "COM9"
            | "LPT0"
            | "LPT1"
            | "LPT2"
            | "LPT3"
            | "LPT4"
            | "LPT5"
            | "LPT6"
            | "LPT7"
            | "LPT8"
            | "LPT"
    )
}

/// Matches <https://github.com/googlefonts/fontra/blob/15bc0b8401054390484cfb86d509d633d29657a1/src/fontra/backends/filenames.py#L40-L64>
pub fn string_to_filename(string: &str, suffix: &str) -> String {
    let string_bytes = string.as_bytes();
    let mut code_digits: Vec<_> = string_bytes
        .chunks(5)
        .map(|chunk| {
            let mut digit = 0;
            let mut bit = 1;
            for byte in chunk {
                if byte.is_ascii_uppercase() {
                    digit |= bit
                }
                bit <<= 1;
            }
            digit
        })
        .collect();
    while let Some(0) = code_digits.last() {
        code_digits.pop();
    }

    let mut filename = String::new();
    for (i, c) in string.chars().enumerate() {
        if i == 0 && c == '.' {
            filename.push_str("%2E");
        } else if !is_reserved_char(c) {
            filename.push(c);
        } else {
            filename.push_str(format!("%{:02X}", c as u32).as_str());
        }
    }

    if code_digits.is_empty() && is_reserved_filename(string) {
        code_digits.push(0);
    }

    if !code_digits.is_empty() {
        filename.push(SEPARATOR_CHAR);
        for d in code_digits {
            assert!(d < 32, "We've made a terrible mistake");
            filename.push(BASE_32_CHARS[d]);
        }
    }

    for c in suffix.chars() {
        filename.push(c);
    }
    filename
}

/// Makes a cursory attempt to not produce bad filenames.
///
/// Intended for things like turning a glyph name into a filename. Not meant to
/// be reversible. Use of illegal filename chars may result in duplicate names.
///
/// See
/// * <https://unifiedfontobject.org/versions/ufo3/conventions/#example-implementation>
/// * <https://github.com/unified-font-object/ufo-spec/issues/164>
pub fn safe_filename(name: &str, suffix: &str) -> String {
    let mut filename = Vec::new();
    for ch in name.chars() {
        if ok_for_filenames(ch) {
            filename.push(ch);
        } else {
            filename.push('_');
        }
        if ch == '_' || ch.is_uppercase() {
            filename.push('_');
        }
    }
    filename.extend(suffix.chars());

    if let Some(ch) = filename.first() {
        if *ch == '.' {
            filename[0] = '_';
        }
    }

    let filename: String = filename.into_iter().collect();

    // Windows fears no _
    if scary_for_windows(&filename) {
        "_".to_string() + &filename
    } else {
        filename
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::safe_filename;

    /// <https://github.com/googlefonts/fontc/issues/41>
    fn assert_unique_for_caseinsensitive_fs(names: &[&str]) {
        let filenames: HashSet<_> = names
            .iter()
            .map(|n| safe_filename(n, ""))
            .map(|n| n.to_lowercase())
            .collect();
        assert_eq!(
            names.len(),
            filenames.len(),
            "{names:?} became {filenames:?}"
        );
    }

    #[test]
    fn lower_and_upper_a() {
        assert_unique_for_caseinsensitive_fs(&["a", "A"]);
    }

    #[test]
    fn adding_underscore_avoids_collisions() {
        // if we don't add _ to _ the resulting names are identical
        assert_unique_for_caseinsensitive_fs(&["Aa", "a_a"]);
    }

    #[test]
    fn starts_with_dot() {
        assert_eq!("_notdef", safe_filename(".notdef", ""));
        assert_eq!("_notdef", safe_filename(".not", "def"));
    }

    #[test]
    fn dont_scare_windows() {
        assert_eq!(
            vec!["N_U_L_", "_nul.tar.gz", "_", "_.31", "_.127",],
            vec![
                safe_filename("NUL", ""),
                safe_filename("nul", ".tar.gz"),
                safe_filename("\u{0}", ""),
                safe_filename("\u{1f}", ".31"),
                safe_filename("\u{7f}", ".127"),
            ]
        );
    }
}
