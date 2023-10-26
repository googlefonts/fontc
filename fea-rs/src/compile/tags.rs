//! tags and constants

use std::ops::RangeInclusive;

use write_fonts::types::Tag;

pub const AALT: Tag = Tag::new(b"aalt");
pub const SIZE: Tag = Tag::new(b"size");
pub const LANG_DFLT: Tag = Tag::new(b"dflt");
pub const SCRIPT_DFLT: Tag = Tag::new(b"DFLT");
pub const GSUB: Tag = Tag::new(b"GSUB");
pub const GPOS: Tag = Tag::new(b"GPOS");

pub const WIN_PLATFORM_ID: u16 = 3;
pub const MAC_PLATFORM_ID: u16 = 1;

/// `true` if this tag is ss01-ss20
pub fn is_stylistic_set(tag: Tag) -> bool {
    is_numbered_tag(tag, b"ss", 1..=20)
}

/// `true` if this tag is cv01-cv99
pub fn is_character_variant(tag: Tag) -> bool {
    is_numbered_tag(tag, b"cv", 1..=99)
}

fn is_numbered_tag(tag: Tag, prefix: &[u8], range: RangeInclusive<u8>) -> bool {
    let bytes = tag.into_bytes();
    bytes.starts_with(prefix)
        && std::str::from_utf8(&bytes[2..])
            .ok()
            .and_then(|s| s.parse::<u8>().ok())
            .filter(|n| range.contains(n))
            .is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stylistic_set() {
        assert!(is_stylistic_set(Tag::new(b"ss01")));
        assert!(is_stylistic_set(Tag::new(b"ss10")));
        assert!(is_stylistic_set(Tag::new(b"ss20")));
        assert!(is_stylistic_set(Tag::new(b"ss19")));
        assert!(!is_stylistic_set(Tag::new(b"ss00")));
        assert!(!is_stylistic_set(Tag::new(b"ss21")));
        assert!(!is_stylistic_set(Tag::new(b"ss1 ")));
        assert!(!is_stylistic_set(Tag::new(b"ss0f")));
    }

    #[test]
    fn character_variant() {
        assert!(is_character_variant(Tag::new(b"cv01")));
        assert!(is_character_variant(Tag::new(b"cv10")));
        assert!(is_character_variant(Tag::new(b"cv20")));
        assert!(is_character_variant(Tag::new(b"cv19")));
        assert!(is_character_variant(Tag::new(b"cv99")));
        assert!(!is_character_variant(Tag::new(b"cv00")));
        assert!(!is_character_variant(Tag::new(b"cv1 ")));
        assert!(!is_character_variant(Tag::new(b"cv9f")));
    }
}
