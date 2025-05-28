//! mapping opentype tags to unicode scripts
//!
//! based on
//! <https://github.com/fonttools/fonttools/blob/8697f91cdc/Lib/fontTools/unicodedata/OTTags.py>

use write_fonts::types::Tag;

pub(crate) const DFLT_SCRIPT: Tag = Tag::new(b"DFLT");

pub(crate) static SCRIPT_ALIASES: &[(Tag, Tag)] = &[(Tag::new(b"jamo"), Tag::new(b"hang"))];

pub(crate) static SCRIPT_EXCEPTIONS: &[(&str, Tag)] = &[
    ("Hira", Tag::new(b"kana")),
    ("Hrkt", Tag::new(b"kana")),
    ("Laoo", Tag::new(b"lao ")),
    ("Nkoo", Tag::new(b"nko ")),
    ("Vaii", Tag::new(b"vai ")),
    ("Yiii", Tag::new(b"yi  ")),
    ("Zinh", DFLT_SCRIPT),
    ("Zmth", Tag::new(b"math")),
    ("Zyyy", DFLT_SCRIPT),
    ("Zzzz", DFLT_SCRIPT),
];

// 'math' is used as a script in opentype features:
// <https://github.com/harfbuzz/harfbuzz/pull/3417>
pub(crate) static SCRIPT_EXCEPTIONS_REVERSED: &[(Tag, &str)] = &[(Tag::new(b"math"), "Zmth")];

pub(crate) static NEW_SCRIPTS: &[(Tag, &str)] = &[
    (Tag::new(b"bng2"), "Beng"),
    (Tag::new(b"dev2"), "Deva"),
    (Tag::new(b"gjr2"), "Gujr"),
    (Tag::new(b"gur2"), "Guru"),
    (Tag::new(b"knd2"), "Knda"),
    (Tag::new(b"mlm2"), "Mlym"),
    (Tag::new(b"mym2"), "Mymr"),
    (Tag::new(b"ory2"), "Orya"),
    (Tag::new(b"tel2"), "Telu"),
    (Tag::new(b"tml2"), "Taml"),
];

pub(crate) static NEW_SCRIPT_TAGS: &[(&str, Tag)] = &[
    ("Beng", Tag::new(b"bng2")),
    ("Deva", Tag::new(b"dev2")),
    ("Gujr", Tag::new(b"gjr2")),
    ("Guru", Tag::new(b"gur2")),
    ("Knda", Tag::new(b"knd2")),
    ("Mlym", Tag::new(b"mlm2")),
    ("Mymr", Tag::new(b"mym2")),
    ("Orya", Tag::new(b"ory2")),
    ("Taml", Tag::new(b"tml2")),
    ("Telu", Tag::new(b"tel2")),
];

pub(crate) static INDIC_SCRIPTS: &[&str] = &[
    "Beng", // Bengali
    "Deva", // Devanagari
    "Gujr", // Gujarati
    "Guru", // Gurmukhi
    "Knda", // Kannada
    "Mlym", // Malayalam
    "Orya", // Oriya
    "Sinh", // Sinhala
    "Taml", // Tamil
    "Telu", // Telugu
];

pub(crate) static USE_SCRIPTS: &[&str] = &[
    // Correct as at Unicode 15.0
    "Adlm", // Adlam
    "Ahom", // Ahom
    "Bali", // Balinese
    "Batk", // Batak
    "Bhks", // Bhaiksuki
    "Brah", // Brahmi
    "Bugi", // Buginese
    "Buhd", // Buhid
    "Cakm", // Chakma
    "Cham", // Cham
    "Chrs", // Chorasmian
    "Cpmn", // Cypro Minoan
    "Diak", // Dives Akuru
    "Dogr", // Dogra
    "Dupl", // Duployan
    "Egyp", // Egyptian Hieroglyphs
    "Elym", // Elymaic
    "Gong", // Gunjala Gondi
    "Gonm", // Masaram Gondi
    "Gran", // Grantha
    "Hano", // Hanunoo
    "Hmng", // Pahawh Hmong
    "Hmnp", // Nyiakeng Puachue Hmong
    "Java", // Javanese
    "Kali", // Kayah Li
    "Kawi", // Kawi
    "Khar", // Kharosthi
    "Khoj", // Khojki
    "Kits", // Khitan Small Script
    "Kthi", // Kaithi
    "Lana", // Tai Tham
    "Lepc", // Lepcha
    "Limb", // Limbu
    "Mahj", // Mahajani
    "Maka", // Makasar
    "Mand", // Mandaic
    "Mani", // Manichaean
    "Marc", // Marchen
    "Medf", // Medefaidrin
    "Modi", // Modi
    "Mong", // Mongolian
    "Mtei", // Meetei Mayek
    "Mult", // Multani
    "Nagm", // Nag Mundari
    "Nand", // Nandinagari
    "Newa", // Newa
    "Nkoo", // Nko
    "Ougr", // Old Uyghur
    "Phag", // Phags Pa
    "Phlp", // Psalter Pahlavi
    "Plrd", // Miao
    "Rjng", // Rejang
    "Rohg", // Hanifi Rohingya
    "Saur", // Saurashtra
    "Shrd", // Sharada
    "Sidd", // Siddham
    "Sind", // Khudawadi
    "Sogd", // Sogdian
    "Sogo", // Old Sogdian
    "Soyo", // Soyombo
    "Sund", // Sundanese
    "Sylo", // Syloti Nagri
    "Tagb", // Tagbanwa
    "Takr", // Takri
    "Tale", // Tai Le
    "Tavt", // Tai Viet
    "Tfng", // Tifinagh
    "Tglg", // Tagalog
    "Tibt", // Tibetan
    "Tirh", // Tirhuta
    "Tnsa", // Tangsa
    "Toto", // Toto
    "Vith", // Vithkuqi
    "Wcho", // Wancho
    "Yezi", // Yezidi
    "Zanb", // Zanabazar Square
];

#[cfg(test)]
mod tests {
    use super::*;

    /// we want to binary search these, so let's enforce that they are sorted,
    /// to avoid future headaches
    #[test]
    fn const_arrays_are_sorted() {
        fn get_original_and_sorted_items<T: Clone + Ord + Eq, U>(
            items: &[(T, U)],
        ) -> (Vec<T>, Vec<T>) {
            let originals = items.iter().map(|(a, _)| a.clone()).collect::<Vec<_>>();
            let mut sorted = originals.clone();
            sorted.sort();
            (originals, sorted)
        }

        let (actual, expected) = get_original_and_sorted_items(SCRIPT_ALIASES);
        assert_eq!(actual, expected);
        let (actual, expected) = get_original_and_sorted_items(SCRIPT_EXCEPTIONS_REVERSED);
        assert_eq!(actual, expected);
        let (actual, expected) = get_original_and_sorted_items(NEW_SCRIPTS);
        assert_eq!(actual, expected);
        let (actual, expected) = get_original_and_sorted_items(NEW_SCRIPT_TAGS);
        assert_eq!(actual, expected);
        let (actual, expected) = get_original_and_sorted_items(SCRIPT_EXCEPTIONS);
        assert_eq!(actual, expected);
    }
}
