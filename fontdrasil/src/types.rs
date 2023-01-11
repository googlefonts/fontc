//! Basic types useful for font compilation.
//!
//! Particularly types where it's nice for FE and BE to match.

use arraystring::ArrayString;
use serde::{Deserialize, Serialize};

// A Copy stack-allocated string
//
// Using this for *long* strings is a bad idea but for glyph names, which are usually short,
// it should be fine.
//
// Wasn't sure how to loop in macro_rules so I used python, e.g.
// python -c 'for i in range(1, 33): print(f"    L{i}(ArrayString<typenum::U{i}>),")'
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum StackString {
    L0(ArrayString<typenum::U1>), // U0 doesn't compile
    L1(ArrayString<typenum::U1>),
    L2(ArrayString<typenum::U2>),
    L3(ArrayString<typenum::U3>),
    L4(ArrayString<typenum::U4>),
    L5(ArrayString<typenum::U5>),
    L6(ArrayString<typenum::U6>),
    L7(ArrayString<typenum::U7>),
    L8(ArrayString<typenum::U8>),
    L9(ArrayString<typenum::U9>),
    L10(ArrayString<typenum::U10>),
    L11(ArrayString<typenum::U11>),
    L12(ArrayString<typenum::U12>),
    L13(ArrayString<typenum::U13>),
    L14(ArrayString<typenum::U14>),
    L15(ArrayString<typenum::U15>),
    L16(ArrayString<typenum::U16>),
    L17(ArrayString<typenum::U17>),
    L18(ArrayString<typenum::U18>),
    L19(ArrayString<typenum::U19>),
    L20(ArrayString<typenum::U20>),
    L21(ArrayString<typenum::U21>),
    L22(ArrayString<typenum::U22>),
    L23(ArrayString<typenum::U23>),
    L24(ArrayString<typenum::U24>),
    L25(ArrayString<typenum::U25>),
    L26(ArrayString<typenum::U26>),
    L27(ArrayString<typenum::U27>),
    L28(ArrayString<typenum::U28>),
    L29(ArrayString<typenum::U29>),
    L30(ArrayString<typenum::U30>),
    L31(ArrayString<typenum::U31>),
    L32(ArrayString<typenum::U32>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(from = "GlyphNameSerdeRepr", into = "GlyphNameSerdeRepr")]
pub struct GlyphName(StackString);

impl GlyphName {
    pub const MAX_LENGTH: usize = 32;

    // Wasn't sure how to loop in macro_rules so I used python, e.g.
    // python -c 'for i in range(1, 33): print(f"                        StackString::L{i}(a) => a.as_str(),")'
    pub fn as_str(&self) -> &str {
        match &self.0 {
            StackString::L0(..) => "",
            StackString::L1(a) => a.as_str(),
            StackString::L2(a) => a.as_str(),
            StackString::L3(a) => a.as_str(),
            StackString::L4(a) => a.as_str(),
            StackString::L5(a) => a.as_str(),
            StackString::L6(a) => a.as_str(),
            StackString::L7(a) => a.as_str(),
            StackString::L8(a) => a.as_str(),
            StackString::L9(a) => a.as_str(),
            StackString::L10(a) => a.as_str(),
            StackString::L11(a) => a.as_str(),
            StackString::L12(a) => a.as_str(),
            StackString::L13(a) => a.as_str(),
            StackString::L14(a) => a.as_str(),
            StackString::L15(a) => a.as_str(),
            StackString::L16(a) => a.as_str(),
            StackString::L17(a) => a.as_str(),
            StackString::L18(a) => a.as_str(),
            StackString::L19(a) => a.as_str(),
            StackString::L20(a) => a.as_str(),
            StackString::L21(a) => a.as_str(),
            StackString::L22(a) => a.as_str(),
            StackString::L23(a) => a.as_str(),
            StackString::L24(a) => a.as_str(),
            StackString::L25(a) => a.as_str(),
            StackString::L26(a) => a.as_str(),
            StackString::L27(a) => a.as_str(),
            StackString::L28(a) => a.as_str(),
            StackString::L29(a) => a.as_str(),
            StackString::L30(a) => a.as_str(),
            StackString::L31(a) => a.as_str(),
            StackString::L32(a) => a.as_str(),
        }
    }
}

impl From<&String> for GlyphName {
    fn from(value: &String) -> Self {
        value.as_str().into()
    }
}

impl From<String> for GlyphName {
    fn from(value: String) -> Self {
        value.as_str().into()
    }
}

impl From<&str> for GlyphName {
    // Wasn't sure how to loop in macro_rules so I used python, e.g.
    // python -c 'for i in range(1, 33): print(f"            {i} => GlyphName(StackString::L{i}(ArrayString::from_str_truncate(value))),")'
    fn from(value: &str) -> Self {
        match value.len() {
            0 => GlyphName(StackString::L0(ArrayString::from_str_truncate(" "))),
            1 => GlyphName(StackString::L1(ArrayString::from_str_truncate(value))),
            2 => GlyphName(StackString::L2(ArrayString::from_str_truncate(value))),
            3 => GlyphName(StackString::L3(ArrayString::from_str_truncate(value))),
            4 => GlyphName(StackString::L4(ArrayString::from_str_truncate(value))),
            5 => GlyphName(StackString::L5(ArrayString::from_str_truncate(value))),
            6 => GlyphName(StackString::L6(ArrayString::from_str_truncate(value))),
            7 => GlyphName(StackString::L7(ArrayString::from_str_truncate(value))),
            8 => GlyphName(StackString::L8(ArrayString::from_str_truncate(value))),
            9 => GlyphName(StackString::L9(ArrayString::from_str_truncate(value))),
            10 => GlyphName(StackString::L10(ArrayString::from_str_truncate(value))),
            11 => GlyphName(StackString::L11(ArrayString::from_str_truncate(value))),
            12 => GlyphName(StackString::L12(ArrayString::from_str_truncate(value))),
            13 => GlyphName(StackString::L13(ArrayString::from_str_truncate(value))),
            14 => GlyphName(StackString::L14(ArrayString::from_str_truncate(value))),
            15 => GlyphName(StackString::L15(ArrayString::from_str_truncate(value))),
            16 => GlyphName(StackString::L16(ArrayString::from_str_truncate(value))),
            17 => GlyphName(StackString::L17(ArrayString::from_str_truncate(value))),
            18 => GlyphName(StackString::L18(ArrayString::from_str_truncate(value))),
            19 => GlyphName(StackString::L19(ArrayString::from_str_truncate(value))),
            20 => GlyphName(StackString::L20(ArrayString::from_str_truncate(value))),
            21 => GlyphName(StackString::L21(ArrayString::from_str_truncate(value))),
            22 => GlyphName(StackString::L22(ArrayString::from_str_truncate(value))),
            23 => GlyphName(StackString::L23(ArrayString::from_str_truncate(value))),
            24 => GlyphName(StackString::L24(ArrayString::from_str_truncate(value))),
            25 => GlyphName(StackString::L25(ArrayString::from_str_truncate(value))),
            26 => GlyphName(StackString::L26(ArrayString::from_str_truncate(value))),
            27 => GlyphName(StackString::L27(ArrayString::from_str_truncate(value))),
            28 => GlyphName(StackString::L28(ArrayString::from_str_truncate(value))),
            29 => GlyphName(StackString::L29(ArrayString::from_str_truncate(value))),
            30 => GlyphName(StackString::L30(ArrayString::from_str_truncate(value))),
            31 => GlyphName(StackString::L31(ArrayString::from_str_truncate(value))),
            32 => GlyphName(StackString::L32(ArrayString::from_str_truncate(value))),
            _ => panic!("StackString cannot accomodate {}", value),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct GlyphNameSerdeRepr {
    name: String,
}

impl From<GlyphName> for GlyphNameSerdeRepr {
    fn from(value: GlyphName) -> Self {
        GlyphNameSerdeRepr {
            name: value.as_str().to_string(),
        }
    }
}

impl From<GlyphNameSerdeRepr> for GlyphName {
    fn from(value: GlyphNameSerdeRepr) -> Self {
        value.name.into()
    }
}
