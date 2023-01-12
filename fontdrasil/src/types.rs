//! Basic types useful for font compilation.
//!
//! Particularly types where it's nice for FE and BE to match.

use std::{
    fmt::{Debug, Display},
    sync::Arc,
};

use indexmap::IndexSet;
use once_cell::sync::OnceCell;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};

#[derive(Default)]
struct BigStrings {
    strings: IndexSet<String>,
}

impl BigStrings {
    fn get(&self, str: &str) -> Option<usize> {
        self.strings.get_index_of(str)
    }

    fn resolve(&self, tok: usize) -> &String {
        self.strings
            .get_index(tok)
            .expect("We should only resolve what we previously inserted")
    }

    fn get_or_insert(&mut self, str: &str) -> usize {
        if self.strings.insert(str.to_string()) {
            self.strings.len() - 1 // we're the new tail
        } else {
            self.get(str).unwrap()
        }
    }
}

static BIG_STRINGS: OnceCell<Arc<RwLock<BigStrings>>> = OnceCell::new();

// Unique identifier of a glyph, stack friendly.
//
// >90% of glyph names are <= 16 bytes so we take those directly and spill anything larger
// into a string interner.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum StringRepr {
    Small(u8, [u8; 16]),
    Big(usize),
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(from = "GlyphNameSerdeRepr", into = "GlyphNameSerdeRepr")]
pub struct GlyphName(StringRepr);

impl GlyphName {
    pub fn as_str(&self) -> &str {
        match &self.0 {
            StringRepr::Small(len, content) => unsafe {
                // safe because it was populated from a validated str
                std::str::from_utf8_unchecked(&content[0..*len as usize])
            },
            StringRepr::Big(tok) => {
                let rl = BIG_STRINGS
                    .get()
                    .expect("This object cannot exist without an interner existing")
                    .read();
                let str = rl.resolve(*tok).as_ref();
                // safe because we never deallocate big strings
                unsafe { std::mem::transmute::<&str, &'static str>(str) }
            }
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
        if value.len() > 16 {
            let interner = BIG_STRINGS.get_or_init(|| Arc::from(RwLock::new(Default::default())));
            {
                let rl = interner.read();
                if let Some(tok) = rl.get(value) {
                    return GlyphName(StringRepr::Big(tok));
                }
            }

            let mut wl = interner.write();

            // Someone might have written this when we let down the read lock so don't blindly insert
            let tok = wl.get_or_insert(value);
            GlyphName(StringRepr::Big(tok))
        } else {
            let mut content = [0u8; 16];
            content[0..value.len()].copy_from_slice(value.as_bytes());
            GlyphName(StringRepr::Small(value.len() as u8, content))
        }
    }
}

impl Debug for GlyphName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Display for GlyphName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
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
