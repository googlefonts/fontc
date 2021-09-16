use super::{GlyphId, GlyphName};
use std::{collections::HashMap, convert::TryFrom, iter::FromIterator};

//construct with collect() on an iterator of cids or names :shrug:
#[derive(Clone, Debug, Default)]
pub struct GlyphMap {
    names: HashMap<GlyphName, GlyphId>,
    cids: HashMap<u32, GlyphId>,
}

impl GlyphMap {
    pub fn contains<Q: ?Sized + sealed::AsGlyphIdent>(&self, key: &Q) -> bool {
        if let Some(name) = key.named() {
            self.names.contains_key(name)
        } else if let Some(cid) = key.cid() {
            self.cids.contains_key(cid)
        } else {
            unreachable!()
        }
    }

    pub fn get<Q: ?Sized + sealed::AsGlyphIdent>(&self, key: &Q) -> Option<GlyphId> {
        if let Some(name) = key.named() {
            self.names.get(name).copied()
        } else if let Some(cid) = key.cid() {
            self.cids.get(cid).copied()
        } else {
            unreachable!()
        }
    }
}

impl FromIterator<u32> for GlyphMap {
    fn from_iter<T: IntoIterator<Item = u32>>(iter: T) -> Self {
        GlyphMap {
            names: HashMap::new(),
            cids: iter
                .into_iter()
                .enumerate()
                .map(|(i, cid)| (cid, GlyphId::try_from(i).unwrap()))
                .collect(),
        }
    }
}

impl FromIterator<GlyphName> for GlyphMap {
    fn from_iter<T: IntoIterator<Item = GlyphName>>(iter: T) -> Self {
        GlyphMap {
            names: iter
                .into_iter()
                .enumerate()
                .map(|(i, cid)| (cid, GlyphId::try_from(i).unwrap()))
                .collect(),
            cids: HashMap::new(),
        }
    }
}

mod sealed {
    use super::{super::GlyphIdent, GlyphName};

    /// Something that is either a Cid or a glyph name.
    ///
    /// This is only implemented internally.
    ///
    /// Invariant: an implementor must return `Some` from exactly one of these
    /// two methods.
    pub trait AsGlyphIdent {
        fn named(&self) -> Option<&str> {
            None
        }
        fn cid(&self) -> Option<&u32> {
            None
        }
    }

    impl AsGlyphIdent for str {
        fn named(&self) -> Option<&str> {
            Some(self)
        }
    }

    impl AsGlyphIdent for GlyphName {
        fn named(&self) -> Option<&str> {
            Some(&self)
        }
    }

    impl AsGlyphIdent for GlyphIdent {
        fn named(&self) -> Option<&str> {
            if let GlyphIdent::Name(name) = self {
                Some(&name)
            } else {
                None
            }
        }

        fn cid(&self) -> Option<&u32> {
            if let GlyphIdent::Cid(cid) = self {
                Some(&cid)
            } else {
                None
            }
        }
    }
}
