use super::{GlyphId, GlyphIdent, GlyphName};
use std::{
    collections::{BTreeMap, HashMap},
    convert::TryFrom,
    iter::FromIterator,
};

//construct with collect() on an iterator of cids or names :shrug:
#[derive(Clone, Debug, Default)]
pub struct GlyphMap {
    names: HashMap<GlyphName, GlyphId>,
    cids: HashMap<u16, GlyphId>,
}

impl GlyphMap {
    pub fn len(&self) -> usize {
        self.names.len() + self.cids.len()
    }

    pub fn is_empty(&self) -> bool {
        self.names.is_empty() && self.cids.is_empty()
    }

    // maybe just for testing?
    pub fn reverse_map(&self) -> BTreeMap<GlyphId, GlyphIdent> {
        self.names
            .iter()
            .map(|(name, id)| (*id, GlyphIdent::Name(name.clone())))
            .chain(
                self.cids
                    .iter()
                    .map(|(cid, id)| (*id, GlyphIdent::Cid(*cid))),
            )
            .collect()
    }

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

impl FromIterator<u16> for GlyphMap {
    fn from_iter<T: IntoIterator<Item = u16>>(iter: T) -> Self {
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

// only intended for testing.
impl FromIterator<GlyphIdent> for GlyphMap {
    fn from_iter<T: IntoIterator<Item = GlyphIdent>>(iter: T) -> Self {
        let mut names = HashMap::new();
        let mut cids = HashMap::new();
        for (idx, item) in iter.into_iter().enumerate() {
            let idx = GlyphId::try_from(idx).unwrap();
            match item {
                GlyphIdent::Cid(cid) => cids.insert(cid, idx),
                GlyphIdent::Name(name) => names.insert(name, idx),
            };
        }
        GlyphMap { names, cids }
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

        fn cid(&self) -> Option<&u16> {
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
            Some(self)
        }
    }

    impl AsGlyphIdent for u16 {
        fn cid(&self) -> Option<&u16> {
            Some(self)
        }
    }

    impl AsGlyphIdent for GlyphIdent {
        fn named(&self) -> Option<&str> {
            if let GlyphIdent::Name(name) = self {
                Some(name)
            } else {
                None
            }
        }

        fn cid(&self) -> Option<&u16> {
            if let GlyphIdent::Cid(cid) = self {
                Some(cid)
            } else {
                None
            }
        }
    }
}
