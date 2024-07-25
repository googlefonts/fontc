use write_fonts::tables::post::Post;

use super::{GlyphId16, GlyphIdent};
use fontdrasil::types::GlyphName;
use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    convert::TryInto,
    iter::FromIterator,
};

/// A glyph map for mapping from raw glyph identifiers to numeral `GlyphId16`s.
///
/// This is used to map from names or CIDS encountered in a FEA file to the actual
/// GlyphId16s that will be used in the final font.
///
/// Currently, the only way to construct this type is by calling `collect()`
/// on an iterator of cids or names.
#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct GlyphMap {
    names: HashMap<GlyphName, GlyphId16>,
    cids: HashMap<u16, GlyphId16>,
}

impl GlyphMap {
    /// The total number of glyphs
    pub fn len(&self) -> usize {
        self.names.len() + self.cids.len()
    }

    /// Returns `true` if this map contains no glyphs
    pub fn is_empty(&self) -> bool {
        self.names.is_empty() && self.cids.is_empty()
    }

    /// Generates a reverse map of ids -> raw identifers (names or CIDs)
    //  maybe just for testing?
    pub fn reverse_map(&self) -> BTreeMap<GlyphId16, GlyphIdent> {
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

    /// Iterate the idents in this map, in GID order.
    ///
    /// This is really only intended to be used to create new glyphmaps for testing.
    pub fn iter(&self) -> impl Iterator<Item = GlyphIdent> + '_ {
        self.reverse_map().into_values()
    }

    /// Return `true` if the map contains the provided `GlyphIdent`.
    pub fn contains<Q: ?Sized + sealed::AsGlyphIdent>(&self, key: &Q) -> bool {
        if let Some(name) = key.named() {
            self.names.contains_key(name)
        } else if let Some(cid) = key.cid() {
            self.cids.contains_key(cid)
        } else {
            unreachable!()
        }
    }

    /// Return the `GlyphId16` for the provided `GlyphIdent`
    pub fn get<Q: ?Sized + sealed::AsGlyphIdent>(&self, key: &Q) -> Option<GlyphId16> {
        if let Some(name) = key.named() {
            self.names.get(name).copied()
        } else if let Some(cid) = key.cid() {
            self.cids.get(cid).copied()
        } else {
            unreachable!()
        }
    }

    /// Generate a post table from this glyph map
    pub fn make_post_table(&self) -> Post {
        let reverse = self.reverse_map();
        let rev_vec = reverse
            .values()
            .map(|val| match val {
                GlyphIdent::Name(s) => Cow::Borrowed(s.as_str()),
                GlyphIdent::Cid(cid) => Cow::Owned(format!("cid{:05}", *cid)),
            })
            .collect::<Vec<_>>();

        Post::new_v2(rev_vec.iter().map(Cow::as_ref))
    }
}

impl FromIterator<u16> for GlyphMap {
    fn from_iter<T: IntoIterator<Item = u16>>(iter: T) -> Self {
        GlyphMap {
            names: HashMap::new(),
            cids: iter
                .into_iter()
                .enumerate()
                .map(|(i, cid)| (cid, GlyphId16::new(i.try_into().unwrap())))
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
                .map(|(i, cid)| (cid, GlyphId16::new(i.try_into().unwrap())))
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
            let idx = GlyphId16::new(idx.try_into().unwrap());
            match item {
                GlyphIdent::Cid(cid) => cids.insert(cid, idx),
                GlyphIdent::Name(name) => names.insert(name, idx),
            };
        }
        GlyphMap { names, cids }
    }
}

mod sealed {
    use super::super::GlyphIdent;
    use fontdrasil::types::GlyphName;
    use smol_str::SmolStr;

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

    impl AsGlyphIdent for SmolStr {
        fn named(&self) -> Option<&str> {
            Some(self.as_str())
        }
    }

    impl AsGlyphIdent for GlyphName {
        fn named(&self) -> Option<&str> {
            Some(self.as_str())
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
                Some(name.as_str())
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
