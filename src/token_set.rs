//! A bitset of token kinds
//!
//! This is taken directly from rust-analzyer

use super::token::Kind;

/// A bit-set of `Kind`s
#[derive(Clone, Copy)]
pub(crate) struct TokenSet(u128);

impl TokenSet {
    pub(crate) const EMPTY: TokenSet = TokenSet(0);

    pub(crate) const TOP_LEVEL: TokenSet = TokenSet::new(&[
        Kind::TableKw,
        Kind::IncludeKw,
        Kind::LookupKw,
        Kind::LanguagesystemKw,
        Kind::AnchorDefKw,
        Kind::FeatureKw,
        Kind::MarkClassKw,
        Kind::AnonKw,
        Kind::GlyphClass,
    ]);

    /// Top level items + semi
    pub(crate) const TOP_SEMI: TokenSet = TokenSet::TOP_LEVEL.union(TokenSet::new(&[Kind::Semi]));

    /// keywords expected to start items in a feature block
    pub(crate) const FEATURE_BODY_ITEM: TokenSet = TokenSet::TOP_LEVEL.union(TokenSet::new(&[
        Kind::PosKw,
        Kind::EnumKw,
        Kind::IgnoreKw,
        Kind::SubKw,
        Kind::RsubKw,
        Kind::ParametersKw,
    ]));

    pub(crate) const RSQUARE: TokenSet = TokenSet::new(&[Kind::RSquare]);
    //pub(crate) const SEMI: TokenSet = TokenSet::new(&[Kind::Semi]);

    pub(crate) const fn new(kinds: &[Kind]) -> TokenSet {
        let mut res = 0u128;
        let mut i = 0;
        while i < kinds.len() {
            res |= mask(kinds[i]);
            i += 1
        }
        TokenSet(res)
    }

    pub(crate) const fn union(self, other: TokenSet) -> TokenSet {
        TokenSet(self.0 | other.0)
    }

    pub(crate) const fn contains(&self, kind: Kind) -> bool {
        self.0 & mask(kind) != 0
    }
}

const fn mask(kind: Kind) -> u128 {
    1u128 << (kind as usize)
}

impl From<Kind> for TokenSet {
    fn from(src: Kind) -> TokenSet {
        TokenSet::new(&[src])
    }
}

#[test]
fn token_set_works_for_tokens() {
    let ts = TokenSet::new(&[Kind::Eof, Kind::Whitespace]);
    assert!(ts.contains(Kind::Eof));
    assert!(ts.contains(Kind::Whitespace));
    assert!(!ts.contains(Kind::Eq));
}
