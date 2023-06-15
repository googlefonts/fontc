//! A bitset of token kinds
//!
//! This is taken directly from rust-analzyer

use super::Kind;

/// A bit-set of `Kind`s
#[derive(Clone, Copy)]
pub struct TokenSet(u128);

impl TokenSet {
    pub(crate) const EMPTY: TokenSet = TokenSet(0);

    pub(crate) const SEMI: TokenSet = TokenSet::new(&[Kind::Semi]);

    pub(crate) const SEMI_RBRACE: TokenSet = TokenSet::new(&[Kind::Semi, Kind::RBrace]);

    pub(crate) const TOP_LEVEL: TokenSet = TokenSet::new(&[
        Kind::TableKw,
        Kind::IncludeKw,
        Kind::LookupKw,
        Kind::LanguagesystemKw,
        Kind::AnchorDefKw,
        Kind::FeatureKw,
        Kind::MarkClassKw,
        Kind::AnonKw,
        Kind::NamedGlyphClass,
    ]);

    /// Tokens that may be a tag.
    pub(crate) const TAG_LIKE: TokenSet =
        TokenSet::new(&[Kind::MarkKw, Kind::NameKw, Kind::FlagKw, Kind::Ident]);

    /// Tokens that may be identifiers.
    ///
    /// This includes tokens that have special meaning only in certain contexts.
    pub(crate) const IDENT_LIKE: TokenSet = TokenSet::new(&[
        Kind::Ident,
        Kind::HorizAxisBaseScriptListKw,
        Kind::HorizAxisBaseTagListKw,
        Kind::HorizAxisMinMaxKw,
        Kind::VertAxisBaseScriptListKw,
        Kind::VertAxisBaseTagListKw,
        Kind::VertAxisMinMaxKw,
        Kind::AttachKw,
        Kind::GlyphClassDefKw,
        Kind::LigatureCaretByDevKw,
        Kind::LigatureCaretByIndexKw,
        Kind::LigatureCaretByPosKw,
        Kind::MarkAttachClassKw,
        Kind::FontRevisionKw,
        Kind::AscenderKw,
        Kind::CaretOffsetKw,
        Kind::DescenderKw,
        Kind::LineGapKw,
        Kind::CapHeightKw,
        Kind::CodePageRangeKw,
        Kind::PanoseKw,
        Kind::TypoAscenderKw,
        Kind::TypoDescenderKw,
        Kind::TypoLineGapKw,
        Kind::UnicodeRangeKw,
        Kind::VendorKw,
        Kind::WinAscentKw,
        Kind::WinDescentKw,
        Kind::XHeightKw,
        Kind::SizemenunameKw,
        Kind::VertTypoAscenderKw,
        Kind::VertTypoDescenderKw,
        Kind::VertTypoLineGapKw,
        Kind::VertAdvanceYKw,
        Kind::VertOriginYKw,
        Kind::ElidedFallbackNameKw,
        Kind::ElidedFallbackNameIDKw,
        Kind::DesignAxisKw,
        Kind::AxisValueKw,
        Kind::FlagKw,
        Kind::LocationKw,
        Kind::ElidableAxisValueNameKw,
        Kind::OlderSiblingFontAttributeKw,
        Kind::FeatureNamesKw,
        Kind::NameKw,
    ]);

    /// Top level items + semi
    pub(crate) const TOP_SEMI: TokenSet = TokenSet::TOP_LEVEL.union(TokenSet::new(&[Kind::Semi]));

    /// keywords that start a gsub or gpos rule
    pub(crate) const RULES: TokenSet = TokenSet::new(&[
        Kind::PosKw,
        Kind::EnumKw,
        Kind::IgnoreKw,
        Kind::SubKw,
        Kind::RsubKw,
    ]);

    /// top level items in a feature or lookup block
    pub(crate) const STATEMENT: TokenSet = TokenSet::new(&[
        Kind::NamedGlyphClass,
        Kind::MarkClassKw,
        Kind::ParametersKw,
        Kind::SubtableKw,
        Kind::LookupflagKw,
        Kind::ScriptKw,
        Kind::LanguageKw,
        Kind::FeatureKw,      //aalt only
        Kind::SizemenunameKw, // size only
        Kind::FeatureNamesKw, //ss01 - ss20 only
    ])
    .union(TokenSet::RULES);

    /// feature block only:
    pub(crate) const FEATURE_STATEMENT: TokenSet =
        TokenSet::new(&[Kind::CvParametersKw, Kind::LookupKw]).union(TokenSet::STATEMENT);

    pub(crate) const TOP_AND_FEATURE: TokenSet = TokenSet::TOP_LEVEL.union(TokenSet::STATEMENT);

    pub(crate) const NUM_TYPES: TokenSet = TokenSet::new(&[Kind::Number, Kind::Octal, Kind::Hex]);

    pub(crate) const FLOAT_LIKE: TokenSet = TokenSet::new(&[Kind::Number, Kind::Float]);

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

    pub(crate) const fn add(self, token: Kind) -> TokenSet {
        assert!((token as u16) < 128);
        TokenSet(self.0 | mask(token))
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

impl std::fmt::Display for TokenSet {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.0.count_ones() == 0 {
            return write!(f, "no tokens");
        }

        let mut first = true;
        for kind in iter_tokens(*self) {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{}", kind)?;
        }
        Ok(())
    }
}

fn iter_tokens(set: TokenSet) -> impl Iterator<Item = Kind> {
    let mut raw = set.0;
    std::iter::from_fn(move || {
        let idx = raw.trailing_zeros();
        if idx == u128::BITS {
            return None;
        }
        let raw_next = idx as u16;
        // safety: Kind is repr(u16), and has more than 128 members, so this
        // will at least generate a valid Kind (not UB)
        let next: Kind = unsafe { std::mem::transmute(raw_next) };
        raw ^= 1u128 << idx;
        Some(next)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_set_works_for_tokens() {
        let ts = TokenSet::new(&[Kind::Eof, Kind::Whitespace]);
        assert!(ts.contains(Kind::Eof));
        assert!(ts.contains(Kind::Whitespace));
        assert!(!ts.contains(Kind::Eq));
    }

    #[test]
    fn iter_tokens_smoke_test() {
        let set = TokenSet::new(&[
            Kind::Ident,
            Kind::LAngle,
            Kind::Cid,
            Kind::OlderSiblingFontAttributeKw,
        ]);

        assert_eq!(iter_tokens(set).count(), 4);
        for token in iter_tokens(set) {
            assert!(set.contains(token));
        }
        for token in &[
            Kind::String,
            Kind::RAngle,
            Kind::RParen,
            Kind::NamedGlyphClass,
            Kind::TableKw,
            Kind::ElidableAxisValueNameKw,
        ] {
            assert!(!set.contains(*token));
        }
    }

    #[test]
    fn display() {
        let empty = TokenSet::EMPTY;
        assert_eq!(empty.to_string(), "no tokens");
        let solo = TokenSet::from(Kind::LParen);
        assert_eq!(solo.to_string(), "(");
        let multi = TokenSet::new(&[Kind::TableKw, Kind::Comma, Kind::Hex]);
        assert_eq!(multi.to_string(), "HEX, ,, TableKw");
    }
}
