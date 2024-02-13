use super::glyph;

use crate::parse::{
    lexer::{Kind, TokenSet},
    Parser,
};
use crate::token_tree::Kind as AstKind;

/// parse a single gsub statement
pub(crate) fn gsub_rule(parser: &mut Parser, recovery: TokenSet) {
    fn gsub_body(parser: &mut Parser, recovery: TokenSet) -> AstKind {
        const RECOVERY: TokenSet = TokenSet::new(&[
            Kind::ByKw,
            Kind::FromKw,
            Kind::SingleQuote,
            Kind::Comma,
            Kind::LookupKw,
            Kind::Semi,
        ]);
        if parser.matches(0, Kind::IgnoreKw) {
            return parse_ignore(parser, recovery);
        }

        if parser.matches(0, Kind::RsubKw) {
            return parse_rsub(parser, recovery);
        }

        assert!(parser.eat(Kind::SubKw));

        let is_class = matches!(parser.nth(0).kind, Kind::LSquare | Kind::NamedGlyphClass);
        if !glyph::eat_glyph_or_glyph_class(parser, recovery.union(RECOVERY)) {
            parser.err_and_bump("Expected glyph or glyph class");
            parser.eat_until(recovery.union(Kind::Semi.into()));
            return AstKind::GsubNode;
        }

        // sub glyph by (type 1 or 2)
        if parser.eat(Kind::Semi) {
            // absense of 'by _' clause means 'by null
            return AstKind::GsubType1;
        } else if parser.eat(Kind::ByKw) {
            if parser.eat(Kind::NullKw) {
                parser.expect_semi();
                return AstKind::GsubType1;
            }

            if is_class && glyph::eat_named_or_unnamed_glyph_class(parser, recovery.union(RECOVERY))
            {
                // type 1, format C
                parser.expect_semi();
                return AstKind::GsubType1;
            }

            glyph::expect_glyph_name_like(parser, recovery.union(RECOVERY));
            let is_seq = glyph::eat_glyph_name_like(parser);
            while glyph::eat_glyph_name_like(parser) {
                continue;
            }
            parser.expect_semi();
            if is_seq {
                return AstKind::GsubType2;
            } else {
                return AstKind::GsubType1;
            }
        // sub glyph from (type 3)
        } else if !is_class && parser.eat(Kind::FromKw) {
            glyph::eat_named_or_unnamed_glyph_class(parser, recovery.union(RECOVERY));
            parser.expect_semi();
            return AstKind::GsubType3;
        } else if parser.matches(0, Kind::FromKw) {
            parser.err_and_bump("'from' can only follow glyph, not glyph class");
            parser.eat_until(recovery.union(Kind::Semi.into()));
            return AstKind::GsubNode;
        }

        // now either ligature or chain
        let is_seq = glyph::eat_glyph_or_glyph_class(parser, recovery.union(RECOVERY));
        while glyph::eat_glyph_or_glyph_class(parser, recovery.union(RECOVERY)) {
            continue;
        }

        // ligature sub
        if is_seq && parser.eat(Kind::ByKw) {
            glyph::expect_glyph_name_like(parser, recovery.union(RECOVERY));
            parser.expect_semi();
            AstKind::GsubType4
        } else if parser.matches(0, Kind::SingleQuote) {
            finish_chain_rule(parser, recovery)
        } else {
            if parser.matches(0, Kind::ByKw) {
                parser.err("ligature substitution must replace two or more glyphs");
            } else {
                parser.err("expected ligature substitution or marked glyph");
            }
            parser.eat_until(recovery.union(Kind::Semi.into()));
            AstKind::GsubNode
        }
    }

    parser.eat_trivia();

    parser.start_node(AstKind::GsubNode);
    let kind = gsub_body(parser, recovery);
    parser.finish_and_remap_node(kind);
}

fn finish_chain_rule(parser: &mut Parser, recovery: TokenSet) -> AstKind {
    debug_assert!(parser.matches(0, Kind::SingleQuote));
    let recovery = recovery.union(Kind::Semi.into());
    // eat all the marked glyphs + their lookups
    while parser.eat(Kind::SingleQuote) {
        while parser.eat(Kind::LookupKw) {
            if !parser.eat(Kind::Ident) {
                parser.err("expected named lookup");
                parser.eat_until(recovery);
                return AstKind::GsubNode;
            }
        }
        glyph::eat_glyph_or_glyph_class(parser, recovery);
    }

    // eat the lookahead glyphs
    while glyph::eat_glyph_or_glyph_class(parser, recovery) {
        continue;
    }

    // now we may be done, or we may have a single inline rule
    if parser.eat(Kind::ByKw) {
        if parser.eat(Kind::NullKw) {
            // allowed, continue down to 'expect_semi'
        } else if glyph::eat_glyph_name_like(parser) {
            while glyph::eat_glyph_name_like(parser) {
                continue;
            }
        } else if !glyph::expect_named_or_unnamed_glyph_class(parser, recovery) {
            // unexpected thing here?
            parser.eat_until(recovery);
            parser.eat(Kind::Semi);
            return AstKind::GsubNode;
        }
    } else if parser.eat(Kind::FromKw)
        && !glyph::expect_named_or_unnamed_glyph_class(parser, recovery)
    {
        parser.eat_until(recovery);
        parser.eat(Kind::Semi);
        return AstKind::GsubNode;
    }

    if parser.expect_semi() {
        // this looks like a valid contextual rule, but we will need to
        // reparse it. This happens automatically when the node is added to
        // the `AstSink`
        AstKind::GsubNodeNeedsRewrite
    } else {
        AstKind::GsubNode
    }
}

fn parse_ignore(parser: &mut Parser, recovery: TokenSet) -> AstKind {
    assert!(parser.eat(Kind::IgnoreKw));
    assert!(parser.eat(Kind::SubKw));
    if super::expect_ignore_pattern_body(parser, recovery) {
        AstKind::GsubNodeNeedsRewrite
    } else {
        AstKind::GsubNode
    }
}

fn parse_rsub(parser: &mut Parser, recovery: TokenSet) -> AstKind {
    assert!(parser.eat(Kind::RsubKw));
    let recovery = recovery.add(Kind::Semi);

    super::greedy(glyph::eat_glyph_or_glyph_class)(parser, recovery);

    if !parser.expect(Kind::SingleQuote) {
        parser.eat_until(recovery);
        parser.expect_semi();
        return AstKind::GsubNode;
    }

    super::greedy(glyph::eat_glyph_or_glyph_class)(parser, recovery);

    if parser.matches(0, Kind::SingleQuote) {
        parser.err("reversesub rule can have only one marked glyph");
        parser.eat_until(recovery);
        parser.expect_semi();
        return AstKind::GsubNode;
    }
    if parser.eat(Kind::ByKw) {
        if parser.matches(0, Kind::NullKw) {
            parser.err("Although explicitly part of the FEA spec, 'by NULL' in rsub rules is meaningless.\nSee https://github.com/fonttools/fonttools/issues/2952 for more information");
            parser.eat_until(recovery);
            parser.expect_semi();
            return AstKind::GsubNode;
        }
        glyph::expect_glyph_or_glyph_class(parser, recovery);
    }
    parser.expect_semi();
    AstKind::GsubNodeNeedsRewrite
}

#[cfg(test)]
mod tests {
    use super::super::debug_parse_output;
    use super::*;

    #[test]
    fn gsub_smoke_test() {
        let not_allowed = [
            "substitute a by [A.sc - Z.sc];", // glyph by glyph class is disallowed
            "sub a by b [c-d];",              // by sequence can't include classes
            "sub a by b @c;",                 // by sequence can't include classes
            "rsub a b' c' d;",                // only one mark glyph in rsub
            "sub a b' c d' by g;",            // only one run of marked glyphs
        ];

        for bad in not_allowed {
            let (_out, errors, _errstr) =
                debug_parse_output(bad, |parser| gsub_rule(parser, TokenSet::from(Kind::Eof)));
            assert!(!errors.is_empty(), "{}", bad);
        }
    }
}
