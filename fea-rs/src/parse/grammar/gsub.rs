use super::glyph;
use crate::parse::{Kind, Parser, TokenSet};

pub(crate) fn gsub(parser: &mut Parser, recovery: TokenSet) {
    fn gsub_body(parser: &mut Parser, recovery: TokenSet) -> Kind {
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
            return Kind::GsubNode;
        }

        // sub glyph by (type 1 or 2)
        if parser.eat(Kind::ByKw) {
            if parser.eat(Kind::NullKw) {
                parser.expect_semi();
                return Kind::GsubType1;
            }

            if is_class && glyph::eat_named_or_unnamed_glyph_class(parser, recovery.union(RECOVERY))
            {
                // type 1, format C
                parser.expect_semi();
                return Kind::GsubType1;
            }

            glyph::expect_glyph_name_like(parser, recovery.union(RECOVERY));
            let is_seq = glyph::eat_glyph_name_like(parser);
            while glyph::eat_glyph_name_like(parser) {
                continue;
            }
            parser.expect_semi();
            if is_seq {
                return Kind::GsubType2;
            } else {
                return Kind::GsubType1;
            }
        // sub glyph from (type 3)
        } else if !is_class && parser.eat(Kind::FromKw) {
            glyph::eat_named_or_unnamed_glyph_class(parser, recovery.union(RECOVERY));
            parser.expect_semi();
            return Kind::GsubType3;
        } else if parser.matches(0, Kind::FromKw) {
            parser.err_and_bump("'from' can only follow glyph, not glyph class");
            parser.eat_until(recovery.union(Kind::Semi.into()));
            return Kind::GsubNode;
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
            Kind::GsubType4
        } else if parser.matches(0, Kind::SingleQuote) {
            finish_chain_rule(parser, recovery)
        } else {
            if parser.matches(0, Kind::ByKw) {
                parser.err("ligature substitution must replace two or more glyphs");
            } else {
                parser.err("expected ligature substitution or marked glyph");
            }
            parser.eat_until(recovery.union(Kind::Semi.into()));
            Kind::GsubNode
        }
    }

    parser.eat_trivia();
    parser.start_node(Kind::GsubNode);
    let kind = gsub_body(parser, recovery);
    parser.finish_and_remap_node(kind);
}

fn finish_chain_rule(parser: &mut Parser, recovery: TokenSet) -> Kind {
    debug_assert!(parser.matches(0, Kind::SingleQuote));
    let recovery = recovery.union(Kind::Semi.into());
    // eat all the marked glyphs + their lookups
    while parser.eat(Kind::SingleQuote) {
        if parser.eat(Kind::LookupKw) {
            if !parser.eat(Kind::Ident) {
                parser.err("expected named lookup");
                parser.eat_until(recovery);
                return Kind::GsubNode;
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
        if glyph::eat_glyph_name_like(parser) {
            while glyph::eat_glyph_name_like(parser) {
                continue;
            }
        } else if !glyph::expect_named_or_unnamed_glyph_class(parser, recovery) {
            // unexpected thing here?
            parser.eat_until(recovery);
            return Kind::GsubNode;
        }
    } else if parser.eat(Kind::FromKw)
        && !glyph::expect_named_or_unnamed_glyph_class(parser, recovery)
    {
        parser.eat_until(recovery);
        return Kind::GsubNode;
    }

    if parser.expect_semi() {
        Kind::GsubType6
    } else {
        Kind::GsubNode
    }
}

fn parse_ignore(parser: &mut Parser, recovery: TokenSet) -> Kind {
    assert!(parser.eat(Kind::IgnoreKw));
    assert!(parser.eat(Kind::SubKw));
    if super::expect_ignore_pattern_body(parser, recovery) {
        Kind::GsubIgnore
    } else {
        Kind::GsubNode
    }
}

fn parse_rsub(parser: &mut Parser, recovery: TokenSet) -> Kind {
    assert!(parser.eat(Kind::RsubKw));
    if !glyph::expect_glyph_or_glyph_class(parser, recovery) {
        parser.eat_until(recovery);
        return Kind::GsubNode;
    }

    super::greedy(glyph::eat_glyph_or_glyph_class)(parser, recovery);

    if !parser.expect(Kind::SingleQuote) {
        parser.eat_until(recovery);
        return Kind::GsubNode;
    }

    super::greedy(glyph::eat_glyph_or_glyph_class)(parser, recovery);

    if parser.matches(0, Kind::SingleQuote) {
        parser.err("reversesub rule can have only one marked glyph");
        parser.eat_until(recovery);
        return Kind::GsubNode;
    }
    if parser.eat(Kind::ByKw) {
        glyph::expect_glyph_name_like(parser, recovery);
    }
    parser.expect_semi();
    Kind::GsubType8
}

#[cfg(test)]
mod tests {
    use super::super::debug_parse_output;
    use super::*;

    #[test]
    fn gsub_1_a() {
        let fea = "sub a by A.sc;";
        let (out, _, errstr) =
            debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(errstr.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GsubType1
  SubKw
  WS( )
  GlyphName(a)
  WS( )
  ByKw
  WS( )
  GlyphName(A.sc)
  ;
END GsubType1
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn gsub_1_b() {
        let fea = "substitute [one.fitted one.oldstyle one.tab.oldstyle] by one;";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GsubType1
  SubKw
  WS( )
  START GlyphClass
    [
    GlyphName(one.fitted)
    WS( )
    GlyphName(one.oldstyle)
    WS( )
    GlyphName(one.tab.oldstyle)
    ]
  END GlyphClass
  WS( )
  ByKw
  WS( )
  GlyphName(one)
  ;
END GsubType1
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn gsub_1_c() {
        let fea = "substitute [a - z] by [A.sc - Z.sc];";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GsubType1
  SubKw
  WS( )
  START GlyphClass
    [
    START GlyphRange
      GlyphName(a)
      WS( )
      -
      WS( )
      GlyphName(z)
    END GlyphRange
    ]
  END GlyphClass
  WS( )
  ByKw
  WS( )
  START GlyphClass
    [
    START GlyphRange
      GlyphName(A.sc)
      WS( )
      -
      WS( )
      GlyphName(Z.sc)
    END GlyphRange
    ]
  END GlyphClass
  ;
END GsubType1
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn gsub_1_d() {
        let fea = "substitute [one.fitted one.oldstyle one.tab.oldstyle] by one;";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GsubType1
  SubKw
  WS( )
  START GlyphClass
    [
    GlyphName(one.fitted)
    WS( )
    GlyphName(one.oldstyle)
    WS( )
    GlyphName(one.tab.oldstyle)
    ]
  END GlyphClass
  WS( )
  ByKw
  WS( )
  GlyphName(one)
  ;
END GsubType1
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn gsub_2() {
        let fea = "substitute f_f_i by f f i;";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GsubType2
  SubKw
  WS( )
  GlyphName(f_f_i)
  WS( )
  ByKw
  WS( )
  GlyphName(f)
  WS( )
  GlyphName(f)
  WS( )
  GlyphName(i)
  ;
END GsubType2
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn gsub_3() {
        let fea = "substitute ampersand from [ampersand.1 ampersand.2 ampersand.3];";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GsubType3
  SubKw
  WS( )
  GlyphName(ampersand)
  WS( )
  FromKw
  WS( )
  START GlyphClass
    [
    GlyphName(ampersand.1)
    WS( )
    GlyphName(ampersand.2)
    WS( )
    GlyphName(ampersand.3)
    ]
  END GlyphClass
  ;
END GsubType3
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn gsub_4() {
        let fea = "substitute [one one.oldstyle] [slash fraction] [two two.oldstyle] by onehalf;
";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GsubType4
  SubKw
  WS( )
  START GlyphClass
    [
    GlyphName(one)
    WS( )
    GlyphName(one.oldstyle)
    ]
  END GlyphClass
  WS( )
  START GlyphClass
    [
    GlyphName(slash)
    WS( )
    GlyphName(fraction)
    ]
  END GlyphClass
  WS( )
  START GlyphClass
    [
    GlyphName(two)
    WS( )
    GlyphName(two.oldstyle)
    ]
  END GlyphClass
  WS( )
  ByKw
  WS( )
  GlyphName(onehalf)
  ;
END GsubType4
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn gsub_6() {
        let fea = "substitute [ a e i o u] f' lookup CNTXT_LIGS i' n' lookup CNTXT_SUB;";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GsubType6
  SubKw
  WS( )
  START GlyphClass
    [
    WS( )
    GlyphName(a)
    WS( )
    GlyphName(e)
    WS( )
    GlyphName(i)
    WS( )
    GlyphName(o)
    WS( )
    GlyphName(u)
    ]
  END GlyphClass
  WS( )
  GlyphName(f)
  '
  WS( )
  LookupKw
  WS( )
  ID(CNTXT_LIGS)
  WS( )
  GlyphName(i)
  '
  WS( )
  GlyphName(n)
  '
  WS( )
  LookupKw
  WS( )
  ID(CNTXT_SUB)
  ;
END GsubType6
",
            out.simple_parse_tree(),
        );
    }

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
                debug_parse_output(bad, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
            assert!(!errors.is_empty(), "{}", bad);
        }
    }
}
