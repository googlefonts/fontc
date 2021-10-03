use super::{glyph, metrics};
use crate::parse::{Kind, Parser, TokenSet};

// 6.a: pos <glyph|glyphclass> <valuerecord>;
// 6.b: [enum] pos <glyph|glyphclass> <valuerecord>
//          <glyph|glyphclass> <valuerecord>;
// or   [enum] pos <glyph|class> <glyph|class> <valuerecord>;
// 6.c: pos cursive <glyph|class> <anchor> <anchor>;
// 6.d: pos base <glyph|class>
//      <anchor> mark <named mark class>+ ;
// 6.e: pos ligature <glyph|class>
//      <anchor> mark <named mark class> +
//      ligComponent
//      <anchor> mark <named mark class> + ;
// 6.f: pos mark <glyph|class>
//      <anchor> mark <named mark class> + ;
pub(crate) fn gpos(parser: &mut Parser, recovery: TokenSet) {
    fn gpos_body(parser: &mut Parser, recovery: TokenSet) -> Kind {
        if parser.matches(0, Kind::IgnoreKw) {
            return parse_ignore(parser, recovery);
        }
        let recovery = recovery.union(Kind::Semi.into());
        if parser.eat(Kind::EnumKw) {
            assert!(parser.eat(Kind::PosKw));
            if glyph::expect_glyph_or_glyph_class(parser, recovery)
                && glyph::expect_glyph_or_glyph_class(parser, recovery)
                && metrics::expect_value_record(parser, recovery)
                && parser.expect_semi()
            {
                return Kind::GposType2;
            } else {
                return Kind::GposNode;
            }
        }
        assert!(parser.eat(Kind::PosKw));
        if parser.matches(0, Kind::CursiveKw) {
            gpos_cursive(parser, recovery);
            Kind::GposType3
        } else if parser.matches(0, Kind::MarkKw) {
            gpos_mark_to_mark(parser, recovery);
            Kind::GposType6
        } else if parser.nth_raw(0) == b"base" {
            gpos_mark_to_base(parser, recovery);
            Kind::GposType4
        } else if parser.nth_raw(0) == b"ligature" {
            gpos_ligature(parser, recovery);
            Kind::GposType5
        } else {
            // all other statements start with glyph or glyph class:
            if !glyph::expect_glyph_or_glyph_class(parser, recovery) {
                parser.eat_until(recovery);
                return Kind::GposNode;
            }
            // now either a single or pair (type A)
            if metrics::eat_value_record(parser, recovery) {
                if glyph::eat_glyph_or_glyph_class(parser, recovery) {
                    metrics::expect_value_record(parser, recovery);
                    parser.expect_semi();
                    return Kind::GposType2;
                }
                parser.expect_semi();
                return Kind::GposType1;
            }
            // pair type B
            if glyph::eat_glyph_or_glyph_class(parser, recovery) {
                if metrics::eat_value_record(parser, recovery) {
                    parser.expect_semi();
                    return Kind::GposType2;
                }
            }

            finish_chain_rule(parser, recovery)
        }
    }

    parser.eat_trivia();
    parser.start_node(Kind::GposNode);
    let kind = gpos_body(parser, recovery);
    parser.finish_and_remap_node(kind);
}

fn parse_ignore(parser: &mut Parser, recovery: TokenSet) -> Kind {
    assert!(parser.eat(Kind::IgnoreKw));
    assert!(parser.eat(Kind::PosKw));

    if super::expect_ignore_pattern_body(parser, recovery) {
        Kind::GposIgnore
    } else {
        Kind::GposNode
    }
}

fn gpos_cursive(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.eat(Kind::CursiveKw));
    glyph::eat_glyph_or_glyph_class(parser, recovery.union(Kind::LAngle.into()));
    metrics::anchor(parser, recovery.union(Kind::LAngle.into()));
    metrics::anchor(parser, recovery);
    parser.expect_semi();
}

fn gpos_mark_to_mark(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.eat(Kind::MarkKw));
    gpos_mark_to_(parser, recovery);
}

fn gpos_mark_to_base(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.nth_raw(0) == b"base");
    parser.eat_remap(Kind::Ident, Kind::BaseKw);
    gpos_mark_to_(parser, recovery);
}

fn gpos_mark_to_(parser: &mut Parser, recovery: TokenSet) {
    glyph::eat_glyph_or_glyph_class(
        parser,
        recovery.union(TokenSet::new(&[Kind::LAngle, Kind::AnchorKw])),
    );
    //FIXME: pass in more recovery?
    super::greedy(anchor_mark)(parser, recovery);
    parser.expect_semi();
}

fn gpos_ligature(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.nth_raw(0) == b"ligature");
    parser.eat_remap(Kind::Ident, Kind::LigatureKw);
    glyph::eat_glyph_or_glyph_class(
        parser,
        recovery.union(TokenSet::new(&[Kind::LAngle, Kind::AnchorKw])),
    );
    super::greedy(anchor_mark)(parser, recovery);

    while parser.nth_raw(0) == b"ligComponent" {
        parser.eat_raw();
        super::greedy(anchor_mark)(parser, recovery);
    }
    parser.expect_semi();
}

fn finish_chain_rule(parser: &mut Parser, recovery: TokenSet) -> Kind {
    const RECOVERY: TokenSet = TokenSet::new(&[
        Kind::LAngle,
        Kind::SingleQuote,
        Kind::Number,
        Kind::LookupKw,
    ]);

    //I should do this more:
    debug_assert!(recovery.contains(Kind::Semi));
    debug_assert!(recovery.contains(Kind::PosKw));
    debug_assert!(recovery.contains(Kind::EnumKw));
    debug_assert!(recovery.contains(Kind::SubKw));

    // finish eating any prefix glyphes/classes
    super::greedy(glyph::eat_glyph_or_glyph_class)(parser, recovery.union(RECOVERY));

    if !parser.matches(0, Kind::SingleQuote) {
        parser.err("expected marked glyph");
        parser.eat_until(recovery);
        return Kind::GposNode;
    }

    while parser.eat(Kind::SingleQuote) {
        if !super::greedy(eat_lookup)(parser, recovery) {
            metrics::eat_value_record(parser, recovery);
        }
        // do something else
        glyph::eat_glyph_or_glyph_class(parser, recovery.union(RECOVERY));
    }

    // eat any suffix glyphs
    super::greedy(glyph::eat_glyph_or_glyph_class)(parser, recovery);

    //TODO: we should be done? but we also don't know how this works? inline rules
    //are weird for gpos I need to rethink this
    if parser.expect_semi() {
        Kind::GposType8
    } else {
        Kind::GposNode
    }
}

fn eat_lookup(parser: &mut Parser, recovery: TokenSet) -> bool {
    if parser.eat(Kind::LookupKw) {
        if !parser.eat(Kind::Ident) {
            parser.err_recover("expected named lookup", recovery);
        }
        return true;
    }
    false
}

// <anchor> mark <named mark glyphclass>
/// returns true if we advance
fn anchor_mark(parser: &mut Parser, recovery: TokenSet) -> bool {
    const RECOVERY: TokenSet = TokenSet::new(&[Kind::MarkKw, Kind::NamedGlyphClass, Kind::Semi]);

    if !(parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::AnchorKw)) {
        return false;
    }
    parser.eat_trivia();
    parser.start_node(Kind::AnchorMarkNode);
    metrics::anchor(parser, recovery.union(RECOVERY));
    // we will verify later that the anchor was NULL
    if !parser.matches(0, Kind::Semi) {
        parser.expect_recover(Kind::MarkKw, recovery.union(RECOVERY));
        parser.expect_recover(Kind::NamedGlyphClass, recovery.union(RECOVERY));
    }
    parser.finish_node();
    true
}

#[cfg(test)]
mod tests {
    use super::super::debug_parse_output;
    use super::*;

    #[test]
    fn single() {
        let fea = "position one <-80 0 -160 0>;";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gpos(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GposType1
  PosKw
  WS( )
  GlyphName(one)
  WS( )
  START ValueRecordNode
    <
    NUM(-80)
    WS( )
    NUM(0)
    WS( )
    NUM(-160)
    WS( )
    NUM(0)
    >
  END ValueRecordNode
  ;
END GposType1
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn pair_1() {
        let fea = "pos T a -100;";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gpos(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GposType2
  PosKw
  WS( )
  GlyphName(T)
  WS( )
  GlyphName(a)
  WS( )
  START ValueRecordNode
    NUM(-100)
  END ValueRecordNode
  ;
END GposType2
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn pair_2() {
        let fea = "pos [T] a -100;";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gpos(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GposType2
  PosKw
  WS( )
  START GlyphClass
    [
    GlyphName(T)
    ]
  END GlyphClass
  WS( )
  GlyphName(a)
  WS( )
  START ValueRecordNode
    NUM(-100)
  END ValueRecordNode
  ;
END GposType2
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn pair_3() {
        let fea = "pos [T] @a -100;";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gpos(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GposType2
  PosKw
  WS( )
  START GlyphClass
    [
    GlyphName(T)
    ]
  END GlyphClass
  WS( )
  @GlyphClass(@a)
  WS( )
  START ValueRecordNode
    NUM(-100)
  END ValueRecordNode
  ;
END GposType2
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn pair_4() {
        let fea = "pos @T [a o u] -100;";
        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gpos(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GposType2
  PosKw
  WS( )
  @GlyphClass(@T)
  WS( )
  START GlyphClass
    [
    GlyphName(a)
    WS( )
    GlyphName(o)
    WS( )
    GlyphName(u)
    ]
  END GlyphClass
  WS( )
  START ValueRecordNode
    NUM(-100)
  END ValueRecordNode
  ;
END GposType2
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn mark_ligature() {
        let fea = r#"position ligature lam_meem_jeem
    <anchor 625 1800> mark @TOP_MARKS     # mark above lam
    ligComponent                          # start specifying marks for meem
    <anchor 376 -368> mark @BOTTOM_MARKS
    ligComponent
    <anchor NULL>;"#;

        let (out, errors, errstr) =
            debug_parse_output(fea, |parser| gpos(parser, TokenSet::from(Kind::Eof)));
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START GposType5
  PosKw
  WS( )
  LigatureKw
  WS( )
  GlyphName(lam_meem_jeem)
  WS(\\n    )
  START AnchorMarkNode
    START AnchorNode
      <
      AnchorKw
      WS( )
      METRIC(625)
      WS( )
      METRIC(1800)
      >
    END AnchorNode
    WS( )
    MarkKw
    WS( )
    @GlyphClass(@TOP_MARKS)
  END AnchorMarkNode
  WS(     )
  #(# mark above lam)
  WS(\\n    )
  ID(ligComponent)
  WS(                          )
  #(# start specifying marks for meem)
  WS(\\n    )
  START AnchorMarkNode
    START AnchorNode
      <
      AnchorKw
      WS( )
      METRIC(376)
      WS( )
      METRIC(-368)
      >
    END AnchorNode
    WS( )
    MarkKw
    WS( )
    @GlyphClass(@BOTTOM_MARKS)
  END AnchorMarkNode
  WS(\\n    )
  ID(ligComponent)
  WS(\\n    )
  START AnchorMarkNode
    START AnchorNode
      <
      AnchorKw
      WS( )
      NullKw
      >
    END AnchorNode
  END AnchorMarkNode
  ;
END GposType5
",
            out.simple_parse_tree(),
        );
    }
}
