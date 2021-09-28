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
    fn gpos_body(parser: &mut Parser, recovery: TokenSet) {
        parser.eat(Kind::IgnoreKw);
        parser.eat(Kind::EnumKw);
        assert!(parser.eat(Kind::PosKw));
        if parser.matches(0, Kind::CursiveKw) {
            gpos_cursive(parser, recovery);
        } else if parser.matches(0, Kind::MarkKw) {
            gpos_mark_to_mark(parser, recovery);
        } else if parser.nth_raw(0) == b"base" {
            gpos_mark_to_base(parser, recovery);
        } else if parser.nth_raw(0) == b"ligature" {
            gpos_ligature(parser, recovery);
        } else {
            gpos_single_pair_or_chain(parser, recovery);
        }
    }

    parser.eat_trivia();
    parser.start_node(Kind::GposNode);
    gpos_body(parser, recovery);
    parser.finish_node();
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
    while anchor_mark(parser, recovery) {
        continue;
    }
    parser.expect_semi();
}

fn gpos_ligature(parser: &mut Parser, recovery: TokenSet) {
    assert!(parser.nth_raw(0) == b"ligature");
    parser.eat_remap(Kind::Ident, Kind::LigatureKw);
    glyph::eat_glyph_or_glyph_class(
        parser,
        recovery.union(TokenSet::new(&[Kind::LAngle, Kind::AnchorKw])),
    );
    while anchor_mark(parser, recovery) {
        continue;
    }
    while parser.nth_raw(0) == b"ligComponent" {
        parser.eat_raw();
        while anchor_mark(parser, recovery) {
            continue;
        }
    }
    parser.expect_semi();
}

// single:
// position one <-80 0 -160 0>;
// position one -80;
// pair A::
// position T -60 a <-40 0 -40 0>;
// pair B:
// pos T a -100;        # specific pair (no glyph class present)
// pos [T] a -100;      # class pair (singleton glyph class present)
// pos T @a -100;       # class pair (glyph class present, even if singleton)
// pos @T [a o u] -80
fn gpos_single_pair_or_chain(parser: &mut Parser, recovery: TokenSet) {
    parser.eat(Kind::EnumKw);
    const RECOVERY: TokenSet = TokenSet::new(&[
        Kind::LAngle,
        Kind::SingleQuote,
        Kind::Number,
        Kind::Semi,
        Kind::LookupKw,
    ]);
    glyph::eat_glyph_or_glyph_class(parser, recovery.union(RECOVERY));
    if metrics::eat_value_record(parser, recovery) {
        parser.current_token_text();
        if parser.eat(Kind::Semi) {
            // singleton
            return;
        }
        // pair type A
        glyph::eat_glyph_or_glyph_class(parser, recovery);
        metrics::eat_value_record(parser, recovery);
        parser.expect_semi();
        return;
    }
    if glyph::eat_glyph_or_glyph_class(parser, recovery.union(RECOVERY)) {
        // pair type B
        if metrics::eat_value_record(parser, recovery) {
            parser.expect_semi();
            return;
        }
    }
    // else a chain rule. we just eat all the tokens, since we will make sense
    // of it in the AST
    while eat_chain_element(parser, recovery) {
        continue;
    }
    // in-line single pos rule (6.h.iii)
    if metrics::eat_value_record(parser, recovery.union(Kind::Semi.into())) {
        eat_chain_element(parser, recovery.union(Kind::Semi.into()));
    }
    parser.expect_semi();
}

fn eat_chain_element(parser: &mut Parser, recovery: TokenSet) -> bool {
    parser.eat(Kind::SingleQuote)
        || parser.eat(Kind::LookupKw)
        || parser.eat(Kind::Ident)
        || metrics::eat_value_record(parser, recovery)
        || glyph::eat_glyph_or_glyph_class(parser, recovery)
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
START GposNode
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
END GposNode
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
START GposNode
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
END GposNode
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
START GposNode
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
END GposNode
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
START GposNode
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
END GposNode
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
START GposNode
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
END GposNode
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
START GposNode
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
END GposNode
",
            out.simple_parse_tree(),
        );
    }
}
