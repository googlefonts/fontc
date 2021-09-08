use super::glyph;
use crate::parse::{Kind, Parser, TokenSet};

pub(crate) fn gsub(parser: &mut Parser, recovery: TokenSet) {
    fn gsub_body(parser: &mut Parser, recovery: TokenSet) {
        const RECOVERY: TokenSet = TokenSet::new(&[
            Kind::ByKw,
            Kind::FromKw,
            Kind::SingleQuote,
            Kind::Comma,
            Kind::LookupKw,
            Kind::Semi,
        ]);
        parser.eat(Kind::IgnoreKw);
        assert!(parser.eat(Kind::SubKw) || parser.eat(Kind::RsubKw));
        while eat_glyph_sequence_and_marks(parser, recovery.union(RECOVERY)) {
            continue;
        }
        if parser.eat(Kind::ByKw) || parser.eat(Kind::FromKw) {
            if !parser.eat(Kind::NullKw) {
                expect_glyph_sequence_and_marks(parser, recovery.union(RECOVERY), || {
                    "Expected glyph, glyph class, or glyph sequence.".into()
                });
            }
            // is this an ignore sequence?
        } else if parser.matches(0, Kind::Comma) {
            while parser.eat(Kind::Comma) {
                expect_glyph_sequence_and_marks(parser, recovery, || {
                    "Comma should be followed by glyph sequence".into()
                });
            }
        } else if parser.matches(0, Kind::LookupKw) {
            while parser.eat(Kind::LookupKw) {
                parser.expect_recover(
                    Kind::Ident,
                    recovery.union(TokenSet::new(&[Kind::LSquare, Kind::NamedGlyphClass])),
                );
                if parser.matches(0, Kind::Semi) {
                    break;
                }
                if !parser.matches(0, Kind::LookupKw) {
                    expect_glyph_sequence_and_marks(parser, recovery, || {
                        "Expected glyph or glyph sequence.".into()
                    });
                }
            }
        }
        parser.expect_semi();

        // else this should be a chain rule?
    }

    parser.eat_trivia();
    parser.start_node(Kind::GsubNode);
    gsub_body(parser, recovery);
    parser.finish_node();
}

fn expect_glyph_sequence_and_marks(
    parser: &mut Parser,
    recovery: TokenSet,
    on_err: impl FnOnce() -> String,
) {
    if !eat_glyph_sequence_and_marks(parser, recovery) {
        parser.err_recover(on_err(), recovery);
    }
}

fn eat_glyph_sequence_and_marks(parser: &mut Parser, recovery: TokenSet) -> bool {
    if glyph::eat_glyph_or_glyph_class(parser, recovery) {
        parser.eat(Kind::SingleQuote);
        while glyph::eat_glyph_or_glyph_class(parser, recovery) {
            parser.eat(Kind::SingleQuote);
            //continue
        }
        return true;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::super::debug_parse_output;
    use super::*;

    #[test]
    fn gsub_1_a() {
        let fea = "sub a by A.sc;";
        let out = debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START GsubNode
  SubKw
  WS( )
  GlyphName(a)
  WS( )
  ByKw
  WS( )
  GlyphName(A.sc)
  ;
END GsubNode
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn gsub_1_b() {
        let fea = "substitute [one.fitted one.oldstyle one.tab.oldstyle] by one;";
        let out = debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START GsubNode
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
END GsubNode
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn gsub_1_c() {
        let fea = "substitute [a - z] by [A.sc - Z.sc];";
        let out = debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START GsubNode
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
END GsubNode
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn gsub_1_d() {
        let fea = "substitute [one.fitted one.oldstyle one.tab.oldstyle] by one;";
        let out = debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START GsubNode
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
END GsubNode
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn gsub_2() {
        let fea = "substitute f_f_i by f f i;";
        let out = debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START GsubNode
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
END GsubNode
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn gsub_3() {
        let fea = "substitute ampersand from [ampersand.1 ampersand.2 ampersand.3];";
        let out = debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START GsubNode
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
END GsubNode
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn gsub_4() {
        let fea = "substitute [one one.oldstyle] [slash fraction] [two two.oldstyle] by onehalf;
";
        let out = debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START GsubNode
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
END GsubNode
",
            out.simple_parse_tree(fea),
        );
    }

    #[test]
    fn gsub_6() {
        let fea = "substitute [ a e i o u] f' lookup CNTXT_LIGS i' n' lookup CNTXT_SUB;";
        let out = debug_parse_output(fea, |parser| gsub(parser, TokenSet::from(Kind::Eof)));
        assert!(out.errors().is_empty(), "{}", out.print_errs(fea));
        crate::assert_eq_str!(
            "\
START GsubNode
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
END GsubNode
",
            out.simple_parse_tree(fea),
        );
    }
}
