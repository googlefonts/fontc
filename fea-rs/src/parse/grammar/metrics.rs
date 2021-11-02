use super::super::{Kind, Parser, TokenSet};

// A: <anchor <metric> <metric>> (<anchor 120 -20>)
// B: <anchor <metric> <metric>  # x coordinate, y coordinate
//    <contour point>> (<anchor 120 -20 contourpoint 5>)
// C: <anchor <metric> <metric>   # x coordinate, y coordinate
//    <device> <device>>  # x coordinate device, y coordinate device
//    (<anchor 120 -20 <device 11 1> <device NULL>>)
// D: <anchor NULL>
// E: <anchor <name>> (<anchor TOP_ANCHOR_1>)
pub(crate) fn anchor(parser: &mut Parser, recovery: TokenSet) -> bool {
    fn anchor_body(parser: &mut Parser, recovery: TokenSet) -> bool {
        parser.expect(Kind::LAngle);
        parser.expect(Kind::AnchorKw);
        if parser.eat(Kind::NullKw) || parser.eat(Kind::Ident) {
            return parser.expect_recover(Kind::RAngle, recovery);
        }

        const RANGLE: TokenSet = TokenSet::new(&[Kind::RAngle]);
        let recovery = recovery.union(RANGLE);
        // now either:
        // <metric> metric>
        // <metric> <metric> <contour point>
        // <metric> <metric> <device> <device>
        parser.expect_remap_recover(Kind::Number, Kind::Metric, recovery);
        parser.expect_remap_recover(Kind::Number, Kind::Metric, recovery);
        if parser.eat(Kind::ContourpointKw) {
            parser.expect_recover(Kind::Number, recovery);
        } else if parser.matches(0, Kind::LAngle)
            && parser.matches(1, Kind::DeviceKw)
            && expect_device(parser, recovery)
        {
            expect_device(parser, recovery);
        }
        parser.expect_recover(Kind::RAngle, recovery)
    }

    parser.eat_trivia();
    parser.start_node(Kind::AnchorNode);
    let r = anchor_body(parser, recovery);
    parser.finish_node();
    r
}

// A: <metric> (-5)
// B: <<metric> <metric> <metric> <metric>> (<1 2 -5 242>)
// C: <<metric> <metric> <metric> <metric> <device> <device> <device> <device>>
// (<1 2 -5 242 <device 1 2, 3 4> <device NULL> <device 1 1, 2 2> <device NULL>>)
// return 'true' if we make any progress (this looks like a value record)
pub(crate) fn eat_value_record(parser: &mut Parser, recovery: TokenSet) -> bool {
    fn value_record_body(parser: &mut Parser, recovery: TokenSet) {
        if parser.eat(Kind::Number) {
            return;
        }

        let recovery = recovery.union(TokenSet::new(&[Kind::RAngle]));
        parser.expect_recover(Kind::LAngle, recovery);
        if parser.eat(Kind::NullKw) {
            parser.expect_recover(Kind::RAngle, recovery);
            return;
        }

        parser.expect_recover(Kind::Number, recovery);
        parser.expect_recover(Kind::Number, recovery);
        parser.expect_recover(Kind::Number, recovery);
        parser.expect_recover(Kind::Number, recovery);
        if parser.eat(Kind::RAngle) {
            return;
        }
        // type C:
        expect_device(parser, recovery);
        expect_device(parser, recovery);
        expect_device(parser, recovery);
        expect_device(parser, recovery);
        parser.expect_recover(Kind::RAngle, recovery);
    }

    let looks_like_record = parser.matches(0, Kind::Number)
        || (parser.matches(0, Kind::LAngle)
            && parser.matches(1, TokenSet::new(&[Kind::Number, Kind::NullKw])));

    if !looks_like_record {
        return false;
    }

    parser.eat_trivia();
    parser.start_node(Kind::ValueRecordNode);
    value_record_body(parser, recovery);
    parser.finish_node();
    true
}

pub(crate) fn expect_value_record(parser: &mut Parser, recovery: TokenSet) -> bool {
    if eat_value_record(parser, recovery) {
        true
    } else {
        parser.err_recover("expected valuerecord", recovery);
        false
    }
}

fn expect_device(parser: &mut Parser, recovery: TokenSet) -> bool {
    fn device_body(parser: &mut Parser, recovery: TokenSet) {
        let recovery = recovery.union(TokenSet::new(&[Kind::LAngle, Kind::RAngle, Kind::Comma]));
        parser.expect_recover(Kind::LAngle, recovery);
        parser.expect_recover(Kind::DeviceKw, recovery);
        if parser.eat(Kind::NullKw) {
            parser.expect_recover(Kind::RAngle, recovery);
            return;
        }

        parser.expect_recover(Kind::Number, recovery);
        parser.expect_recover(Kind::Number, recovery);

        while parser.eat(Kind::Comma) {
            // according to the spec <1 2,> should be legal?
            if parser.eat(Kind::RAngle) {
                return;
            }
            parser.expect_recover(Kind::Number, recovery);
            parser.expect_recover(Kind::Number, recovery);
        }

        // FIXME: this should handle an arbitary number of pairs? but also isn't
        // supported yet?
        // I don't know what's going on tbh
        parser.expect(Kind::RAngle);
    }

    if parser.matches(0, Kind::LAngle) && parser.matches(1, Kind::DeviceKw) {
        parser.eat_trivia();
        parser.start_node(Kind::DeviceKw);
        device_body(parser, recovery);
        parser.finish_node();
        true
    } else {
        false
    }
}

pub(crate) fn parameters(parser: &mut Parser, recovery: TokenSet) {
    fn expect_param_item(parser: &mut Parser, recovery: TokenSet) {
        if !parser.eat(Kind::Float) {
            parser.expect_recover(Kind::Number, recovery);
        }
    }

    parser.eat_trivia();
    parser.start_node(Kind::ParametersKw);
    assert!(parser.eat(Kind::ParametersKw));
    expect_param_item(parser, recovery.union(Kind::Semi.into()));
    expect_param_item(parser, recovery.union(Kind::Semi.into()));
    if !parser.matches(0, Kind::Semi) {
        expect_param_item(parser, recovery.union(Kind::Semi.into()));
        expect_param_item(parser, recovery.union(Kind::Semi.into()));
    }
    parser.expect_semi();
    parser.finish_node();
}

/// used in size feature and name table.
///
/// This doesn't include the leading keyword or ID, which vary.
pub(crate) fn expect_name_record(parser: &mut Parser, recovery: TokenSet) {
    const NUM_TYPES: TokenSet = TokenSet::new(&[Kind::Number, Kind::Octal, Kind::Hex]);
    parser.in_node(Kind::NameRecordEntryNode, |parser| {
        if parser.eat(NUM_TYPES) && parser.eat(NUM_TYPES) && !parser.eat(NUM_TYPES) {
            parser.err_recover("name record must contain 1 or 3 numbers", recovery);
        }
        parser.expect_recover(Kind::String, recovery.union(Kind::Semi.into()));
    })
}

#[cfg(test)]
mod tests {
    use super::super::debug_parse_output;
    use super::*;

    #[test]
    fn anchor_a() {
        let fea = "<anchor 120 -30>";
        let (out, errors, errstr) = debug_parse_output(fea, |parser| {
            anchor(parser, TokenSet::EMPTY);
        });
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START AnchorNode
  <
  AnchorKw
  WS( )
  METRIC(120)
  WS( )
  METRIC(-30)
  >
END AnchorNode
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn anchor_a_octal() {
        let fea = "<anchor 070 -30>";
        let (out, errors, _errstr) = debug_parse_output(fea, |parser| {
            anchor(parser, TokenSet::EMPTY);
        });
        assert_eq!(errors.len(), 1);
        assert!(
            errors[0].text().contains("Expected METRIC"),
            "{}",
            errors[0].text()
        );
        crate::assert_eq_str!(
            "\
START AnchorNode
  <
  AnchorKw
  WS( )
  OCT(070)
  WS( )
  METRIC(-30)
  >
END AnchorNode
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn anchor_b() {
        let fea = "<anchor 5 -5 contourpoint 14>";
        let (out, errors, errstr) = debug_parse_output(fea, |parser| {
            anchor(parser, TokenSet::EMPTY);
        });
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
START AnchorNode
  <
  AnchorKw
  WS( )
  METRIC(5)
  WS( )
  METRIC(-5)
  WS( )
  ContourpointKw
  WS( )
  NUM(14)
  >
END AnchorNode
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn value_record_b() {
        let fea = "<-80 0 -160 0>";
        let (out, errors, errstr) = debug_parse_output(fea, |parser| {
            eat_value_record(parser, TokenSet::EMPTY);
        });
        assert!(errors.is_empty(), "{}", errstr);
        crate::assert_eq_str!(
            "\
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
",
            out.simple_parse_tree(),
        );
    }

    #[test]
    fn device_record_smoke_test() {
        let fea = "\
<device NULL>
<device 1 2>
<device 1 2,>
<device 1 2, 3 4, 5 6, 7 8>
";
        let (_out, _, errstr) = debug_parse_output(fea, |parser| {
            expect_device(parser, TokenSet::EMPTY);
        });
        assert!(errstr.is_empty(), "{}", errstr);
    }
}
