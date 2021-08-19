//sfdhhmm {}

use crate::lexer::Kind as RawKind;
use crate::parse::{Kind, Parser};

pub(crate) fn root(parser: &mut Parser) {
    parser.start_node(Kind::SourceFile);
    while !parser.at_eof() {
        top_level_element(parser);
    }

    parser.finish_node();
}

fn top_level_element(parser: &mut Parser) {
    parser.eat_trivia();

    if parser.at_eof() {
        ()
    } else if parser.current_match(keywords::TABLE) {
        table(parser)
    } else if parser.current_match(keywords::LOOKUP) {
        lookup(parser)
    } else if parser.current_match(keywords::LANGUAGESYSTEM) {
        language_system(parser)
    } else if parser.current_match(keywords::FEATURE) {
        feature(parser)
    } else if parser.current_match(keywords::MARK_CLASS) {
        mark_class(parser)
    } else if parser.current_match(keywords::ANCHOR_DEF) {
        anchor_def(parser)
    } else if parser.current_match(keywords::ANON) || parser.current_match(keywords::ANONYMOUS) {
        anonymous(parser)
    } else if parser.current_match(RawKind::At) {
        glyph_class(parser)
    } else {
        parser.err_and_bump(format!(
            "Unexpected token '{}', expected global keyword.",
            parser.current_token_text()
        ));
        advance_to_top_level(parser);
    }
}

fn advance_to_top_level(parser: &mut Parser) {
    loop {
        parser.eat_trivia();
        if parser.at_eof() || parser.current_match(keywords::TOPLEVEL) {
            break;
        }
        parser.eat_raw();
    }
}

fn language_system(parser: &mut Parser) {
    parser.start_node(Kind::LanguagesystemKw);
    language_system_body(parser);
    parser.finish_node();
}

// languagesystem $ hi;
// languagesystem 'okay
// languagesystem ;
// languagesystem[]
fn language_system_body(parser: &mut Parser) {
    assert!(
        parser.current_match(keywords::LANGUAGESYSTEM),
        "{}",
        parser.current_token_text()
    );
    parser.eat_remap(Kind::LanguagesystemKw);
    if !parser.expect(RawKind::Whitespace)
        || !parser.expect_tag()
        || !parser.expect(RawKind::Whitespace)
        || !parser.expect_tag()
    {
        return advance_to_top_level(parser);
    }
    expect_semi_or_ws_semi(parser);
}

fn expect_semi_or_ws_semi(parser: &mut Parser) {
    if parser.current_match(RawKind::Whitespace) && parser.next_match(RawKind::Semi) {
        parser.eat_raw();
    }
    parser.expect(RawKind::Semi);
}

fn table(parser: &mut Parser) {
    unimplemented!()
}

fn lookup(parser: &mut Parser) {
    unimplemented!()
}

fn feature(parser: &mut Parser) {
    unimplemented!()
}

fn mark_class(parser: &mut Parser) {
    unimplemented!()
}

fn anchor_def(parser: &mut Parser) {
    unimplemented!()
}

fn anonymous(parser: &mut Parser) {
    unimplemented!()
}

fn glyph_class(parser: &mut Parser) {
    unimplemented!()
}
//;

mod keywords {
    pub struct Whitespace;
    pub const WHITESPACE: Whitespace = Whitespace;

    pub const TOPLEVEL: &[&[u8]] = &[
        TABLE,
        LOOKUP,
        LANGUAGESYSTEM,
        ANCHOR_DEF,
        FEATURE,
        MARK_CLASS,
        ANON,
        ANONYMOUS,
        AT,
    ];

    pub const AT: &[u8] = b"@";

    pub const ANCHOR: &[u8] = b"anchor";
    pub const ANCHOR_DEF: &[u8] = b"anchorDef";
    pub const ANON: &[u8] = b"anon";
    pub const ANONYMOUS: &[u8] = b"anonymous";
    pub const BY: &[u8] = b"by";
    pub const CONTOURPOINT: &[u8] = b"contourpoint";
    pub const CURSIVE: &[u8] = b"cursive";
    pub const DEVICE: &[u8] = b"device"; //[ Not implemented ];
    pub const ENUM: &[u8] = b"enum";
    pub const ENUMERATE: &[u8] = b"enumerate";
    pub const EXCLUDE_DFLT: &[u8] = b"exclude_dflt";
    pub const FEATURE: &[u8] = b"feature"; //(used as a block and as a statement);
    pub const FROM: &[u8] = b"from";
    pub const IGNORE: &[u8] = b"ignore"; //(used with substitute and position);
    pub const IGNORE_BASE_GLYPHS: &[u8] = b"IgnoreBaseGlyphs";
    pub const IGNORE_LIGATURES: &[u8] = b"IgnoreLigatures";
    pub const IGNORE_MARKS: &[u8] = b"IgnoreMarks";
    pub const INCLUDE: &[u8] = b"include";
    pub const INCLUDE_DFLT: &[u8] = b"include_dflt";
    pub const LANGUAGE: &[u8] = b"language";
    pub const LANGUAGESYSTEM: &[u8] = b"languagesystem";
    pub const LOOKUP: &[u8] = b"lookup";
    pub const LOOKUPFLAG: &[u8] = b"lookupflag";
    pub const MARK: &[u8] = b"mark";
    pub const MARK_ATTACHMENT_TYPE: &[u8] = b"MarkAttachmentType";
    pub const MARK_CLASS: &[u8] = b"markClass";
    pub const NAMEID: &[u8] = b"nameid";
    pub const NULL: &[u8] = b"NULL"; //(used in substitute, device, value record, anchor);
    pub const PARAMETERS: &[u8] = b"parameters";
    pub const POS: &[u8] = b"pos";
    pub const POSITION: &[u8] = b"position";
    pub const REQUIRED: &[u8] = b"required"; //[ Not implemented ];
    pub const REVERSESUB: &[u8] = b"reversesub";
    pub const RIGHT_TO_LEFT: &[u8] = b"RightToLeft";
    pub const RSUB: &[u8] = b"rsub";
    pub const SCRIPT: &[u8] = b"script";
    pub const SUB: &[u8] = b"sub";
    pub const SUBSTITUTE: &[u8] = b"substitute";
    pub const SUBTABLE: &[u8] = b"subtable";
    pub const TABLE: &[u8] = b"table";
    pub const USE_EXTENSION: &[u8] = b"useExtension";
    pub const USE_MARK_FILTERING_SET: &[u8] = b"UseMarkFilteringSet";
    pub const VALUE_RECORD_DEF: &[u8] = b"valueRecordDef";
    pub const EXCLUDE_DFLT2: &[u8] = b"excludeDFLT"; //(deprecated);
    pub const INCLUDE_DFLT2: &[u8] = b"includeDFLT"; //(deprecated);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::DebugSink;

    fn debug_parse_output(text: &str, f: impl FnOnce(&mut Parser)) -> String {
        let mut sink = DebugSink::default();
        let mut parser = Parser::new(text, &mut sink);
        f(&mut parser);
        sink.to_string()
    }

    #[test]
    fn languagesystem() {
        let input = "languagesystem dflt cool;";
        let out = debug_parse_output(input, |parser| language_system(parser));
        pretty_assertions::assert_eq!(
            out,
            "\
START LanguagesystemKw
  0..14 LanguagesystemKw
  14..15 WS
  15..19 Tag
  19..20 WS
  20..24 Tag
  24..25 Semi
END LanguagesystemKw
"
        )
    }

    #[test]
    fn top_level() {
        let input = "\
languagesystem dflt DFTL;
languagesystem okay cool;
";
        let out = debug_parse_output(input, root);
        let exp = "\
START FILE
  START LanguagesystemKw
    0..14 LanguagesystemKw
    14..15 WS
    15..19 Tag
    19..20 WS
    20..24 Tag
    24..25 Semi
  END LanguagesystemKw
  25..26 WS
  START LanguagesystemKw
    26..40 LanguagesystemKw
    40..41 WS
    41..45 Tag
    45..46 WS
    46..50 Tag
    50..51 Semi
  END LanguagesystemKw
  51..52 WS
END FILE
";

        pretty_assertions::assert_eq!(out, exp);
    }
}
