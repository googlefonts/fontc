pub use gpos::Rule as GposRule;
pub use gsub::Rule as GsubRule;

mod gsub {
    use std::rc::Rc;

    use crate::{
        token_tree::Token,
        types::{GlyphId, GlyphOrClass, GlyphSequence},
        Kind, Node, NodeOrToken, TokenSet,
    };

    //use super::{GlyphId, GlyphOrClass, GlyphSequence};

    pub enum Rule {
        Type1(Single),
        Type2(Multiple),
        Type3(Alternate),
        Type4(Ligature),
        Type5(Contextual),
        Type6(ChainingContextual),
        Type7(Extension),
        Type8(ReverseChaining),
        Ignore(Ignore),
    }

    enum GsubParseError {
        /// A catchall for major issues like missing a required keyword.
        InvalidStatement,
        /// An expected token was missing
        Expected(Kind),
        /// A placeholder for work we haven't done yet.
        Unimplemented(&'static str),
    }

    impl Rule {
        fn from_node(node: &Node) -> Result<Self, GsubParseError> {
            //const IGNORE: TokenSet = TokenSet::new(&[Kind::Whitespace, Kind::Comment, Kind::])
            assert_eq!(node.kind, Kind::GsubNode);
            let mut buffer = Vec::new();
            for child in node.children() {
                if !child.kind().is_trivia() {
                    buffer.push(child)
                }
            }

            let (first, body) = buffer
                .split_first()
                .ok_or(GsubParseError::InvalidStatement)?;
            match first.kind() {
                Kind::IgnoreKw => return Err(GsubParseError::Unimplemented("'ignore' keyword")),
                Kind::SubKw => (),
                Kind::RsubKw => (),
                other => panic!("unexpected kind {}", other),
            }
            let is_reverse = first.kind() == Kind::RsubKw;
            let is_contextual = body.iter().any(|t| t.kind() == Kind::SingleQuote);

            if !is_contextual {
                let mut iter = body.iter();
                let first = iter.next().ok_or(GsubParseError::InvalidStatement)?;
                let second = iter.next().ok_or(GsubParseError::InvalidStatement)?;

                match first.kind() {
                    Kind::GlyphClass | Kind::NamedGlyphClass => {}
                    _ => (),
                }
            }

            // now we have our non-trivia nodes in the buffer.

            //Err(())
            unimplemented!()
        }
    }

    pub struct Single {
        sub: GlyphOrClass,
        by: GlyphOrClass,
    }

    pub struct Multiple {
        sub: GlyphId,
        // cannot contain classes; must have length > 1
        by: GlyphSequence,
    }

    pub struct Alternate {
        sub: GlyphId,
        from: Rc<[GlyphId]>,
    }

    pub struct Ligature {
        sub: GlyphSequence,
        by: GlyphId,
    }

    pub struct Contextual;
    pub struct Ignore;
    pub struct ChainingContextual;
    pub struct Extension;
    pub struct ReverseChaining;
}

mod gpos {

    pub enum Rule {}
}
