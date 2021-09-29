pub mod gsub {
    use crate::types::{GlyphClass, GlyphId, GlyphOrClass, GlyphSequence};
    use std::rc::Rc;

    pub enum Rule {
        Type1(Single),
        Type2(Multiple),
        Type3(Alternate),
        Type4(Ligature),
        //Type5(Contextual),
        //Type6(ChainingContextual),
        //Type7(Extension),
        //Type8(ReverseChaining),
        //Ignore(Ignore),
    }

    pub struct Single {
        pub target: GlyphOrClass,
        pub replacement: GlyphOrClass,
    }

    pub struct Multiple {
        pub target: GlyphId,
        // cannot contain classes; must have length > 1
        pub replacement: Rc<[GlyphId]>,
    }

    pub struct Alternate {
        pub target: GlyphId,
        //TODO: this cannot include other glyph classes, needs to happen
        //during validation
        pub alternates: GlyphClass,
    }

    pub struct Ligature {
        pub target: GlyphSequence,
        pub replacement: GlyphId,
    }

    //pub struct Contextual;
    //pub struct Ignore;
    //pub struct ChainingContextual;
    //pub struct Extension;
    //pub struct ReverseChaining;
}

pub mod gpos {

    pub enum Rule {}
}
