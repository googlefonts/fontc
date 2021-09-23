use std::rc::Rc;

use smol_str::SmolStr;

use super::GlyphId;

#[derive(Debug, Clone)]
struct GlyphSequence(Rc<[GlyphId]>);

//struct TwoOrMoreGlyphs

#[derive(Debug, Clone)]
#[allow(dead_code)]
enum ValueRecord {
    Advance(i32),
    Placement {
        x_placement: i32,
        y_placement: i32,
        x_advance: i32,
        y_advance: i32,
    },
    // unimplemented
    Device,
    Null,
    Named(SmolStr),
}

#[allow(dead_code)]
enum Anchor {
    Coord { x: i32, y: i32 },
    Contour { x: i32, y: i32, point: u32 },
    //un
    //Device { x: i32, y: i32, x_dev: (), y_dev: (), },
    //unimplemented
    Device,
    Null,
    Named(SmolStr),
}

//struct ParsePattern {
//prefix: Vec<
//}

//mod gpos {
//use smol_str::SmolStr;

//use super::{Anchor, GlyphOrClass, ValueRecord};

//struct Mark {
//anchor: Anchor,
//mark: SmolStr,
//}

//struct Single {
//pos: GlyphOrClass,
//value: ValueRecord,
//}

//struct Pair {
//pos1: GlyphOrClass,
//value1: ValueRecord,
//pos2: GlyphOrClass,
//value2: ValueRecord,
//}

//struct Cursive {
//pos: GlyphOrClass,
//entry: Anchor,
//exit: Anchor,
//}

//struct MarkToBase {
//base: GlyphOrClass,
//marks: Vec<Mark>,
//}

//struct MarkToLigature {
//ligature: GlyphOrClass,
//marks: Vec<Vec<Mark>>,
//}

//struct MarkToMark {
//mark: GlyphOrClass,
//marks: Vec<Mark>,
//}

//struct Contextual;
//struct ChainingContextual;
//struct Extension;
//}
