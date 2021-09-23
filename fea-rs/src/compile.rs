use std::{
    collections::{HashMap, HashSet},
    ops::Range,
    str::FromStr,
};

use smol_str::SmolStr;

use crate::{
    token_tree::Token,
    types::{GlyphClass, GlyphIdent, GposRule, GsubRule, LanguageSystem, Tag},
    GlyphMap, Kind, Node, NodeOrToken, SyntaxError, TokenSet,
};

//pub struct CompileCtx {
//glyph_map: GlyphMap,
//errors: Vec<SyntaxError>,
//tables: Tables,
//default_lang_systems: HashSet<LanguageSystem>,
//seen_non_default_script: bool,
//lookups: Vec<(usize, LookupTable)>,
//features: HashMap<Tag, Feature>,
//}

pub struct ValidationCtx<'a> {
    glyph_map: &'a GlyphMap,
    errors: Vec<SyntaxError>,
    tables: Tables,
    default_lang_systems: HashSet<LanguageSystem>,
    lang_systems: HashSet<LanguageSystem>,
    seen_non_default_script: bool,
    lookups: Vec<(usize, LookupTable)>,
    // class and position
    glyph_class_defs: HashMap<SmolStr, (GlyphClass, usize)>,
    features: HashMap<Tag, Feature>,
}

struct Feature {
    pos: usize,
    tag: Tag,
    statements: Vec<Statement>,
}

/// A thing in a feature block
enum Statement {
    Gpos(GposRule),
    GSub(GsubRule),
    Script(Tag),
    Lang(Tag),
    LookupFlag(()),
    GlyphClassDef { name: SmolStr, class: GlyphClass },
    MarkStatement(()),
    Params(()),
    SizeMenuName(()),
    FeatureNames(()),
    Subtable,
    Include(()),
}

#[derive(Clone, Debug, Default)]
struct Tables {
    head: Option<tables::head>,
    hhea: Option<tables::hhea>,
    vhea: Option<tables::vhea>,
    //name: Option<tables::name>,
    //OS2: Option<tables::OS2>,
    //STAT: Option<tables::STAT>,
}

struct LookupTable {
    pos: usize,
    name: SmolStr,
    use_extension: bool,
    statements: Vec<Statement>,
}

//#[derive(Clone, Debug)]
//struct ValidationError {
//range: Range<usize>,
//kind: ErrKind,
//}

//#[derive(Clone, Debug)]
//enum ErrKind {
//UnexpectedToken { expected: Kind, found: Token },
//UnexpectedNode { expected: Kind, found: Kind },
//MissingToken { expected: Kind },
//InvalidTag(InvalidTag),
//}

impl<'a> ValidationCtx<'a> {
    fn new(glyph_map: &'a GlyphMap) -> Self {
        ValidationCtx {
            glyph_map,
            errors: Vec::new(),
            tables: Tables::default(),
            default_lang_systems: Default::default(),
            lang_systems: Default::default(),
            seen_non_default_script: false,
            glyph_class_defs: Default::default(),
            lookups: Vec::new(),
            features: Default::default(),
        }
    }

    fn error(&mut self, range: Range<usize>, message: String) {
        self.errors.push(SyntaxError { range, message })
    }

    fn add_language_system(&mut self, language_system: LanguageSystem, loc: Range<usize>) {
        if language_system == LanguageSystem::DEFAULT && !self.default_lang_systems.is_empty() {
            self.error(
                loc,
                "DFLT dflt must be first languagesystem statement.".into(),
            );
            return;
        }
        if language_system.script == Tag::DFLT_SCRIPT {
            if self.seen_non_default_script {
                self.error(
                    loc,
                    "languagesystem with 'DFLT' script tag must precede non-'DFLT' languagesystems.".into(),
                );
                return;
            } else {
                self.seen_non_default_script = true;
            }
        }

        if self.default_lang_systems.insert(language_system) {
            self.error(loc, "Duplicate languagesystem definition".into());
        }
    }
}

/// Token types to skip during validation
const TO_SKIP: TokenSet = TokenSet::new(&[Kind::Comment, Kind::Whitespace, Kind::Semi]);

pub fn validate<'a>(node: &Node, glyph_map: &'a GlyphMap) -> ValidationCtx<'a> {
    let mut ctx = ValidationCtx::new(glyph_map);

    let mut pos = 0;
    let mut cursor = node.cursor();
    while let Some(item) = cursor.current() {
        //for item in node.children() {
        match item.kind() {
            Kind::LanguageSystemNode => language_system(&mut ctx, item.as_node().unwrap()),
            Kind::GlyphClassDefNode => glyph_class_def(&mut ctx, item.as_node().unwrap(), pos),
            //Kind::MarkClassNode => mark_class_def(n, &mut ctx),
            //Kind::AnchorDefNode => anchor_def(n, &mut ctx),
            //Kind::ValueRecordNode => value_record_def(n, &mut ctx),
            //Kind::IncludeNode => include(n, &mut ctx),
            //Kind::FeatureNode => feature(n, &mut ctx),
            //Kind::TableNode => table(n, &mut ctx),
            //Kind::AnonBlockNode => anon(n, &mut ctx),
            //Kind::LookupBlockNode => lookup_block_top_level(n, &mut ctx),
            Kind::Comment | Kind::Whitespace | Kind::Semi => (),
            // maybe don't return an error? parsing should ensure we don't get here
            other => panic!("I should maybe return an error?"),
        }
        pos += item.text_len();
        cursor.step_over();
    }

    ctx
}

fn language_system(ctx: &mut ValidationCtx, node: &Node) {
    let language_system = get_language_system(node).unwrap();
    ctx.add_language_system(language_system, node.range())
}

/// assumes that node is an error-free LanguageSystemNode
fn get_language_system(node: &Node) -> Result<LanguageSystem, ()> {
    let mut cursor = node.cursor();
    let _kw = cursor.next(TO_SKIP);
    debug_assert_eq!(_kw.map(NodeOrToken::kind), Some(Kind::LanguagesystemKw));
    let script_token = cursor
        .next(TO_SKIP)
        .and_then(NodeOrToken::as_token)
        .unwrap();
    let lang_token = cursor
        .next(TO_SKIP)
        .and_then(NodeOrToken::as_token)
        .unwrap();
    debug_assert_eq!(script_token.kind, Kind::Tag);
    debug_assert_eq!(lang_token.kind, Kind::Tag);
    let script = Tag::from_str(script_token.as_str()).unwrap();
    let language = Tag::from_str(lang_token.as_str()).unwrap();
    Ok(LanguageSystem { script, language })
}

fn glyph_class_def(ctx: &mut ValidationCtx, node: &Node, pos: usize) {
    let mut cursor = node.cursor();
    let class_name = cursor
        .next(TO_SKIP)
        .and_then(NodeOrToken::as_token)
        .unwrap();
    debug_assert_eq!(class_name.kind, Kind::NamedGlyphClass);
    let _eq = cursor.next(TO_SKIP);
    debug_assert_eq!(_eq.map(NodeOrToken::kind), Some(Kind::Eq));
    let class_list_or_name = cursor.next(TO_SKIP).unwrap();
    let resolved_class = match class_list_or_name {
        NodeOrToken::Token(t) if t.kind == Kind::NamedGlyphClass => {
            match ctx.glyph_class_defs.get(t.as_str()).cloned() {
                Some((class, _pos)) => class,
                None => {
                    let range = class_name.range();
                    ctx.error(
                        range.start + pos..range.end + pos,
                        "Glyph class already defined".into(),
                    );
                    return;
                }
            }
        }
        NodeOrToken::Node(node) if node.kind == Kind::GlyphClass => {
            //TODO: resolve this class
            glyph_class_list(ctx, node)
        }
        _other => unreachable!("glyph class def already validated"),
    };
}

fn glyph_class_list(ctx: &mut ValidationCtx, node: &Node) -> GlyphClass {
    let mut result = Vec::new();
    let mut cursor = node.cursor();

    let to_skip = TO_SKIP.union(TokenSet::new(&[Kind::LSquare, Kind::RSquare]));
    while let Some(item) = cursor.next(to_skip) {
        match item.kind() {
            Kind::GlyphName | Kind::Cid => {
                let id = if item.kind() == Kind::GlyphName {
                    ctx.glyph_map.get(item.token_text().unwrap())
                } else {
                    let cid = item.token_text().unwrap().parse::<u32>().unwrap();
                    ctx.glyph_map.get(&cid)
                };

                match id {
                    Some(id) => result.push(id),
                    None => ctx.error(item.range(), "glyph does not exist in font".to_string()),
                }
            }
            Kind::GlyphRange => {
                match glyph_range(item.as_node().unwrap()) {
                    Ok(idents) => {
                        for ident in &idents {
                            match ctx.glyph_map.get(ident) {
                                Some(id) => result.push(id),
                                // technically allowed? we don't have warnings yet
                                None => eprintln!(
                                    "missing glyph {} in range {}..{}",
                                    ident,
                                    idents.first().unwrap(),
                                    idents.last().unwrap()
                                ),
                            }
                        }
                    }
                    Err(err) => ctx.error(item.range(), err),
                }
            }

            Kind::NamedGlyphClass => match ctx.glyph_class_defs.get(item.token_text().unwrap()) {
                Some((class, idx)) if *idx < node.range().start => {
                    result.extend(class.items());
                }
                _ => ctx.error(item.range(), "undefined glyph class".into()),
            },
            other => panic!("unexpected item kind in glyph class list: '{}'", other),
        }
    }
    result.into()
}

fn glyph_range(
    //ctx: &mut ValidationCtx,
    node: &Node,
    //pos: usize,
) -> Result<Vec<GlyphIdent>, String> {
    debug_assert_eq!(node.kind(), Kind::GlyphRange);
    let mut cursor = node.cursor();
    let start = cursor
        .next(TO_SKIP)
        .and_then(NodeOrToken::as_token)
        .unwrap();
    let _hyphen = cursor
        .next(TO_SKIP)
        .and_then(NodeOrToken::as_token)
        .unwrap();
    debug_assert_eq!(_hyphen.kind, Kind::Hyphen);
    let end = cursor
        .next(TO_SKIP)
        .and_then(NodeOrToken::as_token)
        .unwrap();

    match (start.kind, end.kind) {
        (Kind::Cid, Kind::Cid) => cid_range(start, end),
        (Kind::GlyphName, Kind::GlyphName) => named_range(start, end),
        (_, _) => return Err("Invalid glyph range".to_string()),
    }
}

/// iter glyph ids in a cid range
fn cid_range(
    //ctx: &mut ValidationCtx,
    start: &Token,
    end: &Token,
) -> Result<Vec<GlyphIdent>, String> {
    let start_cid = start.text.parse::<u32>().unwrap();
    let end_cid = end.text.parse::<u32>().unwrap();
    if start_cid >= end_cid {
        return Err("Range end must be greater than start".into());
    }

    Ok((start_cid..=end_cid).map(GlyphIdent::Cid).collect())
}

fn named_range(
    //ctx: &mut ValidationCtx,
    start: &Token,
    end: &Token,
) -> Result<Vec<GlyphIdent>, String> {
    if start.text.len() != end.text.len() {
        return Err("glyph range names must be equal length".into());
    }
    let diff_range = get_diff_range(&start.text, &end.text);

    if diff_range.len() == 1 {
        let one_byte = start.text.as_bytes()[diff_range.start];
        let two_byte = end.text.as_bytes()[diff_range.start];
        if one_byte >= two_byte {
            return Err("glyph range end must be greater than start".into());
        }
        if one_byte.is_ascii_alphabetic() && two_byte.is_ascii_alphabetic()
            // range must be between two lowercase or two uppercase ascii letters
            && ((one_byte > b'Z') == (two_byte > b'Z'))
        {
            return Ok(alpha_range(&start.text, &end.text, diff_range));
        }
    }
    let one = &start.text[diff_range.clone()];
    let two = &end.text[diff_range.clone()];
    match (one.parse::<u32>(), two.parse::<u32>()) {
        (Ok(one), Ok(two)) if one < two => Ok(num_range(&start.text, one..two, diff_range)),
            _ => Err("range glyphs must differ by a single letter a-Z or A-Z, or by a run of up to three decimal digits".to_string()),
        }
}

fn alpha_range(start: &str, end: &str, range: Range<usize>) -> Vec<GlyphIdent> {
    let mut result = Vec::new();
    let mut template = start.to_string();
    let start_char = start.as_bytes()[range.start] as char;
    let end_char = end.as_bytes()[range.start] as char;
    for chr in start_char..=end_char {
        debug_assert_eq!(chr.len_utf8(), 1);
        // safety: validate glyph name is all ascii, so we only ever overwrite
        // a single byte with another single byte
        unsafe {
            chr.encode_utf8(&mut template.as_bytes_mut()[range.start..range.end]);
        }
        result.push(SmolStr::from(template.as_str()).into());
    }
    result
}

fn num_range(start: &str, sub_range: Range<u32>, text_range: Range<usize>) -> Vec<GlyphIdent> {
    let mut result = Vec::new();
    let mut temp = String::new();
    let mut template = start.to_string();

    use std::fmt::Write;
    let width = text_range.len();
    for val in sub_range {
        temp.clear();
        write!(&mut temp, "{:0width$}", val, width = width).unwrap();
        template.replace_range(text_range.clone(), &temp);
        result.push(SmolStr::from(&template).into());
    }
    result
}

fn get_diff_range(one: &str, two: &str) -> Range<usize> {
    assert_eq!(one.len(), two.len());
    let front = one
        .bytes()
        .zip(two.bytes())
        .take_while(|(a, b)| a == b)
        .count();
    let back = one
        .bytes()
        .rev()
        .zip(two.bytes().rev())
        .take_while(|(a, b)| a == b)
        .count();
    let back = one.len() - back;
    if back < front {
        0..0
    } else {
        // expand number range to all adjacent digits
        let mut front = front;
        while front > 0 && one.as_bytes()[front - 1].is_ascii_digit() {
            front -= 1;
        }
        let mut back = back;
        while back < one.len()
            && one
                .as_bytes()
                .get(back)
                .map(u8::is_ascii_digit)
                .unwrap_or(false)
        {
            back += 1;
        }

        front..back
    }
}

//fn expect_next_token

mod tables {

    #[derive(Debug, Clone)]
    #[allow(non_camel_case_types)]
    pub struct head {
        font_revision: u16,
    }

    #[derive(Debug, Clone)]
    #[allow(non_camel_case_types)]
    pub struct hhea {
        caret_offset: i32,
        ascender: i32,
        descender: i32,
        line_gap: i32,
    }

    #[derive(Debug, Clone)]
    #[allow(non_camel_case_types)]
    pub struct vhea {
        vert_typo_ascender: i32,
        vert_typo_descender: i32,
        vert_typo_line_gap: i32,
    }
}

#[cfg(test)]
mod tests {
    use crate::token_tree::TreeBuilder;

    use super::*;

    #[test]
    fn diff_range_smoke_test() {
        let one = "hi.a";
        let two = "hi.z";
        assert_eq!(&one[get_diff_range(one, two)], "a");

        let one = "hi";
        let two = "hi";
        assert_eq!(&one[get_diff_range(one, two)], "");

        let one = "A.hi";
        let two = "C.hi";
        assert_eq!(&one[get_diff_range(one, two)], "A");

        let one = "f_x_i";
        let two = "f_g_i";
        assert_eq!(&one[get_diff_range(one, two)], "x");

        let one = "a.01";
        let two = "a.42";
        assert_eq!(&one[get_diff_range(one, two)], "01");

        let one = "a.123a";
        let two = "a.153a";
        assert_eq!(&one[get_diff_range(one, two)], "123");
    }

    fn make_range_node(k1: Kind, t1: &str, k2: Kind, t2: &str) -> Node {
        let mut builder = TreeBuilder::default();
        builder.start_node(Kind::GlyphRange);
        builder.token(k1, t1);
        builder.token(Kind::Hyphen, "-");
        builder.token(k2, t2);
        builder.finish_node(false);
        builder.finish()
    }

    #[test]
    fn cid_range() {
        let range = make_range_node(Kind::Cid, "4", Kind::Cid, "12");
        let idents = glyph_range(&range).unwrap();
        let map: GlyphMap = idents.into_iter().collect();
        for val in 4u32..=12 {
            assert!(map.contains(&val));
        }
    }

    #[test]
    fn cid_range_bad() {
        let range = make_range_node(Kind::Cid, "12", Kind::Cid, "1");
        let idents = glyph_range(&range);
        assert!(idents.is_err());
    }

    #[test]
    fn mixed_range() {
        let range = make_range_node(Kind::Cid, "12", Kind::GlyphName, "hi");
        let idents = glyph_range(&range);
        assert!(idents.is_err());
    }

    #[test]
    fn this_is_really_bad() {
        let range = make_range_node(Kind::Number, "12", Kind::GlyphName, "hi");
        let idents = glyph_range(&range);
        assert!(idents.is_err());
    }

    #[test]
    fn named_range_() {
        let range = make_range_node(Kind::GlyphName, "A.hi", Kind::GlyphName, "E.hi");
        let idents = glyph_range(&range).unwrap();
        let map: GlyphMap = idents.into_iter().collect();
        assert_eq!(map.len(), 5, "{:?}", map);
        for val in ["A.hi", "B.hi", "C.hi", "D.hi", "E.hi"] {
            assert!(map.contains(val));
        }
    }

    #[test]
    fn named_range_bad() {
        let range = make_range_node(Kind::GlyphName, "A.hi", Kind::GlyphName, "Ez.hi");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "A.hi", Kind::GlyphName, "B");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "A1.hi", Kind::GlyphName, "B1.hi");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "Z.hi", Kind::GlyphName, "A.hi");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "a", Kind::GlyphName, "A");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "Z", Kind::GlyphName, "z");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "a", Kind::GlyphName, "z");
        let idents = glyph_range(&range);
        assert!(idents.is_ok());
    }
}
