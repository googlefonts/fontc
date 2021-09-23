use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

use smol_str::SmolStr;

use crate::{
    token_tree::{
        typed::{self, AstNode},
        Token,
    },
    types::{GlyphClass, GlyphId, GposRule, GsubRule, Tag},
    GlyphMap, Kind, Node, SyntaxError,
};

#[cfg(test)]
use crate::types::GlyphIdent;

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
    pub errors: Vec<SyntaxError>,
    tables: Tables,
    default_lang_systems: HashSet<(Tag, Tag)>,
    lang_systems: HashSet<(Tag, Tag)>,
    seen_non_default_script: bool,
    lookups: Vec<(usize, LookupTable)>,
    // class and position
    glyph_class_defs: HashMap<SmolStr, (GlyphClass, usize)>,
    features: HashMap<Tag, Feature>,
}

#[allow(dead_code)]
struct Feature {
    pos: usize,
    tag: Tag,
    statements: Vec<Statement>,
}

/// A thing in a feature block
#[allow(dead_code)]
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

#[allow(dead_code)]
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

/// given a ctx and N Result types, report any errors.
///
/// If errors are found, return after reporting; otherwise convert Result<T> to T.
#[macro_export]
macro_rules! report_errs {
    ($val:expr) => {
        match $val {
            Ok(v) => v,
            Err(_) => return,
        }
    };
    ($ctx:expr, $val:expr $(,)?) => {
        match &$val {
            Ok(_) => (),
            Err((range, err)) => $ctx.error(range.clone(), err.to_string()),
        }
    };
    ($ctx:expr, $($val:expr),+ $(,)?) => {
        {
            // first report any errors, without discarding them
            $($crate::report_errs!($ctx, $val);)+
            // then convert to T, returning if there was an error anywhere.
            ($($crate::report_errs!($val)),+,)
        }
    };
}

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

    fn add_language_system(&mut self, language_system: typed::LanguageSystem) {
        let script = language_system.script();
        let language = language_system.language();
        let script_tag = script.parse().map_err(|err| (script.range(), err));
        let lang_tag = language.parse().map_err(|err| (language.range(), err));
        let (script_tag, lang_tag) = report_errs!(self, script_tag, lang_tag);

        if script_tag == Tag::DFLT_SCRIPT
            && lang_tag == Tag::DFLT_LANG
            && !self.default_lang_systems.is_empty()
        {
            self.error(
                language_system.range(),
                "DFLT dflt must be first languagesystem statement.".into(),
            );
            return;
        }
        if script_tag == Tag::DFLT_SCRIPT {
            if self.seen_non_default_script {
                self.error(
                    script.range(),
                    "languagesystem with 'DFLT' script tag must precede non-'DFLT' languagesystems.".into(),
                );
                return;
            } else {
                self.seen_non_default_script = true;
            }
        }

        if !self.default_lang_systems.insert((script_tag, lang_tag)) {
            self.error(
                language_system.range(),
                "Duplicate languagesystem definition".into(),
            );
        }
    }

    fn define_glyph_class(&mut self, class_decl: typed::GlyphClassDef) {
        let name = class_decl.class_name();
        if self.glyph_class_defs.contains_key(name.text()) {
            self.error(
                name.range(),
                "duplicate definition for named glyph class".into(),
            );
            return;
        }
        let glyphs = if let Some(class) = class_decl.class_def() {
            self.resolve_glyph_class(&class)
        } else if let Some(alias) = class_decl.class_alias() {
            match self.glyph_class_defs.get(alias.text()) {
                Some((class, pos)) if *pos < class_decl.range().start => class.clone(),
                _ => return self.error(alias.range(), "Named glyph class is not defined".into()),
            }
        } else {
            panic!("write more code I guess");
        };

        self.glyph_class_defs
            .insert(name.text().clone(), (glyphs, class_decl.range().start));
    }

    fn resolve_glyph_class(&mut self, class: &typed::GlyphClass) -> GlyphClass {
        let mut glyphs = Vec::new();
        for item in class.iter() {
            if let Some(id) =
                typed::GlyphName::cast(item).and_then(|name| self.resolve_glyph_name(&name))
            {
                glyphs.push(id);
            } else if let Some(id) = typed::Cid::cast(item).and_then(|cid| self.resolve_cid(&cid)) {
                glyphs.push(id);
            } else if let Some(range) = typed::GlyphRange::cast(item) {
                self.add_glyphs_from_range(&range, &mut glyphs);
            }
        }
        glyphs.into()
    }

    fn resolve_glyph_name(&mut self, name: &typed::GlyphName) -> Option<GlyphId> {
        let id = self.glyph_map.get(name.text());
        if id.is_none() {
            self.error(name.range(), "glyph not in font".into());
        }
        id
    }

    fn resolve_cid(&mut self, cid: &typed::Cid) -> Option<GlyphId> {
        let id = self.glyph_map.get(&cid.parse());
        if id.is_none() {
            self.error(cid.range(), "CID not in font".into());
        }
        id
    }

    fn add_glyphs_from_range(&mut self, range: &typed::GlyphRange, out: &mut Vec<GlyphId>) {
        let start = range.start();
        let end = range.end();

        match (start.kind, end.kind) {
            (Kind::Cid, Kind::Cid) => {
                if let Err(err) = cid_range(start, end, |cid| {
                    match self.glyph_map.get(&cid) {
                        Some(id) => out.push(id),
                        None => {
                            // this is techincally allowed, but we error for now
                            self.error(
                                range.range(),
                                format!("Range member '{}' does not exist in font", cid),
                            );
                        }
                    }
                }) {
                    self.error(range.range(), err);
                }
            }
            (Kind::GlyphName, Kind::GlyphName) => {
                if let Err(err) = named_range(start, end, |name| {
                    match self.glyph_map.get(name) {
                        Some(id) => out.push(id),
                        None => {
                            // this is techincally allowed, but we error for now
                            self.error(
                                range.range(),
                                format!("Range member '{}' does not exist in font", name),
                            );
                        }
                    }
                }) {
                    self.error(range.range(), err);
                }
            }
            (_, _) => self.error(range.range(), "Invalid types in glyph range".into()),
        }
    }
}

pub fn validate<'a>(node: &Node, glyph_map: &'a GlyphMap) -> ValidationCtx<'a> {
    let mut ctx = ValidationCtx::new(glyph_map);

    for item in node.iter_children() {
        if let Some(language_system) = typed::LanguageSystem::cast(item) {
            ctx.add_language_system(language_system);
        } else if let Some(class_def) = typed::GlyphClassDef::cast(item) {
            ctx.define_glyph_class(class_def);
        }
        //for item in node.children() {
        //match item.kind() {
        ////Kind::LanguageSystemNode => language_system(&mut ctx, item.as_node().unwrap()),
        ////Kind::GlyphClassDefNode => glyph_class_def(&mut ctx, item.as_node().unwrap(), pos),
        ////Kind::MarkClassNode => mark_class_def(&mut ctx, item.as_node().unwrap(), pos),
        ////Kind::AnchorDefNode => anchor_def(n, &mut ctx),
        ////Kind::ValueRecordNode => value_record_def(n, &mut ctx),
        ////Kind::IncludeNode => include(n, &mut ctx),
        ////Kind::FeatureNode => feature(n, &mut ctx),
        ////Kind::TableNode => table(n, &mut ctx),
        ////Kind::AnonBlockNode => anon(n, &mut ctx),
        ////Kind::LookupBlockNode => lookup_block_top_level(n, &mut ctx),
        //Kind::Comment | Kind::Whitespace | Kind::Semi => (),
        //// maybe don't return an error? parsing should ensure we don't get here
        //other => panic!("I should maybe return an error?"),
        //}
        //cursor.step_over();
    }

    ctx
}

//fn mark_class_def(ctx: &mut ValidationCtx, node: &Node, pos: usize) {
//let mut cursor = node.cursor();
//let _kw = cursor.next(TO_SKIP);
//debug_assert_eq!(_kw.map(NodeOrToken::kind), Some(Kind::MarkClassKw));
//let glyph_or_glyph_class = cursor.next(TO_SKIP).unwrap();

//let resolved_class = match class_list_or_name {
//NodeOrToken::Token(t) if t.kind == Kind::NamedGlyphClass => {
//match ctx.glyph_class_defs.get(t.as_str()).cloned() {
//Some((class, _pos)) => class,
//None => {
//let range = class_name.range();
//ctx.error(
//range.start + pos..range.end + pos,
//"Glyph class already defined".into(),
//);
//return;
//}
//}
//}
//NodeOrToken::Node(node) if node.kind == Kind::GlyphClass => {
////TODO: resolve this class
//glyph_class_list(ctx, node, pos + node.rel_pos())
//}
//_other => unreachable!("glyph class def already validated"),
//};
//}

//fn glyph_class_maybe_named_or_singleton(ctx: &mut ValidationCtx, item: &NodeOrToken, pos: usize) -> Option<GlyphClass> {
//match item {
//NodeOrToken::Token(t) if t.kind == Kind::NamedGlyphClass => {
//match ctx.glyph_class_defs.get(t.as_str()).cloned() {
//Some((class, _pos)) => Some(class),
//None => {
//let range = t.range();
//ctx.error(
//range.start + pos..range.end + pos,
//"Glyph class not defined".into(),
//);
//None
//}
//}
//}
//NodeOrToken::Token(t) if t.kind == Kind::GlyphName => {
//match ctx.glyph_map.get(&t.text) {
//Some(id) => Some(GlyphClass::from(&[id])),
//None => (),
//}
//}
//NodeOrToken::Node(node) if node.kind == Kind::GlyphClass => {
//Some(glyph_class_list(ctx, node, pos + node.rel_pos()))
//}
//_other => unreachable!("glyph class def already validated"),
//}

//}

/// A helper for testing, that just returns the names/cids that should be part
/// of a given range. (This does not test if they're in the font.)
#[cfg(test)]
fn glyph_range(node: &Node) -> Result<Vec<GlyphIdent>, String> {
    let range = typed::GlyphRange::cast(&node.clone().into()).unwrap();
    let start = range.start();
    let end = range.end();
    let mut result = Vec::new();

    match (start.kind, end.kind) {
        (Kind::Cid, Kind::Cid) => cid_range(start, end, |cid| result.push(GlyphIdent::Cid(cid)))?,
        (Kind::GlyphName, Kind::GlyphName) => named_range(start, end, |string| {
            result.push(GlyphIdent::Name(string.into()))
        })?,
        (_, _) => return Err("Invalid glyph range".to_string()),
    }

    Ok(result)
}

//NOTE: in order to save allocation for each item in the range, we adopt
//the pattern of having the caller pass in a callback that is called with
//each member in the range. The caller is then responsible for doing things like
//ensuring that the item is in the glyph map.

/// iter glyph ids in a cid range.
///
/// Returns an error if the range is not well-formed. If it is well-formed,
/// the `callback` is called with each cid in the range.
fn cid_range(start: &Token, end: &Token, mut callback: impl FnMut(u32)) -> Result<(), String> {
    let start_cid = start.text.parse::<u32>().unwrap();
    let end_cid = end.text.parse::<u32>().unwrap();
    if start_cid >= end_cid {
        return Err("Range end must be greater than start".into());
    }

    for i in start_cid..=end_cid {
        callback(i);
    }
    Ok(())
}

/// iter glyph ids in a named range.
///
/// Returns an error if the range is not well-formed. If it is well-formed,
/// the `callback` is called with each name in the range.
fn named_range(start: &Token, end: &Token, callback: impl FnMut(&str)) -> Result<(), String> {
    if start.text.len() != end.text.len() {
        return Err("glyph range components must have equal length".into());
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
            alpha_range(&start.text, &end.text, diff_range, callback);
            return Ok(());
        }
    }
    let one = &start.text[diff_range.clone()];
    let two = &end.text[diff_range.clone()];
    match (one.parse::<u32>(), two.parse::<u32>()) {
        (Ok(one), Ok(two)) if one < two => num_range(&start.text, one..two, diff_range, callback),
            _ => return Err("range glyphs must differ by a single letter a-Z or A-Z, or by a run of up to three decimal digits".into()),
        };
    Ok(())
}

fn alpha_range(start: &str, end: &str, sub_range: Range<usize>, mut out: impl FnMut(&str)) {
    let mut template = start.to_string();
    let start_char = start.as_bytes()[sub_range.start] as char;
    let end_char = end.as_bytes()[sub_range.start] as char;
    for chr in start_char..=end_char {
        debug_assert_eq!(chr.len_utf8(), 1);
        // safety: validate glyph name is all ascii, so we only ever overwrite
        // a single byte with another single byte
        unsafe {
            chr.encode_utf8(&mut template.as_bytes_mut()[sub_range.start..sub_range.end]);
        }
        out(&template);
    }
}

fn num_range(
    start: &str,
    sub_range: Range<u32>,
    text_range: Range<usize>,
    mut out: impl FnMut(&str),
) {
    let mut temp = String::new();
    let mut template = start.to_string();

    use std::fmt::Write;
    let width = text_range.len();
    for val in sub_range {
        temp.clear();
        write!(&mut temp, "{:0width$}", val, width = width).unwrap();
        template.replace_range(text_range.clone(), &temp);
        out(&template);
    }
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
