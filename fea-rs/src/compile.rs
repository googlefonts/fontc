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
    types::{gpos, gsub, Anchor, GlyphClass, GlyphId, GlyphOrClass, Tag},
    Diagnostic, GlyphMap, Kind, Node, NodeOrToken,
};

mod rules;

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

pub struct CompilationCtx<'a> {
    glyph_map: &'a GlyphMap,
    pub errors: Vec<Diagnostic>,
    #[allow(dead_code)]
    tables: Tables,
    default_lang_systems: HashSet<(Tag, Tag)>,
    #[allow(dead_code)]
    lang_systems: HashSet<(Tag, Tag)>,
    seen_non_default_script: bool,
    #[allow(dead_code)]
    lookups: Vec<(usize, LookupTable)>,
    // class and position
    glyph_class_defs: HashMap<SmolStr, (GlyphClass, usize)>,
    #[allow(dead_code)]
    features: HashMap<Tag, Feature>,
    mark_classes: HashMap<SmolStr, MarkClass>,
    anchor_defs: HashMap<SmolStr, (Anchor, usize)>,
}

#[allow(dead_code)]
struct Feature {
    pos: usize,
    tag: Tag,
    statements: Vec<Statement>,
}

struct MarkClass {
    #[allow(dead_code)]
    pos: usize,
    members: Vec<(GlyphClass, typed::Anchor)>,
}

/// A thing in a feature block
#[allow(dead_code)]
enum Statement {
    Gpos(gpos::Rule),
    Gsub(gsub::Rule),
    Script(Tag),
    Language {
        tag: Tag,
        exclude_dflt: bool,
        required: bool,
    },
    LookupFlag(LookupFlag),
    LookupRef(Token),
    LookupBlock(()),
    //GlyphClassDef {
    //name: SmolStr,
    //class: GlyphClass,
    //},
    //MarkStatement(()),
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

impl<'a> CompilationCtx<'a> {
    fn new(glyph_map: &'a GlyphMap) -> Self {
        CompilationCtx {
            glyph_map,
            errors: Vec::new(),
            tables: Tables::default(),
            default_lang_systems: Default::default(),
            lang_systems: Default::default(),
            seen_non_default_script: false,
            glyph_class_defs: Default::default(),
            lookups: Vec::new(),
            features: Default::default(),
            mark_classes: Default::default(),
            anchor_defs: Default::default(),
        }
    }

    fn error(&mut self, range: Range<usize>, message: String) {
        self.errors.push(Diagnostic::error(range, message));
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
            self.resolve_glyph_class_literal(&class)
        } else if let Some(alias) = class_decl.class_alias() {
            match self.resolve_named_glyph_class(&alias) {
                Some(class) => class,
                None => return,
            }
        } else {
            panic!("write more code I guess");
        };

        self.glyph_class_defs
            .insert(name.text().clone(), (glyphs, class_decl.range().start));
    }

    fn define_mark_class(&mut self, class_decl: typed::MarkClassDef) {
        let class_items = class_decl.glyph_class();
        let class_items = match self.resolve_glyph_or_class(&class_items) {
            Some(GlyphOrClass::Class(cls)) => cls,
            Some(GlyphOrClass::Glyph(id)) => id.into(),
            None => return,
        };

        let anchor = class_decl.anchor();
        let class_name = class_decl.mark_class_name();
        if let Some(class) = self.mark_classes.get_mut(class_name.text()) {
            class.members.push((class_items, anchor));
        } else {
            let class = MarkClass {
                pos: class_decl.range().start,
                members: vec![(class_items, anchor)],
            };
            self.mark_classes.insert(class_name.text().clone(), class);
        }
    }

    fn add_feature(&mut self, feature: typed::Feature) {
        let tag = feature.tag();
        if tag.text() == "aalt" {
            self.error(
                tag.range(),
                "aalt feature currently is unimplemented".into(),
            );
            return;
        }
        let mut statements = Vec::new();
        for item in feature.statements() {
            if let Some(statement) = self.resolve_statement(item) {
                statements.push(statement);
            }
        }
    }

    fn add_lookup(&mut self, lookup: typed::LookupBlock) {
        if let Some(item) = self.resolve_lookup_block(&lookup, true) {
            self.lookups.push((lookup.range().start, item))
        }
    }

    fn resolve_lookup_block(
        &mut self,
        lookup: &typed::LookupBlock,
        top_level: bool,
    ) -> Option<LookupTable> {
        let tag = lookup.tag();
        let use_extension = lookup.use_extension().is_some();
        let mut statements = Vec::new();
        let mut kind = None;
        for item in lookup.statements() {
            if top_level && (item.kind() == Kind::ScriptKw || item.kind() == Kind::LanguageKw) {
                self.error(
                    item.range(),
                    "standalone lookup blocks cannot contain 'script' or 'language' statements"
                        .into(),
                );
                return None;
            }
            if item.kind().is_rule() {
                match kind {
                    Some(kind) if kind != item.kind() => {
                        self.error(
                            item.range(),
                            format!(
                                "multiple rule types in lookup block (saw '{}' after '{}')",
                                item.kind(),
                                kind
                            ),
                        );
                        // we continue, so that we can validate other items
                        // in this block
                    }
                    _ => kind = Some(item.kind()),
                }
            }
            if let Some(statement) = self.resolve_statement(item) {
                statements.push(statement);
            }
        }
        Some(LookupTable {
            use_extension,
            statements,
            name: tag.text.clone(),
        })
    }

    fn resolve_statement(&mut self, item: &NodeOrToken) -> Option<Statement> {
        if let Some(script) = typed::Script::cast(item) {
            let tag = script.tag();
            if tag.text() == "dflt" {
                self.error(
                    tag.range(),
                    "'dflt' is not a value value for script tag".into(),
                );
                return None;
            }
            Some(Statement::Script(tag.parse().unwrap()))
        } else if let Some(language) = typed::Language::cast(item) {
            let tag = language.tag();
            if tag.text() == "DFLT" {
                self.error(
                    tag.range(),
                    "'DFLT' is not a value value for language tag".into(),
                );
                return None;
            }
            let required = language.required().is_some();
            let exclude_dflt = language.exclude_dflt().is_some();
            if exclude_dflt {
                if let Some(conflict) = language.include_dflt() {
                    self.error(
                        conflict.range(),
                        "'include_dft' and 'exclude_dflt' are mutually exclusive".into(),
                    );
                }
            }
            Some(Statement::Language {
                tag: tag.parse().unwrap(),
                exclude_dflt,
                required,
            })
        } else if let Some(lookupflag) = typed::LookupFlag::cast(item) {
            resolve_lookupflags(self, &lookupflag).map(Statement::LookupFlag)
        } else if let Some(glyph_def) = typed::GlyphClassDef::cast(item) {
            self.define_glyph_class(glyph_def);
            None
        } else if let Some(glyph_def) = typed::MarkClassDef::cast(item) {
            self.define_mark_class(glyph_def);
            None
        } else if item.kind() == Kind::SubtableKw {
            Some(Statement::Subtable)
        } else if let Some(lookup) = typed::LookupRef::cast(item) {
            Some(Statement::LookupRef(lookup.label().to_owned()))
        //} else if let Some(_lookup) = typed::LookupBlock::cast(item) {
        ////FIXME: actually do lookup block
        //Some(Statement::LookupBlock(()))
        } else if let Some(rule) = typed::GsubStatement::cast(item) {
            rules::resolve_gsub_statement(self, rule).map(Statement::Gsub)
        } else if let Some(rule) = typed::GposStatement::cast(item) {
            rules::resolve_gpos_statement(self, rule).map(Statement::Gpos)
        } else {
            let span = match item {
                NodeOrToken::Token(t) => t.range(),
                NodeOrToken::Node(node) => {
                    let range = node.range();
                    let end = range.end.min(range.start + 16);
                    range.start..end
                }
            };
            self.error(span, format!("unhandled statement: '{}'", item.kind()));
            None
        }
    }

    fn define_named_anchor(&mut self, anchor_def: typed::AnchorDef) {
        let anchor_block = anchor_def.anchor();
        let name = anchor_def.name();
        let anchor = match self.resolve_anchor(&anchor_block) {
            Some(a @ Anchor::Coord { .. } | a @ Anchor::Contour { .. }) => a,
            Some(_) => {
                return self.error(
                    anchor_block.range(),
                    "named anchor definition can only be in format A or B".into(),
                )
            }
            None => return,
        };
        if let Some(_prev) = self
            .anchor_defs
            .insert(name.text.clone(), (anchor, anchor_def.range().start))
        {
            self.error(name.range(), "duplicate anchor definition".into());
        }
    }

    fn resolve_anchor(&mut self, item: &typed::Anchor) -> Option<Anchor> {
        if let Some((x, y)) = item.coords().map(|(x, y)| (x.parse(), y.parse())) {
            if let Some(point) = item.contourpoint() {
                match point.parse_unsigned() {
                    Some(point) => return Some(Anchor::Contour { x, y, point }),
                    None => panic!("negative contourpoint, go fix your parser"),
                }
            } else {
                return Some(Anchor::Coord { x, y });
            }
        } else if let Some(name) = item.name() {
            match self.anchor_defs.get(&name.text) {
                Some((anchor, pos)) if *pos < item.range().start => return Some(anchor.clone()),
                _ => {
                    self.error(name.range(), "anchor is not defined".into());
                    return None;
                }
            }
        } else if let Some(_) = item.null() {
            return Some(Anchor::Null);
        }
        panic!("bad anchor {:?} go check your parser", item);
    }

    fn resolve_glyph_or_class(&mut self, item: &typed::GlyphOrClass) -> Option<GlyphOrClass> {
        match item {
            typed::GlyphOrClass::Glyph(name) => {
                self.resolve_glyph_name(&name).map(GlyphOrClass::Glyph)
            }
            typed::GlyphOrClass::Cid(cid) => self.resolve_cid(&cid).map(GlyphOrClass::Glyph),
            typed::GlyphOrClass::Class(class) => {
                Some(GlyphOrClass::Class(self.resolve_glyph_class_literal(class)))
            }
            typed::GlyphOrClass::NamedClass(name) => self
                .resolve_named_glyph_class(&name)
                .map(GlyphOrClass::Class),
            typed::GlyphOrClass::Null(_) => None,
        }
    }

    fn resolve_glyph(&mut self, item: &typed::Glyph) -> Option<GlyphId> {
        match item {
            typed::Glyph::Named(name) => self.resolve_glyph_name(name),
            typed::Glyph::Cid(name) => self.resolve_cid(name),
            typed::Glyph::Null(_) => None,
        }
    }

    fn resolve_glyph_class(&mut self, item: &typed::GlyphClass) -> Option<GlyphClass> {
        match item {
            typed::GlyphClass::Named(name) => self.resolve_named_glyph_class(name),
            typed::GlyphClass::Literal(lit) => Some(self.resolve_glyph_class_literal(lit)),
        }
    }

    fn resolve_glyph_class_literal(&mut self, class: &typed::GlyphClassLiteral) -> GlyphClass {
        let mut glyphs = Vec::new();
        for item in class.items() {
            if let Some(id) =
                typed::GlyphName::cast(item).and_then(|name| self.resolve_glyph_name(&name))
            {
                glyphs.push(id);
            } else if let Some(id) = typed::Cid::cast(item).and_then(|cid| self.resolve_cid(&cid)) {
                glyphs.push(id);
            } else if let Some(range) = typed::GlyphRange::cast(item) {
                self.add_glyphs_from_range(&range, &mut glyphs);
            } else if let Some(alias) = typed::GlyphClassName::cast(item) {
                if let Some(class) = self.resolve_named_glyph_class(&alias) {
                    glyphs.extend(class.items().iter().clone());
                }
            } else {
                panic!("unexptected kind in class literal: '{}'", item.kind());
            }
        }
        glyphs.into()
    }

    fn resolve_named_glyph_class(&mut self, name: &typed::GlyphClassName) -> Option<GlyphClass> {
        match self.glyph_class_defs.get(name.text()) {
            Some((class, pos)) if *pos < name.range().start => Some(class.clone()),
            _ => {
                self.error(name.range(), "Named glyph class is not defined".into());
                None
            }
        }
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

pub fn compile<'a>(node: &Node, glyph_map: &'a GlyphMap) -> CompilationCtx<'a> {
    let mut ctx = CompilationCtx::new(glyph_map);

    for item in node.iter_children() {
        if let Some(language_system) = typed::LanguageSystem::cast(item) {
            ctx.add_language_system(language_system);
        } else if let Some(class_def) = typed::GlyphClassDef::cast(item) {
            ctx.define_glyph_class(class_def);
        } else if let Some(mark_def) = typed::MarkClassDef::cast(item) {
            ctx.define_mark_class(mark_def);
        } else if let Some(anchor_def) = typed::AnchorDef::cast(item) {
            ctx.define_named_anchor(anchor_def);
        } else if let Some(feature) = typed::Feature::cast(item) {
            ctx.add_feature(feature);
        } else if let Some(lookup) = typed::LookupBlock::cast(item) {
            ctx.add_lookup(lookup);

        //TODO: includes, eh? maybe resolved before now?
        } else if !item.kind().is_trivia() {
            let span = match item {
                NodeOrToken::Token(t) => t.range(),
                NodeOrToken::Node(node) => {
                    let range = node.range();
                    let end = range.end.min(range.start + 16);
                    range.start..end
                }
            };
            ctx.error(span, format!("unhandled top-level item: '{}'", item.kind()));
        }
        //for item in node.children() {
        //match item.kind() {
        ////Kind::IncludeNode => include(n, &mut ctx),
        ////Kind::FeatureNode => feature(n, &mut ctx),
        ////Kind::TableNode => table(n, &mut ctx),
        ////Kind::AnonBlockNode => anon(n, &mut ctx),
        ////Kind::LookupBlockNode => lookup_block_top_level(n, &mut ctx),
        ////Kind::ValueRecordNode => value_record_def(n, &mut ctx),
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

#[derive(Clone, Debug, Default)]
struct LookupFlag {
    raw: u16,
    mark_attachment: Option<GlyphClass>,
    mark_filter: Option<GlyphClass>,
}

fn resolve_lookupflags(ctx: &mut CompilationCtx, node: &typed::LookupFlag) -> Option<LookupFlag> {
    if let Some(number) = node.number() {
        if let Ok(mask) = number.text().parse::<u16>() {
            return Some(LookupFlag {
                raw: mask,
                ..Default::default()
            });
        } else {
            ctx.error(number.range(), "value out of range".into());
            return None;
        }
    }

    //FIXME: this is a placeholder, we'll use the fonttools types later
    let mut rtl = false;
    let mut ignore_base = false;
    let mut ignore_lig = false;
    let mut ignore_marks = false;
    let mut mark_set = None;
    let mut filter_set = None;

    let mut iter = node.iter();
    while let Some(next) = iter.next() {
        match next.kind() {
            Kind::RightToLeftKw if !rtl => rtl = true,
            Kind::IgnoreBaseGlyphsKw if !ignore_base => ignore_base = true,
            Kind::IgnoreLigaturesKw if !ignore_lig => ignore_lig = true,
            Kind::IgnoreMarksKw if !ignore_marks => ignore_marks = true,

            //FIXME: we are not enforcing some requirements here. in particular,
            // The glyph sets of the referenced classes must not overlap, and the MarkAttachmentType statement can reference at most 15 different classes.
            // ALSO: this should accept mark classes.
            Kind::MarkAttachmentTypeKw if mark_set.is_none() => {
                match iter
                    .find(|t| t.kind() == Kind::NamedGlyphClass || t.kind() == Kind::GlyphClass)
                    .and_then(typed::GlyphOrClass::cast)
                {
                    Some(node) => {
                        mark_set = ctx.resolve_glyph_or_class(&node);
                    }
                    None => {
                        ctx.error(
                            next.range(),
                            "MarkAttachmentType should be followed by glyph class".into(),
                        );
                        return None;
                    }
                }
            }
            Kind::UseMarkFilteringSetKw if filter_set.is_none() => {
                match iter
                    .find(|t| t.kind() == Kind::NamedGlyphClass || t.kind() == Kind::GlyphClass)
                    .and_then(typed::GlyphOrClass::cast)
                {
                    Some(node) => {
                        filter_set = ctx.resolve_glyph_or_class(&node);
                    }
                    None => {
                        ctx.error(
                            next.range(),
                            "UseMarkFilteringSet should be followed by glyph class".into(),
                        );
                        return None;
                    }
                }
            }
            Kind::RightToLeftKw
            | Kind::IgnoreBaseGlyphsKw
            | Kind::IgnoreMarksKw
            | Kind::IgnoreLigaturesKw
            | Kind::MarkAttachmentTypeKw
            | Kind::UseMarkFilteringSetKw => {
                ctx.error(next.range(), "duplicate value in lookupflag".into())
            }
            _ => (),
        }
    }

    let mut raw = 0u16;
    if rtl {
        raw &= 1
    };
    if ignore_base {
        raw &= 2
    };
    if ignore_lig {
        raw &= 4
    };
    if ignore_marks {
        raw &= 8
    };

    Some(LookupFlag {
        raw,
        mark_attachment: mark_set.map(Into::into),
        mark_filter: filter_set.map(Into::into),
    })
}

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
pub(crate) fn cid_range(
    start: &Token,
    end: &Token,
    mut callback: impl FnMut(u32),
) -> Result<(), String> {
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
pub(crate) fn named_range(
    start: &Token,
    end: &Token,
    callback: impl FnMut(&str),
) -> Result<(), String> {
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
        builder.finish_node(false, None);
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
