//! the validation pass
//!
//! Before we start compilation, we do a validation pass. This checks for things
//! like the existence of named glyphs, that referenced classes are defined,
//! and that other constraints of the spec are upheld.

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
    Diagnostic, GlyphMap, Kind, Node,
};

pub struct ValidationCtx<'a> {
    pub errors: Vec<Diagnostic>,
    glyph_map: &'a GlyphMap,
    default_lang_systems: HashSet<(SmolStr, SmolStr)>,
    seen_non_default_script: bool,
    lookup_defs: HashMap<SmolStr, Token>,
    // class and position
    glyph_class_defs: HashMap<SmolStr, Token>,
    mark_class_defs: HashSet<SmolStr>,
    mark_class_used: Option<Token>,
    anchor_defs: HashMap<SmolStr, Token>,
    value_record_defs: HashMap<SmolStr, Token>,
}

impl<'a> ValidationCtx<'a> {
    fn new(glyph_map: &'a GlyphMap) -> Self {
        ValidationCtx {
            glyph_map,
            errors: Vec::new(),
            default_lang_systems: Default::default(),
            seen_non_default_script: false,
            glyph_class_defs: Default::default(),
            lookup_defs: Default::default(),
            mark_class_defs: Default::default(),
            mark_class_used: None,
            anchor_defs: Default::default(),
            value_record_defs: Default::default(),
        }
    }

    fn error(&mut self, range: Range<usize>, message: impl Into<String>) {
        self.errors.push(Diagnostic::error(range, message));
    }

    fn warning(&mut self, range: Range<usize>, message: impl Into<String>) {
        self.errors.push(Diagnostic::warning(range, message));
    }

    fn validate_root(&mut self, node: &typed::Root) {
        for item in node.statements() {
            if let Some(language_system) = typed::LanguageSystem::cast(item) {
                self.validate_language_system(&language_system)
            } else if let Some(class_def) = typed::GlyphClassDef::cast(item) {
                self.validate_glyph_class_def(&class_def);
            } else if let Some(mark_def) = typed::MarkClassDef::cast(item) {
                self.validate_mark_class_def(&mark_def);
            } else if let Some(anchor_def) = typed::AnchorDef::cast(item) {
                self.validate_anchor_def(&anchor_def);
            } else if let Some(_include) = typed::Include::cast(item) {
                //TODO: includes, eh? maybe resolved before now?
            } else if let Some(feature) = typed::Feature::cast(item) {
                self.validate_feature(&feature);
            } else if let Some(table) = typed::Table::cast(item) {
                self.validate_table(&table);
            } else if let Some(lookup) = typed::LookupBlock::cast(item) {
                self.validate_lookup_block(&lookup, true);
            } else if let Some(_value_record_def) = typed::ValueRecordDef::cast(item) {
                unimplemented!("valueRecordDef")
            } else if item.kind() == Kind::AnonKw {
                unimplemented!("anon")
            }
        }
    }

    fn validate_language_system(&mut self, node: &typed::LanguageSystem) {
        let script = node.script();
        let lang = node.language();

        if script.text() == "DFLT" && lang.text() == "dflt" && !self.default_lang_systems.is_empty()
        {
            self.error(
                node.range(),
                "'DFLT dftl' must be first languagesystem statement",
            );
            return;
        }
        if script.text() == "DFLT" {
            if self.seen_non_default_script {
                self.error(
                    script.range(),
                    "languagesystem with 'DFLT' script tag must precede non-'DFLT' languagesystems",
                );
                return;
            } else {
                self.seen_non_default_script = true;
            }
        }

        if !self
            .default_lang_systems
            .insert((script.text().clone(), lang.text().clone()))
        {
            self.warning(node.range(), "Duplicate languagesystem definition");
        }
    }

    fn validate_glyph_class_def(&mut self, node: &typed::GlyphClassDef) {
        let name = node.class_name();
        if let Some(_prev) = self
            .glyph_class_defs
            .insert(name.text().to_owned(), name.token().clone())
        {
            self.warning(name.range(), "duplicate glyph class definition");
            //TODO: use previous span to show previous declaration
            //TODO: have help message
        }
        if let Some(literal) = node.class_def() {
            self.validate_glyph_class_literal(&literal, false);
        } else if let Some(alias) = node.class_alias() {
            self.validate_glyph_class_ref(&alias, false);
        } else {
            unreachable!("there's a parser bug?");
        }
    }

    fn validate_anchor_def(&mut self, node: &typed::AnchorDef) {
        if let Some(_prev) = self
            .anchor_defs
            .insert(node.name().text.clone(), node.name().clone())
        {
            self.warning(node.name().range(), "duplicate anchor name");
        }
    }

    fn validate_mark_class_def(&mut self, node: &typed::MarkClassDef) {
        if let Some(_use_site) = self.mark_class_used.as_ref() {
            self.error(
                node.keyword().range(),
                "all markClass definitions must precede any use of a mark class in the file",
            );
            //TODO: annotate error with site of use
            //TODO: figure out this:
            //
            // "Note: The mark classes used within a single lookup must be
            // disjoint: none may include a glyph which is in another mark class
            // that is used within the same lookup."
        }
        self.mark_class_defs
            .insert(node.mark_class_name().text().clone());
        self.validate_anchor(&node.anchor());
    }

    fn validate_table(&mut self, _node: &typed::Table) {}

    // simple: 'include', 'script', 'language', 'subtable', 'lookup', 'lookupflag',
    // rules: 'enumerate', 'enum', 'ignore', 'substitute', 'sub', 'reversesub', 'rsub', 'position', 'pos',
    // decls: 'markClass', GCLASS}
    // special: 'feature', 'parameters', 'featureNames', 'cvParameters', 'sizemenuname'
    fn validate_feature(&mut self, node: &typed::Feature) {
        let tag = node.tag();
        let _is_aalt = tag.text() == "aalt";
        // - must occur before anything it references

        let _is_size = tag.text() == "size";
        let _is_ss = tag.text().starts_with("ss")
            && tag.text()[2..]
                .parse::<u8>()
                .map(|val| val > 1 && val <= 20)
                .unwrap_or(false);

        let _is_cv = tag.text().starts_with("cv")
            && tag.text()[2..]
                .parse::<u8>()
                .map(|val| val > 1 && val <= 99)
                .unwrap_or(false);

        for item in node.statements() {
            if item.kind() == Kind::ScriptNode
                || item.kind() == Kind::LanguageNode
                || item.kind() == Kind::SubtableKw
            {
                // lgtm
            } else if let Some(node) = typed::LookupRef::cast(item) {
                if !self.lookup_defs.contains_key(&node.label().text) {
                    self.error(node.label().range(), "lookup is not defined");
                }
            } else if let Some(node) = typed::LookupBlock::cast(item) {
                self.validate_lookup_block(&node, false);
            } else if let Some(node) = typed::LookupFlag::cast(item) {
                self.validate_lookupflag(&node);
            } else if let Some(node) = typed::GsubStatement::cast(item) {
                self.validate_gsub_statement(&node);
            } else if let Some(node) = typed::GposStatement::cast(item) {
                self.validate_gpos_statement(&node);
            } else if let Some(node) = typed::GlyphClassDef::cast(item) {
                self.validate_glyph_class_def(&node);
            } else if let Some(node) = typed::MarkClassDef::cast(item) {
                self.validate_mark_class_def(&node);
            } else {
                self.warning(
                    node.range(),
                    format!("unhandled item '{}' in feature", node.node().kind),
                );
            }
        }
    }

    fn validate_lookup_block(&mut self, node: &typed::LookupBlock, top_level: bool) {
        let name = node.label();
        if let Some(_prev) = self.lookup_defs.insert(name.text.clone(), name.clone()) {
            //TODO: annotate with previous location
            self.warning(name.range(), "layout label already defined");
        }
        for item in node.statements() {
            if item.kind() == Kind::ScriptNode {
                if top_level {
                    self.error(
                        item.range(),
                        "script statements not allowed in standalone lookup blocks",
                    );
                }
            } else if item.kind() == Kind::LanguageNode {
                if top_level {
                    self.error(
                        item.range(),
                        "script statements not allowed in standalone lookup blocks",
                    );
                }
            } else if item.kind() == Kind::SubtableKw {
                // lgtm
            } else if let Some(node) = typed::LookupRef::cast(item) {
                if !self.lookup_defs.contains_key(&node.label().text) {
                    self.error(node.label().range(), "lookup is not defined");
                }
            } else if let Some(node) = typed::LookupBlock::cast(item) {
                self.error(
                    node.keyword().range(),
                    "lookup blocks cannot contain other blocks",
                );
                self.validate_lookup_block(&node, false);
            } else if let Some(node) = typed::LookupFlag::cast(item) {
                self.validate_lookupflag(&node);
            } else if let Some(node) = typed::GsubStatement::cast(item) {
                self.validate_gsub_statement(&node);
            } else if let Some(node) = typed::GposStatement::cast(item) {
                self.validate_gpos_statement(&node);
            } else if let Some(node) = typed::GlyphClassDef::cast(item) {
                self.validate_glyph_class_def(&node);
            } else if let Some(node) = typed::MarkClassDef::cast(item) {
                self.validate_mark_class_def(&node);
            } else {
                self.warning(
                    item.range(),
                    format!("unhandled item {} in feature", item.kind()),
                );
            }
        }
    }

    fn validate_gpos_statement(&mut self, node: &typed::GposStatement) {
        match node {
            typed::GposStatement::Type1(rule) => {
                self.validate_glyph_or_class(&rule.target());
                self.validate_value_record(&rule.value());
            }
            typed::GposStatement::Type2(rule) => {
                self.validate_glyph_or_class(&rule.first_item());
                self.validate_glyph_or_class(&rule.second_item());
                self.validate_value_record(&rule.first_value());
                if let Some(second) = rule.second_value() {
                    self.validate_value_record(&second);
                }
            }
            _ => self.fallback_validate_rule(node.node()),
        }
    }

    fn validate_gsub_statement(&mut self, node: &typed::GsubStatement) {
        match node {
            typed::GsubStatement::Type1(rule) => {
                self.validate_glyph_or_class(&rule.target());
                self.validate_glyph_or_class(&rule.replacement());
            }
            typed::GsubStatement::Type2(rule) => {
                self.validate_glyph(&rule.target());
                for item in rule.replacement() {
                    self.validate_glyph(&item);
                }
            }
            typed::GsubStatement::Type3(rule) => {
                self.validate_glyph(&rule.target());
                self.validate_glyph_class(&rule.alternates(), false);
            }
            typed::GsubStatement::Type4(rule) => {
                for item in rule.target() {
                    self.validate_glyph_or_class(&item);
                }
                self.validate_glyph(&rule.replacement());
            }
            _ => self.fallback_validate_rule(node.node()),
        }
    }

    /// we don't currently handle all rules, but we at least check glyph names etc
    fn fallback_validate_rule(&mut self, node: &Node) {
        let range = node
            .iter_tokens()
            .filter(|t| !t.kind.is_trivia())
            .find(|t| t.text.len() > 2)
            .map(|t| t.range())
            .unwrap_or_else(|| node.range());
        self.warning(range, format!("unimplemented rule type {}", node.kind));
        for item in node.iter_children() {
            if let Some(node) = typed::GlyphOrClass::cast(item) {
                self.validate_glyph_or_class(&node);
            } else if let Some(anchor) = typed::Anchor::cast(item) {
                self.validate_anchor(&anchor);
            }
        }
    }

    fn validate_lookupflag(&mut self, node: &typed::LookupFlag) {
        if let Some(number) = node.number() {
            if number.text().parse::<u16>().is_err() {
                self.error(number.range(), "value must be a positive 16 bit integer");
            }
            return;
        }

        let mut rtl = false;
        let mut ignore_base = false;
        let mut ignore_lig = false;
        let mut ignore_marks = false;
        let mut mark_set = false;
        let mut filter_set = false;

        let mut iter = node.iter();
        while let Some(next) = iter.next() {
            match next.kind() {
                Kind::RightToLeftKw if !rtl => rtl = true,
                Kind::IgnoreBaseGlyphsKw if !ignore_base => ignore_base = true,
                Kind::IgnoreLigaturesKw if !ignore_lig => ignore_lig = true,
                Kind::IgnoreMarksKw if !ignore_marks => ignore_marks = true,

                //FIXME: we are not enforcing some requirements here. in particular,
                // The glyph sets of the referenced classes must not overlap, and the MarkAttachmentType statement can reference at most 15 different classes.
                Kind::MarkAttachmentTypeKw if !mark_set => {
                    mark_set = true;
                    match iter.find_map(typed::GlyphClass::cast) {
                        Some(node) => self.validate_glyph_class(&node, true),
                        None => self.error(
                            next.range(),
                            "MarkAttachmentType should be followed by glyph class",
                        ),
                    }
                }
                Kind::UseMarkFilteringSetKw if !filter_set => {
                    filter_set = true;
                    match iter.find_map(typed::GlyphClass::cast) {
                        Some(node) => self.validate_glyph_class(&node, true),
                        None => self.error(
                            next.range(),
                            "MarkAttachmentType should be followed by glyph class",
                        ),
                    }
                }
                Kind::RightToLeftKw
                | Kind::IgnoreBaseGlyphsKw
                | Kind::IgnoreMarksKw
                | Kind::IgnoreLigaturesKw
                | Kind::MarkAttachmentTypeKw
                | Kind::UseMarkFilteringSetKw => {
                    self.error(next.range(), "duplicate value in lookupflag")
                }

                _ => (),
            }
        }
    }

    fn validate_glyph_or_class(&mut self, node: &typed::GlyphOrClass) {
        match node {
            typed::GlyphOrClass::Glyph(name) => self.validate_glyph_name(&name),
            typed::GlyphOrClass::Cid(cid) => self.validate_cid(&cid),
            typed::GlyphOrClass::Class(class) => self.validate_glyph_class_literal(class, false),
            typed::GlyphOrClass::NamedClass(name) => self.validate_glyph_class_ref(name, false),
        }
    }

    fn validate_glyph(&mut self, node: &typed::Glyph) {
        match node {
            typed::Glyph::Named(name) => self.validate_glyph_name(&name),
            typed::Glyph::Cid(cid) => self.validate_cid(&cid),
        }
    }

    fn validate_glyph_class(&mut self, node: &typed::GlyphClass, accept_mark_class: bool) {
        match node {
            typed::GlyphClass::Literal(lit) => {
                self.validate_glyph_class_literal(&lit, accept_mark_class)
            }
            typed::GlyphClass::Named(name) => {
                self.validate_glyph_class_ref(&name, accept_mark_class)
            }
        }
    }

    fn validate_glyph_class_literal(
        &mut self,
        node: &typed::GlyphClassLiteral,
        accept_mark_class: bool,
    ) {
        for item in node.items() {
            if let Some(id) = typed::GlyphName::cast(item) {
                self.validate_glyph_name(&id);
            } else if let Some(id) = typed::Cid::cast(item) {
                self.validate_cid(&id);
            } else if let Some(range) = typed::GlyphRange::cast(item) {
                self.validate_glyph_range(&range);
            } else if let Some(alias) = typed::GlyphClassName::cast(item) {
                self.validate_glyph_class_ref(&alias, accept_mark_class);
                // these two cases indicate existing errors
            } else if !item.kind().is_trivia()
                && item.kind() != Kind::Ident
                && item.kind() != Kind::GlyphNameOrRange
            {
                self.warning(item.range(), format!("unexpected item {}", item.kind()));
            }
        }
    }

    fn validate_glyph_name(&mut self, name: &typed::GlyphName) {
        if self.glyph_map.get(name.text()).is_none() {
            self.error(name.range(), "glyph not in font");
        }
    }

    fn validate_cid(&mut self, cid: &typed::Cid) {
        if self.glyph_map.get(&cid.parse()).is_none() {
            self.error(cid.range(), "CID not in font");
        }
    }

    fn validate_glyph_class_ref(&mut self, node: &typed::GlyphClassName, accept_mark_class: bool) {
        if accept_mark_class && self.mark_class_defs.contains(node.text()) {
            return;
        }
        if !self.glyph_class_defs.contains_key(node.text()) {
            self.error(node.range(), "undefined glyph class");
        }
    }

    fn validate_glyph_range(&mut self, range: &typed::GlyphRange) {
        let start = range.start();
        let end = range.end();

        match (start.kind, end.kind) {
            (Kind::Cid, Kind::Cid) => {
                if let Err(err) = crate::compile::cid_range(start, end, |cid| {
                    if self.glyph_map.get(&cid).is_none() {
                        // this is techincally allowed, but we error for now
                        self.warning(
                            range.range(),
                            format!("Range member '{}' does not exist in font", cid),
                        );
                    }
                }) {
                    self.error(range.range(), err);
                }
            }
            (Kind::GlyphName, Kind::GlyphName) => {
                if let Err(err) = crate::compile::named_range(start, end, |name| {
                    if self.glyph_map.get(name).is_none() {
                        self.warning(
                            range.range(),
                            format!("Range member '{}' does not exist in font", name),
                        );
                    }
                }) {
                    self.error(range.range(), err);
                }
            }
            (_, _) => self.error(range.range(), "Invalid types in glyph range"),
        }
    }

    fn validate_value_record(&mut self, node: &typed::ValueRecord) {
        if let Some(name) = node.named() {
            if !self.value_record_defs.contains_key(&name.text) {
                self.error(name.range(), "undefined value record name");
            }
        }
    }

    fn validate_anchor(&mut self, anchor: &typed::Anchor) {
        if let Some(name) = anchor.name() {
            if !self.anchor_defs.contains_key(&name.text) {
                self.error(name.range(), "undefined anchor name");
            }
        }
    }
}

pub fn validate<'a>(node: &Node, glyph_map: &'a GlyphMap) -> Vec<Diagnostic> {
    let mut ctx = ValidationCtx::new(glyph_map);
    if let Some(node) = typed::Root::try_from_node(node) {
        ctx.validate_root(&node);
    } else {
        panic!("validate called with invalid root node '{}'", node.kind());
    }
    ctx.errors
}
