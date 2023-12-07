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
use write_fonts::{read::tables::name::Encoding, types::Tag};

use super::{
    glyph_range,
    tags::{self, WIN_PLATFORM_ID},
    VariationInfo,
};
use crate::{
    parse::SourceMap,
    token_tree::{
        typed::{self, AstNode},
        Token,
    },
    typed::ContextualRuleNode,
    Diagnostic, GlyphMap, Kind, NodeOrToken,
};

pub struct ValidationCtx<'a, V: VariationInfo> {
    pub errors: Vec<Diagnostic>,
    glyph_map: &'a GlyphMap,
    source_map: &'a SourceMap,
    variation_info: Option<&'a V>,
    default_lang_systems: HashSet<(SmolStr, SmolStr)>,
    seen_non_default_script: bool,
    lookup_defs: HashMap<SmolStr, Token>,
    // class and position
    glyph_class_defs: HashMap<SmolStr, Token>,
    mark_class_defs: HashSet<SmolStr>,
    mark_class_used: Option<Token>,
    anchor_defs: HashMap<SmolStr, Token>,
    value_record_defs: HashMap<SmolStr, Token>,
    condition_set_defs: HashMap<SmolStr, Token>,
    aalt_referenced_features: HashMap<Tag, typed::Tag>,
    all_features: HashSet<Tag>,
}

impl<'a, V: VariationInfo> ValidationCtx<'a, V> {
    pub(crate) fn new(
        source_map: &'a SourceMap,
        glyph_map: &'a GlyphMap,
        variation_info: Option<&'a V>,
    ) -> Self {
        ValidationCtx {
            glyph_map,
            source_map,
            errors: Vec::new(),
            variation_info,
            default_lang_systems: Default::default(),
            seen_non_default_script: false,
            glyph_class_defs: Default::default(),
            lookup_defs: Default::default(),
            mark_class_defs: Default::default(),
            mark_class_used: None,
            anchor_defs: Default::default(),
            value_record_defs: Default::default(),
            condition_set_defs: Default::default(),
            aalt_referenced_features: Default::default(),
            all_features: Default::default(),
        }
    }

    fn error(&mut self, range: Range<usize>, message: impl Into<String>) {
        let (file, range) = self.source_map.resolve_range(range);
        self.errors.push(Diagnostic::error(file, range, message));
    }

    fn warning(&mut self, range: Range<usize>, message: impl Into<String>) {
        let (file, range) = self.source_map.resolve_range(range);
        self.errors.push(Diagnostic::warning(file, range, message));
    }

    pub(crate) fn validate_root(&mut self, node: &typed::Root) {
        for item in node.statements() {
            if let Some(language_system) = typed::LanguageSystem::cast(item) {
                self.validate_language_system(&language_system)
            } else if let Some(class_def) = typed::GlyphClassDef::cast(item) {
                self.validate_glyph_class_def(&class_def);
            } else if let Some(mark_def) = typed::MarkClassDef::cast(item) {
                self.validate_mark_class_def(&mark_def);
            } else if let Some(anchor_def) = typed::AnchorDef::cast(item) {
                self.validate_anchor_def(&anchor_def);
            } else if let Some(feature) = typed::Feature::cast(item) {
                self.validate_feature(&feature);
            } else if let Some(table) = typed::Table::cast(item) {
                self.validate_table(&table);
            } else if let Some(lookup) = typed::LookupBlock::cast(item) {
                self.validate_lookup_block(&lookup, None);
            } else if let Some(node) = typed::ValueRecordDef::cast(item) {
                self.validate_value_record_def(&node);
            } else if let Some(node) = typed::ConditionSet::cast(item) {
                self.validate_condition_set(&node);
            } else if let Some(node) = typed::FeatureVariation::cast(item) {
                self.validate_feature_variation(&node);
            } else if item.kind() == Kind::AnonKw {
                unimplemented!("anon")
            }
        }
        self.finalize();
    }

    /// perform any analysis required after seeing all items
    fn finalize(&mut self) {
        self.finalize_aalt();
    }

    fn finalize_aalt(&mut self) {
        // get around borrowck
        let bad = self
            .aalt_referenced_features
            .iter()
            .filter(|(tag, _)| !self.all_features.contains(*tag))
            .map(|(_, node)| node.clone())
            .collect::<Vec<_>>();
        for tag in bad {
            self.error(tag.range(), "Referenced feature not found.");
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
            }
        } else {
            self.seen_non_default_script = true;
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
            self.error(node.range(), "unknown parser bug?");
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
        self.validate_glyph_or_class(&node.glyph_class());
        self.mark_class_defs
            .insert(node.mark_class_name().text().clone());
        self.validate_anchor(&node.anchor());
    }

    fn validate_value_record_def(&mut self, node: &typed::ValueRecordDef) {
        let record = node.value_record();
        self.validate_value_record(&record);
        let name = node.name();
        if let Some(_prev) = self
            .value_record_defs
            .insert(name.text.clone(), name.clone())
        {
            self.warning(name.range(), "duplicate value record name");
        }
    }

    fn validate_condition_set(&mut self, node: &typed::ConditionSet) {
        let label = node.label().to_owned();
        if self.variation_info.is_none() {
            self.error(
                node.keyword().range(),
                "conditionset only valid when compiling variable font",
            );
        }
        if let Some(_prev) = self.condition_set_defs.insert(label.text.clone(), label) {
            self.warning(node.label().range(), "duplicate condition set definition");
        }

        for condition in node.conditions() {
            self.validate_condition(&condition);
        }
    }

    fn validate_condition(&mut self, condition: &typed::Condition) {
        // we've already errored if this is missing
        let Some(var_info) = self.variation_info else {
            return;
        };
        let Some((_, axis)) = var_info.axis(condition.tag().to_raw()) else {
            self.error(condition.tag().range(), "unknown axis");
            return;
        };
        if (condition.min_value().parse_signed() as f64) < axis.min.into_inner().0 as f64 {
            self.error(
                condition.min_value().range(),
                format!(
                    "value is less than axis minimum ({})",
                    axis.min.into_inner()
                ),
            );
        }
        if (condition.max_value().parse_signed() as f64) > axis.max.into_inner().0 as f64 {
            self.error(
                condition.max_value().range(),
                format!(
                    "value is more than axis maximum ({})",
                    axis.max.into_inner()
                ),
            );
        }
    }

    fn validate_feature_variation(&mut self, node: &typed::FeatureVariation) {
        let feature_tag = node.tag();
        if let Some(cond_set) = node.condition_set() {
            if !self.condition_set_defs.contains_key(cond_set.as_str()) {
                self.error(cond_set.range(), "undefined conditionset");
            }
        } else {
            assert!(node.null().is_some(), "go fix your parser");
        }
        self.validate_feature_statements(feature_tag.to_raw(), node.statements());
    }

    fn validate_mark_class(&mut self, node: &typed::GlyphClassName) {
        if !self.mark_class_defs.contains(node.text()) {
            self.error(node.range(), "undefined mark class");
        }
    }

    fn validate_table(&mut self, node: &typed::Table) {
        match node {
            typed::Table::Base(table) => self.validate_base(table),
            typed::Table::Gdef(table) => self.validate_gdef(table),
            typed::Table::Head(table) => self.validate_head(table),
            typed::Table::Hhea(table) => self.validate_hhea(table),
            typed::Table::Vhea(table) => self.validate_vhea(table),
            typed::Table::Vmtx(table) => self.validate_vmtx(table),
            typed::Table::Name(table) => self.validate_name(table),
            typed::Table::Os2(table) => self.validate_os2(table),
            typed::Table::Stat(table) => self.validate_stat(table),
            _ => self.error(node.tag().range(), "unsupported table type"),
        }
    }

    fn validate_base(&mut self, _node: &typed::BaseTable) {
        //TODO: same number of records as there are number of baseline tags
    }

    fn validate_hhea(&mut self, node: &typed::HheaTable) {
        self.ensure_metrics_are_non_variable(node.metrics());
        for metric in node.metrics() {
            self.validate_metric(&metric.metric());
        }
    }

    fn validate_vhea(&mut self, node: &typed::VheaTable) {
        self.ensure_metrics_are_non_variable(node.metrics());
        for metric in node.metrics() {
            self.validate_metric(&metric.metric());
        }
    }

    //TODO: remove me when we are compiling this
    fn ensure_metrics_are_non_variable(
        &mut self,
        metrics: impl Iterator<Item = typed::MetricRecord>,
    ) {
        for record in metrics {
            if record.metric().parse_simple().is_none() {
                self.error(
                    record.metric().range(),
                    "variable metrics not yet supported",
                );
            }
        }
    }

    fn validate_vmtx(&mut self, node: &typed::VmtxTable) {
        for statement in node.statements() {
            self.validate_glyph(&statement.glyph());
        }
    }

    fn validate_os2(&mut self, node: &typed::Os2Table) {
        for item in node.statements() {
            match item {
                typed::Os2TableItem::NumberList(item) => match item.keyword().kind {
                    Kind::PanoseKw => {
                        for number in item.values() {
                            match number.parse_unsigned() {
                                None => self.error(number.range(), "expected positive number"),
                                Some(0..=127) => (),
                                Some(_) => {
                                    self.error(number.range(), "expected value in range 0..128")
                                }
                            }
                        }
                    }
                    Kind::UnicodeRangeKw => {
                        for number in item.values() {
                            if !(0..128).contains(&number.parse_signed()) {
                                self.error(
                                    number.range(),
                                    "expected value in unicode character range 0..=127",
                                );
                            }
                        }
                    }
                    Kind::CodePageRangeKw => {
                        for number in item.values() {
                            if super::tables::CodePageRange::bit_for_code_page(
                                number.parse_signed() as u16,
                            )
                            .is_none()
                            {
                                self.error(number.range(), "not a valid code page");
                            }
                        }
                    }
                    _ => unreachable!(),
                },
                typed::Os2TableItem::FamilyClass(item) => {
                    let val = item.value();
                    match val.parse() {
                        Ok(raw_val) => {
                            if let Err((cls, sub)) = validate_os2_family_class(raw_val) {
                                self.warning(
                                    val.range(),
                                    format!(
                                        "Class {cls}, subclass {sub} is not a known sFamilyClass"
                                    ),
                                )
                            }
                        }
                        Err(e) => self.error(val.range(), e),
                    };
                }
                typed::Os2TableItem::Metric(i) => {
                    if matches!(i.keyword().kind, Kind::WinAscentKw | Kind::WinDescentKw) {
                        let val = i.metric();
                        match val.parse_simple() {
                            None => {
                                self.error(val.range(), "variable metrics not yet supports in OS/2")
                            }
                            Some(x) if x.is_negative() => {
                                self.error(val.range(), "expected positive number")
                            }
                            Some(_) => (),
                        }
                    }
                }
                typed::Os2TableItem::Number(item) => {
                    let val = item.number();
                    if val.parse_unsigned().is_none() {
                        self.error(val.range(), "expected positive number");
                    }
                }
                typed::Os2TableItem::Vendor(item) => {
                    if let Err(e) = item.parse_tag() {
                        self.error(item.value().range(), format!("invalid tag: '{}'", e));
                    }
                }
            }
        }
    }

    fn validate_stat(&mut self, node: &typed::StatTable) {
        let mut seen_fallback_name = false;
        for item in node.statements() {
            match item {
                typed::StatTableItem::ElidedFallbackName(_) => {
                    if seen_fallback_name {
                        self.error(item.range(), "fallback name must only be defined once");
                    }
                    seen_fallback_name = true;
                }
                typed::StatTableItem::AxisValue(axis) => {
                    let mut seen_location_format = None;
                    for item in axis.statements() {
                        if let typed::StatAxisValueItem::Location(loc) = item {
                            let format = match loc.value() {
                                typed::StatLocationValue::Value(_) => 'a',
                                typed::StatLocationValue::MinMax { .. } => 'b',
                                typed::StatLocationValue::Linked { .. } => 'c',
                            };
                            let prev_format = seen_location_format.replace(format);
                            match (prev_format, format) {
                                (Some('a'), 'a') => (),
                                (Some(_), 'a') => self.error(loc.range(), "multiple location statements, but previous statement was not format 'a'"),
                                (Some(_), 'b' | 'c') => self.error(loc.range(),format!("location statement format '{}' must be only statement", format)),
                                _ => (),
                            }
                        }
                    }
                }
                _ => (),
            }
        }
        if !seen_fallback_name {
            self.error(
                node.tag().range(),
                "STAT table must include 'ElidedFallbackName' or 'ElidedFallbackNameID'",
            );
        }
    }

    fn validate_name(&mut self, node: &typed::NameTable) {
        for record in node.statements() {
            let name_id = record.name_id();
            if let Err(e) = name_id.parse() {
                self.error(name_id.range(), e);
            }
            self.validate_name_spec(&record.entry());
        }
    }

    fn validate_name_spec(&mut self, spec: &typed::NameSpec) {
        let mut platform = None;
        if let Some(id) = spec.platform_id() {
            match id.parse() {
                Err(e) => self.error(id.range(), e),
                Ok(n @ 1 | n @ 3) => platform = Some(n),
                Ok(_) => self.error(id.range(), "platform id must be one of '1' or '3'"),
            }
        };

        let platform = platform.unwrap_or(WIN_PLATFORM_ID);

        if let Err((range, err)) = validate_name_string_encoding(platform, spec.string()) {
            self.error(range, err);
        }
        if let Some((platspec, language)) = spec.platform_and_language_ids() {
            match (platspec.parse(), language.parse()) {
                (Ok(a), Ok(_)) if Encoding::new(platform, a) == Encoding::Unknown => {
                    self.warning(spec.range(), "character encoding unsupported")
                }
                (a, b) => {
                    if let Err(e) = a {
                        self.error(platspec.range(), e);
                    }
                    if let Err(e) = b {
                        self.error(language.range(), e);
                    }
                }
            };
        }
    }

    fn validate_gdef(&mut self, node: &typed::GdefTable) {
        for statement in node.statements() {
            match statement {
                typed::GdefTableItem::ClassDef(node) => {
                    if let Some(cls) = node.base_glyphs() {
                        self.validate_glyph_class(&cls, true);
                    }

                    if let Some(cls) = node.ligature_glyphs() {
                        self.validate_glyph_class(&cls, true);
                    }

                    if let Some(cls) = node.mark_glyphs() {
                        self.validate_glyph_class(&cls, true);
                    }

                    if let Some(cls) = node.component_glyphs() {
                        self.validate_glyph_class(&cls, true);
                    }
                }
                typed::GdefTableItem::Attach(node) => {
                    self.validate_glyph_or_class(&node.target());
                    for idx in node.indices() {
                        if idx.parse_unsigned().is_none() {
                            self.error(idx.range(), "contourpoint indexes must be non-negative");
                        }
                    }
                }
                //FIXME: only one rule allowed per glyph; we need
                //to resolve glyphs here in order to track that.
                typed::GdefTableItem::LigatureCaret(node) => {
                    self.validate_glyph_or_class(&node.target());
                    if let typed::LigatureCaretValue::Pos(node) = node.values() {
                        for idx in node.values() {
                            if idx.parse_unsigned().is_none() {
                                self.error(idx.range(), "contourpoint index must be non-negative");
                            }
                        }
                    }
                }
            }
        }
    }

    fn validate_head(&mut self, node: &typed::HeadTable) {
        let mut prev = None;
        for statement in node.statements() {
            if let Some(prev) = prev.replace(statement.range()) {
                self.warning(prev, "FontRevision overwritten by subsequent statement");
            }
            let value = statement.value();
            let (int, fract) = value.text().split_once('.').expect("checked at parse time");
            if int.parse::<i16>().is_err() {
                let start = value.range().start;
                self.error(start..start + int.len(), "value exceeds 16bit limit");
            }
            if fract.len() != 3 {
                let start = value.range().start + int.len();
                self.warning(
                    start..start + fract.len(),
                    "version number should have exactly three decimal places",
                );
                //TODO: richer error, showing suggested input
            }
        }
    }

    // simple: 'include', 'script', 'language', 'subtable', 'lookup', 'lookupflag',
    // rules: 'enumerate', 'enum', 'ignore', 'substitute', 'sub', 'reversesub', 'rsub', 'position', 'pos',
    // decls: 'markClass', GCLASS}
    // special: 'feature', 'parameters', 'featureNames', 'cvParameters', 'sizemenuname'
    fn validate_feature(&mut self, node: &typed::Feature) {
        let tag = node.tag();
        let tag_raw = tag.to_raw();
        self.all_features.insert(tag_raw);

        if tag_raw == tags::SIZE {
            return self.validate_size_feature(node);
        }

        if tag_raw == tags::AALT {
            return self.validate_aalt_feature(node);
        }

        let mut statement_iter = node.statements();

        if tags::is_stylistic_set(tag_raw) {
            self.validate_stylistic_set_items(&mut statement_iter);
        }

        if tags::is_character_variant(tag_raw) {
            self.validate_character_variant_items(&mut statement_iter);
        }
        self.validate_feature_statements(tag_raw, statement_iter);
    }

    // shared between features and feature variations
    fn validate_feature_statements<'b>(
        &mut self,
        feature_tag: Tag,
        iter: impl Iterator<Item = &'b NodeOrToken>,
    ) {
        for item in iter {
            if item.kind() == Kind::ScriptNode
                || item.kind() == Kind::LanguageNode
                || item.kind() == Kind::SubtableNode
                || item.kind() == Kind::Semi
            {
                // lgtm
            } else if let Some(node) = typed::LookupRef::cast(item) {
                self.validate_lookup_ref(&node);
            } else if let Some(node) = typed::LookupBlock::cast(item) {
                self.validate_lookup_block(&node, Some(feature_tag));
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
            } else if let Some(_node) = typed::FeatureNames::cast(item) {
                self.warning(item.range(), "Only one featureNames block is allowed, it must preceed all rules, and it is only valid in features ss01-ss20");
            } else if let Some(node) = typed::FeatureRef::cast(item) {
                self.error(
                    node.keyword().range(),
                    "feature reference only valid in 'aalt' feature",
                );
            } else {
                self.error(
                    item.range(),
                    format!("unhandled item '{}' in feature", item.kind()),
                );
            }
        }
    }

    fn validate_stylistic_set_items<'b>(
        &mut self,
        iter: &mut impl Iterator<Item = &'b NodeOrToken>,
    ) {
        let mut iter = iter.peekable();
        if let Some(node) = iter.peek().and_then(|x| typed::FeatureNames::cast(x)) {
            for name in node.statements() {
                self.validate_name_spec(&name);
            }
            iter.next();
        }
    }

    fn validate_character_variant_items<'b>(
        &mut self,
        iter: &mut impl Iterator<Item = &'b NodeOrToken>,
    ) {
        let mut iter = iter.peekable();
        if let Some(node) = iter.peek().and_then(|x| typed::CvParameters::cast(x)) {
            for kind in [
                Kind::FeatUiLabelNameIdKw,
                Kind::FeatUiTooltipTextNameIdKw,
                Kind::SampleTextNameIdKw,
                Kind::ParamUiLabelNameIdKw,
            ] {
                if !node.iter().any(|x| x.kind() == kind) {
                    self.warning(node.keyword().range(), format!("missing '{kind}' node"));
                }
            }

            iter.next();
        }
    }

    fn validate_aalt_feature(&mut self, node: &typed::Feature) {
        for item in node.statements() {
            if let Some(node) = typed::GsubStatement::cast(item) {
                match node {
                    typed::GsubStatement::Type1(_) | typed::GsubStatement::Type3(_) => {
                        self.validate_gsub_statement(&node)
                    }
                    _ => self.error(
                        node.range(),
                        "only Single and Alternate rules allowed in aalt feature",
                    ),
                }
            } else if let Some(node) = typed::FeatureRef::cast(item) {
                let tag = node.feature();
                let range = tag.range();
                let raw_tag = tag.to_raw();
                if self.aalt_referenced_features.insert(raw_tag, tag).is_some() {
                    self.warning(range, "feature already declared")
                }
            } else if !item.kind().is_trivia() {
                self.error(
                    item.range(),
                    "aalt can only contain feature names and single or alternate sub rules.",
                );
            }
        }
    }

    fn validate_size_feature(&mut self, node: &typed::Feature) {
        let mut param = None;
        let mut menu_name_count = 0;
        for item in node.statements() {
            if let Some(node) = typed::Parameters::cast(item) {
                if param.is_some() {
                    self.error(
                        node.range(),
                        "size feature can have only one 'parameters' statement",
                    );
                }
                param = Some(node);
            } else if let Some(node) = typed::SizeMenuName::cast(item) {
                self.validate_name_spec(&node.spec());
                menu_name_count += 1;
            } else if !item.kind().is_trivia() {
                self.error(
                    item.range(),
                    "size can only contain feature names and single or alternate sub rules.",
                );
            }
        }

        match param {
            None => self.error(
                node.tag().range(),
                "size feature must include a 'parameters' statement",
            ),
            Some(param) => {
                if param.subfamily().parse_signed() == 0
                    && param.range_start().map(|x| x.parse() as i32).unwrap_or(0) == 0
                    && param.range_end().map(|x| x.parse() as i32).unwrap_or(0) == 0
                    && menu_name_count != 0
                {
                    //TODO: better diagnostics
                    self.error(
                        param.range(),
                        "if subfamily is omitted, there must be no 'sizemenuname' statements",
                    );
                }
            }
        }
    }

    fn validate_lookup_block(&mut self, node: &typed::LookupBlock, in_feature: Option<Tag>) {
        let name = node.label();
        if in_feature == Some(tags::AALT) || in_feature == Some(tags::SIZE) {
            self.error(
                name.range(),
                format!(
                    "lookups are not allowed in '{}' feature",
                    in_feature.unwrap()
                ),
            );
        }
        let mut kind = None;

        // you can set a lookupflag before seeing any rules, after the last rule,
        // and not anywhere else. Instead of a bool we store the decl range,
        // for error reporting
        let mut has_reset_lookup_flag = None;
        if let Some(_prev) = self.lookup_defs.insert(name.text.clone(), name.clone()) {
            //TODO: annotate with previous location
            self.error(
                name.range(),
                format!("A lookup named '{}' has already been defined", name.text),
            );
        }
        for item in node.statements() {
            if item.kind().is_rule() {
                if let Some(lookup_flag) = has_reset_lookup_flag.take() {
                    self.error(
                        lookup_flag,
                        "all rules in named lookup must have same lookup flags",
                    );
                }
                match kind {
                    // we allow mixed rules in this specific case
                    Some(Kind::GsubType1 | Kind::GsubType2)
                        if matches!(item.kind(), Kind::GsubType1 | Kind::GsubType2) => {}
                    Some(kind) if kind != item.kind() => self.error(
                        item.range(),
                        format!(
                            "multiple rule types in lookup block (saw '{}' after '{}')",
                            item.kind(),
                            kind
                        ),
                    ),
                    _ => kind = Some(item.kind()),
                }
            }
            if item.kind() == Kind::ScriptNode || item.kind() == Kind::LanguageNode {
                if in_feature.is_none() {
                    self.error(
                        item.range(),
                        "script and language statements not allowed in standalone lookup blocks",
                    );
                }
            } else if item.kind() == Kind::SubtableNode {
                // lgtm
            } else if let Some(node) = typed::LookupRef::cast(item) {
                if in_feature.is_none() {
                    //TODO: verify that this is accurate
                    self.warning(
                        node.range(),
                        "lookup reference outside of feature is ignored",
                    );
                }
                self.validate_lookup_ref(&node);
            } else if let Some(node) = typed::LookupBlock::cast(item) {
                self.error(
                    node.keyword().range(),
                    "lookup blocks cannot contain other blocks",
                );
            } else if let Some(node) = typed::LookupFlag::cast(item) {
                if kind.is_some() {
                    has_reset_lookup_flag = Some(node.range());
                }
                self.validate_lookupflag(&node);
            } else if let Some(node) = typed::GsubStatement::cast(item) {
                self.validate_gsub_statement(&node);
            } else if let Some(node) = typed::GposStatement::cast(item) {
                self.validate_gpos_statement(&node);
            } else if let Some(node) = typed::GlyphClassDef::cast(item) {
                self.validate_glyph_class_def(&node);
            } else if let Some(node) = typed::MarkClassDef::cast(item) {
                self.validate_mark_class_def(&node);
            } else if item.kind() == Kind::Semi {
                // continue
            } else {
                self.error(
                    item.range(),
                    format!("unhandled item '{}' in lookup block", item.kind()),
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
            typed::GposStatement::Type3(rule) => {
                self.validate_glyph_or_class(&rule.target());
                self.validate_anchor(&rule.entry());
                self.validate_anchor(&rule.exit());
            }
            //NOTE: this should be also checking that all mark classes referenced
            //in this rule are disjoint, but we don't know the glyph ids in validation
            //so we validate this in compile_ctx
            typed::GposStatement::Type4(rule) => {
                self.validate_glyph_or_class(&rule.base());
                for mark in rule.attachments() {
                    self.validate_anchor(&mark.anchor());
                    match mark.mark_class_name() {
                        Some(name) => self.validate_mark_class(&name),
                        None => {
                            self.error(mark.range(), "mark-to-base attachments should not be null")
                        }
                    }
                }
            }
            typed::GposStatement::Type5(rule) => {
                //FIXME: if this is a class each member should have the same
                //number of ligature components? not sure how we check this.
                self.validate_glyph_or_class(&rule.base());
                for component in rule.ligature_components() {
                    for mark in component.attachments() {
                        let anchor = mark.anchor();
                        match mark.mark_class_name() {
                            Some(name) => self.validate_mark_class(&name),
                            None => {
                                if anchor.null().is_none() {
                                    self.error(
                                        anchor.range(),
                                        "non-NULL anchor must specify mark class",
                                    );
                                }
                            }
                        }
                        self.validate_anchor(&anchor);
                    }
                }
            }
            typed::GposStatement::Type6(rule) => {
                self.validate_glyph_or_class(&rule.base());
                for mark in rule.attachments() {
                    self.validate_anchor(&mark.anchor());
                    match mark.mark_class_name() {
                        Some(name) => self.validate_mark_class(&name),
                        None => {
                            self.error(mark.range(), "mark-to-mark attachments should not be null")
                        }
                    }
                }
            }
            typed::GposStatement::Type8(rule) => self.validate_gpos_contextual_rule(rule),
            typed::GposStatement::Ignore(node) => {
                for rule in node.rules() {
                    for item in rule.backtrack().items().chain(rule.lookahead().items()) {
                        self.validate_glyph_or_class(&item);
                    }
                    for item in rule.input().items() {
                        self.validate_glyph_or_class(&item.target());
                    }
                }
            }
        }
    }

    fn validate_gpos_contextual_rule(&mut self, node: &typed::Gpos8) {
        for item in node.backtrack().items().chain(node.lookahead().items()) {
            self.validate_glyph_or_class(&item);
        }

        let mut seen_lookup = false;
        let mut seen_inline = false;
        for item in node.input().items() {
            self.validate_glyph_or_class(&item.target());
            for lookup in item.lookups() {
                self.validate_lookup_ref(&lookup);
                if seen_inline {
                    self.error(
                        lookup.range(),
                        "rule cannot have both explicit lookups and inline position values",
                    );
                }
                seen_lookup = true;
            }
            if let Some(value) = item.valuerecord() {
                if seen_lookup {
                    self.error(
                        value.range(),
                        "rule cannot have both inline rules and explicit lookups",
                    );
                }
                seen_inline = true;
                self.validate_value_record(&value);
            }
        }
    }

    fn validate_gsub_statement(&mut self, node: &typed::GsubStatement) {
        match node {
            typed::GsubStatement::Type1(rule) => {
                //TODO: ensure equal lengths, other requirements
                self.validate_glyph_or_class(&rule.target());
                if let Some(replacement) = rule.replacement() {
                    self.validate_glyph_or_class(&replacement);
                }
            }
            typed::GsubStatement::Type2(rule) => {
                self.validate_glyph(&rule.target());
                let mut count = 0;
                for item in rule.replacement() {
                    self.validate_glyph(&item);
                    count += 1;
                }
                if count < 2 {
                    let range = range_for_iter(rule.replacement()).unwrap_or_else(|| rule.range());
                    self.error(range, "sequence must contain at least two items");
                }
            }
            typed::GsubStatement::Type3(rule) => {
                self.validate_glyph(&rule.target());
                self.validate_glyph_class(&rule.alternates(), false);
            }
            typed::GsubStatement::Type4(rule) => {
                let mut count = 0;
                for item in rule.target() {
                    self.validate_glyph_or_class(&item);
                    count += 1;
                }
                if count < 2 {
                    let range = range_for_iter(rule.target()).unwrap_or_else(|| rule.range());
                    self.error(range, "sequence must contain at least two items");
                }
                self.validate_glyph(&rule.replacement());
            }
            typed::GsubStatement::Type5(_) => {
                panic!("gsub 5 rules are not currently generated by parser")
            }
            typed::GsubStatement::Type6(rule) => {
                for item in rule.backtrack().items() {
                    self.validate_glyph_or_class(&item);
                }

                for item in rule.lookahead().items() {
                    self.validate_glyph_or_class(&item);
                }

                let mut inline_class_sub = false;
                let mut has_inline_rule = false;
                if let Some(inline) = rule.inline_rule() {
                    has_inline_rule = true;
                    if let Some(class) = inline.replacement_class() {
                        debug_assert!(inline.replacement_glyphs().next().is_none());
                        self.validate_glyph_class(&class, true);
                        inline_class_sub = true;
                    }
                    for glyph in inline.replacement_glyphs() {
                        self.validate_glyph(&glyph);
                    }
                }

                let input_seq = rule.input();
                for (i, item) in input_seq.items().enumerate() {
                    let target = item.target();
                    if i == 0 && inline_class_sub && !target.is_class() {
                        self.error(
                            input_seq.range(),
                            "if replacing by glyph class, input sequence must be a single glyph class",
                        );
                    }
                    self.validate_glyph_or_class(&item.target());
                    for lookup in item.lookups() {
                        if has_inline_rule {
                            self.error(
                                lookup.range(),
                                "named lookup not allowed in statement that includes inline rule",
                            );
                        }
                        self.validate_lookup_ref(&lookup);
                    }
                }
            }
            typed::GsubStatement::Type8(rule) => {
                for item in rule.backtrack().items().chain(rule.lookahead().items()) {
                    self.validate_glyph_or_class(&item);
                }
                let mut input_class = false;
                for (i, item) in rule.input().items().enumerate() {
                    if i > 0 {
                        self.error(
                            item.range(),
                            "rsub rules can have only one item in the input sequence",
                        );
                    } else {
                        let target = item.target();
                        self.validate_glyph_or_class(&target);
                        input_class = item.target().is_class();
                        if let Some(lookup) = item.lookups().next() {
                            self.error(lookup.range(), "explicit lookups in rsub rules are not supported, although they should be. Please file an issue at https://github.com/cmyr/fea-rs/issues");
                        }
                    }
                }
                if let Some(inline) = rule.inline_rule() {
                    if let Some(class) = inline.replacement_class() {
                        debug_assert!(inline.replacement_glyphs().next().is_none());
                        self.validate_glyph_class(&class, true);
                        if !input_class {
                            self.error(class.range(), "class can only substitute another class");
                        }
                    } else if let Some(glyph) = inline.replacement_glyphs().next() {
                        self.validate_glyph(&glyph);
                    }
                }
            }
            typed::GsubStatement::Ignore(node) => {
                for rule in node.rules() {
                    for item in rule.backtrack().items().chain(rule.lookahead().items()) {
                        self.validate_glyph_or_class(&item);
                    }
                    for item in rule.input().items() {
                        self.validate_glyph_or_class(&item.target());
                    }
                }
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

        let mut iter = node.values();
        while let Some(next) = iter.next() {
            match next.kind() {
                Kind::RightToLeftKw if !rtl => rtl = true,
                Kind::IgnoreBaseGlyphsKw if !ignore_base => ignore_base = true,
                Kind::IgnoreLigaturesKw if !ignore_lig => ignore_lig = true,
                Kind::IgnoreMarksKw if !ignore_marks => ignore_marks = true,

                Kind::MarkAttachmentTypeKw if !mark_set => {
                    mark_set = true;
                    match iter.next().and_then(typed::GlyphClass::cast) {
                        Some(node) => self.validate_glyph_class(&node, true),
                        None => self.error(
                            next.range(),
                            "MarkAttachmentType should be followed by glyph class",
                        ),
                    }
                }
                Kind::UseMarkFilteringSetKw if !filter_set => {
                    filter_set = true;
                    match iter.next().and_then(typed::GlyphClass::cast) {
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

                _ => self.error(next.range(), "invalid lookupflag value"),
            }
        }
    }

    fn validate_glyph_or_class(&mut self, node: &typed::GlyphOrClass) {
        match node {
            typed::GlyphOrClass::Glyph(name) => self.validate_glyph_name(name),
            typed::GlyphOrClass::Cid(cid) => self.validate_cid(cid),
            typed::GlyphOrClass::Class(class) => self.validate_glyph_class_literal(class, true),
            typed::GlyphOrClass::NamedClass(name) => self.validate_glyph_class_ref(name, true),
            typed::GlyphOrClass::Null(_) => (),
        }
    }

    fn validate_glyph(&mut self, node: &typed::Glyph) {
        match node {
            typed::Glyph::Named(name) => self.validate_glyph_name(name),
            typed::Glyph::Cid(cid) => self.validate_cid(cid),
            typed::Glyph::Null(_) => (),
        }
    }

    fn validate_glyph_class(&mut self, node: &typed::GlyphClass, accept_mark_class: bool) {
        match node {
            typed::GlyphClass::Literal(lit) => {
                self.validate_glyph_class_literal(lit, accept_mark_class)
            }
            typed::GlyphClass::Named(name) => {
                self.validate_glyph_class_ref(name, accept_mark_class)
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

    fn validate_lookup_ref(&mut self, node: &typed::LookupRef) {
        if !self.lookup_defs.contains_key(&node.label().text) {
            self.error(node.label().range(), "lookup is not defined");
        }
    }

    fn validate_glyph_range(&mut self, range: &typed::GlyphRange) {
        let start = range.start();
        let end = range.end();

        match (start.kind, end.kind) {
            (Kind::Cid, Kind::Cid) => {
                if let Err(err) = glyph_range::cid(start, end, |cid| {
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
                if let Err(err) = glyph_range::named(start, end, |name| {
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

        for metric in node.all_metrics() {
            self.validate_metric(&metric);
        }
    }

    fn validate_anchor(&mut self, anchor: &typed::Anchor) {
        if let Some(name) = anchor.name() {
            if !self.anchor_defs.contains_key(&name.text) {
                self.error(name.range(), "undefined anchor name");
            }
        }
        if let Some((one, two)) = anchor.coords() {
            self.validate_metric(&one);
            self.validate_metric(&two);
        }
    }

    fn validate_metric(&mut self, metric: &typed::Metric) {
        let typed::Metric::Variable(metric) = metric else {
            return;
        };
        let Some(var_info) = self.variation_info else {
            self.error(
                metric.range(),
                "variable metrics only supported in variable font",
            );
            return;
        };

        for location_val in metric.location_values() {
            for item in location_val.location().items() {
                let Some((_, axis)) = var_info.axis(item.axis_tag().to_raw()) else {
                    self.error(item.axis_tag().range(), "unknown axis");
                    continue;
                };
                let val = item.value().parse();
                match val {
                    super::AxisLocation::User(val) => {
                        let min = axis.min.into_inner().0;
                        let max = axis.max.into_inner().0;
                        if val.0 < min || val.0 > max {
                            self.error(
                                item.value().range(),
                                format!("value exceeds expected range ({min}, {max})"),
                            );
                        }
                    }
                    super::AxisLocation::Design(_) => (), // we don't have info to validate this
                    super::AxisLocation::Normalized(val) => {
                        if val.0 < -1.0 || val.0 > 1.0 {
                            self.error(
                                item.value().range(),
                                "normalized value should be in range (-1.0, 1.0)",
                            );
                        }
                    }
                }
            }
        }
    }
}

fn range_for_iter<T: AstNode>(mut iter: impl Iterator<Item = T>) -> Option<Range<usize>> {
    let start = iter.next()?.range();
    Some(iter.fold(start, |cur, node| cur.start..node.range().end))
}

fn validate_name_string_encoding(
    platform: u16,
    string: &Token,
) -> Result<(), (Range<usize>, String)> {
    let mut to_scan: &str = string.as_str();
    debug_assert!(to_scan.starts_with('"'));
    debug_assert!(to_scan.ends_with('"'));
    to_scan = &to_scan[1..to_scan.len() - 1];
    let token_start = string.range().start;
    let mut cur_off = 1;
    while !to_scan.is_empty() {
        match to_scan.bytes().position(|b| b == b'\\') {
            None => to_scan = "",
            Some(pos) if platform == WIN_PLATFORM_ID => {
                let range_start = token_start + cur_off + pos;
                if let Some(val) = to_scan.get(pos + 1..pos + 5) {
                    if let Some(idx) = val.bytes().position(|b| !b.is_ascii_hexdigit()) {
                        return Err((
                            range_start + idx..range_start + idx + 1,
                            format!(
                                "invalid escape sequence: '{}' is not a hex digit",
                                val.as_bytes()[idx] as char
                            ),
                        ));
                    }
                } else {
                    return Err((
                        range_start..range_start + to_scan[pos..].len(),
                        "windows escape sequences must be four hex digits long".into(),
                    ));
                }
                cur_off += to_scan[..pos].len();
                to_scan = &to_scan[pos + 5..];
            }
            Some(pos) => {
                let range_start = token_start + cur_off + pos;
                if let Some(val) = to_scan.get(pos + 1..pos + 3) {
                    if let Some(idx) = val.bytes().position(|b| !b.is_ascii_hexdigit()) {
                        return Err((
                            range_start + idx..range_start + idx + 1,
                            format!(
                                "invalid escape sequence: '{}' is not a hex digit",
                                val.as_bytes()[idx] as char
                            ),
                        ));
                    }

                    if let Err(e) = u8::from_str_radix(val, 16) {
                        return Err((
                            range_start..range_start + 3,
                            format!("invalid escape sequence '{}'", e),
                        ));
                    }
                } else {
                    return Err((
                        range_start..range_start + to_scan[pos..].len(),
                        "mac escape sequences must be two hex digits long".into(),
                    ));
                }
                cur_off += to_scan[..pos].len();
                to_scan = &to_scan[pos + 3..];
            }
        }
    }
    Ok(())
}

/// adapted from <https://learn.microsoft.com/en-us/typography/opentype/spec/ibmfc>
fn validate_os2_family_class(raw: u16) -> Result<u16, (u8, u8)> {
    let [cls, subcls] = raw.to_be_bytes();
    match (cls, subcls) {
        (0, _) => Ok(raw),
        (1, 0..=8 | 15) => Ok(raw),
        (2, 0..=2 | 15) => Ok(raw),
        (3, 0..=2 | 15) => Ok(raw),
        (4, 0..=7 | 15) => Ok(raw),
        (5, 0..=5 | 15) => Ok(raw),
        (7, 0..=1 | 15) => Ok(raw),
        (8, 0..=6 | 9 | 10 | 15) => Ok(raw),
        (9, 0..=4 | 15) => Ok(raw),
        (10, 0..=8 | 15) => Ok(raw),
        (12, 0 | 3 | 6 | 7 | 15) => Ok(raw),
        _ => Err((cls, subcls)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn os2_family_class() {
        assert!(validate_os2_family_class(0x0108).is_ok());
        assert!(validate_os2_family_class(0x0008).is_ok());
        assert!(validate_os2_family_class(0x0202).is_ok());
        assert!(validate_os2_family_class(0x0203).is_err());
        assert!(validate_os2_family_class(0x0600).is_err());
    }
}
