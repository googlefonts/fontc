//! typing for ast nodes. based on rust-analyzer.
//!
//! Here, we use macros to generate distinct types for specific AST nodes.
//! We cast from generic nodes or tokens to these distinct types based on their
//! location in the tree, and the underlying [`Kind`] of the node.
//!
//! This lets us implement useful methods on specific AST nodes, which are
//! internally working on untyped `NodeOrToken`s.

use std::convert::TryFrom;
use std::ops::Range;

use smol_str::SmolStr;
use write_fonts::types::Fixed;

use crate::{Kind, Node, NodeOrToken};

use super::{ChildIter, Token};

/// A trait for types that exist in the AST.
///
/// Implementations of this type are generally generated via macro.
pub trait AstNode {
    /// Attempt to cast from some node or token to this type.
    fn cast(node: &NodeOrToken) -> Option<Self>
    where
        Self: Sized;

    /// The range in the source of this item.
    ///
    /// This is used for better diagnostic reporting.
    fn range(&self) -> Range<usize>;

    /// If this is a node, iterate over its children
    fn iter(&self) -> ChildIter {
        Default::default()
    }
}

/// Create a new AstNode wrapping a token.
macro_rules! ast_token {
    ($typ:ident, $kind:expr) => {
        #[derive(Clone, Debug)]
        #[allow(missing_docs)]
        pub struct $typ {
            inner: Token,
        }

        impl $typ {
            /// The raw text for this token
            #[allow(unused)]
            pub fn text(&self) -> &SmolStr {
                &self.inner.text
            }

            /// The underlying `Token`
            #[allow(unused)]
            pub fn token(&self) -> &Token {
                &self.inner
            }

            // just used for the ast_enum macro
            #[allow(dead_code)]
            pub(crate) fn node_(&self) -> Option<&Node> {
                None
            }
        }

        impl AstNode for $typ {
            fn cast(node: &NodeOrToken) -> Option<Self> {
                if let NodeOrToken::Token(t) = node {
                    if t.kind == $kind {
                        return Some(Self { inner: t.clone() });
                    }
                }
                None
            }

            fn range(&self) -> std::ops::Range<usize> {
                self.inner.range()
            }
        }
    };
}

/// Create a new AstNode, wrapping a Node
macro_rules! ast_node {
    ($typ:ident, $kind:expr) => {
        #[derive(Clone, Debug)]
        #[allow(missing_docs)]
        pub struct $typ {
            inner: Node,
        }

        impl $typ {
            pub(crate) fn try_from_node(node: &Node) -> Option<Self> {
                if node.kind == $kind {
                    return Some(Self {
                        inner: node.clone(),
                    });
                }
                None
            }

            #[allow(dead_code)]
            pub(crate) fn find_token(&self, kind: Kind) -> Option<&Token> {
                self.iter()
                    .find(|t| t.kind() == kind)
                    .and_then(NodeOrToken::as_token)
            }

            /// Return a reference to the underlying `Node`.
            #[allow(dead_code)]
            pub fn node(&self) -> &Node {
                &self.inner
            }

            // just used for the ast_enum macro
            #[allow(dead_code)]
            pub(crate) fn node_(&self) -> Option<&Node> {
                Some(&self.inner)
            }
        }

        impl AstNode for $typ {
            fn cast(node: &NodeOrToken) -> Option<Self> {
                if let NodeOrToken::Node(inner) = node {
                    return Self::try_from_node(inner);
                }
                None
            }

            fn range(&self) -> std::ops::Range<usize> {
                self.inner.range()
            }

            fn iter(&self) -> ChildIter {
                self.inner.iter_children()
            }
        }
    };
}

/// Create an enum from some set of AstNodes.
///
/// This is useful when you have places in the tree where you aceept multiple
/// different concrete types.
macro_rules! ast_enum {
    ($typ:ident{ $($name:ident($member:ident),)*}) => {
        #[derive(Clone, Debug)]
        #[allow(missing_docs)]
        pub enum $typ {
            $($name($member)),*
        }

        impl AstNode for $typ {
            fn cast(node: &NodeOrToken) -> Option<Self> {
                $(
                    if let Some(thing) = $member::cast(node) {
                        return Some(Self::$name(thing));
                    }
                )*
                    None

            }

            fn range(&self) -> std::ops::Range<usize> {
                match self {
                    $(
                        Self::$name(inner) => inner.range(),
                    )*
                }
            }
        }

        impl $typ {
            #[allow(unused)]
            pub(crate) fn node(&self) -> Option<&Node> {
                match self {
                    $(
                        Self::$name(inner) => inner.node_(),
                    )*
                }
            }
        }
    };

}

ast_token!(Cid, Kind::Cid);
ast_token!(GlyphName, Kind::GlyphName);
ast_token!(Tag, Kind::Tag);
ast_token!(GlyphClassName, Kind::NamedGlyphClass);
ast_token!(Number, Kind::Number);
ast_token!(Float, Kind::Float);
ast_token!(Octal, Kind::Octal);
ast_token!(Hex, Kind::Hex);
ast_token!(Null, Kind::NullKw);
ast_node!(Root, Kind::SourceFile);
ast_node!(GlyphRange, Kind::GlyphRange);
ast_node!(GlyphClassDef, Kind::GlyphClassDefNode);
ast_node!(MarkClassDef, Kind::MarkClassNode);
ast_node!(Anchor, Kind::AnchorNode);
ast_node!(AnchorDef, Kind::AnchorDefNode);
ast_node!(ValueRecordDef, Kind::ValueRecordDefNode);
ast_node!(GlyphClassLiteral, Kind::GlyphClass);
ast_node!(LanguageSystem, Kind::LanguageSystemNode);
ast_node!(Include, Kind::IncludeNode);
ast_node!(Feature, Kind::FeatureNode);
ast_node!(Script, Kind::ScriptNode);
ast_node!(Language, Kind::LanguageNode);
ast_node!(LookupFlag, Kind::LookupFlagNode);
ast_node!(LookupRef, Kind::LookupRefNode);
ast_node!(LookupBlock, Kind::LookupBlockNode);
ast_node!(ValueRecord, Kind::ValueRecordNode);
ast_node!(Device, Kind::DeviceNode);
ast_node!(SizeMenuName, Kind::SizeMenuNameNode);
ast_node!(Parameters, Kind::ParametersNode);
ast_node!(FeatureNames, Kind::FeatureNamesKw);
ast_node!(CvParameters, Kind::CvParametersKw);
ast_node!(CvParametersName, Kind::CvParamsNameNode);
ast_node!(CvParametersChar, Kind::CharacterKw);

ast_node!(HeadTable, Kind::HeadTableNode);
ast_node!(HheaTable, Kind::HheaTableNode);
ast_node!(NameTable, Kind::NameTableNode);
ast_node!(BaseTable, Kind::BaseTableNode);
ast_node!(GdefTable, Kind::GdefTableNode);
ast_node!(Os2Table, Kind::Os2TableNode);
ast_node!(VheaTable, Kind::VheaTableNode);
ast_node!(VmtxTable, Kind::VmtxTableNode);
ast_node!(StatTable, Kind::StatTableNode);
ast_node!(UnimplentedTable, Kind::TableNode);

ast_enum!(Table {
    Head(HeadTable),
    Hhea(HheaTable),
    Name(NameTable),
    Base(BaseTable),
    Gdef(GdefTable),
    Os2(Os2Table),
    Vhea(VheaTable),
    Vmtx(VmtxTable),
    Stat(StatTable),
    Other(UnimplentedTable),
});

ast_node!(BaseTagList, Kind::BaseTagListNode);
ast_node!(BaseScriptList, Kind::BaseScriptListNode);
ast_node!(ScriptRecord, Kind::ScriptRecordNode);

ast_node!(MetricRecord, Kind::MetricValueNode);
ast_node!(NumberRecord, Kind::NumberValueNode);
ast_node!(VendorRecord, Kind::Os2VendorNode);
ast_node!(NameRecord, Kind::NameRecordNode);
ast_node!(NameSpec, Kind::NameSpecNode);
ast_node!(VmtxEntry, Kind::VmtxEntryNode);

ast_enum!(DecOctHex {
    Decimal(Number),
    Octal(Octal),
    Hex(Hex),
});

ast_enum!(FloatLike {
    Float(Float),
    Number(Number),
});

ast_node!(ConditionSet, Kind::ConditionSetNode);
ast_node!(Condition, Kind::ConditionNode);
ast_node!(FeatureVariation, Kind::VariationNode);
ast_node!(VariableMetric, Kind::VariableMetricNode);
ast_node!(LocationValue, Kind::LocationValueNode);
ast_node!(LocationSpec, Kind::LocationSpecNode);
ast_node!(LocationSpecItem, Kind::LocationSpecItemNode);
ast_enum!(Metric {
    Scalar(Number),
    Variable(VariableMetric),
});
ast_node!(AxisLocation, Kind::AxisLocationNode);
ast_token!(NumberSuffix, Kind::NumberSuffix);

ast_node!(GdefClassDef, Kind::GdefClassDefNode);
ast_node!(GdefClassDefEntry, Kind::GdefClassDefEntryNode);
ast_node!(GdefAttach, Kind::GdefAttachNode);
ast_node!(GdefLigatureCaret, Kind::GdefLigatureCaretNode);

ast_enum!(GdefTableItem {
    ClassDef(GdefClassDef),
    Attach(GdefAttach),
    LigatureCaret(GdefLigatureCaret),
});

ast_node!(HeadFontRevision, Kind::HeadFontRevisionNode);

ast_node!(Os2NumberList, Kind::Os2NumberListNode);
ast_node!(Os2FamilyClass, Kind::Os2FamilyClassNode);
ast_enum!(Os2TableItem {
    Number(NumberRecord),
    NumberList(Os2NumberList),
    Metric(MetricRecord),
    Vendor(VendorRecord),
    FamilyClass(Os2FamilyClass),
});

ast_node!(StatElidedFallbackName, Kind::StatElidedFallbackNameNode);
ast_node!(StatDesignAxis, Kind::StatDesignAxisNode);
ast_node!(StatAxisValue, Kind::StatAxisValueNode);

ast_enum!(StatTableItem {
    ElidedFallbackName(StatElidedFallbackName),
    DesignAxis(StatDesignAxis),
    AxisValue(StatAxisValue),
});

ast_node!(StatAxisFlag, Kind::StatAxisValueFlagNode);
ast_node!(StatAxisLocation, Kind::StatAxisValueLocationNode);

ast_enum!(StatAxisValueItem {
    NameRecord(NameSpec),
    Flag(StatAxisFlag),
    Location(StatAxisLocation),
});

ast_node!(FeatureRef, Kind::AaltFeatureNode);

ast_node!(Gsub1, Kind::GsubType1);
ast_node!(Gsub2, Kind::GsubType2);
ast_node!(Gsub3, Kind::GsubType3);
ast_node!(Gsub4, Kind::GsubType4);
ast_node!(Gsub5, Kind::GsubType5);
ast_node!(Gsub6, Kind::GsubType6);
ast_node!(Gsub8, Kind::GsubType8);
ast_node!(GsubIgnore, Kind::GsubIgnore);

ast_node!(Gpos1, Kind::GposType1);
ast_node!(Gpos2, Kind::GposType2);
ast_node!(Gpos3, Kind::GposType3);
ast_node!(Gpos4, Kind::GposType4);
ast_node!(Gpos5, Kind::GposType5);
ast_node!(Gpos6, Kind::GposType6);
ast_node!(Gpos8, Kind::GposType8);
ast_node!(GposIgnore, Kind::GposIgnore);
ast_node!(AnchorMark, Kind::AnchorMarkNode);
ast_node!(LigatureComponent, Kind::LigatureComponentNode);

ast_node!(BacktrackSequence, Kind::BacktrackSequence);
ast_node!(LookaheadSequence, Kind::LookaheadSequence);
ast_node!(InputSequence, Kind::ContextSequence);
ast_node!(InputItem, Kind::ContextGlyphNode);
ast_node!(InlineSubRule, Kind::InlineSubNode);
ast_node!(IgnoreRule, Kind::IgnoreRuleStatementNode);

ast_enum!(GposStatement {
    Type1(Gpos1),
    Type2(Gpos2),
    Type3(Gpos3),
    Type4(Gpos4),
    Type5(Gpos5),
    Type6(Gpos6),
    Type8(Gpos8),
    Ignore(GposIgnore),
});

ast_enum!(GsubStatement {
    Type1(Gsub1),
    Type2(Gsub2),
    Type3(Gsub3),
    Type4(Gsub4),
    Type5(Gsub5),
    Type6(Gsub6),
    Type8(Gsub8),
    Ignore(GsubIgnore),
});

ast_enum!(GlyphOrClass {
    Glyph(GlyphName),
    Cid(Cid),
    NamedClass(GlyphClassName),
    Class(GlyphClassLiteral),
    Null(Null),
});

ast_enum!(Glyph {
    Named(GlyphName),
    Cid(Cid),
    Null(Null),
});

ast_enum!(GlyphClass {
    Named(GlyphClassName),
    Literal(GlyphClassLiteral),
});

/// A trait for contextual and chain contextual rule nodes.
///
/// These types share a common implementation, and this lets us reuse code
/// when processing those types.
pub trait ContextualRuleNode: AstNode {
    /// The backtrack sequence
    fn backtrack(&self) -> BacktrackSequence {
        self.iter().find_map(BacktrackSequence::cast).unwrap()
    }

    /// The lookahead sequence
    fn lookahead(&self) -> LookaheadSequence {
        self.iter().find_map(LookaheadSequence::cast).unwrap()
    }

    /// The input sequence
    fn input(&self) -> InputSequence {
        self.iter().find_map(InputSequence::cast).unwrap()
    }
}

impl ContextualRuleNode for Gpos8 {}
impl ContextualRuleNode for Gsub6 {}
impl ContextualRuleNode for Gsub8 {}
impl ContextualRuleNode for IgnoreRule {}

impl Root {
    /// Iterate over all top-level statements
    pub fn statements(&self) -> impl Iterator<Item = &NodeOrToken> {
        self.iter().filter(|t| !t.kind().is_trivia())
    }
}

impl LanguageSystem {
    /// The script tag
    pub fn script(&self) -> Tag {
        self.inner.iter_children().find_map(Tag::cast).unwrap()
    }

    /// The language tag
    pub fn language(&self) -> Tag {
        self.inner
            .iter_children()
            .skip_while(|t| t.kind() != Kind::Tag)
            .skip(1)
            .find_map(Tag::cast)
            .unwrap()
    }
}

impl Include {
    pub(crate) fn path(&self) -> &Token {
        self.find_token(Kind::Path).unwrap()
    }
}

impl Tag {
    pub(crate) fn parse(&self) -> Result<write_fonts::types::Tag, write_fonts::types::InvalidTag> {
        self.inner.text.parse()
    }

    /// Convert this AST tag into a raw `Tag`
    pub fn to_raw(&self) -> write_fonts::types::Tag {
        self.parse().expect("tag is exactly 4 bytes")
    }
}

impl GlyphClassDef {
    pub(crate) fn class_name(&self) -> GlyphClassName {
        self.inner
            .iter_children()
            .find_map(GlyphClassName::cast)
            .unwrap()
    }

    pub(crate) fn class_alias(&self) -> Option<GlyphClassName> {
        //TODO: ensure this returns non in presence of named glyph class inside class block
        self.iter()
            .skip_while(|t| t.kind() != Kind::Eq)
            .find_map(GlyphClassName::cast)
    }

    pub(crate) fn class_def(&self) -> Option<GlyphClassLiteral> {
        self.inner.iter_children().find_map(GlyphClassLiteral::cast)
    }
}

impl GlyphClassLiteral {
    pub(crate) fn items(&self) -> impl Iterator<Item = &NodeOrToken> {
        self.iter()
            .skip_while(|t| t.kind() != Kind::LSquare)
            .skip(1)
            .take_while(|t| t.kind() != Kind::RSquare)
            .filter(|t| !t.kind().is_trivia())
    }
}

impl Cid {
    pub(crate) fn parse(&self) -> u16 {
        self.inner.text.parse().expect("cid is already validated")
    }
}

impl GlyphRange {
    pub(crate) fn start(&self) -> &Token {
        self.iter()
            .find(|i| i.kind() == Kind::Cid || i.kind() == Kind::GlyphName)
            .and_then(NodeOrToken::as_token)
            .unwrap()
    }

    pub(crate) fn end(&self) -> &Token {
        self.iter()
            .skip_while(|t| t.kind() != Kind::Hyphen)
            .find(|i| i.kind() == Kind::Cid || i.kind() == Kind::GlyphName)
            .and_then(NodeOrToken::as_token)
            .unwrap()
    }
}

impl GlyphOrClass {
    pub(crate) fn is_class(&self) -> bool {
        matches!(self, GlyphOrClass::Class(_) | GlyphOrClass::NamedClass(_))
    }
}

impl MarkClassDef {
    pub(crate) fn keyword(&self) -> &Token {
        self.find_token(Kind::MarkClassKw).unwrap()
    }

    pub(crate) fn glyph_class(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).expect("validated")
    }

    pub(crate) fn anchor(&self) -> Anchor {
        self.iter().find_map(Anchor::cast).unwrap()
    }

    pub(crate) fn mark_class_name(&self) -> GlyphClassName {
        self.iter()
            .skip_while(|t| t.kind() != Kind::AnchorNode)
            .find_map(GlyphClassName::cast)
            .unwrap()
    }
}

impl ValueRecordDef {
    pub(crate) fn value_record(&self) -> ValueRecord {
        self.iter().find_map(ValueRecord::cast).unwrap()
    }

    pub(crate) fn name(&self) -> &Token {
        self.find_token(Kind::Ident).expect("validated")
    }
}

impl AnchorDef {
    pub(crate) fn anchor(&self) -> Anchor {
        self.iter().find_map(Anchor::cast).unwrap()
    }

    pub(crate) fn name(&self) -> &Token {
        self.find_token(Kind::Ident).expect("pre-validated")
    }
}

impl Anchor {
    pub(crate) fn coords(&self) -> Option<(Metric, Metric)> {
        let tokens = self.iter();
        let mut first = None;

        for token in tokens {
            if let Some(metric) = Metric::cast(token) {
                if let Some(prev) = first.take() {
                    return Some((prev, metric));
                } else {
                    first = Some(metric);
                }
            }
        }
        None
    }

    pub(crate) fn contourpoint(&self) -> Option<Number> {
        self.iter()
            .skip_while(|x| x.kind() != Kind::ContourpointKw)
            .find_map(Number::cast)
    }

    pub(crate) fn devices(&self) -> Option<(Device, Device)> {
        let mut iter = self.iter().filter_map(Device::cast);
        iter.next()
            .map(|first| (first, iter.next().expect("one device implies another")))
    }

    pub(crate) fn null(&self) -> Option<&Token> {
        self.find_token(Kind::NullKw)
    }

    pub(crate) fn name(&self) -> Option<&Token> {
        self.find_token(Kind::Ident)
    }
}

impl Number {
    pub(crate) fn parse_signed(&self) -> i16 {
        self.text().parse().expect("already validated")
    }

    pub(crate) fn parse_unsigned(&self) -> Option<u16> {
        self.text().parse().ok()
    }
}

impl Float {
    pub(crate) fn parse(&self) -> f32 {
        self.text().parse().unwrap()
    }

    pub(crate) fn parse_fixed(&self) -> Fixed {
        Fixed::from_f64(self.parse() as _)
    }
}

impl FloatLike {
    pub(crate) fn parse(&self) -> f32 {
        match self {
            FloatLike::Number(n) => n.parse_signed() as f32,
            FloatLike::Float(n) => n.parse(),
        }
    }

    pub(crate) fn parse_fixed(&self) -> Fixed {
        Fixed::from_f64(self.parse() as _)
    }
}

impl Feature {
    pub(crate) fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub(crate) fn stylistic_set_feature_names(&self) -> Option<FeatureNames> {
        self.statements().next().and_then(FeatureNames::cast)
    }

    pub(crate) fn character_variant_params(&self) -> Option<CvParameters> {
        self.statements().next().and_then(CvParameters::cast)
    }

    pub(crate) fn statements(&self) -> impl Iterator<Item = &NodeOrToken> {
        self.iter()
            .skip_while(|t| t.kind() != Kind::LBrace)
            .skip(1)
            .filter(|t| !t.kind().is_trivia())
            .take_while(|t| t.kind() != Kind::RBrace)
    }
}

impl LookupBlock {
    #[allow(unused)]
    //TODO: do we want to support this syntax?
    pub(crate) fn use_extension(&self) -> Option<&Token> {
        self.iter()
            .take_while(|t| t.kind() != Kind::LBrace)
            .find(|t| t.kind() == Kind::UseExtensionKw)
            .and_then(NodeOrToken::as_token)
    }

    pub(crate) fn keyword(&self) -> &Token {
        self.find_token(Kind::LookupKw).unwrap()
    }

    pub(crate) fn label(&self) -> &Token {
        self.find_token(Kind::Label).unwrap()
    }

    pub(crate) fn statements(&self) -> impl Iterator<Item = &NodeOrToken> {
        self.iter()
            .skip_while(|t| t.kind() != Kind::LBrace)
            .skip(1)
            .filter(|t| !t.kind().is_trivia())
            .take_while(|t| t.kind() != Kind::RBrace)
    }
}

impl ConditionSet {
    pub(crate) fn keyword(&self) -> &Token {
        self.find_token(Kind::ConditionSetKw).unwrap()
    }

    pub(crate) fn label(&self) -> &Token {
        self.find_token(Kind::Label).unwrap()
    }

    pub(crate) fn conditions(&self) -> impl Iterator<Item = Condition> + '_ {
        self.iter().filter_map(Condition::cast)
    }
}

impl Condition {
    pub(crate) fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub(crate) fn min_value(&self) -> Number {
        self.iter().find_map(Number::cast).unwrap()
    }

    pub(crate) fn max_value(&self) -> Number {
        self.iter().filter_map(Number::cast).nth(1).unwrap()
    }
}

impl FeatureVariation {
    pub(crate) fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    /// optional; if this is 'none' then 'null' must be present
    pub(crate) fn condition_set(&self) -> Option<&Token> {
        self.find_token(Kind::Label)
    }

    pub(crate) fn null(&self) -> Option<&Token> {
        self.find_token(Kind::NullKw)
    }

    pub(crate) fn statements(&self) -> impl Iterator<Item = &NodeOrToken> {
        self.iter()
            .skip_while(|t| t.kind() != Kind::LBrace)
            .skip(1)
            .filter(|t| !t.kind().is_trivia())
            .take_while(|t| t.kind() != Kind::RBrace)
    }
}

impl Script {
    pub(crate) fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }
}

impl Language {
    pub(crate) fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    //FIXME: I believe this is never meaningful, as it is the default behaviour?
    #[allow(unused)]
    pub(crate) fn include_dflt(&self) -> Option<&Token> {
        self.find_token(Kind::IncludeDfltKw)
    }

    pub(crate) fn exclude_dflt(&self) -> Option<&Token> {
        self.find_token(Kind::ExcludeDfltKw)
    }

    pub(crate) fn required(&self) -> Option<&Token> {
        self.find_token(Kind::RequiredKw)
    }
}

impl LookupFlag {
    pub(crate) fn number(&self) -> Option<Number> {
        self.iter().find_map(Number::cast)
    }

    pub(crate) fn values(&self) -> impl Iterator<Item = &NodeOrToken> + '_ {
        self.iter()
            .skip(1)
            .take_while(|t| t.kind() != Kind::Number && t.kind() != Kind::Semi)
            .filter(|t| !t.kind().is_trivia())
    }
}

impl LookupRef {
    pub(crate) fn label(&self) -> &Token {
        self.find_token(Kind::Ident).unwrap()
    }
}

impl Gsub1 {
    pub(crate) fn target(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    pub(crate) fn replacement(&self) -> Option<GlyphOrClass> {
        self.iter()
            .skip_while(|t| t.kind() != Kind::ByKw)
            .find_map(GlyphOrClass::cast)
    }
}

impl Gsub2 {
    pub(crate) fn target(&self) -> Glyph {
        self.iter().find_map(Glyph::cast).unwrap()
    }

    pub(crate) fn replacement(&self) -> impl Iterator<Item = Glyph> + '_ {
        self.iter()
            .skip_while(|t| t.kind() != Kind::ByKw)
            .skip(1)
            .filter_map(Glyph::cast)
    }
}

impl Gsub3 {
    pub(crate) fn target(&self) -> Glyph {
        self.iter().find_map(Glyph::cast).unwrap()
    }

    pub(crate) fn alternates(&self) -> GlyphClass {
        self.iter()
            .skip_while(|t| t.kind() != Kind::FromKw)
            .find_map(GlyphClass::cast)
            .unwrap()
    }
}

impl Gsub4 {
    pub(crate) fn target(&self) -> impl Iterator<Item = GlyphOrClass> + '_ {
        self.iter()
            .take_while(|t| t.kind() != Kind::ByKw)
            .filter_map(GlyphOrClass::cast)
    }

    pub(crate) fn replacement(&self) -> Glyph {
        self.iter()
            .skip_while(|t| t.kind() != Kind::ByKw)
            .find_map(Glyph::cast)
            .unwrap()
    }
}

impl Gsub6 {
    pub(crate) fn inline_rule(&self) -> Option<InlineSubRule> {
        self.iter().find_map(InlineSubRule::cast)
    }
}

impl Gsub8 {
    pub(crate) fn inline_rule(&self) -> Option<InlineSubRule> {
        self.iter().find_map(InlineSubRule::cast)
    }
}

impl GsubIgnore {
    pub(crate) fn rules(&self) -> impl Iterator<Item = IgnoreRule> + '_ {
        self.iter().filter_map(IgnoreRule::cast)
    }
}

impl BacktrackSequence {
    pub(crate) fn items(&self) -> impl Iterator<Item = GlyphOrClass> + '_ {
        self.iter().filter_map(GlyphOrClass::cast)
    }
}

impl LookaheadSequence {
    pub(crate) fn items(&self) -> impl Iterator<Item = GlyphOrClass> + '_ {
        self.iter().filter_map(GlyphOrClass::cast)
    }
}

impl InputSequence {
    pub(crate) fn items(&self) -> impl Iterator<Item = InputItem> + '_ {
        self.iter().filter_map(InputItem::cast)
    }
}

impl InputItem {
    pub(crate) fn target(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    pub(crate) fn lookups(&self) -> impl Iterator<Item = LookupRef> + '_ {
        self.iter().filter_map(LookupRef::cast)
    }

    /// for pos rules only
    pub(crate) fn valuerecord(&self) -> Option<ValueRecord> {
        self.iter().find_map(ValueRecord::cast)
    }
}

impl InlineSubRule {
    pub(crate) fn replacement_class(&self) -> Option<GlyphClass> {
        self.iter().find_map(GlyphClass::cast)
    }

    // if empty, there is a class
    pub(crate) fn replacement_glyphs(&self) -> impl Iterator<Item = Glyph> + '_ {
        self.iter().filter_map(Glyph::cast)
    }

    // this overlaps with the other two? i don't know what the best API is.. :/
    pub(crate) fn replacements(&self) -> impl Iterator<Item = GlyphOrClass> + '_ {
        self.iter().filter_map(GlyphOrClass::cast)
    }

    pub(crate) fn null(&self) -> Option<Null> {
        self.iter().find_map(Null::cast)
    }
}

impl Gpos1 {
    pub(crate) fn target(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    pub(crate) fn value(&self) -> ValueRecord {
        self.iter().find_map(ValueRecord::cast).unwrap()
    }
}

impl Gpos2 {
    pub(crate) fn enum_(&self) -> Option<&Token> {
        self.iter()
            .take_while(|t| t.kind() != Kind::PosKw)
            .find(|t| t.kind() == Kind::EnumKw)
            .and_then(NodeOrToken::as_token)
    }

    pub(crate) fn first_item(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    pub(crate) fn second_item(&self) -> GlyphOrClass {
        self.iter().filter_map(GlyphOrClass::cast).nth(1).unwrap()
    }

    pub(crate) fn first_value(&self) -> ValueRecord {
        self.iter().find_map(ValueRecord::cast).unwrap()
    }

    pub(crate) fn second_value(&self) -> Option<ValueRecord> {
        self.iter().filter_map(ValueRecord::cast).nth(1)
    }
}

impl Gpos3 {
    pub(crate) fn target(&self) -> GlyphOrClass {
        self.iter()
            .filter(|t| !t.kind().is_trivia())
            .nth(2)
            .and_then(GlyphOrClass::cast)
            .unwrap()
    }

    pub(crate) fn entry(&self) -> Anchor {
        self.iter().skip(3).find_map(Anchor::cast).unwrap()
    }

    pub(crate) fn exit(&self) -> Anchor {
        self.iter()
            .skip_while(|t| t.kind() != Kind::AnchorNode)
            .skip(1)
            .find_map(Anchor::cast)
            .unwrap()
    }
}

impl Gpos4 {
    pub(crate) fn base(&self) -> GlyphOrClass {
        self.iter()
            .filter(|t| !t.kind().is_trivia())
            .nth(2)
            .and_then(GlyphOrClass::cast)
            .unwrap()
    }

    pub(crate) fn attachments(&self) -> impl Iterator<Item = AnchorMark> + '_ {
        self.iter().skip(3).filter_map(AnchorMark::cast)
    }
}

impl Gpos5 {
    pub(crate) fn base(&self) -> GlyphOrClass {
        self.iter()
            .filter(|t| !t.kind().is_trivia())
            .nth(2)
            .and_then(GlyphOrClass::cast)
            .unwrap()
    }

    pub(crate) fn ligature_components(&self) -> impl Iterator<Item = LigatureComponent> + '_ {
        self.iter().skip(3).filter_map(LigatureComponent::cast)
    }
}

impl Gpos6 {
    pub(crate) fn base(&self) -> GlyphOrClass {
        self.iter()
            .filter(|t| !t.kind().is_trivia())
            .nth(2)
            .and_then(GlyphOrClass::cast)
            .unwrap()
    }

    pub(crate) fn attachments(&self) -> impl Iterator<Item = AnchorMark> + '_ {
        self.iter().skip(3).filter_map(AnchorMark::cast)
    }
}

impl Gpos8 {
    pub(crate) fn trailing_value_record(&self) -> Option<ValueRecord> {
        self.iter().skip(4).find_map(ValueRecord::cast)
    }
}

impl GposIgnore {
    pub(crate) fn rules(&self) -> impl Iterator<Item = IgnoreRule> + '_ {
        self.iter().filter_map(IgnoreRule::cast)
    }
}

impl LigatureComponent {
    /// If the iterator is empty this is a null anchor
    pub(crate) fn attachments(&self) -> impl Iterator<Item = AnchorMark> + '_ {
        self.iter().filter_map(AnchorMark::cast)
    }
}

impl AnchorMark {
    pub(crate) fn anchor(&self) -> Anchor {
        self.iter().find_map(Anchor::cast).unwrap()
    }

    pub(crate) fn mark_class_name(&self) -> Option<GlyphClassName> {
        self.iter().find_map(GlyphClassName::cast)
    }
}

impl ValueRecord {
    /// If this record is a single metric, return it
    pub(crate) fn advance(&self) -> Option<Metric> {
        self.iter().next().and_then(Metric::cast)
    }

    pub(crate) fn null(&self) -> Option<&Token> {
        self.iter()
            .take(3)
            .find(|t| t.kind() == Kind::NullKw)
            .and_then(NodeOrToken::as_token)
    }

    pub(crate) fn named(&self) -> Option<&Token> {
        self.find_token(Kind::Ident)
    }

    // for validation,
    pub(crate) fn all_metrics(&self) -> impl Iterator<Item = Metric> + '_ {
        self.iter().filter_map(Metric::cast)
    }

    pub(crate) fn placement(&self) -> Option<[Metric; 4]> {
        if self
            .iter()
            .filter(|t| t.kind() == Kind::Number || t.kind() == Kind::VariableMetricNode)
            .count()
            == 4
        {
            let mut iter = self.iter().filter_map(Metric::cast);
            return Some([
                iter.next().unwrap(),
                iter.next().unwrap(),
                iter.next().unwrap(),
                iter.next().unwrap(),
            ]);
        }
        None
    }

    pub(crate) fn device(&self) -> Option<[Device; 4]> {
        if self.iter().skip(4).any(|t| t.kind() == Kind::DeviceNode) {
            let mut iter = self.iter().filter_map(Device::cast);
            return Some([
                iter.next().unwrap(),
                iter.next().unwrap(),
                iter.next().unwrap(),
                iter.next().unwrap(),
            ]);
        }
        None
    }
}

impl Device {
    fn null(&self) -> Option<&Token> {
        self.iter()
            .take(4)
            .find(|t| t.kind() == Kind::NullKw)
            .and_then(NodeOrToken::as_token)
    }

    fn entries(&self) -> impl Iterator<Item = (Number, Number)> + '_ {
        let mut iter = self
            .iter()
            .filter(|i| i.kind() == Kind::Number || i.kind() == Kind::Comma);
        std::iter::from_fn(move || {
            let ppem = iter.next().and_then(Number::cast)?;
            let pixels = iter.next().and_then(Number::cast).unwrap();
            let _maybe_comma = iter.next();
            Some((ppem, pixels))
        })
    }

    pub(crate) fn compile(&self) -> Option<write_fonts::tables::layout::Device> {
        if self.null().is_some() {
            return None;
        }

        let mut entries = Vec::new();
        for (ppem, pix) in self.entries() {
            let ppem = ppem.parse_unsigned().expect("validated before now");
            let pix = pix.parse_signed();
            // if there are gaps in the range, add zeros
            if let Some(prev) = entries.last().map(|(pp, _)| *pp) {
                for missing in (prev + 1)..ppem {
                    entries.push((missing, 0));
                }
            }
            entries.push((ppem, pix));
        }

        let first = entries.first().unwrap().0;
        let last = entries.last().unwrap().0;
        let values = entries
            .into_iter()
            .map(|(_, pix)| i8::try_from(pix).expect("validated before now"))
            .collect::<Vec<_>>();

        Some(write_fonts::tables::layout::Device::new(
            first, last, &values,
        ))
    }
}

impl Table {
    pub(crate) fn tag(&self) -> Tag {
        self.node()
            .unwrap()
            .iter_children()
            .find_map(Tag::cast)
            .unwrap()
    }
}

impl BaseTable {
    pub(crate) fn horiz_base_tag_list(&self) -> Option<BaseTagList> {
        self.iter()
            .filter_map(BaseTagList::cast)
            .find(BaseTagList::is_horiz)
    }

    pub(crate) fn vert_base_tag_list(&self) -> Option<BaseTagList> {
        self.iter()
            .filter_map(BaseTagList::cast)
            .find(|b| !b.is_horiz())
    }

    pub(crate) fn horiz_base_script_record_list(&self) -> Option<BaseScriptList> {
        self.iter()
            .filter_map(BaseScriptList::cast)
            .find(BaseScriptList::is_horiz)
    }

    pub(crate) fn vert_base_script_record_list(&self) -> Option<BaseScriptList> {
        self.iter()
            .filter_map(BaseScriptList::cast)
            .find(|b| !b.is_horiz())
    }
}

impl BaseTagList {
    fn is_horiz(&self) -> bool {
        match self.iter().next().map(|t| t.kind()) {
            Some(Kind::HorizAxisBaseTagListKw) => true,
            Some(Kind::VertAxisBaseTagListKw) => false,
            other => panic!("unexpected token in BaseTagList {:?}", other),
        }
    }

    pub(crate) fn tags(&self) -> impl Iterator<Item = Tag> + '_ {
        self.iter()
            .skip(1)
            .take_while(|t| t.kind() != Kind::Semi)
            .filter_map(Tag::cast)
    }
}

impl BaseScriptList {
    fn is_horiz(&self) -> bool {
        match self.iter().next().map(|t| t.kind()) {
            Some(Kind::HorizAxisBaseScriptListKw) => true,
            Some(Kind::VertAxisBaseTagListKw) => false,
            other => panic!("unexpected token in BaseScriptList {:?}", other),
        }
    }

    pub(crate) fn script_records(&self) -> impl Iterator<Item = ScriptRecord> + '_ {
        self.iter()
            .skip(1)
            .take_while(|t| t.kind() != Kind::Semi)
            .filter_map(ScriptRecord::cast)
    }
}

impl ScriptRecord {
    pub(crate) fn script(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub(crate) fn default_baseline(&self) -> Tag {
        self.iter().filter_map(Tag::cast).nth(1).unwrap()
    }

    pub(crate) fn values(&self) -> impl Iterator<Item = Number> + '_ {
        self.iter().skip(2).filter_map(Number::cast)
    }
}

impl HheaTable {
    pub(crate) fn metrics(&self) -> impl Iterator<Item = MetricRecord> + '_ {
        self.iter().filter_map(MetricRecord::cast)
    }
}

impl VheaTable {
    pub(crate) fn metrics(&self) -> impl Iterator<Item = MetricRecord> + '_ {
        self.iter().filter_map(MetricRecord::cast)
    }
}

impl VmtxTable {
    pub(crate) fn statements(&self) -> impl Iterator<Item = VmtxEntry> + '_ {
        self.iter().filter_map(VmtxEntry::cast)
    }
}

impl VmtxEntry {
    pub(crate) fn keyword(&self) -> &Token {
        self.iter().next().and_then(NodeOrToken::as_token).unwrap()
    }

    pub(crate) fn glyph(&self) -> Glyph {
        self.iter().find_map(Glyph::cast).unwrap()
    }

    pub(crate) fn value(&self) -> Number {
        self.iter().find_map(Number::cast).unwrap()
    }
}

impl MetricRecord {
    pub(crate) fn keyword(&self) -> &Token {
        self.iter().next().and_then(|t| t.as_token()).unwrap()
    }

    pub(crate) fn metric(&self) -> Metric {
        self.iter().find_map(Metric::cast).unwrap()
    }
}

impl Metric {
    /// Returns the value of this metric if it is non-variable
    pub(crate) fn parse_simple(&self) -> Option<i16> {
        match self {
            Metric::Scalar(num) => Some(num.parse_signed()),
            Metric::Variable(_) => None,
        }
    }
}

impl VariableMetric {
    pub(crate) fn location_values(&self) -> impl Iterator<Item = LocationValue> + '_ {
        self.iter().filter_map(LocationValue::cast)
    }
}

impl LocationValue {
    pub(crate) fn location(&self) -> LocationSpec {
        self.iter().find_map(LocationSpec::cast).unwrap()
    }

    pub(crate) fn value(&self) -> Number {
        self.iter().find_map(Number::cast).unwrap()
    }
}

impl LocationSpec {
    pub(crate) fn items(&self) -> impl Iterator<Item = LocationSpecItem> + '_ {
        self.iter().filter_map(LocationSpecItem::cast)
    }
}

impl LocationSpecItem {
    pub(crate) fn axis_tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub(crate) fn value(&self) -> AxisLocation {
        self.iter().skip(2).find_map(AxisLocation::cast).unwrap()
    }
}

impl AxisLocation {
    pub(crate) fn parse(&self) -> crate::compile::AxisLocation {
        use crate::compile::AxisLocation as Output;
        let value = self.value();
        match self.suffix() {
            Some(token) if token.text() == "n" => Output::Normalized(value.into()),
            Some(token) if token.text() == "d" => Output::Design(value.into()),
            Some(token) if token.text() == "u" => Output::User(value.into()),
            None => Output::User(value.into()),
            Some(_) => unreachable!("we only parse three suffixes"),
        }
    }

    fn value(&self) -> f32 {
        let raw = self.iter().next().unwrap();
        Number::cast(raw)
            .map(|num| num.parse_signed() as f32)
            .or_else(|| Float::cast(raw).map(|num| num.parse()))
            .unwrap()
    }

    fn suffix(&self) -> Option<NumberSuffix> {
        self.iter().find_map(NumberSuffix::cast)
    }
}

impl Os2Table {
    pub(crate) fn statements(&self) -> impl Iterator<Item = Os2TableItem> + '_ {
        self.iter().filter_map(Os2TableItem::cast)
    }
}

impl NumberRecord {
    pub(crate) fn keyword(&self) -> &Token {
        self.iter().next().and_then(|t| t.as_token()).unwrap()
    }

    pub(crate) fn number(&self) -> Number {
        self.iter().find_map(Number::cast).unwrap()
    }
}

impl VendorRecord {
    pub(crate) fn value(&self) -> &Token {
        self.find_token(Kind::String).unwrap()
    }

    pub(crate) fn parse_tag(
        &self,
    ) -> Result<write_fonts::types::Tag, write_fonts::types::InvalidTag> {
        let raw = self.value();
        write_fonts::types::Tag::new_checked(raw.text.trim_matches('"').as_bytes())
    }
}

impl Os2NumberList {
    pub(crate) fn keyword(&self) -> &Token {
        self.iter().next().and_then(|t| t.as_token()).unwrap()
    }

    pub(crate) fn values(&self) -> impl Iterator<Item = Number> + '_ {
        self.iter().skip(1).filter_map(Number::cast)
    }
}

impl Os2FamilyClass {
    pub(crate) fn value(&self) -> DecOctHex {
        self.iter().find_map(DecOctHex::cast).unwrap()
    }
}

impl FeatureNames {
    pub(crate) fn statements(&self) -> impl Iterator<Item = NameSpec> + '_ {
        self.iter().filter_map(NameSpec::cast)
    }
}

impl CvParameters {
    pub(crate) fn keyword(&self) -> &Token {
        debug_assert_eq!(self.iter().next().unwrap().kind(), Kind::CvParametersKw);
        self.iter().next().and_then(|t| t.as_token()).unwrap()
    }

    pub(crate) fn find_node(&self, kind: Kind) -> Option<CvParametersName> {
        self.iter()
            .filter_map(CvParametersName::cast)
            .find(|node| node.keyword().kind == kind)
    }

    pub(crate) fn feat_ui_label_name(&self) -> Option<CvParametersName> {
        self.find_node(Kind::FeatUiLabelNameIdKw)
    }

    pub(crate) fn feat_tooltip_text_name(&self) -> Option<CvParametersName> {
        self.find_node(Kind::FeatUiTooltipTextNameIdKw)
    }

    pub(crate) fn sample_text_name(&self) -> Option<CvParametersName> {
        self.find_node(Kind::SampleTextNameIdKw)
    }

    pub(crate) fn param_ui_label_name(&self) -> impl Iterator<Item = CvParametersName> + '_ {
        self.iter()
            .filter_map(CvParametersName::cast)
            .filter(|node| node.keyword().kind == Kind::ParamUiLabelNameIdKw)
    }

    pub(crate) fn characters(&self) -> impl Iterator<Item = CvParametersChar> + '_ {
        self.iter().filter_map(CvParametersChar::cast)
    }
}

impl CvParametersName {
    pub(crate) fn keyword(&self) -> &Token {
        self.iter().next().and_then(|t| t.as_token()).unwrap()
    }

    pub(crate) fn statements(&self) -> impl Iterator<Item = NameSpec> + '_ {
        self.iter().filter_map(NameSpec::cast)
    }
}

impl CvParametersChar {
    pub(crate) fn value(&self) -> DecOctHex {
        self.iter().find_map(DecOctHex::cast).unwrap()
    }
}

impl NameTable {
    pub(crate) fn statements(&self) -> impl Iterator<Item = NameRecord> + '_ {
        self.iter().filter_map(NameRecord::cast)
    }
}

impl NameRecord {
    pub(crate) fn name_id(&self) -> DecOctHex {
        self.iter().find_map(DecOctHex::cast).unwrap()
    }

    pub(crate) fn entry(&self) -> NameSpec {
        self.iter().find_map(NameSpec::cast).unwrap()
    }
}

impl NameSpec {
    pub(crate) fn platform_id(&self) -> Option<DecOctHex> {
        self.iter().find_map(DecOctHex::cast)
    }

    pub(crate) fn platform_and_language_ids(&self) -> Option<(DecOctHex, DecOctHex)> {
        let mut iter = self.iter().filter_map(DecOctHex::cast).skip(1);
        if let Some(platform) = iter.next() {
            let language = iter.next().unwrap();
            Some((platform, language))
        } else {
            None
        }
    }

    pub(crate) fn string(&self) -> &Token {
        self.find_token(Kind::String).unwrap()
    }
}

impl DecOctHex {
    fn parse_raw(&self) -> Result<u32, String> {
        match self {
            DecOctHex::Decimal(num) => num.text().parse::<u32>().map_err(|e| e.to_string()),
            DecOctHex::Octal(num) => u32::from_str_radix(num.text(), 8).map_err(|e| e.to_string()),
            DecOctHex::Hex(num) => u32::from_str_radix(num.text().trim_start_matches("0x"), 16)
                .map_err(|e| e.to_string()),
        }
    }

    pub(crate) fn parse(&self) -> Result<u16, String> {
        self.parse_raw()
            .and_then(|x| u16::try_from(x).map_err(|e| e.to_string()))
    }

    pub(crate) fn parse_char(&self) -> Result<char, String> {
        self.parse_raw().and_then(|int| {
            char::from_u32(int).ok_or_else(|| format!("{int} is not a unicode codepoint"))
        })
    }
}

impl GdefTable {
    pub(crate) fn statements(&self) -> impl Iterator<Item = GdefTableItem> + '_ {
        self.iter().filter_map(GdefTableItem::cast)
    }
}

impl GdefClassDef {
    fn nth_item(&self, n: usize) -> Option<GlyphClass> {
        assert!(n < 4);
        self.iter()
            .filter(|t| t.kind() == Kind::GdefClassDefEntryNode)
            .nth(n)
            .and_then(GdefClassDefEntry::cast)
            .expect("validated")
            .iter()
            .find_map(GlyphClass::cast)
    }

    pub(crate) fn base_glyphs(&self) -> Option<GlyphClass> {
        self.nth_item(0)
    }

    pub(crate) fn ligature_glyphs(&self) -> Option<GlyphClass> {
        self.nth_item(1)
    }

    pub(crate) fn mark_glyphs(&self) -> Option<GlyphClass> {
        self.nth_item(2)
    }

    pub(crate) fn component_glyphs(&self) -> Option<GlyphClass> {
        self.nth_item(3)
    }
}

impl GdefAttach {
    pub(crate) fn target(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    /// of a contourpoint
    pub(crate) fn indices(&self) -> impl Iterator<Item = Number> + '_ {
        self.iter().filter_map(Number::cast)
    }
}

impl GdefLigatureCaret {
    fn by_pos(&self) -> bool {
        match self.iter().next().map(|t| t.kind()) {
            Some(Kind::LigatureCaretByPosKw) => true,
            Some(Kind::LigatureCaretByIndexKw) => false,
            other => panic!("unexpected token in ligaturecaret {:?}", other),
        }
    }

    pub(crate) fn target(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    pub(crate) fn values(&self) -> LigatureCaretValue {
        if self.by_pos() {
            LigatureCaretValue::Pos(LigatureCaretIter(self))
        } else {
            LigatureCaretValue::Index(LigatureCaretIter(self))
        }
    }
}

// some helpers for handling the different caret representations; one is signed,
// the other unsigned.
pub(crate) struct LigatureCaretIter<'a>(&'a GdefLigatureCaret);

impl LigatureCaretIter<'_> {
    pub(crate) fn values(&self) -> impl Iterator<Item = Number> + '_ {
        self.0.iter().filter_map(Number::cast)
    }
}

pub(crate) enum LigatureCaretValue<'a> {
    Pos(LigatureCaretIter<'a>),
    Index(LigatureCaretIter<'a>),
}

impl HeadTable {
    pub(crate) fn statements(&self) -> impl Iterator<Item = HeadFontRevision> + '_ {
        self.iter().filter_map(HeadFontRevision::cast)
    }
}

impl HeadFontRevision {
    pub(crate) fn value(&self) -> Float {
        self.iter().find_map(Float::cast).unwrap()
    }
}

impl StatTable {
    pub(crate) fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub(crate) fn statements(&self) -> impl Iterator<Item = StatTableItem> + '_ {
        self.iter().filter_map(StatTableItem::cast)
    }
}

impl StatElidedFallbackName {
    pub(crate) fn elided_fallback_name_id(&self) -> Option<Number> {
        self.iter()
            .take_while(|t| t.kind() != Kind::NameKw)
            .find_map(Number::cast)
    }

    pub(crate) fn names(&self) -> impl Iterator<Item = NameSpec> + '_ {
        self.iter().filter_map(NameSpec::cast)
    }
}

impl StatDesignAxis {
    pub(crate) fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub(crate) fn ordering(&self) -> Number {
        self.iter()
            .take_while(|t| t.kind() != Kind::LBrace)
            .find_map(Number::cast)
            .unwrap()
    }

    pub(crate) fn names(&self) -> impl Iterator<Item = NameSpec> + '_ {
        self.iter()
            .skip_while(|t| t.kind() != Kind::LBrace)
            .filter_map(NameSpec::cast)
    }
}

impl StatAxisValue {
    pub(crate) fn statements(&self) -> impl Iterator<Item = StatAxisValueItem> + '_ {
        self.iter().skip(2).filter_map(StatAxisValueItem::cast)
    }
}

impl StatAxisFlag {
    /// iterate bits to be accumulated
    pub(crate) fn bits(&self) -> impl Iterator<Item = u16> + '_ {
        self.iter()
            .skip(1)
            .take_while(|t| t.kind() != Kind::Semi)
            .filter_map(|t| match t.kind() {
                Kind::OlderSiblingFontAttributeKw => Some(0x01),
                Kind::ElidableAxisValueNameKw => Some(0x02),
                t if t.is_trivia() => None,
                other => panic!("parser error '{}'", other),
            })
    }
}

impl StatAxisLocation {
    pub(crate) fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub(crate) fn value(&self) -> StatLocationValue {
        let mut iter = self.iter().filter_map(FloatLike::cast);
        let first = iter.next().unwrap();
        let second = match iter.next() {
            Some(second) => second,
            None => return StatLocationValue::Value(first),
        };
        match iter.next() {
            Some(third) => StatLocationValue::MinMax {
                nominal: first,
                min: second,
                max: third,
            },
            None => StatLocationValue::Linked {
                value: first,
                linked: second,
            },
        }
    }
}

pub(crate) enum StatLocationValue {
    Value(FloatLike),
    MinMax {
        nominal: FloatLike,
        min: FloatLike,
        max: FloatLike,
    },
    Linked {
        value: FloatLike,
        linked: FloatLike,
    },
}

impl SizeMenuName {
    pub(crate) fn spec(&self) -> NameSpec {
        self.iter().find_map(NameSpec::cast).unwrap()
    }
}

impl Parameters {
    pub(crate) fn design_size(&self) -> FloatLike {
        self.iter().find_map(FloatLike::cast).unwrap()
    }

    pub(crate) fn subfamily(&self) -> Number {
        self.iter()
            .filter(|t| t.kind() == Kind::Number || t.kind() == Kind::Float)
            .nth(1)
            .and_then(Number::cast)
            .unwrap()
    }

    pub(crate) fn range_start(&self) -> Option<FloatLike> {
        self.iter().filter_map(FloatLike::cast).nth(2)
    }

    pub(crate) fn range_end(&self) -> Option<FloatLike> {
        self.iter().filter_map(FloatLike::cast).nth(3)
    }
}

impl FeatureRef {
    pub(crate) fn keyword(&self) -> &Token {
        self.find_token(Kind::FeatureKw).unwrap()
    }

    pub(crate) fn feature(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }
}
