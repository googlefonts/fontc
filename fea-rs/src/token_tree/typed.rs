//! typing for ast nodes. based on rust-analyzer.

use std::ops::Range;

use smol_str::SmolStr;

use crate::{Kind, Node, NodeOrToken};

use super::Token;

pub trait AstNode {
    fn cast(node: &NodeOrToken) -> Option<Self>
    where
        Self: Sized;

    fn range(&self) -> Range<usize>;
}

macro_rules! ast_token {
    ($typ:ident, $kind:expr) => {
        #[derive(Clone, Debug)]
        pub struct $typ {
            inner: Token,
        }

        impl $typ {
            #[allow(unused)]
            pub fn text(&self) -> &SmolStr {
                &self.inner.text
            }

            #[allow(unused)]
            pub fn token(&self) -> &Token {
                &self.inner
            }

            // just used for the ast_enum macro
            #[allow(dead_code)]
            pub fn node_(&self) -> Option<&Node> {
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

macro_rules! ast_node {
    ($typ:ident, $kind:expr) => {
        #[derive(Clone, Debug)]
        pub struct $typ {
            inner: Node,
        }

        impl $typ {
            pub fn try_from_node(node: &Node) -> Option<Self> {
                if node.kind == $kind {
                    return Some(Self {
                        inner: node.clone(),
                    });
                }
                None
            }

            #[allow(dead_code)]
            pub fn find_token(&self, kind: Kind) -> Option<&Token> {
                self.iter()
                    .find(|t| t.kind() == kind)
                    .and_then(NodeOrToken::as_token)
            }

            #[allow(unused)]
            pub fn iter(&self) -> impl Iterator<Item = &NodeOrToken> {
                self.inner.iter_children()
            }

            #[allow(dead_code)]
            pub fn node(&self) -> &Node {
                &self.inner
            }

            // just used for the ast_enum macro
            #[allow(dead_code)]
            pub fn node_(&self) -> Option<&Node> {
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
            pub fn node(&self) -> Option<&Node> {
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
ast_token!(Octal, Kind::Octal);
ast_token!(Hex, Kind::Hex);
ast_token!(Metric, Kind::Metric);
ast_token!(Null, Kind::NullKw);
ast_token!(Fixed32, Kind::Float);
ast_node!(Root, Kind::SourceFile);
ast_node!(GlyphRange, Kind::GlyphRange);
ast_node!(GlyphClassDef, Kind::GlyphClassDefNode);
ast_node!(MarkClassDef, Kind::MarkClassNode);
ast_node!(Anchor, Kind::AnchorNode);
ast_node!(AnchorDef, Kind::AnchorDefNode);
ast_node!(ValueRecordDef, Kind::ValueRecordDefKw);
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
ast_node!(NameRecordEntry, Kind::NameRecordEntryNode);

ast_enum!(DecOctHex {
    Decimal(Number),
    Octal(Octal),
    Hex(Hex),
});

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

impl Root {
    pub fn statements(&self) -> impl Iterator<Item = &NodeOrToken> {
        self.iter().filter(|t| !t.kind().is_trivia())
    }
}

impl LanguageSystem {
    pub fn script(&self) -> Tag {
        self.inner.iter_children().find_map(Tag::cast).unwrap()
    }

    pub fn language(&self) -> Tag {
        self.inner
            .iter_children()
            .skip_while(|t| t.kind() != Kind::Tag)
            .skip(1)
            .find_map(Tag::cast)
            .unwrap()
    }
}

impl Tag {
    pub fn parse(&self) -> Result<fonttools::types::Tag, fonttools::types::InvalidTag> {
        self.inner.text.parse()
    }

    pub fn to_raw(&self) -> fonttools::types::Tag {
        self.text()
            //.as_bytes()
            .parse()
            .expect("tag is exactly 4 bytes")
    }
}

impl GlyphClassDef {
    pub fn class_name(&self) -> GlyphClassName {
        self.inner
            .iter_children()
            .find_map(GlyphClassName::cast)
            .unwrap()
    }

    pub fn class_alias(&self) -> Option<GlyphClassName> {
        //TODO: ensure this returns non in presence of named glyph class inside class block
        self.iter()
            .skip_while(|t| t.kind() != Kind::Eq)
            .find_map(GlyphClassName::cast)
    }

    pub fn class_def(&self) -> Option<GlyphClassLiteral> {
        self.inner.iter_children().find_map(GlyphClassLiteral::cast)
    }
}

impl GlyphClassLiteral {
    pub fn items(&self) -> impl Iterator<Item = &NodeOrToken> {
        self.iter()
            .skip_while(|t| t.kind() != Kind::LSquare)
            .skip(1)
            .take_while(|t| t.kind() != Kind::RSquare)
            .filter(|t| !t.kind().is_trivia())
    }
}

impl Cid {
    pub fn parse(&self) -> u16 {
        self.inner.text.parse().expect("cid is already validated")
    }
}

impl GlyphRange {
    pub fn start(&self) -> &Token {
        self.iter()
            .find(|i| i.kind() == Kind::Cid || i.kind() == Kind::GlyphName)
            .and_then(NodeOrToken::as_token)
            .unwrap()
    }

    pub fn end(&self) -> &Token {
        self.iter()
            .skip_while(|t| t.kind() != Kind::Hyphen)
            .find(|i| i.kind() == Kind::Cid || i.kind() == Kind::GlyphName)
            .and_then(NodeOrToken::as_token)
            .unwrap()
    }
}

impl MarkClassDef {
    pub fn keyword(&self) -> &Token {
        self.find_token(Kind::MarkClassKw).unwrap()
    }

    pub fn glyph_class(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).expect("validated")
    }

    pub fn anchor(&self) -> Anchor {
        self.iter().find_map(Anchor::cast).unwrap()
    }

    pub fn mark_class_name(&self) -> GlyphClassName {
        self.iter()
            .skip_while(|t| t.kind() != Kind::AnchorNode)
            .find_map(GlyphClassName::cast)
            .unwrap()
    }
}

impl AnchorDef {
    pub fn anchor(&self) -> Anchor {
        self.iter().find_map(Anchor::cast).unwrap()
    }

    pub fn name(&self) -> &Token {
        self.find_token(Kind::Ident).expect("pre-validated")
    }
}

impl Anchor {
    pub fn coords(&self) -> Option<(Metric, Metric)> {
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

    pub fn contourpoint(&self) -> Option<Number> {
        self.iter().find_map(Number::cast)
    }

    pub fn null(&self) -> Option<&Token> {
        self.find_token(Kind::NullKw)
    }

    pub fn name(&self) -> Option<&Token> {
        self.find_token(Kind::Ident)
    }
}

impl Number {
    pub fn parse_signed(&self) -> i16 {
        self.text().parse().expect("already validated")
    }

    pub fn parse_unsigned(&self) -> Option<u16> {
        self.text().parse().ok()
    }
}

impl Metric {
    pub fn parse(&self) -> i16 {
        self.text().parse().expect("already validated")
    }
}

impl Feature {
    pub fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub fn statements(&self) -> impl Iterator<Item = &NodeOrToken> {
        self.iter()
            .skip_while(|t| t.kind() != Kind::LBrace)
            .skip(1)
            .filter(|t| !t.kind().is_trivia())
            .take_while(|t| t.kind() != Kind::RBrace)
    }
}

impl LookupBlock {
    pub fn tag(&self) -> &Token {
        self.find_token(Kind::Label).unwrap()
    }

    pub fn use_extension(&self) -> Option<&Token> {
        self.iter()
            .take_while(|t| t.kind() != Kind::LBrace)
            .find(|t| t.kind() == Kind::UseExtensionKw)
            .and_then(NodeOrToken::as_token)
    }

    pub fn keyword(&self) -> &Token {
        self.find_token(Kind::LookupKw).unwrap()
    }

    pub fn label(&self) -> &Token {
        self.find_token(Kind::Label).unwrap()
    }

    pub fn statements(&self) -> impl Iterator<Item = &NodeOrToken> {
        self.iter()
            .skip_while(|t| t.kind() != Kind::LBrace)
            .skip(1)
            .filter(|t| !t.kind().is_trivia())
            .take_while(|t| t.kind() != Kind::RBrace)
    }
}

impl Script {
    pub fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }
}

impl Language {
    pub fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub fn include_dflt(&self) -> Option<&Token> {
        self.find_token(Kind::IncludeDfltKw)
    }

    pub fn exclude_dflt(&self) -> Option<&Token> {
        self.find_token(Kind::ExcludeDfltKw)
    }

    pub fn required(&self) -> Option<&Token> {
        self.find_token(Kind::RequiredKw)
    }
}

impl LookupFlag {
    pub fn number(&self) -> Option<Number> {
        self.iter().find_map(Number::cast)
    }

    pub fn values(&self) -> impl Iterator<Item = &NodeOrToken> + '_ {
        self.iter()
            .skip(1)
            .take_while(|t| t.kind() != Kind::Number && t.kind() != Kind::Semi)
            .filter(|t| !t.kind().is_trivia())
    }
}

impl LookupRef {
    pub fn label(&self) -> &Token {
        self.find_token(Kind::Ident).unwrap()
    }

    #[allow(dead_code)]
    pub fn use_extension(&self) -> Option<&Token> {
        self.iter()
            .take_while(|t| t.kind() != Kind::LBrace)
            .find(|t| t.kind() == Kind::UseExtensionKw)
            .and_then(NodeOrToken::as_token)
    }
}

impl Gsub1 {
    pub fn target(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    pub fn replacement(&self) -> GlyphOrClass {
        self.iter()
            .skip_while(|t| t.kind() != Kind::ByKw)
            .find_map(GlyphOrClass::cast)
            .unwrap()
    }
}

impl Gsub2 {
    pub fn target(&self) -> Glyph {
        self.iter().find_map(Glyph::cast).unwrap()
    }

    pub fn replacement(&self) -> impl Iterator<Item = Glyph> + '_ {
        self.iter()
            .skip_while(|t| t.kind() != Kind::ByKw)
            .skip(1)
            .filter_map(Glyph::cast)
    }
}

impl Gsub3 {
    pub fn target(&self) -> Glyph {
        self.iter().find_map(Glyph::cast).unwrap()
    }

    pub fn alternates(&self) -> GlyphClass {
        self.iter()
            .skip_while(|t| t.kind() != Kind::FromKw)
            .find_map(GlyphClass::cast)
            .unwrap()
    }
}

impl Gsub4 {
    pub fn target(&self) -> impl Iterator<Item = GlyphOrClass> + '_ {
        self.iter()
            .take_while(|t| t.kind() != Kind::ByKw)
            .filter_map(GlyphOrClass::cast)
    }

    pub fn replacement(&self) -> Glyph {
        self.iter()
            .skip_while(|t| t.kind() != Kind::ByKw)
            .find_map(Glyph::cast)
            .unwrap()
    }
}

impl Gpos1 {
    pub fn target(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    pub fn value(&self) -> ValueRecord {
        self.iter().find_map(ValueRecord::cast).unwrap()
    }
}

impl Gpos2 {
    pub fn enum_(&self) -> Option<&Token> {
        self.iter()
            .take_while(|t| t.kind() != Kind::PosKw)
            .find(|t| t.kind() == Kind::EnumKw)
            .and_then(NodeOrToken::as_token)
    }

    pub fn first_item(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    pub fn second_item(&self) -> GlyphOrClass {
        self.iter().filter_map(GlyphOrClass::cast).nth(1).unwrap()
    }

    pub fn first_value(&self) -> ValueRecord {
        self.iter().find_map(ValueRecord::cast).unwrap()
    }

    pub fn second_value(&self) -> Option<ValueRecord> {
        self.iter().filter_map(ValueRecord::cast).nth(1)
    }
}

impl Gpos3 {
    pub fn target(&self) -> GlyphOrClass {
        self.iter()
            .filter(|t| !t.kind().is_trivia())
            .nth(2)
            .and_then(GlyphOrClass::cast)
            .unwrap()
    }

    pub fn entry(&self) -> Anchor {
        self.iter().skip(3).find_map(Anchor::cast).unwrap()
    }

    pub fn exit(&self) -> Anchor {
        self.iter()
            .skip_while(|t| t.kind() != Kind::AnchorNode)
            .skip(1)
            .find_map(Anchor::cast)
            .unwrap()
    }
}

impl Gpos4 {
    pub fn base(&self) -> GlyphOrClass {
        self.iter()
            .filter(|t| !t.kind().is_trivia())
            .nth(2)
            .and_then(GlyphOrClass::cast)
            .unwrap()
    }

    pub fn attachments(&self) -> impl Iterator<Item = AnchorMark> + '_ {
        self.iter().skip(3).filter_map(AnchorMark::cast)
    }
}

impl Gpos5 {
    pub fn base(&self) -> GlyphOrClass {
        self.iter()
            .filter(|t| !t.kind().is_trivia())
            .nth(2)
            .and_then(GlyphOrClass::cast)
            .unwrap()
    }

    pub fn ligature_components(&self) -> impl Iterator<Item = LigatureComponent> + '_ {
        self.iter().skip(3).filter_map(LigatureComponent::cast)
    }
}

impl Gpos6 {
    pub fn base(&self) -> GlyphOrClass {
        self.iter()
            .filter(|t| !t.kind().is_trivia())
            .nth(2)
            .and_then(GlyphOrClass::cast)
            .unwrap()
    }

    pub fn attachments(&self) -> impl Iterator<Item = AnchorMark> + '_ {
        self.iter().skip(3).filter_map(AnchorMark::cast)
    }
}

impl LigatureComponent {
    /// If the iterator is empty this is a null anchor
    pub fn attachments(&self) -> impl Iterator<Item = AnchorMark> + '_ {
        self.iter().filter_map(AnchorMark::cast)
    }
}

impl AnchorMark {
    pub fn anchor(&self) -> Anchor {
        self.iter().find_map(Anchor::cast).unwrap()
    }

    pub fn mark_class_name(&self) -> Option<GlyphClassName> {
        self.iter().find_map(GlyphClassName::cast)
    }
}

impl ValueRecord {
    pub fn advance(&self) -> Option<Number> {
        self.iter().next().and_then(Number::cast)
    }

    pub fn null(&self) -> Option<&Token> {
        self.iter()
            .take(3)
            .find(|t| t.kind() == Kind::NullKw)
            .and_then(NodeOrToken::as_token)
    }

    pub fn named(&self) -> Option<&Token> {
        self.find_token(Kind::Ident)
    }

    pub fn placement(&self) -> Option<[Number; 4]> {
        if self
            .iter()
            .filter(|t| t.kind() == Kind::Number || t.kind() == Kind::DeviceKw)
            .count()
            == 4
        {
            let mut iter = self.iter().filter_map(Number::cast);
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

impl BaseTable {
    pub fn horiz_base_tag_list(&self) -> Option<BaseTagList> {
        self.iter()
            .filter_map(BaseTagList::cast)
            .find(BaseTagList::is_horiz)
    }

    pub fn vert_base_tag_list(&self) -> Option<BaseTagList> {
        self.iter()
            .filter_map(BaseTagList::cast)
            .find(|b| !b.is_horiz())
    }

    pub fn horiz_base_script_record_list(&self) -> Option<BaseScriptList> {
        self.iter()
            .filter_map(BaseScriptList::cast)
            .find(BaseScriptList::is_horiz)
    }

    pub fn vert_base_script_record_list(&self) -> Option<BaseScriptList> {
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

    pub fn tags(&self) -> impl Iterator<Item = Tag> + '_ {
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

    pub fn script_records(&self) -> impl Iterator<Item = ScriptRecord> + '_ {
        self.iter()
            .skip(1)
            .take_while(|t| t.kind() != Kind::Semi)
            .filter_map(ScriptRecord::cast)
    }
}

impl ScriptRecord {
    pub fn script(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub fn default_baseline(&self) -> Tag {
        self.iter().filter_map(Tag::cast).nth(1).unwrap()
    }

    pub fn values(&self) -> impl Iterator<Item = Number> + '_ {
        self.iter().skip(2).filter_map(Number::cast)
    }
}

impl HheaTable {
    pub fn metrics(&self) -> impl Iterator<Item = MetricRecord> + '_ {
        self.iter().filter_map(MetricRecord::cast)
    }
}

impl VheaTable {
    pub fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub fn metrics(&self) -> impl Iterator<Item = MetricRecord> + '_ {
        self.iter().filter_map(MetricRecord::cast)
    }
}

impl MetricRecord {
    pub fn keyword(&self) -> &Token {
        self.iter().next().and_then(|t| t.as_token()).unwrap()
    }

    pub fn metric(&self) -> Metric {
        self.iter().find_map(Metric::cast).unwrap()
    }
}

impl Os2Table {
    pub fn statements(&self) -> impl Iterator<Item = Os2TableItem> + '_ {
        self.iter().filter_map(Os2TableItem::cast)
    }
}

impl NumberRecord {
    pub fn keyword(&self) -> &Token {
        self.iter().next().and_then(|t| t.as_token()).unwrap()
    }

    pub fn number(&self) -> Number {
        self.iter().find_map(Number::cast).unwrap()
    }
}

impl VendorRecord {
    pub fn keyword(&self) -> &Token {
        self.iter().next().and_then(|t| t.as_token()).unwrap()
    }

    pub fn value(&self) -> &Token {
        self.find_token(Kind::String).unwrap()
    }
}

impl Os2NumberList {
    pub fn keyword(&self) -> &Token {
        self.iter().next().and_then(|t| t.as_token()).unwrap()
    }

    pub fn values(&self) -> impl Iterator<Item = Number> + '_ {
        self.iter().skip(1).filter_map(Number::cast)
    }
}

impl Os2FamilyClass {
    pub fn value(&self) -> DecOctHex {
        self.iter().find_map(DecOctHex::cast).unwrap()
    }
}

impl NameTable {
    pub fn statements(&self) -> impl Iterator<Item = NameRecord> + '_ {
        self.iter().filter_map(NameRecord::cast)
    }
}

impl NameRecord {
    pub fn name_id(&self) -> DecOctHex {
        self.iter().find_map(DecOctHex::cast).unwrap()
    }

    pub fn entry(&self) -> NameRecordEntry {
        self.iter().find_map(NameRecordEntry::cast).unwrap()
    }
}

impl NameRecordEntry {
    pub fn platform_id(&self) -> Option<DecOctHex> {
        self.iter().find_map(DecOctHex::cast)
    }

    pub fn platform_and_language_ids(&self) -> Option<(DecOctHex, DecOctHex)> {
        let mut iter = self.iter().filter_map(DecOctHex::cast).skip(1);
        if let Some(platform) = iter.next() {
            let language = iter.next().unwrap();
            Some((platform, language))
        } else {
            None
        }
    }

    pub fn string(&self) -> &Token {
        self.find_token(Kind::String).unwrap()
    }
}

impl DecOctHex {
    pub fn parse(&self) -> Result<u16, String> {
        match self {
            DecOctHex::Decimal(num) => num.text().parse::<u16>().map_err(|e| e.to_string()),
            DecOctHex::Octal(num) => u16::from_str_radix(num.text(), 8).map_err(|e| e.to_string()),
            DecOctHex::Hex(num) => u16::from_str_radix(num.text().trim_start_matches("0x"), 16)
                .map_err(|e| e.to_string()),
        }
    }
}

impl GdefTable {
    pub fn statements(&self) -> impl Iterator<Item = GdefTableItem> + '_ {
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

    pub fn base_glyphs(&self) -> Option<GlyphClass> {
        self.nth_item(0)
    }

    pub fn ligature_glyphs(&self) -> Option<GlyphClass> {
        self.nth_item(1)
    }

    pub fn mark_glyphs(&self) -> Option<GlyphClass> {
        self.nth_item(2)
    }

    pub fn component_glyphs(&self) -> Option<GlyphClass> {
        self.nth_item(3)
    }
}

impl GdefAttach {
    pub fn target(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    /// of a contourpoint
    pub fn indices(&self) -> impl Iterator<Item = Number> + '_ {
        self.iter().filter_map(Number::cast)
    }
}

impl GdefLigatureCaret {
    pub fn by_pos(&self) -> bool {
        match self.iter().next().map(|t| t.kind()) {
            Some(Kind::LigatureCaretByPosKw) => true,
            Some(Kind::LigatureCaretByIndexKw) => false,
            other => panic!("unexpected token in ligaturecaret {:?}", other),
        }
    }

    pub fn target(&self) -> GlyphOrClass {
        self.iter().find_map(GlyphOrClass::cast).unwrap()
    }

    pub fn values(&self) -> impl Iterator<Item = Number> + '_ {
        self.iter().filter_map(Number::cast)
    }
}

impl HeadTable {
    pub fn statements(&self) -> impl Iterator<Item = HeadFontRevision> + '_ {
        self.iter().filter_map(HeadFontRevision::cast)
    }
}

impl HeadFontRevision {
    pub fn value(&self) -> Fixed32 {
        self.iter().find_map(Fixed32::cast).unwrap()
    }
}
