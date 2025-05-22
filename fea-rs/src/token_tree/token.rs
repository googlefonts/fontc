/// Kinds of tokens assigned during lexing and parsing.
///
/// This includes the set of raw tokens that are generated during lexing,
/// but also includes richer information about the parsed FEA that is used to
/// assign type information to collections of tokens ('nodes').
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
#[repr(u16)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Kind {
    Eof, // the end of the input stream
    // a name or a keyword or any other block of non-whitespace.
    // we will frequently have to disambiguate this based on context.
    Ident,

    String,
    StringUnterminated, // an error handled at a higher level
    Number,
    Octal,
    Hex,      // an error handled at a higher level
    HexEmpty, // naked 0x
    Float,

    // experimental
    NumberSuffix, // 'n' 'd' or 'u'

    Whitespace,
    Comment,

    // special symbols
    Semi,
    Colon,
    Comma,
    Backslash,
    Hyphen,
    Eq,
    LBrace,
    RBrace,
    LSquare,
    RSquare,
    LParen,
    RParen,
    LAngle,
    RAngle,
    SingleQuote,

    NamedGlyphClass,
    Cid,

    // top-level keywords
    TableKw,
    LookupKw,
    LanguagesystemKw,
    AnchorDefKw,
    FeatureKw,
    MarkClassKw,
    AnonKw, // 'anon' and 'anonymous'
    ConditionSetKw,
    VariationKw,

    // other keywords
    AnchorKw,
    ByKw,
    ContourpointKw,
    CursiveKw,
    DeviceKw,
    EnumKw, // 'enum' and 'enumerate'
    ExcludeDfltKw,
    FromKw,
    IgnoreKw,
    IgnoreBaseGlyphsKw,
    IgnoreLigaturesKw,
    IgnoreMarksKw,
    IncludeKw,
    IncludeDfltKw,
    LanguageKw,
    LookupflagKw,
    MarkKw,
    MarkAttachmentTypeKw,
    NameIdKw,
    NullKw,
    ParametersKw,
    PosKw, // 'pos' and 'position'
    RequiredKw,
    RightToLeftKw,
    RsubKw, // 'rsub' and 'reversesub'
    ScriptKw,
    SubKw, // 'sub' and 'substitute'
    SubtableKw,
    UseExtensionKw,
    UseMarkFilteringSetKw,
    ValueRecordDefKw,

    // keywords only in specific table contexts:
    HorizAxisBaseScriptListKw,   //BASE table
    HorizAxisBaseTagListKw,      //BASE table
    HorizAxisMinMaxKw,           //BASE table
    VertAxisBaseScriptListKw,    //BASE table
    VertAxisBaseTagListKw,       //BASE table
    VertAxisMinMaxKw,            //BASE table
    AttachKw,                    //GDEF table
    GlyphClassDefKw,             //GDEF table
    LigatureCaretByDevKw,        //GDEF table
    LigatureCaretByIndexKw,      //GDEF table
    LigatureCaretByPosKw,        //GDEF table
    MarkAttachClassKw,           //GDEF table
    FontRevisionKw,              //head table
    AscenderKw,                  //hhea table
    CaretOffsetKw,               //hhea table
    DescenderKw,                 //hhea table
    LineGapKw,                   //hhea table
    CapHeightKw,                 //OS/2 table
    CodePageRangeKw,             //OS/2 table
    PanoseKw,                    //OS/2 table
    TypoAscenderKw,              //OS/2 table
    TypoDescenderKw,             //OS/2 table
    TypoLineGapKw,               //OS/2 table
    UnicodeRangeKw,              //OS/2 table
    VendorKw,                    //OS/2 table
    WinAscentKw,                 //OS/2 table
    WinDescentKw,                //OS/2 table
    XHeightKw,                   //OS/2 table
    SizemenunameKw,              //size feature
    VertTypoAscenderKw,          //vhea table
    VertTypoDescenderKw,         //vhea table
    VertTypoLineGapKw,           //vhea table
    VertAdvanceYKw,              //vmtx table
    VertOriginYKw,               //vmtx table
    ElidedFallbackNameKw,        //STAT table
    ElidedFallbackNameIDKw,      //STAT table
    DesignAxisKw,                //STAT table
    AxisValueKw,                 //STAT table
    FlagKw,                      //STAT table
    LocationKw,                  //STAT table
    ElidableAxisValueNameKw,     //STAT table
    OlderSiblingFontAttributeKw, //STAT table

    // not technically a keyword but we lex and treat contextually:
    FeatureNamesKw,            // ss01-ss20
    NameKw,                    // ss01-ss20
    CvParametersKw,            // cv01-cv99
    FeatUiLabelNameIdKw,       // cv01-cv99
    FeatUiTooltipTextNameIdKw, // cv01-cv99
    SampleTextNameIdKw,        // cv01-cv99
    ParamUiLabelNameIdKw,      // cv01-cv99
    CharacterKw,               // cv01-cv99
    Path,

    SourceFile, // scope of a file

    // not technically keywords and not lexed, but assigned during parsing
    // in gsub/gpos:
    LigatureKw,
    BaseKw,

    // not lexed
    GlyphRange,
    //Metric,
    Label,
    Tag,
    GlyphName,
    // an ambiguious name, like a-z, which requires a glyphset to disambiguate.
    GlyphNameOrRange,
    GlyphClass,

    // general purpose table node
    TableEntryNode,
    // ## node-only tokens, assigned during parsing ##

    // a catchall, includes gsub nodes with errors
    GsubNode,
    // a contextual or chaining contextual rule that needs to be rewritten.
    // when the sink sees a node finished with this type, it rewrites it before
    // adding it to the parent.
    GsubNodeNeedsRewrite,

    /// Single
    GsubType1,
    /// Multiple
    GsubType2,
    /// Alternate
    GsubType3,
    /// Ligature
    GsubType4,
    /// Contextual
    GsubType5,
    /// Chaining contextual
    GsubType6,
    /// Extension
    GsubType7,
    /// Reverse chaining contextual
    GsubType8,
    GsubIgnore,

    // catchall, including gpos nodes with errors
    GposNode,
    // A node containing marked glyphs, and which needs to be rewritten.
    GposNodeNeedsRewrite,

    /// Single
    GposType1,
    /// Pair
    GposType2,
    /// Cursive
    GposType3,
    /// Mark-to-base
    GposType4,
    /// Mark-to-lig
    GposType5,
    /// Markt-mark
    GposType6,
    /// Contextual
    GposType7,
    /// Chained contextual
    GposType8,
    GposIgnore,

    // context & chaining context rule components:
    BacktrackSequence,
    LookaheadSequence,
    ContextSequence,
    ContextGlyphNode,
    InlineSubNode,
    // there can be multiple ignore rules specified in the same block, separated
    // by commas
    IgnoreRuleStatementNode,

    AnchorMarkNode,
    LigatureComponentNode,
    ValueRecordNode,
    ValueRecordDefNode,
    LookupRefNode,
    LookupBlockNode,
    ScriptRecordNode,
    IncludeNode,
    MarkClassNode,
    AnchorNode,
    DeviceNode,
    AnchorDefNode,
    AnonBlockNode,
    GlyphClassDefNode,
    LanguageSystemNode,
    FeatureNode,
    SizeMenuNameNode,
    ParametersNode,
    ScriptNode,
    LanguageNode,
    LookupFlagNode,
    SubtableNode,
    ConditionSetNode,
    ConditionNode,
    VariationNode,

    VariableMetricNode,
    LocationValueNode,
    LocationSpecNode,
    LocationSpecItemNode,
    AxisLocationNode, // a number or float + optional suffix

    TableNode,
    HeadTableNode,
    HeadFontRevisionNode,
    HheaTableNode,
    MetricValueNode, // shared between hhea, vhea, and os2
    NumberValueNode, // used in os2
    StringValueNode, // used in os2
    Os2NumberListNode,
    Os2FamilyClassNode,
    NameTableNode,
    NameRecordNode,
    NameSpecNode,
    BaseTableNode,
    BaseTagListNode,
    BaseScriptListNode,
    BaseMinMaxNode,
    GdefTableNode,
    GdefClassDefNode,
    GdefClassDefEntryNode,
    GdefAttachNode,
    GdefLigatureCaretNode,
    Os2TableNode,
    Os2PanoseNode,
    Os2UnicodeRangeNode,
    Os2CodePageRangeNode,
    Os2VendorNode,
    VheaTableNode,
    VmtxTableNode,
    VmtxEntryNode,
    StatTableNode,
    StatElidedFallbackNameNode,
    StatDesignAxisNode,
    StatAxisValueNode,
    StatAxisValueLocationNode,
    StatAxisValueFlagNode,
    CvParamsNameNode,
    AaltFeatureNode,

    // glyphs syntax extension
    Dollar,
    Plus,
    Asterisk,
    Slash,
    GlyphsNumberValueNode,
    GlyphsNumberValueExprNode,
    GlyphsNumberIdent,
}

impl Kind {
    pub(crate) fn is_rule(&self) -> bool {
        matches!(
            self,
            Self::GposType1
                | Self::GposType2
                | Self::GposType3
                | Self::GposType4
                | Self::GposType5
                | Self::GposType6
                | Self::GposType7
                | Self::GposType8
                | Self::GsubType1
                | Self::GsubType2
                | Self::GsubType3
                | Self::GsubType4
                | Self::GsubType5
                | Self::GsubType6
                | Self::GsubType7
                | Self::GsubType8
        )
    }

    pub(crate) fn is_trivia(self) -> bool {
        matches!(self, Kind::Comment | Kind::Whitespace | Kind::Backslash)
    }
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "EOF"),
            //Self::Tombstone => write!(f, "X_X"),
            Self::SourceFile => write!(f, "FILE"),
            Self::Ident => write!(f, "ID"),
            Self::StringUnterminated => write!(f, "STR OPEN"),
            Self::String => write!(f, "STR"),
            Self::Number => write!(f, "NUM"),
            Self::Octal => write!(f, "OCT"),
            Self::Hex => write!(f, "HEX"),
            Self::HexEmpty => write!(f, "HEX EMPTY"),
            Self::NumberSuffix => write!(f, "SUFFIX"),
            Self::Float => write!(f, "FLOAT"),
            Self::Whitespace => write!(f, "WS"),
            Self::Semi => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::Backslash => write!(f, "\\"),
            Self::Hyphen => write!(f, "-"), // also minus
            Self::Eq => write!(f, "="),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LSquare => write!(f, "["),
            Self::RSquare => write!(f, "]"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LAngle => write!(f, "<"),
            Self::RAngle => write!(f, ">"),
            Self::SingleQuote => write!(f, "'"),
            Self::Comment => write!(f, "#"),

            Self::Tag => write!(f, "Tag"),
            Self::Path => write!(f, "Path"),
            Self::GlyphClass => write!(f, "GlyphClass"),
            Self::NamedGlyphClass => write!(f, "@GlyphClass"),
            Self::GlyphRange => write!(f, "GlyphRange"),
            Self::GlyphName => write!(f, "GlyphName"),
            Self::GlyphNameOrRange => write!(f, "GlyphNameOrRange"),
            Self::Cid => write!(f, "CID"),
            //Self::Metric => write!(f, "METRIC"),
            Self::Label => write!(f, "LABEL"),

            Self::TableKw => write!(f, "TableKw"),
            Self::LookupKw => write!(f, "LookupKw"),
            Self::LanguagesystemKw => write!(f, "LanguagesystemKw"),
            Self::AnchorDefKw => write!(f, "AnchorDefKw"),
            Self::FeatureKw => write!(f, "FeatureKw"),
            Self::MarkClassKw => write!(f, "MarkClassKw"),
            Self::AnonKw => write!(f, "AnonKw"),
            Self::ConditionSetKw => write!(f, "ConditionSetKw"),
            Self::VariationKw => write!(f, "VariationKw"),
            Self::AnchorKw => write!(f, "AnchorKw"),
            Self::ByKw => write!(f, "ByKw"),
            Self::ContourpointKw => write!(f, "ContourpointKw"),
            Self::CursiveKw => write!(f, "CursiveKw"),
            Self::DeviceKw => write!(f, "DeviceKw"),
            Self::EnumKw => write!(f, "EnumKw"),
            Self::ExcludeDfltKw => write!(f, "ExcludeDfltKw"),
            Self::FromKw => write!(f, "FromKw"),
            Self::IgnoreKw => write!(f, "IgnoreKw"),
            Self::IgnoreBaseGlyphsKw => write!(f, "IgnoreBaseGlyphsKw"),
            Self::IgnoreLigaturesKw => write!(f, "IgnoreLigaturesKw"),
            Self::IgnoreMarksKw => write!(f, "IgnoreMarksKw"),
            Self::IncludeKw => write!(f, "IncludeKw"),
            Self::IncludeDfltKw => write!(f, "IncludeDfltKw"),
            Self::LanguageKw => write!(f, "LanguageKw"),
            Self::LookupflagKw => write!(f, "LookupflagKw"),
            Self::MarkKw => write!(f, "MarkKw"),
            Self::MarkAttachmentTypeKw => write!(f, "MarkAttachmentTypeKw"),
            Self::NameIdKw => write!(f, "NameIdKw"),
            Self::NullKw => write!(f, "NullKw"),
            Self::ParametersKw => write!(f, "ParametersKw"),
            Self::PosKw => write!(f, "PosKw"),
            Self::RequiredKw => write!(f, "RequiredKw"),
            Self::RightToLeftKw => write!(f, "RightToLeftKw"),
            Self::RsubKw => write!(f, "RsubKw"),
            Self::ScriptKw => write!(f, "ScriptKw"),
            Self::SubKw => write!(f, "SubKw"),
            Self::SubtableKw => write!(f, "SubtableKw"),
            Self::UseExtensionKw => write!(f, "UseExtensionKw"),
            Self::UseMarkFilteringSetKw => write!(f, "UseMarkFilteringSetKw"),
            Self::ValueRecordDefKw => write!(f, "ValueRecordDefKw"),
            Self::HorizAxisBaseScriptListKw => write!(f, "HorizAxis.BaseScriptList"),
            Self::HorizAxisBaseTagListKw => write!(f, "HorizAxis.BaseTagList"),
            Self::HorizAxisMinMaxKw => write!(f, "HorizAxis.MinMax"),
            Self::VertAxisBaseScriptListKw => write!(f, "VertAxis.BaseScriptList"),
            Self::VertAxisBaseTagListKw => write!(f, "VertAxis.BaseTagList"),
            Self::VertAxisMinMaxKw => write!(f, "VertAxis.MinMax"),
            Self::AttachKw => write!(f, "Attach"),
            Self::GlyphClassDefKw => write!(f, "GlyphClassDef"),
            Self::LigatureCaretByDevKw => write!(f, "LigatureCaretByDev"),
            Self::LigatureCaretByIndexKw => write!(f, "LigatureCaretByIndex"),
            Self::LigatureCaretByPosKw => write!(f, "LigatureCaretByPos"),
            Self::MarkAttachClassKw => write!(f, "MarkAttachClass"),
            Self::FontRevisionKw => write!(f, "FontRevision"),
            Self::AscenderKw => write!(f, "Ascender"),
            Self::CaretOffsetKw => write!(f, "CaretOffset"),
            Self::DescenderKw => write!(f, "Descender"),
            Self::LineGapKw => write!(f, "LineGap"),
            Self::CapHeightKw => write!(f, "CapHeight"),
            Self::CodePageRangeKw => write!(f, "CodePageRange"),
            Self::PanoseKw => write!(f, "Panose"),
            Self::TypoAscenderKw => write!(f, "TypoAscender"),
            Self::TypoDescenderKw => write!(f, "TypoDescender"),
            Self::TypoLineGapKw => write!(f, "TypoLineGap"),
            Self::UnicodeRangeKw => write!(f, "UnicodeRange"),
            Self::VendorKw => write!(f, "Vendor"),
            Self::WinAscentKw => write!(f, "winAscent"),
            Self::WinDescentKw => write!(f, "winDescent"),
            Self::XHeightKw => write!(f, "XHeight"),
            Self::SizemenunameKw => write!(f, "sizemenuname"),
            Self::VertTypoAscenderKw => write!(f, "VertTypoAscender"),
            Self::VertTypoDescenderKw => write!(f, "VertTypoDescender"),
            Self::VertTypoLineGapKw => write!(f, "VertTypoLineGap"),
            Self::VertAdvanceYKw => write!(f, "VertAdvanceY"),
            Self::VertOriginYKw => write!(f, "VertOriginY"),
            Self::ElidedFallbackNameKw => write!(f, "ElidedFallbackName"),
            Self::ElidedFallbackNameIDKw => write!(f, "ElidedFallbackNameID"),
            Self::DesignAxisKw => write!(f, "DesignAxis"),
            Self::AxisValueKw => write!(f, "AxisValue"),
            Self::FlagKw => write!(f, "flag"),
            Self::LocationKw => write!(f, "location"),
            Self::ElidableAxisValueNameKw => write!(f, "ElidableAxisValueName"),
            Self::OlderSiblingFontAttributeKw => write!(f, "OlderSiblingFontAttribute"),

            Self::FeatureNamesKw => write!(f, "FeatureNamesKw"),
            Self::NameKw => write!(f, "NameKw"),
            Self::CvParametersKw => write!(f, "CvParametersKw"),
            Self::FeatUiLabelNameIdKw => write!(f, "FeatUiLabelNameIdKw"),
            Self::FeatUiTooltipTextNameIdKw => write!(f, "FeatUiTooltipTextNameIdKw"),
            Self::SampleTextNameIdKw => write!(f, "SampleTextNameId"),
            Self::ParamUiLabelNameIdKw => write!(f, "ParamUiLabelNameId"),
            Self::CharacterKw => write!(f, "CharacterKw"),

            Self::LigatureKw => write!(f, "LigatureKw"),
            Self::BaseKw => write!(f, "BaseKw"),

            Self::AnchorMarkNode => write!(f, "AnchorMarkNode"),
            Self::LigatureComponentNode => write!(f, "LigatureComponentNode"),
            Self::ValueRecordNode => write!(f, "ValueRecordNode"),
            Self::ValueRecordDefNode => write!(f, "ValueRecordDefNode"),
            Self::GsubNode => write!(f, "GsubNode"),
            Self::GsubNodeNeedsRewrite => write!(f, "GsubNodeNeedsRewrite"),
            Self::GsubType1 => write!(f, "GsubType1"),
            Self::GsubType2 => write!(f, "GsubType2"),
            Self::GsubType3 => write!(f, "GsubType3"),
            Self::GsubType4 => write!(f, "GsubType4"),
            Self::GsubType5 => write!(f, "GsubType5"),
            Self::GsubType6 => write!(f, "GsubType6"),
            Self::GsubType7 => write!(f, "GsubType7"),
            Self::GsubType8 => write!(f, "GsubType8"),
            Self::GsubIgnore => write!(f, "GsubIgnore"),
            Self::GposNode => write!(f, "GposNode"),
            Self::GposNodeNeedsRewrite => write!(f, "GposNodeNeedsRewrite"),
            Self::GposType1 => write!(f, "GposType1"),
            Self::GposType2 => write!(f, "GposType2"),
            Self::GposType3 => write!(f, "GposType3"),
            Self::GposType4 => write!(f, "GposType4"),
            Self::GposType5 => write!(f, "GposType5"),
            Self::GposType6 => write!(f, "GposType6"),
            Self::GposType7 => write!(f, "GposType7"),
            Self::GposType8 => write!(f, "GposType8"),
            Self::GposIgnore => write!(f, "GposIgnore"),

            Self::BacktrackSequence => write!(f, "BacktrackSequence"),
            Self::LookaheadSequence => write!(f, "LookaheadSequence"),
            Self::ContextSequence => write!(f, "ContextSequence"),
            Self::ContextGlyphNode => write!(f, "ContextGlyphNode"),
            Self::InlineSubNode => write!(f, "InlineSubNode"),
            Self::IgnoreRuleStatementNode => write!(f, "IgnoreRuleStatementNode"),

            Self::LookupRefNode => write!(f, "LookupRefNode"),
            Self::LookupBlockNode => write!(f, "LookupBlockNode"),
            Self::ScriptRecordNode => write!(f, "ScriptRecoordNode"),
            Self::TableEntryNode => write!(f, "TableEntryNode"),
            Self::IncludeNode => write!(f, "IncludeNode"),
            Self::MarkClassNode => write!(f, "MarkClassNode"),
            Self::AnchorDefNode => write!(f, "AnchorDefNode"),
            Self::AnchorNode => write!(f, "AnchorNode"),
            Self::ConditionSetNode => write!(f, "ConditionSetNode"),
            Self::ConditionNode => write!(f, "ConditionNode"),
            Self::VariationNode => write!(f, "VariationNode"),
            Self::VariableMetricNode => write!(f, "VariableMetricNode"),
            Self::LocationValueNode => write!(f, "LocationValueNode"),
            Self::LocationSpecNode => write!(f, "LocationSpecNode"),
            Self::LocationSpecItemNode => write!(f, "LocationSpecItemNode"),
            Self::AxisLocationNode => write!(f, "AxisLocationNode"),

            Self::DeviceNode => write!(f, "DeviceNode"),
            Self::AnonBlockNode => write!(f, "AnonBlockNode"),
            Self::GlyphClassDefNode => write!(f, "GlyphClassDefNode"),
            Self::LanguageSystemNode => write!(f, "LanguageSystemNode"),
            Self::FeatureNode => write!(f, "FeatureNode"),
            Self::SizeMenuNameNode => write!(f, "SizeMenuNameNode"),
            Self::ParametersNode => write!(f, "ParametersNode"),
            Self::ScriptNode => write!(f, "ScriptNode"),
            Self::LanguageNode => write!(f, "LanguageNode"),
            Self::LookupFlagNode => write!(f, "LookupFlagNode"),
            Self::SubtableNode => write!(f, "SubtableNode"),
            Self::TableNode => write!(f, "TableNode"),
            Self::HeadTableNode => write!(f, "HeadTableNode"),
            Self::HheaTableNode => write!(f, "HheaTableNode"),
            Self::NameTableNode => write!(f, "NameTableNode"),
            Self::BaseTableNode => write!(f, "BaseTableNode"),
            Self::BaseTagListNode => write!(f, "BaseTagListNode"),
            Self::BaseScriptListNode => write!(f, "BaseScriptListNode"),
            Self::BaseMinMaxNode => write!(f, "BaseMinMaxNode"),
            Self::GdefTableNode => write!(f, "GdefTableNode"),
            Self::Os2TableNode => write!(f, "Os2TableNode"),
            Self::VheaTableNode => write!(f, "VheaTableNode"),
            Self::VmtxTableNode => write!(f, "VmtxTableNode"),
            Self::StatTableNode => write!(f, "StatTableNode"),
            Self::StatElidedFallbackNameNode => write!(f, "StatElidedFallbackNameNode"),
            Self::StatDesignAxisNode => write!(f, "StatDesignAxisNode"),
            Self::StatAxisValueNode => write!(f, "StatAxisValueNode"),
            Self::StatAxisValueLocationNode => write!(f, "StatAxisValueLocationNode"),
            Self::StatAxisValueFlagNode => write!(f, "StatAxisValueFlagNode"),
            Self::VmtxEntryNode => write!(f, "VmtxEntryNode"),
            Self::Os2PanoseNode => write!(f, "Os2PanoseNode"),
            Self::Os2UnicodeRangeNode => write!(f, "Os2UnicodeRangeNode"),
            Self::Os2CodePageRangeNode => write!(f, "Os2CodePageRangeNode"),
            Self::Os2VendorNode => write!(f, "Os2VendorNode"),
            Self::GdefClassDefNode => write!(f, "GdefClassDefNode"),
            Self::GdefClassDefEntryNode => write!(f, "GdefClassDefEntryNode"),
            Self::GdefAttachNode => write!(f, "GdefAttachNode"),
            Self::GdefLigatureCaretNode => write!(f, "GdefLigatureCaretNode"),
            Self::NameRecordNode => write!(f, "NameRecordNode"),
            Self::NameSpecNode => write!(f, "NameSpecNode"),
            Self::HeadFontRevisionNode => write!(f, "HeadFontRevisionNode"),
            Self::MetricValueNode => write!(f, "MetricValueNode"), // shared between hhea, vhea, and os2
            Self::NumberValueNode => write!(f, "NumberNode"),      // used in os2
            Self::StringValueNode => write!(f, "StringNode"),      // used in os2
            Self::Os2NumberListNode => write!(f, "Os2NumberListNode"),
            Self::Os2FamilyClassNode => write!(f, "Os2FamilyClassNode"),
            Self::CvParamsNameNode => write!(f, "CvParamsNameNode"),
            Self::AaltFeatureNode => write!(f, "AaltFeatureNode"),

            //glyphs syntax
            Self::Dollar => write!(f, "$"),
            Self::Plus => write!(f, "+"),
            Self::Slash => write!(f, "/"),
            Self::Asterisk => write!(f, "*"),
            Self::GlyphsNumberValueNode => write!(f, "GlyphsNumberValueNode"),
            Self::GlyphsNumberValueExprNode => write!(f, "GlyphsNumberValueExprNode"),
            Self::GlyphsNumberIdent => write!(f, "GlyphsNumberIdent"),
        }
    }
}
