#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Token {
    pub(crate) len: usize,
    pub(crate) kind: Kind,
}

impl Token {
    pub const EMPTY: Token = Token {
        len: 0,
        kind: Kind::Tombstone,
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Kind {
    Tombstone,  // a placeholder value
    Eof,        // the end of the input stream
    SourceFile, // scope of a file

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

    Whitespace,
    Comment,

    // special symbols
    Semi,
    Comma,
    //At,
    Backslash,
    Hyphen, // also minus
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

    Tag,
    Path,
    GlyphName,
    GlyphClass,
    NamedGlyphClass,
    GlyphRange,
    Cid,
    Metric,
    Label,

    // keywords:
    // top-level keywords
    TableKw,
    LookupKw,
    LanguagesystemKw,
    AnchorDefKw,
    FeatureKw,
    MarkClassKw,
    AnonKw,
    //AnonymousKw,

    // other keywords
    AnchorKw,
    ByKw,
    ContourpointKw,
    CursiveKw,
    DeviceKw,
    EnumKw,
    //EnumerateKw,
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
    PosKw,
    //PositionKw,
    RequiredKw,
    RightToLeftKw,
    RsubKw,
    //ReversesubKw,
    ScriptKw,
    SubKw,
    //SubstituteKw,
    SubtableKw,
    UseExtensionKw,
    UseMarkFilteringSetKw,
    ValueRecordDefKw,
    //Anonymous,

    // not technically keywords and not lexed, but assigned during parsing
    // in gsub/gpos:
    LigatureKw,
    BaseKw,

    // node-only tokens, assigned during parsing
    AnchorMarkNode,
    ValueRecordNode,
}

impl Kind {
    // only used for debugging
    #[cfg(test)]
    pub(crate) fn has_contents(&self) -> bool {
        matches!(
            self,
            Self::Ident
                | Self::String
                | Self::StringUnterminated
                | Self::Float
                | Self::Hex
                | Self::HexEmpty
                | Self::Octal
                | Self::Comment
                | Self::Whitespace
                | Self::NamedGlyphClass
                | Self::GlyphName
                | Self::Metric
                | Self::Number
        )
    }

    pub(crate) fn from_keyword(word: &[u8]) -> Option<Kind> {
        //eprintln!("{}", std::str::from_utf8(word).unwrap());
        match word {
            b"anchor" => Some(Kind::AnchorKw),
            b"anchorDef" => Some(Kind::AnchorDefKw),
            b"anon" | b"anonymous" => Some(Kind::AnonKw),
            b"by" => Some(Kind::ByKw),
            b"contourpoint" => Some(Kind::ContourpointKw),
            b"cursive" => Some(Kind::CursiveKw),
            b"device" => Some(Kind::DeviceKw), //[ Not implemented ];
            b"enum" | b"enumerate" => Some(Kind::EnumKw),
            b"exclude_dflt" | b"excludeDFLT" => Some(Kind::ExcludeDfltKw),
            b"feature" => Some(Kind::FeatureKw), //(used as a block and as a statement);
            b"from" => Some(Kind::FromKw),
            b"ignore" => Some(Kind::IgnoreKw), //(used with substitute and position);
            b"IgnoreBaseGlyphs" => Some(Kind::IgnoreBaseGlyphsKw),
            b"IgnoreLigatures" => Some(Kind::IgnoreLigaturesKw),
            b"IgnoreMarks" => Some(Kind::IgnoreMarksKw),
            b"include" => Some(Kind::IncludeKw),
            b"include_dflt" | b"includeDFLT" => Some(Kind::IncludeDfltKw),
            b"language" => Some(Kind::LanguageKw),
            b"languagesystem" => Some(Kind::LanguagesystemKw),
            b"lookup" => Some(Kind::LookupKw),
            b"lookupflag" => Some(Kind::LookupflagKw),
            b"mark" => Some(Kind::MarkKw),
            b"MarkAttachmentType" => Some(Kind::MarkAttachmentTypeKw),
            b"markClass" => Some(Kind::MarkClassKw),
            b"nameid" => Some(Kind::NameIdKw),
            b"NULL" => Some(Kind::NullKw), //(used in substitute, device, value record, anchor);
            b"parameters" => Some(Kind::ParametersKw),
            b"pos" | b"position" => Some(Kind::PosKw),
            b"required" => Some(Kind::RequiredKw), //[ Not implemented ];
            b"reversesub" | b"rsub" => Some(Kind::RsubKw),
            b"RightToLeft" => Some(Kind::RightToLeftKw),
            b"script" => Some(Kind::ScriptKw),
            b"substitute" | b"sub" => Some(Kind::SubKw),
            b"subtable" => Some(Kind::SubtableKw),
            b"table" => Some(Kind::TableKw),
            b"useExtension" => Some(Kind::UseExtensionKw),
            b"UseMarkFilteringSet" => Some(Kind::UseMarkFilteringSetKw),
            b"valueRecordDef" => Some(Kind::ValueRecordDefKw),
            _ => None,
        }
    }
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "EOF"),
            Self::Tombstone => write!(f, "X_X"),
            Self::SourceFile => write!(f, "FILE"),
            Self::Ident => write!(f, "ID"),
            Self::StringUnterminated => write!(f, "STR OPEN"),
            Self::String => write!(f, "STR"),
            Self::Number => write!(f, "NUM"),
            Self::Octal => write!(f, "OCT"),
            Self::Hex => write!(f, "HEX"),
            Self::HexEmpty => write!(f, "HEX EMPTY"),
            Self::Float => write!(f, "FLOAT"),
            Self::Whitespace => write!(f, "WS"),
            Self::Semi => write!(f, ";"),
            Self::Comma => write!(f, ","),
            //Self::At => write!(f, "@"),
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
            Self::Cid => write!(f, "CID"),
            Self::Metric => write!(f, "METRIC"),
            Self::Label => write!(f, "LABEL"),

            Self::TableKw => write!(f, "TableKw"),
            Self::LookupKw => write!(f, "LookupKw"),
            Self::LanguagesystemKw => write!(f, "LanguagesystemKw"),
            Self::AnchorDefKw => write!(f, "AnchorDefKw"),
            Self::FeatureKw => write!(f, "FeatureKw"),
            Self::MarkClassKw => write!(f, "MarkClassKw"),
            Self::AnonKw => write!(f, "AnonKw"),
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
            //Self::ReversesubKw => write!(f, "ReversesubKw"),
            Self::ScriptKw => write!(f, "ScriptKw"),
            Self::SubKw => write!(f, "SubKw"),
            Self::SubtableKw => write!(f, "SubtableKw"),
            Self::UseExtensionKw => write!(f, "UseExtensionKw"),
            Self::UseMarkFilteringSetKw => write!(f, "UseMarkFilteringSetKw"),
            Self::ValueRecordDefKw => write!(f, "ValueRecordDefKw"),

            Self::LigatureKw => write!(f, "LigatureKw"),
            Self::BaseKw => write!(f, "BaseKw"),

            Self::AnchorMarkNode => write!(f, "AnchorMarkNode"),
            Self::ValueRecordNode => write!(f, "ValueRecordNode"),
        }
    }
}
