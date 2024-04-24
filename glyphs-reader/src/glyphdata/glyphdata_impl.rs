// NOTE: to avoid a bunch of duplication, this file is also `include!`ed from
// build.rs.

use std::{fmt::Display, num::ParseIntError, path::PathBuf, str::FromStr};

use quick_xml::{
    events::{BytesStart, Event},
    Reader,
};
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

/// Information about a glyph
///
/// In general this is derived from bundled data files, but these fields can
/// also be overridden by the font author
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct GlyphInfo {
    pub name: SmolStr,
    pub category: Category,
    pub subcategory: Subcategory,
    pub unicode: Option<u32>,
    pub production: Option<SmolStr>,
    pub alt_names: Vec<SmolStr>,
}

/// The primary category for a given glyph
///
/// These categories are not the same as the unicode character categories.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[repr(u8)]
pub enum Category {
    Mark,
    Space,
    Separator,
    Letter,
    Number,
    Symbol,
    Punctuation,
    Other,
}

/// The subcategory of a given glyph
#[derive(
    Clone, Copy, Default, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize,
)]
#[repr(u8)]
pub enum Subcategory {
    Spacing,
    Radical,
    Math,
    Superscript,
    Geometry,
    Dash,
    DecimalDigit,
    Currency,
    Fraction,
    Halfform,
    Small,
    Number,
    Quote,
    Space,
    Letter,
    Jamo,
    Format,
    Parenthesis,
    Matra,
    Arrow,
    Nonspacing,
    Compatibility,
    Syllable,
    Ligature,
    Modifier,
    SpacingCombining,
    Emoji,
    Enclosing,
    #[default]
    None,
}

/// Parse glyph info entries out of a GlyphData xml file.
pub fn parse_entries(xml: &[u8]) -> Result<Vec<GlyphInfo>, GlyphDataError> {
    fn check_and_advance_past_preamble(reader: &mut Reader<&[u8]>) -> Result<(), GlyphDataError> {
        loop {
            let event = reader.read_event()?;
            match event {
                Event::Comment(_) => (),
                Event::Decl(_) => (),
                Event::DocType(_) => (),
                Event::Start(start) if start.name().as_ref() == b"glyphData" => return Ok(()),
                _other => {
                    return Err(GlyphDataError::WrongFirstElement);
                }
            }
        }
    }

    let mut reader = Reader::from_reader(xml);
    reader.trim_text(true);

    check_and_advance_past_preamble(&mut reader)?;
    iter_rows(&mut reader)
        .map(|row| row.map_err(Into::into).and_then(parse_glyph_xml))
        .collect::<Result<_, _>>()
}

fn iter_rows<'a, 'b: 'a>(
    reader: &'b mut Reader<&'a [u8]>,
) -> impl Iterator<Item = Result<BytesStart<'a>, quick_xml::Error>> + 'a {
    std::iter::from_fn(|| match reader.read_event() {
        Err(e) => Some(Err(e)),
        Ok(Event::Empty(start)) => Some(Ok(start)),
        _ => None,
    })
}

fn parse_glyph_xml(item: BytesStart) -> Result<GlyphInfo, GlyphDataError> {
    let mut name = None;
    let mut category = None;
    let mut subcategory = None;
    let mut unicode = None;
    let mut production = None;
    let mut alt_names = None;

    for attr in item.attributes() {
        let attr = attr?;
        let value = attr.unescape_value()?;
        match attr.key.as_ref() {
            b"name" => name = Some(value),
            b"category" => category = Some(value),
            b"subCategory" => subcategory = Some(value),
            b"unicode" => unicode = Some(value),
            b"production" => production = Some(value),
            b"altNames" => alt_names = Some(value),
            b"unicodeLegacy" | b"case" | b"direction" | b"script" | b"description" => (),
            other => {
                return Err(GlyphDataError::UnknownAttribute(
                    String::from_utf8_lossy(other).into_owned(),
                ))
            }
        }
    }

    // now we've found some values, let's finalize them

    let name = name
        .map(SmolStr::new)
        .ok_or_else(|| GlyphDataError::missing_attr("name", item.attributes_raw()))?;
    let category = category
        .ok_or_else(|| GlyphDataError::missing_attr("category", item.attributes_raw()))
        .and_then(|cat| {
            Category::from_str(cat.as_ref()).map_err(GlyphDataError::InvalidCategory)
        })?;
    let subcategory = subcategory
        .map(|cat| Subcategory::from_str(cat.as_ref()).map_err(GlyphDataError::InvalidSubcategory))
        .transpose()?
        .unwrap_or(Subcategory::None);
    let production = production.map(SmolStr::new);
    let unicode = unicode
        .map(|s| {
            u32::from_str_radix(&s, 16).map_err(|inner| GlyphDataError::InvalidUnicode {
                raw: s.into_owned(),
                inner,
            })
        })
        .transpose()?;
    let alt_names = alt_names
        .map(|names| {
            names
                .as_ref()
                .split(',')
                .map(|name| SmolStr::from(name.trim()))
                .collect()
        })
        .unwrap_or_default();

    Ok(GlyphInfo {
        name,
        category,
        subcategory,
        unicode,
        production,
        alt_names,
    })
}

impl FromStr for Category {
    type Err = SmolStr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Mark" => Ok(Self::Mark),
            "Space" => Ok(Self::Space),
            "Separator" => Ok(Self::Separator),
            "Letter" => Ok(Self::Letter),
            "Number" => Ok(Self::Number),
            "Symbol" => Ok(Self::Symbol),
            "Punctuation" => Ok(Self::Punctuation),
            "Other" => Ok(Self::Other),
            _ => Err(s.into()),
        }
    }
}

impl FromStr for Subcategory {
    type Err = SmolStr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Spacing" => Ok(Self::Spacing),
            "Radical" => Ok(Self::Radical),
            "Math" => Ok(Self::Math),
            "Superscript" => Ok(Self::Superscript),
            "Geometry" => Ok(Self::Geometry),
            "Dash" => Ok(Self::Dash),
            "Decimal Digit" => Ok(Self::DecimalDigit),
            "Currency" => Ok(Self::Currency),
            "Fraction" => Ok(Self::Fraction),
            "Halfform" => Ok(Self::Halfform),
            "Small" => Ok(Self::Small),
            "Number" => Ok(Self::Number),
            "Quote" => Ok(Self::Quote),
            "Space" => Ok(Self::Space),
            "Letter" => Ok(Self::Letter),
            "Jamo" => Ok(Self::Jamo),
            "Format" => Ok(Self::Format),
            "Parenthesis" => Ok(Self::Parenthesis),
            "Matra" => Ok(Self::Matra),
            "Arrow" => Ok(Self::Arrow),
            "Nonspacing" => Ok(Self::Nonspacing),
            "Compatibility" => Ok(Self::Compatibility),
            "Syllable" => Ok(Self::Syllable),
            "Ligature" => Ok(Self::Ligature),
            "Modifier" => Ok(Self::Modifier),
            "Spacing Combining" => Ok(Self::SpacingCombining),
            "Emoji" => Ok(Self::Emoji),
            "Enclosing" => Ok(Self::Enclosing),
            "None" => Ok(Self::None),
            _ => Err(s.into()),
        }
    }
}

impl Display for Category {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mark => write!(f, "Mark"),
            Self::Space => write!(f, "Space"),
            Self::Separator => write!(f, "Separator"),
            Self::Letter => write!(f, "Letter"),
            Self::Number => write!(f, "Number"),
            Self::Symbol => write!(f, "Symbol"),
            Self::Punctuation => write!(f, "Punctuation"),
            Self::Other => write!(f, "Other"),
        }
    }
}

impl Display for Subcategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Spacing => write!(f, "Spacing"),
            Self::Radical => write!(f, "Radical"),
            Self::Math => write!(f, "Math"),
            Self::Superscript => write!(f, "Superscript"),
            Self::Geometry => write!(f, "Geometry"),
            Self::Dash => write!(f, "Dash"),
            Self::DecimalDigit => write!(f, "Decimal Digit"),
            Self::Currency => write!(f, "Currency"),
            Self::Fraction => write!(f, "Fraction"),
            Self::Halfform => write!(f, "Halfform"),
            Self::Small => write!(f, "Small"),
            Self::Number => write!(f, "Number"),
            Self::Quote => write!(f, "Quote"),
            Self::Space => write!(f, "Space"),
            Self::Letter => write!(f, "Letter"),
            Self::Jamo => write!(f, "Jamo"),
            Self::Format => write!(f, "Format"),
            Self::Parenthesis => write!(f, "Parenthesis"),
            Self::Matra => write!(f, "Matra"),
            Self::Arrow => write!(f, "Arrow"),
            Self::Nonspacing => write!(f, "Nonspacing"),
            Self::Compatibility => write!(f, "Compatibility"),
            Self::Syllable => write!(f, "Syllable"),
            Self::Ligature => write!(f, "Ligature"),
            Self::Modifier => write!(f, "Modifier"),
            Self::SpacingCombining => write!(f, "Spacing Combining"),
            Self::Emoji => write!(f, "Emoji"),
            Self::Enclosing => write!(f, "Enclosing"),
            Self::None => write!(f, "None"),
        }
    }
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum GlyphDataError {
    #[error("Couldn't read user file at '{path}': '{reason}'")]
    UserFile {
        path: PathBuf,
        reason: std::io::ErrorKind,
    },
    #[error("Error parsing XML: '{0}'")]
    ReaderError(#[from] quick_xml::Error),
    #[error("Error parsing XML attribute: '{0}'")]
    XmlAttributeError(#[from] quick_xml::events::attributes::AttrError),
    #[error("Unknown category '{0}'")]
    InvalidCategory(SmolStr),
    #[error("Unknown subcategory '{0}'")]
    InvalidSubcategory(SmolStr),
    #[error("the XML input did not start with a <glyphdata> tag")]
    WrongFirstElement,
    #[error("Missing required attribute '{missing}' in '{attributes}'")]
    MissingRequiredAttribute {
        attributes: String,
        missing: &'static str,
    },
    #[error("Invalid unicode value '{raw}': '{inner}'")]
    InvalidUnicode { raw: String, inner: ParseIntError },
    #[error("Unexpected attribute '{0}'")]
    UnknownAttribute(String),
}

impl GlyphDataError {
    // a little helper here makes our parsing code cleaner
    fn missing_attr(name: &'static str, raw_attrs: &[u8]) -> Self {
        let attributes = String::from_utf8_lossy(raw_attrs).into_owned();
        Self::MissingRequiredAttribute {
            attributes,
            missing: name,
        }
    }
}
