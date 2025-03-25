//! determining glyph properties
//!
//! This module provides access to glyph info extracted from bundled
//! (and potentially user-provided) data files.

use quick_xml::{
    events::{BytesStart, Event},
    Reader,
};
use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
    num::ParseIntError,
    path::{Path, PathBuf},
    str::FromStr,
};

use icu_properties::props::GeneralCategory;

use smol_str::SmolStr;

use crate::glyphdata_bundled::{self as bundled, find_pos_by_prod_name};

/// The primary category for a given glyph
///
/// These categories are not the same as the unicode character categories.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    Composition,
    Other,
}

/// The script of a given glyph
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum Script {
    Adlam,
    Alchemical,
    Arabic,
    Armenian,
    Avestan,
    Balinese,
    Bamum,
    Batak,
    Bengali,
    BlackLetter,
    Bopomofo,
    Brahmi,
    Braille,
    Buginese,
    Canadian,
    Chakma,
    Cham,
    Cherokee,
    Chorasmian,
    Coptic,
    Cyrillic,
    Dentistry,
    Deseret,
    Devanagari,
    Divesakuru,
    Elbasan,
    Elymaic,
    Ethiopic,
    Georgian,
    Glagolitic,
    Gothic,
    Greek,
    Gujarati,
    Gurmukhi,
    Han,
    Hangul,
    Hebrew,
    Javanese,
    Kana,
    Kannada,
    Kawi,
    Kayahli,
    Khmer,
    Khojki,
    Lao,
    Latin,
    Lepcha,
    Lue,
    Mahjong,
    Malayalam,
    Mandaic,
    Math,
    Mongolian,
    Musical,
    Myanmar,
    Nko,
    NyiakengPuachueHmong,
    Ogham,
    Oriya,
    Osage,
    Osmanya,
    PahawhHmong,
    PhaistosDisc,
    Rovas,
    Runic,
    Samaritan,
    Shavian,
    Sinhala,
    Syriac,
    Tamil,
    Telugu,
    Thaana,
    Thai,
    Tham,
    Tibet,
    Tifinagh,
    Vai,
    Yi,
}

/// Production name of a glyph.
///
/// Per [khaled](https://github.com/googlefonts/fontc/pull/1354#pullrequestreview-2707517748)
/// the overwhelming majority follow simple patterns.
///
/// See also <https://github.com/adobe-type-tools/agl-specification?tab=readme-ov-file#2-the-mapping>
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ProductionName {
    // uniHEX, e.g. uni004A
    Bmp(u32),
    // uHEX, e.g. uE007D
    NonBmp(u32),
    // I reject your patterns and choose my own
    Custom(SmolStr),
}

impl From<&str> for ProductionName {
    fn from(v: &str) -> ProductionName {
        fn try_parse(
            v: &str,
            lbound: u32,
            ubound: u32,
            f: impl Fn(u32) -> ProductionName,
        ) -> Option<ProductionName> {
            if let Ok(v) = u32::from_str_radix(v, 16) {
                if v >= lbound && v <= ubound {
                    return Some(f(v));
                }
            }
            None
        }

        match v {
            _ if v.starts_with("uni") => try_parse(&v[3..], 0, 0xFFFF, ProductionName::Bmp),
            _ if v.starts_with("u") => {
                try_parse(&v[1..], 0xFFFF + 1, 0x10FFFF, ProductionName::NonBmp)
            }
            _ => None,
        }
        .unwrap_or_else(|| ProductionName::Custom(v.into()))
    }
}

/// A queryable set of glyph data
///
/// Always queries static data from glyphsLib. Optionally includes a set of override values as well.
///
/// Default/no overrides instances are cheap. Instances created with overrides are more expensive.
#[derive(Default)]
pub struct GlyphData {
    // override-names are preferred to names in data
    overrides: Option<HashMap<SmolStr, QueryResult>>,
    overrrides_by_codepoint: Option<HashMap<u32, SmolStr>>,
}

impl GlyphData {
    /// Overrides, if provided, explicitly assign the result for a given query
    pub(crate) fn new(overrides: Option<HashMap<SmolStr, QueryResult>>) -> Self {
        let overrrides_by_codepoint = overrides.as_ref().map(|overrides| {
            overrides
                .iter()
                .filter_map(|(k, v)| v.codepoint.map(|cp| (cp, k.clone())))
                .collect()
        });
        Self {
            overrides,
            overrrides_by_codepoint,
        }
    }

    /// Create a new data set with user provided overrides
    pub fn with_override_file(override_file: &Path) -> Result<Self, GlyphDataError> {
        let bytes = std::fs::read(override_file).map_err(|err| GlyphDataError::UserFile {
            path: override_file.to_owned(),
            reason: err.kind(),
        })?;
        let overrides = parse_entries(&bytes)?;
        Ok(GlyphData::new(Some(overrides)))
    }
}

/// The category and subcategory to use
///
/// Used for overrides and as the result of [`GlyphData::query`]
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct QueryResult {
    pub category: Category,
    pub subcategory: Option<Subcategory>,
    pub codepoint: Option<u32>,
    pub script: Option<Script>,
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
    #[error("Unknown script '{0}'")]
    InvalidScript(SmolStr),
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

/// Parse glyph info entries out of a GlyphData xml file.
pub(crate) fn parse_entries(xml: &[u8]) -> Result<HashMap<SmolStr, QueryResult>, GlyphDataError> {
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
    reader.config_mut().trim_text(true);

    check_and_advance_past_preamble(&mut reader)?;

    let mut by_name = HashMap::new();
    let mut alt_names = Vec::new();
    for result in
        iter_rows(&mut reader).map(|row| row.map_err(Into::into).and_then(parse_glyph_xml))
    {
        let info = result?;
        by_name.insert(
            info.name.clone(),
            QueryResult {
                category: info.category,
                subcategory: info.subcategory,
                codepoint: info.codepoint,
                script: info.script,
            },
        );
        for alt in info.alt_names {
            alt_names.push((
                alt,
                QueryResult {
                    category: info.category,
                    subcategory: info.subcategory,
                    codepoint: None,
                    script: info.script,
                },
            ));
        }
    }

    // apply alts after to ensure they can't steal "real" names
    for (name, value) in alt_names {
        by_name.entry(name).or_insert(value);
    }

    Ok(by_name)
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

struct GlyphInfoFromXml {
    name: SmolStr,
    alt_names: Vec<SmolStr>,
    category: Category,
    subcategory: Option<Subcategory>,
    codepoint: Option<u32>,
    script: Option<Script>,
}

fn parse_glyph_xml(item: BytesStart) -> Result<GlyphInfoFromXml, GlyphDataError> {
    let mut name = None;
    let mut category = None;
    let mut subcategory = None;
    let mut unicode = None;
    let mut alt_names = None;
    let mut script = None;

    for attr in item.attributes() {
        let attr = attr?;
        let value = attr.unescape_value()?;
        match attr.key.as_ref() {
            b"name" => name = Some(value),
            b"category" => category = Some(value),
            b"subCategory" => subcategory = Some(value),
            b"unicode" => unicode = Some(value),
            b"altNames" => alt_names = Some(value),
            b"script" => script = Some(value),
            b"production" | b"unicodeLegacy" | b"case" | b"direction" | b"description" => (),
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
        .transpose()?;
    let script = script
        .map(|cat| Script::from_str(cat.as_ref()).map_err(GlyphDataError::InvalidScript))
        .transpose()?;
    let codepoint = unicode
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

    Ok(GlyphInfoFromXml {
        name,
        alt_names,
        category,
        subcategory,
        codepoint,
        script,
    })
}

impl GlyphData {
    /// Get the info for the given name/codepoints, attempting to synthesize it if necessary
    ///
    /// Returns, from most to least preferred:
    ///
    /// 1. The matching override value
    /// 1. The matching value from bundled data
    /// 1. A computed value based on name heuristics
    ///
    // See https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L94
    pub fn query(&self, name: &str, codepoints: Option<&BTreeSet<u32>>) -> Option<QueryResult> {
        self.query_no_synthesis(name, codepoints)
            .or_else(|| {
                // Try without suffix, those can confuse matters. E.g. ogonek.A => ogonek
                // <https://github.com/googlefonts/fontc/issues/780#issuecomment-2674853729>
                match name.rfind('.') {
                    Some(idx) if idx > 0 => self.query_no_synthesis(&name[..idx], codepoints),
                    _ => None,
                }
            })
            // we don't have info for this glyph: can we synthesize it?
            .or_else(|| self.construct_category(name))
    }

    /// As [`Self::query`] but without a fallback to computed values.
    ///
    /// Exists to enable result synthesis to query.
    fn query_no_synthesis(
        &self,
        name: &str,
        codepoints: Option<&BTreeSet<u32>>,
    ) -> Option<QueryResult> {
        // Override?
        if let (Some(overrides), Some(overrides_by_codepoint)) = (
            self.overrides.as_ref(),
            self.overrrides_by_codepoint.as_ref(),
        ) {
            let override_result = overrides.get(name).or_else(|| {
                codepoints
                    .into_iter()
                    .flat_map(|cps| cps.iter())
                    .find_map(|cp: &u32| {
                        overrides_by_codepoint
                            .get(cp)
                            .and_then(|n| overrides.get(n))
                    })
            });
            if let Some(override_result) = override_result {
                return Some(QueryResult {
                    category: override_result.category,
                    subcategory: override_result.subcategory,
                    codepoint: override_result.codepoint,
                    script: override_result.script,
                });
            }
        }

        // No override, perhaps we have a direct answer?
        bundled::find_pos_by_name(name)
            .or_else(|| {
                codepoints
                    .into_iter()
                    .flat_map(|cps| cps.iter())
                    .find_map(|cp| bundled::find_pos_by_codepoint(*cp))
            })
            .or_else(|| find_pos_by_prod_name(name.into()))
            .map(|i| {
                bundled::get(i).unwrap_or_else(|| panic!("We found invalid index {i} somehow"))
            })
    }

    fn contains_name(&self, name: &str) -> bool {
        if let Some(overrides) = self.overrides.as_ref() {
            let name: SmolStr = name.into();
            if overrides.contains_key(&name) {
                return true;
            }
        }
        bundled::find_pos_by_name(name).is_some()
    }

    // https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L199
    fn construct_category(&self, name: &str) -> Option<QueryResult> {
        // in glyphs.app '_' prefix means "no export"
        if name.starts_with('_') {
            return None;
        }
        let base_name = self
            .split_glyph_suffix(name)
            .map(|(base, _)| base)
            .unwrap_or(name);
        if let Some(result) = self.query_no_synthesis(base_name, None) {
            return Some(result);
        }

        if let Some(base_names) = self.split_ligature_glyph_name(base_name) {
            let base_names_attributes: Vec<_> = base_names
                .iter()
                .filter_map(|name| self.query_no_synthesis(name, None))
                .collect();
            if let Some(first_attr) = base_names_attributes.first() {
                // if first is mark, we're a mark
                if first_attr.category == Category::Mark {
                    return Some(QueryResult {
                        category: Category::Mark,
                        subcategory: first_attr.subcategory,
                        codepoint: None,
                        script: None,
                    });
                } else if first_attr.category == Category::Letter {
                    // if first is letter and rest are marks/separators, we use info from first
                    if base_names_attributes
                        .iter()
                        .skip(1)
                        .map(|result| result.category)
                        .all(|cat| matches!(cat, Category::Mark | Category::Separator))
                    {
                        return Some(QueryResult {
                            category: first_attr.category,
                            subcategory: first_attr.subcategory,
                            codepoint: None,
                            script: None,
                        });
                    } else {
                        return Some(QueryResult {
                            category: Category::Letter,
                            subcategory: Some(Subcategory::Ligature),
                            codepoint: None,
                            script: None,
                        });
                    }
                }
            }
        };

        // finally fall back to checking the AGLFN for the base name:
        Self::construct_category_via_agl(base_name)
    }

    // this doesn't need a &self param, but we want it locally close to the
    // code that calls it, so we'll make it a type method :shrug:
    fn construct_category_via_agl(base_name: &str) -> Option<QueryResult> {
        if let Some(first_char) = fontdrasil::agl::glyph_name_to_unicode(base_name)
            .chars()
            .next()
        {
            let (category, subcategory) = category_from_icu(first_char);

            // Exception: Something like "one_two" should be a (_, Ligature),
            // "acutecomb_brevecomb" should however stay (Mark, Nonspacing).
            if base_name.contains('_') && category != Category::Mark {
                return Some(QueryResult {
                    category,
                    subcategory: Some(Subcategory::Ligature),
                    codepoint: None,
                    script: None,
                });
            } else {
                return Some(QueryResult {
                    category,
                    subcategory,
                    codepoint: None,
                    script: None,
                });
            }
        }
        None
    }

    fn split_glyph_suffix<'n>(&self, name: &'n str) -> Option<(&'n str, &'n str)> {
        let multi_suffix = name.bytes().filter(|b| *b == b'.').count() > 1;
        if multi_suffix {
            // with multiple suffixes, try adding them one at a time and seeing if
            // we find a known name.
            // basically: for 'char.bottom.alt' we want to return (char.bottom, alt)
            // if
            for idx in name
                .bytes()
                .enumerate()
                .filter_map(|(i, b)| (b == b'.').then_some(i))
                .skip(1)
            {
                let (base, suffix) = name.split_at(idx);
                if self.contains_name(base) {
                    return Some((base, suffix));
                }
            }
        }
        // finally just split at the first dot
        name.split_once('.')
    }

    /// Split a ligature glyph into component parts
    ///
    /// Returns `None` if the name does not contain the '_' character, otherwise
    /// returns a list of names of components, derived from the glyph name.
    /// See
    /// <https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L307>
    fn split_ligature_glyph_name(&self, name: &str) -> Option<Vec<SmolStr>> {
        // if last part has a script suffix, grab it
        let script_suffix = name.rsplit_once('_')?.1.rsplit_once('-').map(|(_, x)| x);

        let mut parts: Vec<_> = name
            .trim_end_matches(script_suffix.unwrap_or_default())
            // after trimming script we also need to trim the '-'!
            .trim_end_matches('-')
            .split('_')
            .map(SmolStr::new)
            .collect();

        let script = match script_suffix {
            // if there was no suffix, we're done
            None => return Some(parts),
            Some(script) => script,
        };

        // otherwise we try adding the script suffix to each part, and see if
        // that's a known glyph name:
        for part in parts.iter_mut() {
            // if the part already has a script, continue
            if part.contains('-') {
                continue;
            }

            let new_part = smol_str::format_smolstr!("{part}-{script}");
            // if non-suffixed exists but suffixed doesn't, keep non-suffixed
            if self.contains_name(part.as_ref()) && !self.contains_name(&new_part) {
                continue;
            }
            *part = new_part;
        }
        Some(parts)
    }
}

// https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L261
fn category_from_icu(c: char) -> (Category, Option<Subcategory>) {
    match icu_properties::CodePointMapData::<GeneralCategory>::new().get(c) {
        GeneralCategory::Unassigned | GeneralCategory::OtherSymbol => (Category::Symbol, None),
        GeneralCategory::UppercaseLetter
        | GeneralCategory::LowercaseLetter
        | GeneralCategory::TitlecaseLetter
        | GeneralCategory::OtherLetter => (Category::Letter, None),
        GeneralCategory::ModifierLetter => (Category::Letter, Some(Subcategory::Modifier)),
        GeneralCategory::NonspacingMark => (Category::Mark, Some(Subcategory::Nonspacing)),
        GeneralCategory::SpacingMark => (Category::Mark, Some(Subcategory::SpacingCombining)),
        GeneralCategory::EnclosingMark => (Category::Mark, Some(Subcategory::Enclosing)),
        GeneralCategory::DecimalNumber | GeneralCategory::OtherNumber => {
            (Category::Number, Some(Subcategory::DecimalDigit))
        }
        GeneralCategory::LetterNumber => (Category::Number, None),
        GeneralCategory::SpaceSeparator => (Category::Separator, Some(Subcategory::Space)),
        GeneralCategory::LineSeparator
        | GeneralCategory::ParagraphSeparator
        | GeneralCategory::Control => (Category::Separator, None),
        GeneralCategory::Format => (Category::Separator, Some(Subcategory::Format)),
        GeneralCategory::PrivateUse => (Category::Letter, Some(Subcategory::Compatibility)),
        GeneralCategory::DashPunctuation => (Category::Punctuation, Some(Subcategory::Dash)),
        GeneralCategory::OpenPunctuation | GeneralCategory::ClosePunctuation => {
            (Category::Punctuation, Some(Subcategory::Parenthesis))
        }
        GeneralCategory::ConnectorPunctuation | GeneralCategory::OtherPunctuation => {
            (Category::Punctuation, None)
        }
        GeneralCategory::InitialPunctuation | GeneralCategory::FinalPunctuation => {
            (Category::Punctuation, Some(Subcategory::Quote))
        }
        GeneralCategory::MathSymbol => (Category::Symbol, Some(Subcategory::Math)),
        GeneralCategory::CurrencySymbol => (Category::Symbol, Some(Subcategory::Currency)),
        GeneralCategory::ModifierSymbol => (Category::Mark, Some(Subcategory::Spacing)),
        GeneralCategory::Surrogate => unreachable!("char cannot represent surrogate code points"),
    }
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
            "Composition" => Ok(Self::Composition),
            "Other" => Ok(Self::Other),
            _ => Err(s.into()),
        }
    }
}

impl FromStr for Script {
    type Err = SmolStr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "adlam" => Ok(Self::Adlam),
            "alchemical" => Ok(Self::Alchemical),
            "arabic" => Ok(Self::Arabic),
            "armenian" => Ok(Self::Armenian),
            "avestan" => Ok(Self::Avestan),
            "balinese" => Ok(Self::Balinese),
            "bamum" => Ok(Self::Bamum),
            "batak" => Ok(Self::Batak),
            "bengali" => Ok(Self::Bengali),
            "blackLetter" => Ok(Self::BlackLetter),
            "bopomofo" => Ok(Self::Bopomofo),
            "brahmi" => Ok(Self::Brahmi),
            "braille" => Ok(Self::Braille),
            "buginese" => Ok(Self::Buginese),
            "canadian" => Ok(Self::Canadian),
            "chakma" => Ok(Self::Chakma),
            "cham" => Ok(Self::Cham),
            "cherokee" => Ok(Self::Cherokee),
            "chorasmian" => Ok(Self::Chorasmian),
            "coptic" => Ok(Self::Coptic),
            "cyrillic" => Ok(Self::Cyrillic),
            "dentistry" => Ok(Self::Dentistry),
            "deseret" => Ok(Self::Deseret),
            "devanagari" => Ok(Self::Devanagari),
            "divesakuru" => Ok(Self::Divesakuru),
            "elbasan" => Ok(Self::Elbasan),
            "elymaic" => Ok(Self::Elymaic),
            "ethiopic" => Ok(Self::Ethiopic),
            "georgian" => Ok(Self::Georgian),
            "glagolitic" => Ok(Self::Glagolitic),
            "gothic" => Ok(Self::Gothic),
            "greek" => Ok(Self::Greek),
            "gujarati" => Ok(Self::Gujarati),
            "gurmukhi" => Ok(Self::Gurmukhi),
            "han" => Ok(Self::Han),
            "hangul" => Ok(Self::Hangul),
            "hebrew" => Ok(Self::Hebrew),
            "javanese" => Ok(Self::Javanese),
            "kana" => Ok(Self::Kana),
            "kannada" => Ok(Self::Kannada),
            "kawi" => Ok(Self::Kawi),
            "kayahli" => Ok(Self::Kayahli),
            "khmer" => Ok(Self::Khmer),
            "khojki" => Ok(Self::Khojki),
            "lao" => Ok(Self::Lao),
            "latin" => Ok(Self::Latin),
            "lepcha" => Ok(Self::Lepcha),
            "lue" => Ok(Self::Lue),
            "mahjong" => Ok(Self::Mahjong),
            "malayalam" => Ok(Self::Malayalam),
            "mandaic" => Ok(Self::Mandaic),
            "math" => Ok(Self::Math),
            "mongolian" => Ok(Self::Mongolian),
            "musical" => Ok(Self::Musical),
            "myanmar" => Ok(Self::Myanmar),
            "nko" => Ok(Self::Nko),
            "nyiakeng puachue hmong" => Ok(Self::NyiakengPuachueHmong),
            "ogham" => Ok(Self::Ogham),
            "oriya" => Ok(Self::Oriya),
            "osage" => Ok(Self::Osage),
            "osmanya" => Ok(Self::Osmanya),
            "pahawh hmong" => Ok(Self::PahawhHmong),
            "phaistosDisc" => Ok(Self::PhaistosDisc),
            "rovas" => Ok(Self::Rovas),
            "runic" => Ok(Self::Runic),
            "samaritan" => Ok(Self::Samaritan),
            "shavian" => Ok(Self::Shavian),
            "sinhala" => Ok(Self::Sinhala),
            "syriac" => Ok(Self::Syriac),
            "tamil" => Ok(Self::Tamil),
            "telugu" => Ok(Self::Telugu),
            "thaana" => Ok(Self::Thaana),
            "thai" => Ok(Self::Thai),
            "tham" => Ok(Self::Tham),
            "tibet" => Ok(Self::Tibet),
            "tifinagh" => Ok(Self::Tifinagh),
            "vai" => Ok(Self::Vai),
            "yi" => Ok(Self::Yi),
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
            Self::Composition => write!(f, "Composition"),
            Self::Other => write!(f, "Other"),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn simple_overrides() {
        let overrides = HashMap::from([(
            "A".into(),
            QueryResult {
                category: Category::Mark,
                subcategory: Some(Subcategory::SpacingCombining),
                codepoint: Some(b'A' as u32),
                script: Some(Script::Alchemical),
            },
        )]);
        let data = GlyphData::new(Some(overrides));

        assert_eq!(data.query("A", None).unwrap().category, Category::Mark);
    }

    #[test]
    fn overrides_from_file() {
        let data =
            GlyphData::with_override_file(Path::new("./data/GlyphData_override_test.xml")).unwrap();
        assert_eq!(data.query("zero", None).unwrap().category, Category::Other);
        assert_eq!(data.query("C", None).unwrap().category, Category::Number);
    }

    fn get_category(name: &str, codepoints: &[u32]) -> Option<(Category, Option<Subcategory>)> {
        let codepoints = codepoints.iter().copied().collect();
        GlyphData::new(None)
            .query(name, Some(&codepoints))
            .map(|result| (result.category, result.subcategory))
    }

    // from python glyphsLib: https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d5/tests/glyphdata_test.py#L106
    #[test]
    fn py_test_category() {
        for (name, expected) in [
            (".notdef", Some((Category::Separator, None))),
            // this test case requires AGL lookup:
            ("uni000D", Some((Category::Separator, None))),
            (
                "boxHeavyUp",
                Some((Category::Symbol, Some(Subcategory::Geometry))),
            ),
            ("eacute", Some((Category::Letter, None))),
            ("Abreveacute", Some((Category::Letter, None))),
            ("C-fraktur", Some((Category::Letter, None))),
            ("fi", Some((Category::Letter, Some(Subcategory::Ligature)))),
            (
                "fi.alt",
                Some((Category::Letter, Some(Subcategory::Ligature))),
            ),
            (
                "hib-ko",
                Some((Category::Letter, Some(Subcategory::Syllable))),
            ),
            (
                "one.foo",
                Some((Category::Number, Some(Subcategory::DecimalDigit))),
            ),
            (
                "one_two.foo",
                Some((Category::Number, Some(Subcategory::Ligature))),
            ),
            (
                "o_f_f_i",
                Some((Category::Letter, Some(Subcategory::Ligature))),
            ),
            (
                "o_f_f_i.foo",
                Some((Category::Letter, Some(Subcategory::Ligature))),
            ),
            (
                "ain_alefMaksura-ar.fina",
                Some((Category::Letter, Some(Subcategory::Ligature))),
            ),
            (
                "brevecomb",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "brevecomb.case",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "brevecomb_acutecomb",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "brevecomb_acutecomb.case",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "caroncomb_dotaccentcomb",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "dieresiscomb_caroncomb",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "dieresiscomb_macroncomb",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "dotaccentcomb_macroncomb",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "macroncomb_dieresiscomb",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "dotaccentcomb_o",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "macronlowmod_O",
                Some((Category::Mark, Some(Subcategory::Modifier))),
            ),
            ("O_o", Some((Category::Letter, Some(Subcategory::Ligature)))),
            (
                "O_dotaccentcomb_o",
                Some((Category::Letter, Some(Subcategory::Ligature))),
            ),
            ("O_dotaccentcomb", Some((Category::Letter, None))),
            (
                "O_period",
                Some((Category::Letter, Some(Subcategory::Ligature))),
            ),
            ("O_nbspace", Some((Category::Letter, None))),
            ("_a", None),
            ("_aaa", None),
            (
                "dal_alef-ar",
                Some((Category::Letter, Some(Subcategory::Ligature))),
            ),
            (
                "dal_lam-ar.dlig",
                Some((Category::Letter, Some(Subcategory::Ligature))),
            ),
            ("po-khmer", Some((Category::Letter, None))),
            (
                "po-khmer.below",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
            (
                "po-khmer.below.ro",
                Some((Category::Mark, Some(Subcategory::Nonspacing))),
            ),
        ] {
            let result = get_category(name, &[]);
            assert_eq!(result, expected, "{name}: {result:?} != {expected:?}");
        }
    }

    // https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/tests/glyphdata_test.py#L145C5-L153C76
    #[test]
    fn py_category_by_unicode() {
        // "SignU.bn" is a non-standard name not defined in GlyphData.xml
        // 0x09C1 should match
        let result = get_category("SignU.bn", &[0x09C1]);
        assert_eq!(
            result,
            Some((Category::Mark, Some(Subcategory::Nonspacing)))
        )
    }

    // https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/tests/glyphdata_test.py#L155C5-L162C1
    // https://github.com/googlefonts/glyphsLib/issues/232
    #[test]
    fn py_bug_232() {
        let u = get_category("uni07F0", &[]);
        assert_eq!(u, Some((Category::Mark, Some(Subcategory::Nonspacing))));
        let g = get_category("longlowtonecomb-nko", &[]);
        assert_eq!(g, Some((Category::Mark, Some(Subcategory::Nonspacing))));
    }

    #[test]
    fn match_prod_name_with_suffix() {
        // https://github.com/googlefonts/fontc/issues/780#issuecomment-2674853729
        // "uni17BF.b" should match against production name uni17BF
        assert_eq!(
            Some((Category::Letter, None)),
            get_category("uni17BF.b", &[]),
        )
    }
}
