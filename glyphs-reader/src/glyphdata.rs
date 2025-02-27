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

use crate::glyphslib_data;

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
}

/// A queryable set of glyph data
///
/// Always includes static data from glyphsLib. Optionally includes a set of override values as well.
///
/// Default/no overrides instances are cheap. Instances created with overrides are more expensive.
pub struct GlyphData {
    // Sorted by name, unique names, therefore safe to bsearch
    data: &'static [(&'static str, QueryPartialResult)],
    // Sorted by codepoint, unique codepoints, therefore safe to bsearch
    codepoint_to_data_index: &'static [(u32, usize)],
    // Same length as data slice, therefore safe to index into; empty str means no production name
    production_names: &'static [&'static str],
    // Sorted by production name, unique production names, therefore safe to bsearch
    production_name_to_data_index: &'static [(&'static str, usize)],

    // override-names are preferred to names in data
    overrides: Option<HashMap<SmolStr, QueryResult>>,
    overrides_by_codepoint: Option<HashMap<u32, SmolStr>>,
    overrides_by_production_name: Option<HashMap<SmolStr, SmolStr>>,
}

impl GlyphData {
    /// Overrides, if provided, explicitly assign the result for a given query
    pub(crate) fn new(overrides: Option<HashMap<SmolStr, QueryResult>>) -> Self {
        let overrides_by_codepoint = overrides.as_ref().map(|overrides| {
            overrides
                .iter()
                .filter_map(|(k, v)| v.codepoint.map(|cp| (cp, k.clone())))
                .collect()
        });
        let overrides_by_production_name = overrides.as_ref().map(|overrides| {
            overrides
                .iter()
                .filter_map(|(k, v)| v.production_name.clone().map(|pn| (pn, k.clone())))
                .collect()
        });
        Self {
            overrides,
            overrides_by_codepoint,
            overrides_by_production_name,
            ..Default::default()
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

impl Default for GlyphData {
    fn default() -> Self {
        debug_assert_eq!(
            glyphslib_data::GLYPH_INFO.len(),
            glyphslib_data::PRODUCTION_NAMES.len(),
            "GLYPH_INFO and PRODUCTION_NAEMS must be the same length"
        );
        Self {
            data: glyphslib_data::GLYPH_INFO,
            codepoint_to_data_index: glyphslib_data::CODEPOINT_TO_INFO_IDX,
            production_names: glyphslib_data::PRODUCTION_NAMES,
            production_name_to_data_index: glyphslib_data::PRODUCTION_NAME_TO_INFO_IDX,
            overrides: None,
            overrides_by_codepoint: None,
            overrides_by_production_name: None,
        }
    }
}

/// Shorthand for construction of a [`QueryPartialResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn qr(
    category: Category,
    subcategory: Subcategory,
    codepoint: u32,
) -> QueryPartialResult {
    QueryPartialResult {
        category,
        subcategory: Some(subcategory),
        codepoint: Some(codepoint),
    }
}

/// Shorthand for construction of a [`QueryPartialResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q1(category: Category) -> QueryPartialResult {
    QueryPartialResult {
        category,
        subcategory: None,
        codepoint: None,
    }
}

/// Shorthand for construction of a [`QueryPartialResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q2(category: Category, codepoint: u32) -> QueryPartialResult {
    QueryPartialResult {
        category,
        subcategory: None,
        codepoint: Some(codepoint),
    }
}

/// Shorthand for construction of a [`QueryPartialResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q3(category: Category, subcategory: Subcategory) -> QueryPartialResult {
    QueryPartialResult {
        category,
        subcategory: Some(subcategory),
        codepoint: None,
    }
}

/// A const-constructible version of QueryResult, without production_name
///
/// This is useful for the bundled glyphslib_data.rs file.
#[derive(Debug, Copy, Clone)]
pub(crate) struct QueryPartialResult {
    category: Category,
    subcategory: Option<Subcategory>,
    codepoint: Option<u32>,
}

/// The category, subcategory, codepoint and production name to use
///
/// Used for overrides and as the result of [`GlyphData::query`]
#[derive(Debug, Clone)]
pub struct QueryResult {
    pub category: Category,
    pub subcategory: Option<Subcategory>,
    pub codepoint: Option<u32>,
    pub production_name: Option<SmolStr>,
}

impl From<QueryPartialResult> for QueryResult {
    fn from(value: QueryPartialResult) -> Self {
        Self {
            category: value.category,
            subcategory: value.subcategory,
            codepoint: value.codepoint,
            production_name: None,
        }
    }
}

impl QueryResult {
    fn with_production_name(self, production_name: &str) -> Self {
        Self {
            production_name: (!production_name.is_empty()).then_some(production_name.into()),
            ..self
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
                production_name: info.production_name,
            },
        );
        for alt in info.alt_names {
            alt_names.push((
                alt,
                QueryResult {
                    category: info.category,
                    subcategory: info.subcategory,
                    codepoint: None,
                    production_name: None,
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
    production_name: Option<SmolStr>,
}

fn parse_glyph_xml(item: BytesStart) -> Result<GlyphInfoFromXml, GlyphDataError> {
    let mut name = None;
    let mut category = None;
    let mut subcategory = None;
    let mut unicode = None;
    let mut alt_names = None;
    let mut production_name = None;

    for attr in item.attributes() {
        let attr = attr?;
        let value = attr.unescape_value()?;
        match attr.key.as_ref() {
            b"name" => name = Some(value),
            b"category" => category = Some(value),
            b"subCategory" => subcategory = Some(value),
            b"unicode" => unicode = Some(value),
            b"altNames" => alt_names = Some(value),
            b"production" => production_name = Some(value),
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
    let production_name = production_name.map(SmolStr::new);

    Ok(GlyphInfoFromXml {
        name,
        alt_names,
        category,
        subcategory,
        codepoint,
        production_name,
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
        if let (Some(overrides), Some(overrides_by_codepoint), Some(overrides_by_production_name)) = (
            self.overrides.as_ref(),
            self.overrides_by_codepoint.as_ref(),
            self.overrides_by_production_name.as_ref(),
        ) {
            let override_result = overrides
                .get(name)
                .or_else(|| {
                    overrides_by_production_name
                        .get(name)
                        .and_then(|n| overrides.get(n))
                })
                .or_else(|| {
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
                    production_name: override_result.production_name.clone(),
                });
            }
        }

        // No override, perhaps we have a direct answer?
        self.data
            .binary_search_by(|(n, _)| (*n).cmp(name))
            .ok()
            .map(|i| (i, self.data[i].1))
            .or_else(|| {
                self.production_name_to_data_index
                    .binary_search_by(|(n, _)| (*n).cmp(name))
                    .ok()
                    .map(|i| {
                        let j = self.production_name_to_data_index[i].1;
                        (j, self.data[j].1)
                    })
            })
            .or_else(|| {
                codepoints
                    .into_iter()
                    .flat_map(|cps| cps.iter())
                    .find_map(|cp| {
                        self.codepoint_to_data_index
                            .binary_search_by(|(info_cp, _)| info_cp.cmp(cp))
                            .ok()
                            .map(|i| {
                                let j = self.codepoint_to_data_index[i].1;
                                (j, self.data[j].1)
                            })
                    })
            })
            .map(|(i, r)| QueryResult::from(r).with_production_name(self.production_names[i]))
    }

    fn contains_name(&self, name: &str) -> bool {
        if let Some(overrides) = self.overrides.as_ref() {
            let name: SmolStr = name.into();
            if overrides.contains_key(&name) {
                return true;
            }
        }
        self.data.binary_search_by(|(n, _)| (*n).cmp(name)).is_ok()
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
                        production_name: None,
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
                            production_name: None,
                        });
                    } else {
                        return Some(QueryResult {
                            category: Category::Letter,
                            subcategory: Some(Subcategory::Ligature),
                            codepoint: None,
                            production_name: None,
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
                    production_name: None,
                });
            } else {
                return Some(QueryResult {
                    category,
                    subcategory,
                    codepoint: None,
                    production_name: None,
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
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_bundled_data() {
        let data = GlyphData::new(None).data;
        assert!(data.len() > 70000, "{}", data.len());
    }

    #[test]
    fn simple_overrides() {
        let overrides = HashMap::from([(
            "A".into(),
            QueryResult {
                category: Category::Mark,
                subcategory: Some(Subcategory::SpacingCombining),
                codepoint: Some(b'A' as u32),
                production_name: Some("u0041".into()),
            },
        )]);
        let data = GlyphData::new(Some(overrides));

        let result = data.query("A", None).unwrap();
        assert_eq!(result.category, Category::Mark);
        assert_eq!(result.production_name, Some("u0041".into()));
    }

    #[test]
    fn category_overrides_from_file() {
        let data =
            GlyphData::with_override_file(Path::new("./data/GlyphData_override_test.xml")).unwrap();
        assert_eq!(data.query("zero", None).unwrap().category, Category::Other);
        assert_eq!(data.query("C", None).unwrap().category, Category::Number);
    }

    fn assert_query_results_are_equal(result: &QueryResult, expected: &QueryResult) {
        assert_eq!(result.category, expected.category);
        assert_eq!(result.subcategory, expected.subcategory);
        assert_eq!(result.codepoint, expected.codepoint);
        assert_eq!(result.production_name, expected.production_name);
    }

    #[test]
    fn production_name_overrides_from_file() {
        let data =
            GlyphData::with_override_file(Path::new("./data/GlyphData_override_test.xml")).unwrap();
        let yogh = data.query("Yogh", None).unwrap();
        assert_eq!(yogh.production_name, Some("Yolo".into()));
        assert_eq!(yogh.codepoint, Some(0x021C));
        assert_eq!(yogh.category, Category::Letter);
        // the same query result can be looked up by codepoint or production name
        let yogh_by_cp = data.query("foo", Some(&[0x021C].into())).unwrap();
        assert_query_results_are_equal(&yogh_by_cp, &yogh);
        let yogh_by_pn = data.query("Yolo", None).unwrap();
        assert_query_results_are_equal(&yogh_by_pn, &yogh);
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
}
