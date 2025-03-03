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
    data: &'static [(&'static str, QueryResult)],
    // Sorted by codepoint, unique codepoints, therefore safe to bsearch
    codepoint_to_data_index: &'static [(u32, usize)],
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
                .filter_map(|(k, v)| {
                    v.production_name
                        .clone()
                        .map(|pn| (pn.smolstr(v.codepoint.unwrap_or_default()), k.clone()))
                })
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
        Self {
            data: glyphslib_data::GLYPH_INFO,
            codepoint_to_data_index: glyphslib_data::CODEPOINT_TO_INFO_IDX,
            production_name_to_data_index: glyphslib_data::PRODUCTION_NAME_TO_INFO_IDX,
            overrides: None,
            overrides_by_codepoint: None,
            overrides_by_production_name: None,
        }
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn qr(
    category: Category,
    subcategory: Subcategory,
    codepoint: u32,
) -> QueryResult {
    QueryResult {
        category,
        subcategory: Some(subcategory),
        codepoint: Some(codepoint),
        production_name: None,
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn qru(
    category: Category,
    subcategory: Subcategory,
    codepoint: u32,
) -> QueryResult {
    QueryResult {
        category,
        subcategory: Some(subcategory),
        codepoint: Some(codepoint),
        production_name: Some(ProductionName::PrefixUni),
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn qrv(
    category: Category,
    subcategory: Subcategory,
    codepoint: u32,
) -> QueryResult {
    QueryResult {
        category,
        subcategory: Some(subcategory),
        codepoint: Some(codepoint),
        production_name: Some(ProductionName::PrefixU),
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn qrc(
    category: Category,
    subcategory: Subcategory,
    codepoint: u32,
    prod_name: &'static str,
) -> QueryResult {
    QueryResult {
        category,
        subcategory: Some(subcategory),
        codepoint: Some(codepoint),
        production_name: Some(ProductionName::Custom(SmolStr::new_static(prod_name))),
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q1(category: Category) -> QueryResult {
    QueryResult {
        category,
        subcategory: None,
        codepoint: None,
        production_name: None,
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q1c(category: Category, prod_name: &'static str) -> QueryResult {
    QueryResult {
        category,
        subcategory: None,
        codepoint: None,
        production_name: Some(ProductionName::Custom(SmolStr::new_static(prod_name))),
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q2(category: Category, codepoint: u32) -> QueryResult {
    QueryResult {
        category,
        subcategory: None,
        codepoint: Some(codepoint),
        production_name: None,
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q2u(category: Category, codepoint: u32) -> QueryResult {
    QueryResult {
        category,
        subcategory: None,
        codepoint: Some(codepoint),
        production_name: Some(ProductionName::PrefixUni),
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q2v(category: Category, codepoint: u32) -> QueryResult {
    QueryResult {
        category,
        subcategory: None,
        codepoint: Some(codepoint),
        production_name: Some(ProductionName::PrefixU),
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q2c(
    category: Category,
    codepoint: u32,
    prod_name: &'static str,
) -> QueryResult {
    QueryResult {
        category,
        subcategory: None,
        codepoint: Some(codepoint),
        production_name: Some(ProductionName::Custom(SmolStr::new_static(prod_name))),
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q3(category: Category, subcategory: Subcategory) -> QueryResult {
    QueryResult {
        category,
        subcategory: Some(subcategory),
        codepoint: None,
        production_name: None,
    }
}

/// Shorthand for construction of a [`QueryResult``] to shorten length of glyphslib_data.rs
pub(crate) const fn q3c(
    category: Category,
    subcategory: Subcategory,
    prod_name: &'static str,
) -> QueryResult {
    QueryResult {
        category,
        subcategory: Some(subcategory),
        codepoint: None,
        production_name: Some(ProductionName::Custom(SmolStr::new_static(prod_name))),
    }
}

// Some quick measurements, at time of writing:
//   45371 records have no prod name
//    5379 records have u{codepoint:04X} names
//   24555 records have uni{codepoint:04X} names
//    2415 records have a prod name of some other nature, such as 2665 => heart
//         1 of ^ appears to be an error: A7D5 is assigned the name uniA7D6
// To avoid storing a giant and almost entirely pointless array lets capture the patterns here
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ProductionName {
    // The production name is u{codepoint:04X}
    PrefixU,
    // The production name is uni{codepoint:04X}
    PrefixUni,
    // The production name is some custom thing
    Custom(SmolStr),
}

impl ProductionName {
    // Only for overlays, static data knows the answer in advance
    fn new(codepoint: Option<u32>, s: impl AsRef<str>) -> Self {
        let s = s.as_ref();
        let Some(codepoint) = codepoint else {
            return ProductionName::Custom(s.into());
        };
        if s.starts_with("uni") && format!("uni{codepoint:04X}") == s {
            return ProductionName::PrefixUni;
        }
        if s.starts_with("u") && format!("u{codepoint:04X}") == s {
            return ProductionName::PrefixU;
        }
        return ProductionName::Custom(s.into());
    }

    fn smolstr(&self, codepoint: u32) -> SmolStr {
        match self {
            ProductionName::PrefixU => format!("u{codepoint:04X}").into(),
            ProductionName::PrefixUni => format!("uni{codepoint:04X}").into(),
            ProductionName::Custom(n) => n.clone(),
        }
    }
}

/// The category, subcategory, codepoint and production name to use
///
/// Used for overrides and as the result of [`GlyphData::query`]
#[derive(Debug, Clone)]
pub struct QueryResult {
    pub category: Category,
    pub subcategory: Option<Subcategory>,
    pub codepoint: Option<u32>,
    pub(crate) production_name: Option<ProductionName>,
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
                production_name: info
                    .production_name
                    .map(|n| ProductionName::new(info.codepoint, n)),
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
        // TODO: if production_name is None, we should try to synthesize it
        // like glyphsLib does here:
        // https://github.com/googlefonts/glyphsLib/blob/c4db6b9/Lib/glyphsLib/glyphdata.py#L351-L453
        self.query_no_synthesis(name, codepoints)
            .cloned()
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
    ) -> Option<&QueryResult> {
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
            if override_result.is_some() {
                return override_result;
            }
        }

        // No override, perhaps we have a direct answer?
        self.data
            .binary_search_by(|(n, _)| (*n).cmp(name))
            .ok()
            .map(|i| (i, &self.data[i].1))
            .or_else(|| {
                self.production_name_to_data_index
                    .binary_search_by(|(n, _)| (*n).cmp(name))
                    .ok()
                    .map(|i| {
                        let j = self.production_name_to_data_index[i].1;
                        (j, &self.data[j].1)
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
                                (j, &self.data[j].1)
                            })
                    })
            })
            .map(|(_, r)| r)
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
            return Some(result.clone());
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
    use rstest::rstest;

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
                production_name: Some(ProductionName::PrefixU),
            },
        )]);
        let data = GlyphData::new(Some(overrides));

        let result = data.query("A", None).unwrap();
        assert_eq!(result.category, Category::Mark);
        assert_eq!(result.production_name, Some(ProductionName::PrefixU));
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
        assert_eq!(
            yogh.production_name,
            Some(ProductionName::Custom("Yolo".into()))
        );
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

    fn query_by_name(name: &str) -> QueryResult {
        GlyphData::new(None).query(name, None).unwrap()
    }

    #[rstest(name, expected,
        case("A", None),  // AGLFN names *are* production names
        case("z", None),
        // The AGLFN authors: "hm, this one looks like an omega..."
        case("omega1", None),
        case("nbspace", Some((ProductionName::PrefixUni, "uni00A0"))),
        // case("nonbreakingspace", Some("uni00A0")),  // FIXME: lookup by altName doesn't work
        // case("uni00A0", Some("uni00A0")),
        // // the «» punctuation marks are spelled with an 'guillemets' in French, but for
        // // some reasons the AGLFN has 'guillemot' (that's actually a bird! :shrug:)
        // case("guillemetleft", Some("guillemotleft")),
        // case("twosevenths", Some("two_fraction_seven")),
        // case("idotaccent", Some("i.loclTRK")),
        // case("idotless", Some("dotlessi")),
        // case("Jacute", Some("uni004A0301")),
        // case("amod-cy", Some("u1E030")),
        // case("scurl", Some("uni1DF1E")),
        // // In the old AGL, Delta was confused with increment 0x2206 so now it's banned
        // // from the Greek alphabet.
        // case("Delta", Some("uni0394")),
        // case("increment", Some("uni2206")),
        // case("dog-ko", Some("uniB3C5")),
        // case("bau-kannada", Some("uni0CAC0CCC")),
        // case("EnglandFlag", Some("u1F3F4E0067E0062E0065E006EE0067E007F")),
        // case("pileOfPoo", Some("u1F4A9")),
    )]
    fn query_production_names(name: &str, expected: Option<(ProductionName, &str)>) {
        let result = query_by_name(name);
        assert_eq!(
            result.production_name.map(|n| (n, n.smol_str(result.codepoint).as_str())),
            expected,
            "{name}: {result:?} != {expected:?}"
        );
    }

    // Python original test cases for synthetic production names:
    // https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d59bec0c9437da3a748c58f2999911/tests/glyphdata_test.py#L196-L409
    // Note that I removed a bunch of them as they were too many and repetitive
    #[ignore] // TODO: remove this once we actually implement
    #[rstest(
        name,
        expected,
        case("Ech_Vew-arm.liga", "uni0535054E.liga"),
        case("aiMatra_anusvara-deva", "uni09480902"),
        case("aiMatra_reph_anusvara-deva", "uni09480930094D0902"),
        case("ca_iMatra-tamil", "uni0B9A0BBF"),
        case("ch_ya-deva", "uni091B094D092F"),
        case("d_dh_ya-deva", "uni0926094D0927094D092F"),
        case("da-khmer.below.ro", "uni17D2178A.ro"),
        case("da_rVocalicMatra-deva", "uni09260943"),
        case("dd_dda-deva", "uni0921094D0921"),
        case("eShortMatra_reph_anusvara-deva", "uni09460930094D0902"),
        case("ech_vew-arm.liga.sc", "uni0565057E.liga.sc"),
        case("finalkaf_qamats-hb", "uni05DA05B8"),
        case("finalkaf_sheva-hb", "uni05DA05B0"),
        case("finalkafdagesh_qamats-hb", "uniFB3A05B8"),
        case("finalkafdagesh_sheva-hb", "uniFB3A05B0"),
        case("h_la-deva", "uni0939094D0932"),
        case("ha_iMatra-tamil", "uni0BB90BBF"),
        case("hatafpatah_siluqleft-hb", "uni05B205BD"),
        case("iMark_toandakhiat-khmer.narrow", "uni17B717CD.narrow"),
        case("idotaccent.sc", "i.loclTRK.sc"),
        case("iiMatra_reph-deva", "uni09400930094D"),
        case("iiMatra_reph-deva.alt2", "uni09400930094D.alt2"),
        case("j_ny-deva", "uni091C094D091E094D"),
        case("j_ny-deva.alt2", "uni091C094D091E094D.alt2"),
        case("mo-khmer.below.ro", "uni17D21798.ro"),
        case("moMa_underscore-thai", "uni0E21005F"),
        case("nno-khmer.below.narrow1", "uni17D2178E.narrow1"),
        case("nyo-khmer.full.below.narrow", "uni17D21789.full.below.narrow"),
        case("sh_ra_iiMatra-tamil", "uni0BB60BCD0BB00BC0")
    )]
    fn synthetic_production_names(name: &str, expected: &str) {
        let result = get_production_name(name);
        assert_eq!(
            result,
            Some(expected.into()),
            "{name}: {result:?} != {expected:?}"
        );
    }
}
