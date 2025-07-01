//! determining glyph properties
//!
//! This module provides access to glyph info extracted from bundled
//! (and potentially user-provided) data files.

use quick_xml::{
    events::{BytesStart, Event},
    Reader,
};
use std::{
    borrow::Cow,
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
    Lowercase,
    Uppercase,
    Smallcaps,
    Conjunct,
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
    Yezidi,
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

impl From<u32> for ProductionName {
    fn from(v: u32) -> ProductionName {
        if v <= 0xFFFF {
            ProductionName::Bmp(v)
        } else {
            ProductionName::NonBmp(v)
        }
    }
}

impl Display for ProductionName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProductionName::Bmp(cp) => write!(f, "uni{cp:04X}"),
            ProductionName::NonBmp(cp) => write!(f, "u{cp:X}"),
            ProductionName::Custom(s) => write!(f, "{s}"),
        }
    }
}

impl From<ProductionName> for SmolStr {
    fn from(v: ProductionName) -> SmolStr {
        match v {
            ProductionName::Bmp(cp) => smol_str::format_smolstr!("uni{cp:04X}"),
            ProductionName::NonBmp(cp) => smol_str::format_smolstr!("u{cp:X}"),
            ProductionName::Custom(s) => s,
        }
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
#[derive(Debug, Clone, PartialEq)]
pub struct QueryResult {
    pub category: Category,
    pub subcategory: Option<Subcategory>,
    pub codepoint: Option<u32>,
    pub script: Option<Script>,
    pub production_name: Option<ProductionName>,
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
                production_name: info.production_name.clone(),
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
                    production_name: info.production_name.clone(),
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
    production_name: Option<ProductionName>,
}

fn parse_glyph_xml(item: BytesStart) -> Result<GlyphInfoFromXml, GlyphDataError> {
    let mut name = None;
    let mut category = None;
    let mut subcategory = None;
    let mut unicode = None;
    let mut alt_names = None;
    let mut script = None;
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
            b"script" => script = Some(value),
            b"production" => production_name = Some(value.as_ref().into()),
            b"unicodeLegacy" | b"case" | b"direction" | b"description" => (),
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
            .or_else(|| self.construct_result(name))
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
                    production_name: override_result.production_name.clone(),
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

    fn construct_result(&self, name: &str) -> Option<QueryResult> {
        let category_subcategory = self.construct_category(name);
        let production_name = self.construct_production_name(name);
        if category_subcategory.is_none() && production_name.is_none() {
            return None;
        }
        // if we have a production name but no category, 'Other' is good enough
        let (category, subcategory) = category_subcategory.unwrap_or((Category::Other, None));
        Some(QueryResult {
            category,
            subcategory,
            codepoint: None,
            script: None,
            production_name,
        })
    }

    // https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L199
    fn construct_category(&self, name: &str) -> Option<(Category, Option<Subcategory>)> {
        // in glyphs.app '_' prefix means "no export"
        if name.starts_with('_') {
            return None;
        }
        let (base_name, _) = self.split_glyph_suffix(name);
        if let Some(result) = self.query_no_synthesis(base_name, None) {
            return Some((result.category, result.subcategory));
        }

        if let Some(base_names) = self.split_ligature_glyph_name(base_name) {
            let base_names_attributes: Vec<_> = base_names
                .iter()
                .filter_map(|name| self.query_no_synthesis(name, None))
                .collect();
            if let Some(first_attr) = base_names_attributes.first() {
                // if first is mark, we're a mark
                if first_attr.category == Category::Mark {
                    return Some((Category::Mark, first_attr.subcategory));
                } else if first_attr.category == Category::Letter {
                    // if first is letter and rest are marks/separators, we use info from first
                    if base_names_attributes
                        .iter()
                        .skip(1)
                        .map(|result| result.category)
                        .all(|cat| matches!(cat, Category::Mark | Category::Separator))
                    {
                        return Some((first_attr.category, first_attr.subcategory));
                    } else {
                        return Some((Category::Letter, Some(Subcategory::Ligature)));
                    }
                }
            }
        };

        // finally fall back to checking the AGLFN for the base name:
        Self::construct_category_via_agl(base_name)
    }

    // https://github.com/googlefonts/glyphsLib/blob/c4db6b981d5/Lib/glyphsLib/glyphdata.py#L351
    fn construct_production_name(&self, name: &str) -> Option<ProductionName> {
        fn append_suffix(base_name: &mut String, suffix: Option<&str>) {
            if let Some(suffix) = suffix {
                base_name.push('.');
                base_name.push_str(suffix);
            }
        }

        fn is_u_name(name: &str) -> bool {
            name.starts_with("u") && name[1..].bytes().all(|b| b.is_ascii_hexdigit())
        }

        let (base_name, suffix) = self.split_glyph_suffix(name);

        // if we have a production name for the base name, append the suffix and go home
        let prod_name_with_suffix = suffix.and_then(|_| {
            self.query_no_synthesis(base_name, None)
                .and_then(|result| result.production_name)
                .map(|base_prod_name| {
                    let mut prod_name = base_prod_name.to_string();
                    append_suffix(&mut prod_name, suffix);
                    prod_name.as_str().into()
                })
        });
        if prod_name_with_suffix.is_some() {
            return prod_name_with_suffix;
        }

        let base_names = self
            .split_ligature_glyph_name(base_name)
            .unwrap_or_else(|| vec![base_name.into()]);
        // Attempt to find a production name for each ligature component (or the whole base name).
        // Return early if any such names have no GlyphData entry
        // OR the entry doesn't specify a production name AND they aren't already AGLFN names...
        let prod_names: Vec<SmolStr> = base_names
            .into_iter()
            .map(|name| {
                self.query_no_synthesis(&name, None).and_then(|result| {
                    result.production_name.map(Into::into).or_else(|| {
                        // if no production name, return the name itself if already in AGLFN
                        fontdrasil::agl::char_for_agl_name(name.as_ref()).map(|_| name)
                    })
                })
            })
            .collect::<Option<_>>()?;

        // only (uniXXXX, uniYYYY, etc.) names with 4 hex digits can be concatenated using the
        // more compact format uniXXXXYYYY... uXXXXX names for characters beyond BMP are joined
        // in ligatures using the usual '_'.
        let any_characters_outside_bmp = prod_names
            .iter()
            .any(|name| name.len() > 5 && is_u_name(name.as_ref()));
        let any_uni_names = prod_names.iter().any(|name| name.starts_with("uni"));

        if !any_characters_outside_bmp && any_uni_names {
            let mut uni_names: Vec<Cow<str>> = Vec::new();
            for part in &prod_names {
                if let Some(stripped) = part.strip_prefix("uni") {
                    uni_names.push(Cow::Borrowed(stripped));
                } else if part.len() == 5 && is_u_name(part.as_ref()) {
                    uni_names.push(Cow::Borrowed(&part.as_ref()[1..]));
                } else if let Some(ch) = fontdrasil::agl::char_for_agl_name(part.as_ref()) {
                    uni_names.push(Cow::Owned(format!("{:04X}", ch as u32)));
                } else {
                    panic!("Unexpected part while constructing production name: {part}");
                }
            }
            let mut result = String::from("uni");
            for segment in uni_names {
                result.push_str(segment.as_ref());
            }
            append_suffix(&mut result, suffix);
            return Some(result.as_str().into());
        }

        let mut result = prod_names.join("_");
        append_suffix(&mut result, suffix);
        Some(result.as_str().into())
    }

    // this doesn't need a &self param, but we want it locally close to the
    // code that calls it, so we'll make it a type method :shrug:
    fn construct_category_via_agl(base_name: &str) -> Option<(Category, Option<Subcategory>)> {
        if let Some(first_char) = fontdrasil::agl::glyph_name_to_unicode(base_name)
            .chars()
            .next()
        {
            let (category, subcategory) = category_from_icu(first_char);

            // Exception: Something like "one_two" should be a (_, Ligature),
            // "acutecomb_brevecomb" should however stay (Mark, Nonspacing).
            if base_name.contains('_') && category != Category::Mark {
                return Some((category, Some(Subcategory::Ligature)));
            } else {
                return Some((category, subcategory));
            }
        }
        None
    }

    fn split_glyph_suffix<'n>(&self, name: &'n str) -> (&'n str, Option<&'n str>) {
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
                    // suffix starts with '.' so we strip it to match split_once below
                    return (base, Some(&suffix[1..]));
                }
            }
        }
        // finally just split at the first dot, or the whole name if no suffix
        name.split_once('.')
            .map_or_else(|| (name, None), |(base, suffix)| (base, Some(suffix)))
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
            "Lowercase" => Ok(Self::Lowercase),
            "Uppercase" => Ok(Self::Uppercase),
            "Smallcaps" => Ok(Self::Smallcaps),
            "Conjunct" => Ok(Self::Conjunct),

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
            Self::Lowercase => write!(f, "Lowercase"),
            Self::Uppercase => write!(f, "Uppercase"),
            Self::Smallcaps => write!(f, "Smallcaps"),
            Self::Conjunct => write!(f, "Conjunct"),
            Self::Other => write!(f, "Other"),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use rstest::rstest;

    #[test]
    fn simple_overrides() {
        let overrides = HashMap::from([(
            "A".into(),
            QueryResult {
                category: Category::Mark,
                subcategory: Some(Subcategory::SpacingCombining),
                codepoint: Some(b'A' as u32),
                script: Some(Script::Alchemical),
                production_name: Some(ProductionName::Custom("MagicA".into())),
            },
        )]);
        let data = GlyphData::new(Some(overrides));

        let result = data.query("A", None).unwrap();
        assert_eq!(result.category, Category::Mark);
        assert_eq!(result.subcategory, Some(Subcategory::SpacingCombining));
        assert_eq!(result.codepoint, Some(b'A' as u32));
        assert_eq!(result.script, Some(Script::Alchemical));
        assert_eq!(result.production_name, Some("MagicA".into()));
    }

    #[test]
    fn overrides_from_file() {
        let data =
            GlyphData::with_override_file(Path::new("./data/GlyphData_override_test.xml")).unwrap();
        assert_eq!(data.query("zero", None).unwrap().category, Category::Other);
        assert_eq!(data.query("C", None).unwrap().category, Category::Number);
        assert_eq!(
            data.query("Yogh", None).unwrap().production_name,
            Some("Yolo".into())
        );
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

    #[rstest(name, expected,
        case("A", None),  // AGLFN names *are* production names
        case("z", None),
        case("nbspace", Some("uni00A0")),
        case("nonbreakingspace", Some("uni00A0")),  // altNames map to the same prod name
        case("uni00A0", Some("uni00A0")),  // prod names are already prod
        // the «» punctuation marks are spelled with an 'guillemets' in French, but for
        // some reasons the AGLFN has 'guillemot' (that's actually a bird! :shrug:)
        case("guillemetleft", Some("guillemotleft")),
        case("twosevenths", Some("two_fraction_seven")),
        case("idotaccent", Some("i.loclTRK")),
        case("idotless", Some("dotlessi")),
        case("Jacute", Some("uni004A0301")),
        case("scurl", Some("u1DF1E")),
        // In the old AGL, Delta was confused with increment 0x2206 so now it's banned
        // from the Greek alphabet.
        case("Delta", Some("uni0394")),
        case("increment", Some("uni2206")),
        case("dog-ko", Some("uniB3C5")),
        case("bau-kannada", Some("uni0CAC0CCC")),
        case("EnglandFlag", Some("u1F3F4E0067E0062E0065E006EE0067E007F")),
        case("pileOfPoo", Some("u1F4A9")),
        case("lam_alef-ar.fina", Some("uni06440627.fina")),
    )]
    fn query_production_names(name: &str, expected: Option<&str>) {
        let production_name = GlyphData::new(None)
            .query_no_synthesis(name, None)
            .unwrap()
            .production_name
            .map(|p| p.to_string());
        assert_eq!(
            production_name,
            expected.map(Into::into),
            "{name}: {production_name:?} != {expected:?}"
        );
    }

    // Python original test cases for synthetic production names:
    // https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d59bec0c9437da3a748c58f2999911/tests/glyphdata_test.py#L196-L409
    // Note that I removed a bunch of them as they were too many and repetitive
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
        case("sh_ra_iiMatra-tamil", "uni0BB60BCD0BB00BC0"),
        // plus some more tests that are not in glyphsLib
        case("A_A", "A_A"),
        case("a_a.sc", "a_a.sc"),
        case("brevecomb_acutecomb", "uni03060301"),
        case("brevecomb_acutecomb.case", "uni03060301.case"),
        case("pileOfPoo_pileOfPoo", "u1F4A9_u1F4A9"),
        case("pileOfPoo.ss01", "u1F4A9.ss01"),
        case("lam_alef-ar.fina.ss02", "uni06440627.fina.ss02"),
    )]
    fn synthetic_production_names(name: &str, expected: &str) {
        let production_name = GlyphData::new(None)
            .query(name, None)
            .unwrap()
            .production_name
            .unwrap()
            .to_string();
        assert_eq!(
            &production_name, expected,
            "{name}: {production_name:?} != {expected:?}"
        );
    }
}
