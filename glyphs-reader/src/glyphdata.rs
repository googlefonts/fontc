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
    num::ParseIntError,
    path::{Path, PathBuf},
    str::FromStr,
};

use icu_properties::GeneralCategory;

use smol_str::SmolStr;

use crate::{glyphslib_data, Category, Subcategory};

/// A queryable set of glyph data
///
/// Always includes static data from glyphsLib. Optionally includes a set of override values as well.
///
/// Access via [`GlyphData::glyphs_lib_data`] is cheap. Instances created with overrides
/// are more expensive.
pub struct GlyphData {
    // Sorted by name, unique names, therefore safe to bsearch
    data: &'static [GlyphInfo],
    // Sorted by codepoint, unique codepoints, therefore safe to bsearch
    codepoint_to_data_index: &'static [(u32, usize)],

    // override-names are preferred to names in data
    overrides: Option<HashMap<SmolStr, GlyphOverride>>,
    overrrides_by_codepoint: Option<HashMap<u32, SmolStr>>,
}

impl GlyphData {
    /// Return the default glyph data set, derived from Python glyphsLib resources
    pub fn glyphs_lib_data() -> Self {
        Self {
            data: glyphslib_data::GLYPH_INFO,
            codepoint_to_data_index: glyphslib_data::CODEPOINT_TO_INFO_IDX,
            overrides: None,
            overrrides_by_codepoint: None,
        }
    }

    /// Create a new data set with user provided overrides
    pub fn with_override_file(override_file: &Path) -> Result<Self, GlyphDataError> {
        let bytes = std::fs::read(override_file).map_err(|err| GlyphDataError::UserFile {
            path: override_file.to_owned(),
            reason: err.kind(),
        })?;
        let overrides = parse_entries(&bytes)?;
        GlyphData::with_overrides(overrides)
    }

    /// Create a new data set with user provided overrides
    pub(crate) fn with_overrides(
        overrides: HashMap<SmolStr, GlyphOverride>,
    ) -> Result<Self, GlyphDataError> {
        let override_by_codepoint = overrides
            .iter()
            .filter_map(|(k, v)| v.codepoint.map(|cp| (cp, k.clone())))
            .collect();
        Ok(Self {
            data: glyphslib_data::GLYPH_INFO,
            codepoint_to_data_index: glyphslib_data::CODEPOINT_TO_INFO_IDX,
            overrides: Some(overrides),
            overrrides_by_codepoint: Some(override_by_codepoint),
        })
    }
}

/// The subset of GlyphData.xml or GlyphData_Ideographs.xml we care about
#[derive(Clone, Copy, Debug)]
pub(crate) struct GlyphInfo {
    name: &'static str,
    category: Category,
    subcategory: Option<Subcategory>,
    codepoint: Option<u32>,
}

impl GlyphInfo {
    pub(crate) const fn new(
        name: &'static str,
        category: Category,
        subcategory: Option<Subcategory>,
        codepoint: Option<u32>,
    ) -> Self {
        Self {
            name,
            category,
            subcategory,
            codepoint,
        }
    }
}

/// The category and subcategory to use when specified by an override
pub(crate) struct GlyphOverride {
    category: Category,
    subcategory: Option<Subcategory>,
    codepoint: Option<u32>,
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
pub(crate) fn parse_entries(xml: &[u8]) -> Result<HashMap<SmolStr, GlyphOverride>, GlyphDataError> {
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
            GlyphOverride {
                category: info.category,
                subcategory: info.subcategory,
                codepoint: info.codepoint,
            },
        );
        for alt in info.alt_names {
            alt_names.push((
                alt,
                GlyphOverride {
                    category: info.category,
                    subcategory: info.subcategory,
                    codepoint: None,
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
}

fn parse_glyph_xml(item: BytesStart) -> Result<GlyphInfoFromXml, GlyphDataError> {
    let mut name = None;
    let mut category = None;
    let mut subcategory = None;
    let mut unicode = None;
    let mut alt_names = None;

    for attr in item.attributes() {
        let attr = attr?;
        let value = attr.unescape_value()?;
        match attr.key.as_ref() {
            b"name" => name = Some(value),
            b"category" => category = Some(value),
            b"subCategory" => subcategory = Some(value),
            b"unicode" => unicode = Some(value),
            b"altNames" => alt_names = Some(value),
            b"production" | b"unicodeLegacy" | b"case" | b"direction" | b"script"
            | b"description" => (),
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

    Ok(GlyphInfoFromXml {
        name,
        alt_names,
        category,
        subcategory,
        codepoint,
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
    pub fn query(
        &self,
        name: &str,
        codepoints: Option<&BTreeSet<u32>>,
    ) -> Option<(Category, Option<Subcategory>, Option<u32>)> {
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
    ) -> Option<(Category, Option<Subcategory>, Option<u32>)> {
        // Override?
        if let (Some(overrides), Some(overrides_by_codepoint)) = (
            self.overrides.as_ref(),
            self.overrrides_by_codepoint.as_ref(),
        ) {
            let name: SmolStr = name.into();
            let override_result = overrides.get(&name).or_else(|| {
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
                return Some((
                    override_result.category,
                    override_result.subcategory,
                    override_result.codepoint,
                ));
            }
        }

        // No override, perhaps we have a direct answer?
        let info = self
            .data
            .binary_search_by(|gi| gi.name.cmp(name))
            .ok()
            .map(|i| &self.data[i])
            .or_else(|| {
                codepoints
                    .into_iter()
                    .flat_map(|cps| cps.iter())
                    .find_map(|cp| {
                        self.codepoint_to_data_index
                            .binary_search_by(|(info_cp, _)| info_cp.cmp(cp))
                            .ok()
                            .map(|i| &self.data[self.codepoint_to_data_index[i].1])
                    })
            });
        info.map(|info| (info.category, info.subcategory, info.codepoint))
    }

    fn contains_name(&self, name: &str) -> bool {
        if let Some(overrides) = self.overrides.as_ref() {
            let name: SmolStr = name.into();
            if overrides.contains_key(&name) {
                return true;
            }
        }
        self.data.binary_search_by(|gi| gi.name.cmp(name)).is_ok()
    }

    // https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L199
    fn construct_category(
        &self,
        name: &str,
    ) -> Option<(Category, Option<Subcategory>, Option<u32>)> {
        // TODO: python does production name here.
        // see https://github.com/googlefonts/fontc/issues/780

        // in glyphs.app '_' prefix means "no export"
        if name.starts_with('_') {
            return None;
        }
        let base_name = self
            .split_glyph_suffix(name)
            .map(|(base, _)| base)
            .unwrap_or(name);
        if let Some(info) = self.query_no_synthesis(base_name, None) {
            return Some(info);
        }

        if let Some(base_names) = self.split_ligature_glyph_name(base_name) {
            let base_names_attributes: Vec<_> = base_names
                .iter()
                .filter_map(|name| self.query_no_synthesis(name, None))
                .collect();
            if let Some(first_attr) = base_names_attributes.first() {
                // if first is mark, we're a mark
                if first_attr.0 == Category::Mark {
                    return Some((Category::Mark, first_attr.1, None));
                } else if first_attr.0 == Category::Letter {
                    // if first is letter and rest are marks/separators, we use info from first
                    if base_names_attributes
                        .iter()
                        .skip(1)
                        .map(|(cat, ..)| cat)
                        .all(|cat| matches!(cat, Category::Mark | Category::Separator))
                    {
                        return Some((first_attr.0, first_attr.1, None));
                    } else {
                        return Some((Category::Letter, Some(Subcategory::Ligature), None));
                    }
                }
            }
        };

        // finally fall back to checking the AGLFN for the base name:
        Self::construct_category_via_agl(base_name)
    }

    // this doesn't need a &self param, but we want it locally close to the
    // code that calls it, so we'll make it a type method :shrug:
    fn construct_category_via_agl(
        base_name: &str,
    ) -> Option<(Category, Option<Subcategory>, Option<u32>)> {
        if let Some(first_char) = fontdrasil::agl::glyph_name_to_unicode(base_name)
            .chars()
            .next()
        {
            let (category, subcategory) = category_from_icu(first_char);

            // Exception: Something like "one_two" should be a (_, Ligature),
            // "acutecomb_brevecomb" should however stay (Mark, Nonspacing).
            if base_name.contains('_') && category != Category::Mark {
                return Some((category, Some(Subcategory::Ligature), None));
            } else {
                return Some((category, subcategory, None));
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
    match icu_properties::maps::general_category().get(c) {
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

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_bundled_data() {
        let data = GlyphData::glyphs_lib_data().data;
        assert!(data.len() > 70000, "{}", data.len());
    }

    #[test]
    fn simple_overrides() {
        let overrides = HashMap::from([(
            "A".into(),
            GlyphOverride {
                category: Category::Mark,
                subcategory: Some(Subcategory::SpacingCombining),
                codepoint: Some(b'A' as u32),
            },
        )]);
        let data = GlyphData::with_overrides(overrides).unwrap();

        assert_eq!(data.query("A", None).unwrap().0, Category::Mark);
    }

    #[test]
    fn overrides_from_file() {
        let data =
            GlyphData::with_override_file(Path::new("./data/GlyphData_override_test.xml")).unwrap();
        assert_eq!(data.query("zero", None).unwrap().0, Category::Other);
        assert_eq!(data.query("C", None).unwrap().0, Category::Number);
    }

    fn get_category(name: &str, codepoints: &[u32]) -> Option<(Category, Option<Subcategory>)> {
        let codepoints = codepoints.iter().copied().collect();
        GlyphData::glyphs_lib_data()
            .query(name, Some(&codepoints))
            .map(|(cat, sub, _)| (cat, sub))
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
