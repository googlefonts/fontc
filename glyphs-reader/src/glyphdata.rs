//! determining glyph properties
//!
//! This module provides access to glyph info extracted from bundled
//! (and potentially user-provided) data files.

// NOTE: we define the types and parsing code in a separate file, so that
// we can borrow it in our build.rs script without causing a cycle
mod glyphdata_impl;
use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap, HashSet},
    path::Path,
    sync::OnceLock,
};

pub use glyphdata_impl::*;
use icu_properties::GeneralCategory;

use smol_str::SmolStr;

static BUNDLED_DATA: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/glyphdata.bin"));

/// A queryable set of glyph data
///
/// This is generally expensive to create, and is intended to be cached, or
/// used behind a OnceCell. It is never modified after initial creation.
pub struct GlyphData {
    // The info for all the glyphs we know of.
    data: Vec<GlyphInfo>,
    // the values in all maps are indices into the `data` vec. we use u32 to save space.
    name_map: HashMap<SmolStr, u32>,
    unicode_map: HashMap<u32, u32>,
    alt_name_map: HashMap<SmolStr, u32>,
}

impl GlyphData {
    /// Return the default glyph data set, derived from GlyphData.xml files
    pub fn bundled() -> &'static GlyphData {
        static GLYPH_DATA: OnceLock<GlyphData> = OnceLock::new();
        GLYPH_DATA.get_or_init(|| GlyphData::new(None).unwrap())
    }

    /// Create a new data set, optionally loading user provided overrides
    pub fn new(user_overrides: Option<&Path>) -> Result<Self, GlyphDataError> {
        let user_overrides = user_overrides
            .map(|path| {
                let bytes = std::fs::read(path).map_err(|err| GlyphDataError::UserFile {
                    path: path.to_owned(),
                    reason: err.kind(),
                });
                bytes.and_then(|xml| parse_entries(&xml))
            })
            .transpose()?;
        let bundled = load_bundled_data();
        let all_entries = match user_overrides {
            Some(user_overrides) => merge_data(bundled, user_overrides),
            None => bundled,
        };

        Ok(Self::new_impl(all_entries))
    }

    fn new_impl(entries: Vec<GlyphInfo>) -> Self {
        let mut name_map = HashMap::with_capacity(entries.len());
        let mut unicode_map = HashMap::with_capacity(entries.len());
        let mut alt_name_map = HashMap::new();

        for (i, entry) in entries.iter().enumerate() {
            name_map.insert(entry.name.clone(), i as u32);
            if let Some(cp) = entry.unicode {
                unicode_map.insert(cp, i as _);
            }
            for alt in &entry.alt_names {
                alt_name_map.insert(alt.clone(), i as _);
            }
        }

        Self {
            data: entries,
            name_map,
            unicode_map,
            alt_name_map,
        }
    }

    /// Get the info for the given name/codepoints, attempting to synthesize it if necessary
    ///
    /// If this name or these unicode values were included in the bundled data,
    /// that will be returned; otherwise we will attempt to compute the value
    /// by performing various heuristics based on the name.
    ///
    // See https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L94
    pub fn get_glyph(
        &self,
        name: &str,
        codepoints: Option<&BTreeSet<u32>>,
    ) -> Option<Cow<GlyphInfo>> {
        if let Some(info) = self.get_by_name(name).or_else(|| {
            codepoints
                .into_iter()
                .flat_map(|cps| cps.iter())
                .find_map(|cp| self.get_by_codepoint(*cp))
        }) {
            return Some(Cow::Borrowed(info));
        }

        // we don't have info for this glyph: can we synthesize it?
        // TODO: python does production name here.
        // see https://github.com/googlefonts/fontc/issues/780

        let (category, subcategory) = self.construct_category(name)?;
        Some(Cow::Owned(GlyphInfo {
            name: name.into(),
            category,
            subcategory,
            unicode: None,
            production: None,
            alt_names: Default::default(),
        }))
    }

    /// Look up info for a glyph by name
    ///
    /// This checks primary names first, and alternates afterwards.
    ///
    /// Note: this is only checking the loaded data, it does not handle
    /// computing info if it is missing.
    fn get_by_name(&self, name: impl AsRef<str>) -> Option<&GlyphInfo> {
        let name = name.as_ref();
        self.name_map
            .get(name)
            .or_else(|| self.alt_name_map.get(name))
            .and_then(|idx| self.data.get(*idx as usize))
    }

    /// Look up info for a glyph by codepoint
    fn get_by_codepoint(&self, codepoint: u32) -> Option<&GlyphInfo> {
        self.unicode_map
            .get(&codepoint)
            .and_then(|idx| self.data.get(*idx as usize))
    }

    // https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L199
    fn construct_category(&self, name: &str) -> Option<(Category, Subcategory)> {
        // in glyphs.app '_' prefix means "no export"
        if name.starts_with('_') {
            return None;
        }
        let base_name = self
            .split_glyph_suffix(name)
            .map(|(base, _)| base)
            .unwrap_or(name);
        if let Some(info) = self.get_by_name(base_name) {
            return Some((info.category, info.subcategory));
        }

        if let Some(base_names) = self.split_ligature_glyph_name(base_name) {
            let base_names_attributes: Vec<_> = base_names
                .iter()
                .map(|name| self.get_by_name(name))
                .collect();
            if let Some(first_attr) = base_names_attributes.first().and_then(Option::as_ref) {
                // if first is mark, we're a mark
                if first_attr.category == Category::Mark {
                    return Some((Category::Mark, first_attr.subcategory));
                } else if first_attr.category == Category::Letter {
                    // if first is letter and rest are marks/separators, we use info from first
                    if base_names_attributes
                        .iter()
                        .skip(1)
                        .filter_map(|attr| attr.map(|attr| attr.category))
                        .all(|cat| matches!(cat, Category::Mark | Category::Separator))
                    {
                        return Some((first_attr.category, first_attr.subcategory));
                    } else {
                        return Some((Category::Letter, Subcategory::Ligature));
                    }
                }
            }
        };

        // finally fall back to checking the AGLFN for the base name:
        Self::construct_category_via_agl(base_name)
    }

    // this doesn't need a &self param, but we want it locally close to the
    // code that calls it, so we'll make it a type method :shrug:
    fn construct_category_via_agl(base_name: &str) -> Option<(Category, Subcategory)> {
        if let Some(first_char) = fontdrasil::agl::glyph_name_to_unicode(base_name)
            .chars()
            .next()
        {
            let (category, subcategory) = category_from_icu(first_char);

            // Exception: Something like "one_two" should be a (_, Ligature),
            // "acutecomb_brevecomb" should however stay (Mark, Nonspacing).
            if base_name.contains('_') && category != Category::Mark {
                return Some((category, Subcategory::Ligature));
            } else {
                return Some((category, subcategory));
            }
        }
        None
    }

    fn split_glyph_suffix<'a>(&self, name: &'a str) -> Option<(&'a str, &'a str)> {
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
                if self.get_by_name(base).is_some() {
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
            if self.get_by_name(&part).is_some() && self.get_by_name(&new_part).is_none() {
                continue;
            }
            *part = new_part;
        }
        Some(parts)
    }
}

// https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L261
fn category_from_icu(c: char) -> (Category, Subcategory) {
    match icu_properties::maps::general_category().get(c) {
        GeneralCategory::Unassigned | GeneralCategory::OtherSymbol => {
            (Category::Symbol, Subcategory::None)
        }
        GeneralCategory::UppercaseLetter
        | GeneralCategory::LowercaseLetter
        | GeneralCategory::TitlecaseLetter
        | GeneralCategory::OtherLetter => (Category::Letter, Subcategory::None),
        GeneralCategory::ModifierLetter => (Category::Letter, Subcategory::Modifier),
        GeneralCategory::NonspacingMark => (Category::Mark, Subcategory::Nonspacing),
        GeneralCategory::SpacingMark => (Category::Mark, Subcategory::SpacingCombining),
        GeneralCategory::EnclosingMark => (Category::Mark, Subcategory::Enclosing),
        GeneralCategory::DecimalNumber | GeneralCategory::OtherNumber => {
            (Category::Number, Subcategory::DecimalDigit)
        }
        GeneralCategory::LetterNumber => (Category::Number, Subcategory::None),
        GeneralCategory::SpaceSeparator => (Category::Separator, Subcategory::Space),
        GeneralCategory::LineSeparator
        | GeneralCategory::ParagraphSeparator
        | GeneralCategory::Control => (Category::Separator, Subcategory::None),
        GeneralCategory::Format => (Category::Separator, Subcategory::Format),
        GeneralCategory::PrivateUse => (Category::Letter, Subcategory::Compatibility),
        GeneralCategory::DashPunctuation => (Category::Punctuation, Subcategory::Dash),
        GeneralCategory::OpenPunctuation | GeneralCategory::ClosePunctuation => {
            (Category::Punctuation, Subcategory::Parenthesis)
        }
        GeneralCategory::ConnectorPunctuation | GeneralCategory::OtherPunctuation => {
            (Category::Punctuation, Subcategory::None)
        }
        GeneralCategory::InitialPunctuation | GeneralCategory::FinalPunctuation => {
            (Category::Punctuation, Subcategory::Quote)
        }
        GeneralCategory::MathSymbol => (Category::Symbol, Subcategory::Math),
        GeneralCategory::CurrencySymbol => (Category::Symbol, Subcategory::Currency),
        GeneralCategory::ModifierSymbol => (Category::Mark, Subcategory::Spacing),
        GeneralCategory::Surrogate => unreachable!("char cannot represent surrogate code points"),
    }
}

fn load_bundled_data() -> Vec<GlyphInfo> {
    bincode::deserialize(BUNDLED_DATA).unwrap()
}

fn merge_data(mut base: Vec<GlyphInfo>, overrides: Vec<GlyphInfo>) -> Vec<GlyphInfo> {
    let skip_names = overrides
        .iter()
        .map(|info| &info.name)
        .collect::<HashSet<_>>();
    base.retain(|info| !skip_names.contains(&info.name));
    base.extend(overrides);
    base
}

#[cfg(test)]
mod tests {
    use std::sync::OnceLock;

    use super::*;

    #[test]
    fn test_bundled_data() {
        let data = load_bundled_data();
        assert_eq!(data.len(), 73329);
    }

    #[test]
    fn simple_overrides() {
        let overrides = vec![GlyphInfo {
            name: "A".into(),
            category: Category::Mark,
            subcategory: Subcategory::SpacingCombining,
            unicode: Some(b'A' as u32),
            production: None,
            alt_names: Default::default(),
        }];
        let bundled = load_bundled_data();
        let merged = merge_data(bundled, overrides);
        let data = GlyphData::new_impl(merged);

        assert_eq!(data.get_by_name("A").unwrap().category, Category::Mark);
    }

    #[test]
    fn overrides_from_file() {
        let data = GlyphData::new(Some(Path::new("./data/GlyphData_override_test.xml"))).unwrap();
        assert_eq!(data.get_by_name("zero").unwrap().category, Category::Other);
        assert_eq!(data.get_by_name("C").unwrap().category, Category::Number);
        assert_eq!(
            data.get_by_name("Yogh").unwrap().production,
            Some("Yolo".into())
        );
    }

    fn get_category(name: &str, codepoints: &[u32]) -> Option<(Category, Subcategory)> {
        static GLYPH_DATA: OnceLock<GlyphData> = OnceLock::new();
        let data = GLYPH_DATA.get_or_init(|| GlyphData::new(None).unwrap());
        let codepoints = codepoints.iter().copied().collect();
        data.get_glyph(name, Some(&codepoints))
            .map(|info| (info.category, info.subcategory))
    }

    // from python glyphsLib: https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d5/tests/glyphdata_test.py#L106
    #[test]
    fn py_test_category() {
        for (name, expected) in [
            (".notdef", Some((Category::Separator, Subcategory::None))),
            // this test case requires AGL lookup:
            ("uni000D", Some((Category::Separator, Subcategory::None))),
            (
                "boxHeavyUp",
                Some((Category::Symbol, Subcategory::Geometry)),
            ),
            ("eacute", Some((Category::Letter, Subcategory::None))),
            ("Abreveacute", Some((Category::Letter, Subcategory::None))),
            ("C-fraktur", Some((Category::Letter, Subcategory::None))),
            ("fi", Some((Category::Letter, Subcategory::Ligature))),
            ("fi.alt", Some((Category::Letter, Subcategory::Ligature))),
            ("hib-ko", Some((Category::Letter, Subcategory::Syllable))),
            (
                "one.foo",
                Some((Category::Number, Subcategory::DecimalDigit)),
            ),
            (
                "one_two.foo",
                Some((Category::Number, Subcategory::Ligature)),
            ),
            ("o_f_f_i", Some((Category::Letter, Subcategory::Ligature))),
            (
                "o_f_f_i.foo",
                Some((Category::Letter, Subcategory::Ligature)),
            ),
            (
                "ain_alefMaksura-ar.fina",
                Some((Category::Letter, Subcategory::Ligature)),
            ),
            ("brevecomb", Some((Category::Mark, Subcategory::Nonspacing))),
            (
                "brevecomb.case",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
            (
                "brevecomb_acutecomb",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
            (
                "brevecomb_acutecomb.case",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
            (
                "caroncomb_dotaccentcomb",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
            (
                "dieresiscomb_caroncomb",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
            (
                "dieresiscomb_macroncomb",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
            (
                "dotaccentcomb_macroncomb",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
            (
                "macroncomb_dieresiscomb",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
            (
                "dotaccentcomb_o",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
            (
                "macronlowmod_O",
                Some((Category::Mark, Subcategory::Modifier)),
            ),
            ("O_o", Some((Category::Letter, Subcategory::Ligature))),
            (
                "O_dotaccentcomb_o",
                Some((Category::Letter, Subcategory::Ligature)),
            ),
            (
                "O_dotaccentcomb",
                Some((Category::Letter, Subcategory::None)),
            ),
            ("O_period", Some((Category::Letter, Subcategory::Ligature))),
            ("O_nbspace", Some((Category::Letter, Subcategory::None))),
            ("_a", None),
            ("_aaa", None),
            (
                "dal_alef-ar",
                Some((Category::Letter, Subcategory::Ligature)),
            ),
            (
                "dal_lam-ar.dlig",
                Some((Category::Letter, Subcategory::Ligature)),
            ),
            ("po-khmer", Some((Category::Letter, Subcategory::None))),
            (
                "po-khmer.below",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
            (
                "po-khmer.below.ro",
                Some((Category::Mark, Subcategory::Nonspacing)),
            ),
        ] {
            let result = get_category(name, &[]);
            assert_eq!(result, expected, "{name}: {result:?} != {expected:?}");
        }
    }

    // https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/tests/glyphdata_test.py#L145C5-L153C76
    #[test]
    fn py_category_by_unicode() {
        //# "SignU.bn" is a non-standard name not defined in GlyphData.xml
        let result = get_category("SignU.bn", &[0x09C1]);
        assert_eq!(result, Some((Category::Mark, Subcategory::Nonspacing)))
    }

    // https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/tests/glyphdata_test.py#L155C5-L162C1
    // https://github.com/googlefonts/glyphsLib/issues/232
    #[test]
    fn py_bug_232() {
        let u = get_category("uni07F0", &[]);
        assert_eq!(u, Some((Category::Mark, Subcategory::Nonspacing)));
        let g = get_category("longlowtonecomb-nko", &[]);
        assert_eq!(g, Some((Category::Mark, Subcategory::Nonspacing)));
    }
}
