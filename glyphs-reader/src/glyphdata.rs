//! determining glyph properties
//!
//! This module provides access to glyph info extracted from bundled
//! (and potentially user-provided) data files.

use quick_xml::{
    Reader,
    events::{BytesStart, Event},
};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::Display,
    path::{Path, PathBuf},
    str::FromStr,
};

use fontdrasil::unicode18;
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
    OldGeorgian,
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
    Tagalog,
    Tamil,
    Telugu,
    Thaana,
    Thai,
    Tham,
    Tibet,
    Tifinagh,
    TuluTigalari,
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
            if let Ok(v) = u32::from_str_radix(v, 16)
                && v >= lbound
                && v <= ubound
            {
                return Some(f(v));
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
#[derive(Clone, Debug, Default)]
pub struct GlyphData {
    overrides: Option<Overrides>,
}

/// Glyph info from user-provided GlyphData files, indexed the way glyphsLib indexes it.
///
/// Every mapping is keyed independently and later entries replace earlier ones, so when
/// several files are loaded the last one to mention a given key wins.
/// <https://github.com/googlefonts/glyphsLib/blob/f40abf38fb5d/Lib/glyphsLib/glyphdata.py#L62-L89>
#[derive(Clone, Debug, Default)]
struct Overrides {
    names: HashMap<SmolStr, Entry>,
    alt_names: HashMap<SmolStr, Entry>,
    production_names: HashMap<SmolStr, Entry>,
    codepoints: HashMap<u32, Entry>,
}

/// The attributes of one glyph entry.
///
/// Unlike [`QueryResult`] every field is optional: user GlyphData files often only
/// set some attributes, and the ones they leave out fall back to other sources.
#[derive(Clone, Debug, Default, PartialEq)]
struct Entry {
    category: Option<Category>,
    subcategory: Option<Subcategory>,
    codepoint: Option<u32>,
    script: Option<Script>,
    production_name: Option<ProductionName>,
}

impl From<QueryResult> for Entry {
    fn from(value: QueryResult) -> Self {
        Entry {
            category: Some(value.category),
            subcategory: value.subcategory,
            codepoint: value.codepoint,
            script: value.script,
            production_name: value.production_name,
        }
    }
}

impl Overrides {
    fn add_xml(&mut self, xml: &[u8]) -> Result<(), GlyphDataError> {
        for info in parse_entries(xml)? {
            self.add(
                info.name,
                &info.alt_names,
                Entry {
                    category: info.category,
                    subcategory: info.subcategory,
                    codepoint: info.codepoint,
                    script: info.script,
                    production_name: info.production_name,
                },
            );
        }
        Ok(())
    }

    fn add(&mut self, name: SmolStr, alt_names: &[SmolStr], entry: Entry) {
        for alt in alt_names {
            self.alt_names.insert(alt.clone(), entry.clone());
        }
        if let Some(production_name) = entry.production_name.as_ref() {
            self.production_names
                .insert(production_name.clone().into(), entry.clone());
        }
        if let Some(codepoint) = entry.codepoint {
            self.codepoints.insert(codepoint, entry.clone());
        }
        self.names.insert(name, entry);
    }

    /// Look up by name, then alternative name, then production name.
    ///
    /// <https://github.com/googlefonts/glyphsLib/blob/f40abf38fb5d/Lib/glyphsLib/glyphdata.py#L154-L170>
    fn get(&self, name: &str) -> Option<&Entry> {
        self.names
            .get(name)
            .or_else(|| self.alt_names.get(name))
            .or_else(|| self.production_names.get(name))
    }
}

impl GlyphData {
    /// Overrides, if provided, take precedence over the bundled data for the fields
    /// they set; fields left as `None` fall back to the bundled data, as they do in
    /// GlyphData files.
    pub fn new(overrides: Option<HashMap<SmolStr, QueryResult>>) -> Self {
        let overrides = overrides.map(|overrides| {
            let mut result = Overrides::default();
            for (name, value) in overrides {
                result.add(name, &[], value.into());
            }
            result
        });
        Self { overrides }
    }

    /// Create a new data set with user provided overrides
    pub fn with_override_file(override_file: &Path) -> Result<Self, GlyphDataError> {
        Self::with_override_files(&[override_file])
    }

    /// Create a new data set with overrides from several user provided files.
    ///
    /// Files are applied in order, so where two files describe the same glyph,
    /// the later one wins. This matches glyphsLib and fontmake's `--glyph-data`.
    pub fn with_override_files<P: AsRef<Path>>(
        override_files: &[P],
    ) -> Result<Self, GlyphDataError> {
        let mut overrides = Overrides::default();
        for path in override_files {
            let path = path.as_ref();
            let bytes = std::fs::read(path).map_err(|err| GlyphDataError::UserFile {
                path: path.to_owned(),
                reason: err.kind(),
            })?;
            overrides.add_xml(&bytes)?;
        }
        Ok(Self {
            overrides: Some(overrides),
        })
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
#[non_exhaustive]
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
    #[error("the XML input did not start with a <glyphdata> tag")]
    WrongFirstElement,
    #[error("Invalid GlyphData XML: {0}")]
    InvalidDocument(&'static str),
    #[error("Missing required attribute '{missing}' in '{attributes}'")]
    MissingRequiredAttribute {
        attributes: String,
        missing: &'static str,
    },
}

impl GlyphDataError {
    // a little helper here makes our parsing code cleaner
    fn missing_attr(name: &'static str, raw_attrs: &str) -> Self {
        Self::MissingRequiredAttribute {
            attributes: raw_attrs.to_owned(),
            missing: name,
        }
    }
}

/// Parse glyph info entries out of a GlyphData xml file, in file order.
fn parse_entries(xml: &[u8]) -> Result<Vec<GlyphInfoFromXml>, GlyphDataError> {
    fn check_and_advance_past_preamble(reader: &mut Reader<&[u8]>) -> Result<(), GlyphDataError> {
        loop {
            let event = reader.read_event()?;
            match event {
                Event::Comment(_) | Event::PI(_) => (),
                Event::Decl(_) => (),
                Event::DocType(_) => (),
                Event::Start(start) if start.name().as_ref() == "glyphData" => return Ok(()),
                _other => {
                    return Err(GlyphDataError::WrongFirstElement);
                }
            }
        }
    }

    let mut reader = Reader::from_reader(xml);
    reader.config_mut().trim_text(true);
    reader.config_mut().expand_empty_elements = true;

    check_and_advance_past_preamble(&mut reader)?;

    // unknown values are reported once each, rather than once per glyph
    let mut unknown_values = HashSet::new();
    let mut entries = Vec::new();
    loop {
        match reader.read_event()? {
            Event::Comment(_) | Event::PI(_) => (),
            Event::Start(row) if row.name().as_ref() == "glyph" => {
                let entry = parse_glyph_xml(row.clone(), &mut unknown_values)?;
                reader.read_to_end(row.name())?;
                entries.push(entry);
            }
            Event::End(end) if end.name().as_ref() == "glyphData" => break,
            _ => {
                return Err(GlyphDataError::InvalidDocument(
                    "expected a <glyph> element or </glyphData>",
                ));
            }
        }
    }
    // Read through EOF so trailing malformed XML or another root is not ignored.
    loop {
        match reader.read_event()? {
            Event::Comment(_) | Event::PI(_) => (),
            Event::Eof => break,
            _ => {
                return Err(GlyphDataError::InvalidDocument(
                    "expected end of document after </glyphData>",
                ));
            }
        }
    }
    let mut unknown_values: Vec<_> = unknown_values.into_iter().collect();
    unknown_values.sort();
    for (attr, value) in unknown_values {
        log::warn!("Ignoring unknown {attr} '{value}' in GlyphData");
    }
    Ok(entries)
}

struct GlyphInfoFromXml {
    name: SmolStr,
    alt_names: Vec<SmolStr>,
    category: Option<Category>,
    subcategory: Option<Subcategory>,
    codepoint: Option<u32>,
    script: Option<Script>,
    production_name: Option<ProductionName>,
}

fn parse_glyph_xml(
    item: BytesStart,
    unknown_values: &mut HashSet<(&'static str, String)>,
) -> Result<GlyphInfoFromXml, GlyphDataError> {
    let mut name = None;
    let mut category = None;
    let mut subcategory = None;
    let mut unicode = None;
    let mut alt_names = None;
    let mut script = None;
    let mut production_name = None;

    for attr in item.attributes() {
        let attr = attr?;
        // GlyphData files may lack an XML declaration (and we don't track it),
        // so per the XML spec version 1.0 is assumed.
        let value = attr.normalized_value(quick_xml::XmlVersion::Implicit1_0)?;
        match attr.key.as_ref() {
            "name" => name = Some(value),
            "category" => category = Some(value),
            "subCategory" => subcategory = Some(value),
            "unicode" => unicode = Some(value),
            "altNames" => alt_names = Some(value),
            "script" => script = Some(value),
            "production" if !value.is_empty() => production_name = Some(value.as_ref().into()),
            // glyphsLib reads these files without a schema, and Glyphs.app writes many
            // attributes (sortName, unicode2, decompose, anchors, ...) we have no use for.
            _ => (),
        }
    }

    // now we've found some values, let's finalize them
    let name = name
        .map(SmolStr::new)
        .ok_or_else(|| GlyphDataError::missing_attr("name", item.attributes_raw()))?;

    // glyphsLib keeps attribute values as plain strings; values we can't represent
    // are dropped (and reported), so the field falls back as though it were unset.
    // Empty values are treated as unset, as they are falsy in glyphsLib.
    fn parse_value<T>(
        value: Option<Cow<str>>,
        attr: &'static str,
        parse: impl FnOnce(&str) -> Option<T>,
        unknown_values: &mut HashSet<(&'static str, String)>,
    ) -> Option<T> {
        let value = value.filter(|value| !value.is_empty())?;
        let parsed = parse(&value);
        if parsed.is_none() {
            unknown_values.insert((attr, value.into_owned()));
        }
        parsed
    }
    let category = parse_value(
        category,
        "category",
        |v| Category::from_str(v).ok(),
        unknown_values,
    );
    let subcategory = parse_value(
        subcategory,
        "subCategory",
        |v| Subcategory::from_str(v).ok(),
        unknown_values,
    );
    let codepoint = parse_value(
        unicode,
        "unicode",
        |v| u32::from_str_radix(v, 16).ok(),
        unknown_values,
    );
    // fontc doesn't use the script, and custom data often uses names we don't know
    // ('Sans', 'Arabic Math Isolated', ...), so these aren't worth a warning.
    let script = script
        .filter(|value| !value.is_empty())
        .and_then(|value| Script::from_str(&value).ok());
    let alt_names = alt_names
        .map(|names| {
            names
                .as_ref()
                .split(',')
                .map(|name| SmolStr::from(name.trim()))
                .filter(|name| !name.is_empty())
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
    /// Without overrides, the first of these that matches wins:
    ///
    /// 1. the bundled data, by name;
    /// 2. the bundled data, by production name;
    /// 3. the bundled data, by codepoint;
    /// 4. a value computed from the name (suffixes, ligatures, `uniXXXX` names).
    ///
    /// With overrides, the glyph is looked up twice, as glyphsLib does, and each
    /// field comes from the second lookup when it has one, else from the first:
    ///
    /// 1. the lookup above, ignoring the overrides;
    /// 2. a lookup that tries, in order:
    ///    1. the overrides, by name, alternative name or production name;
    ///    2. the bundled data, by name or production name;
    ///    3. the overrides, by codepoint;
    ///    4. the bundled data, by codepoint.
    ///
    ///    Fields it still lacks are then filled from the glyph name: a category,
    ///    and with it a subcategory, guessed from the name, and a production
    ///    name built from it, or else the glyph name itself.
    // See https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L94
    pub fn query(&self, name: &str, codepoints: Option<&[u32]>) -> Option<QueryResult> {
        let Some(overrides) = self.overrides.as_ref() else {
            return Self::query_bundled(name, codepoints)
                // we don't have info for this glyph: can we synthesize it?
                .or_else(|| self.construct_result(name));
        };

        // glyphsLib looks the glyph up twice, once in the bundled data and once in the
        // custom data, and then takes each field from the custom result when it is set
        // (and differs), else from the bundled one:
        // https://github.com/googlefonts/glyphsLib/blob/f40abf38fb5d/Lib/glyphsLib/builder/glyph.py#L111-L166
        let bundled = GlyphData::default().query(name, codepoints);
        let custom = self.query_custom(overrides, name, codepoints);
        let bundled = bundled.map(Entry::from).unwrap_or_default();

        let production_name = custom.production_name.or(bundled.production_name);
        let category = match custom.category.or(bundled.category) {
            Some(category) => category,
            // if we have a production name but no category, 'Other' is good enough;
            // but not if it is only the glyph name itself, which tells us nothing
            None if production_name.as_ref().is_some_and(|prod| {
                agl_compliant_name(name).as_deref() != Some(prod.to_string().as_str())
            }) =>
            {
                Category::Other
            }
            None => return None,
        };
        Some(QueryResult {
            category,
            subcategory: custom.subcategory.or(bundled.subcategory),
            codepoint: custom.codepoint.or(bundled.codepoint),
            script: custom.script.or(bundled.script),
            production_name,
        })
    }

    /// The equivalent of glyphsLib's `get_glyph` with custom data.
    ///
    /// <https://github.com/googlefonts/glyphsLib/blob/f40abf38fb5d/Lib/glyphsLib/glyphdata.py#L92-L151>
    fn query_custom(&self, overrides: &Overrides, name: &str, codepoints: Option<&[u32]>) -> Entry {
        // by name (custom, then bundled), and only then by codepoint (custom, then
        // bundled), so custom data doesn't change the production name of a glyph the
        // bundled data only knows by codepoint (e.g. 'uni0965' for 'dandaDouble').
        // https://github.com/googlefonts/glyphsLib/blob/edf5ce51fdcd/Lib/glyphsLib/glyphdata.py#L173-L181
        let attrs = self
            .lookup_attributes(name)
            .or_else(|| {
                codepoints
                    .into_iter()
                    .flat_map(|cps| cps.iter())
                    .find_map(|cp| overrides.codepoints.get(cp).cloned())
            })
            .or_else(|| Self::query_bundled(name, codepoints).map(Into::into))
            .unwrap_or_default();
        // Where we give up on constructing a production name, glyphsLib falls back to
        // the sanitized glyph name.
        // https://github.com/googlefonts/glyphsLib/blob/f40abf38fb5d/Lib/glyphsLib/glyphdata.py#L380-L384
        let production_name = attrs
            .production_name
            .or_else(|| self.construct_production_name(name))
            .or_else(|| agl_compliant_name(name).map(|name| name.as_str().into()));
        // a missing category means both are computed, even if subCategory was set
        let (category, subcategory) = match attrs.category {
            Some(category) => (Some(category), attrs.subcategory),
            None => match self.construct_category(name) {
                Some((category, subcategory)) => (Some(category), subcategory),
                None => (None, None),
            },
        };
        Entry {
            category,
            subcategory,
            codepoint: attrs.codepoint,
            script: attrs.script,
            production_name,
        }
    }

    /// Look up the bundled data by name, production name or codepoint, without synthesis.
    fn query_bundled(name: &str, codepoints: Option<&[u32]>) -> Option<QueryResult> {
        bundled::find_pos_by_name(name)
            .or_else(|| find_pos_by_prod_name(name))
            .or_else(|| {
                codepoints
                    .into_iter()
                    .flat_map(|cps| cps.iter())
                    .find_map(|cp| bundled::find_pos_by_codepoint(*cp))
            })
            .map(|i| {
                bundled::get(i).unwrap_or_else(|| panic!("We found invalid index {i} somehow"))
            })
    }

    /// Look up the attributes for a name, preferring the overrides, without synthesis.
    ///
    /// <https://github.com/googlefonts/glyphsLib/blob/f40abf38fb5d/Lib/glyphsLib/glyphdata.py#L154-L170>
    fn lookup_attributes(&self, name: &str) -> Option<Entry> {
        self.overrides
            .as_ref()
            .and_then(|overrides| overrides.get(name).cloned())
            .or_else(|| Self::query_bundled(name, None).map(Into::into))
    }

    fn contains_name(&self, name: &str) -> bool {
        self.overrides
            .as_ref()
            .is_some_and(|overrides| overrides.get(name).is_some())
            || bundled::find_pos_by_name(name).is_some()
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
        if let Some(attrs) = self.lookup_attributes(base_name) {
            return attrs.category.map(|category| (category, attrs.subcategory));
        }

        if let Some(base_names) = self.split_ligature_glyph_name(base_name) {
            let base_names_attributes: Vec<_> = base_names
                .iter()
                .map(|name| self.lookup_attributes(name))
                .collect();
            if let Some(first_attr) = base_names_attributes
                .first()
                .expect("if we have base_names it is non-empty")
                .as_ref()
                .filter(|attr| attr.category.is_some())
            {
                // if first is mark, we're a mark
                if first_attr.category == Some(Category::Mark) {
                    return Some((Category::Mark, first_attr.subcategory));
                } else if first_attr.category == Some(Category::Letter) {
                    // if first is letter and rest are marks/separators, we use info from first
                    if base_names_attributes
                        .iter()
                        .skip(1)
                        .map(|result| result.as_ref().and_then(|r| r.category))
                        .all(|cat| matches!(cat, None | Some(Category::Mark | Category::Separator)))
                    {
                        return Some((Category::Letter, first_attr.subcategory));
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
            self.lookup_attributes(base_name)
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
                self.lookup_attributes(&name).and_then(|result| {
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
                    uni_names.push(Cow::Borrowed(&part.as_str()[1..]));
                } else {
                    let ch = fontdrasil::agl::char_for_agl_name(part.as_ref())?;
                    uni_names.push(Cow::Owned(format!("{:04X}", ch as u32)));
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

/// The glyph name without characters that aren't allowed in AGL names, if not too long.
///
/// <https://github.com/googlefonts/glyphsLib/blob/f40abf38fb5d/Lib/glyphsLib/glyphdata.py#L181-L187>
fn agl_compliant_name(name: &str) -> Option<String> {
    const MAX_GLYPH_NAME_LENGTH: usize = 63;
    let clean: String = name
        .chars()
        .filter(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '.')
        .collect();
    (clean.len() <= MAX_GLYPH_NAME_LENGTH).then_some(clean)
}

// https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517d/Lib/glyphsLib/glyphdata.py#L261
fn category_from_icu(c: char) -> (Category, Option<Subcategory>) {
    match unicode18::general_category(c) {
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
            "old georgian" => Ok(Self::OldGeorgian),
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
            "tagalog" => Ok(Self::Tagalog),
            "tamil" => Ok(Self::Tamil),
            "telugu" => Ok(Self::Telugu),
            "thaana" => Ok(Self::Thaana),
            "thai" => Ok(Self::Thai),
            "tham" => Ok(Self::Tham),
            "tibet" => Ok(Self::Tibet),
            "tifinagh" => Ok(Self::Tifinagh),
            "tulu tigalari" => Ok(Self::TuluTigalari),
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
    fn unicode_18_general_category() {
        assert_eq!(
            category_from_icu('\u{11DF0}'),
            (Category::Mark, Some(Subcategory::Nonspacing))
        );
        assert_eq!(category_from_icu('\u{12550}'), (Category::Number, None));
    }

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

    /// Build a data set from XML strings, as if each were a separate file.
    fn from_xml(xmls: &[&str]) -> GlyphData {
        let mut overrides = Overrides::default();
        for xml in xmls {
            overrides.add_xml(xml.as_bytes()).unwrap();
        }
        GlyphData {
            overrides: Some(overrides),
        }
    }

    fn glyph_data_xml(rows: &str) -> String {
        format!("<?xml version=\"1.0\"?>\n<glyphData>\n{rows}\n</glyphData>")
    }

    fn categories(data: &GlyphData, name: &str) -> Option<(Category, Option<Subcategory>)> {
        data.query(name, None)
            .map(|result| (result.category, result.subcategory))
    }

    fn production_name(data: &GlyphData, name: &str) -> Option<String> {
        data.query(name, None)
            .and_then(|result| result.production_name)
            .map(|name| name.to_string())
    }

    // Modelled on the custom GlyphData files of real Noto projects (e.g. notofonts/math,
    // notofonts/nag-mundari), which Glyphs.app writes with many attributes we don't use.
    const GLYPHS_APP_STYLE_XML: &str = r#"<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE glyphData [
<!ELEMENT glyphData (glyph)+>
<!ATTLIST glyphData
	format			CDATA		#IMPLIED>
<!ELEMENT glyph EMPTY>
<!ATTLIST glyph
	unicode			CDATA		#IMPLIED
	unicodeLegacy	CDATA		#IMPLIED
	name			CDATA		#REQUIRED
	sortName		CDATA		#IMPLIED
	sortNameKeep	CDATA		#IMPLIED
	category		CDATA		#REQUIRED
	subCategory		CDATA		#IMPLIED
	case			CDATA		#IMPLIED
	direction		CDATA		#IMPLIED
	script			CDATA		#IMPLIED
	description		CDATA		#IMPLIED
	production		CDATA		#IMPLIED
	altNames		CDATA		#IMPLIED
	decompose		CDATA		#IMPLIED
	anchors			CDATA		#IMPLIED
	marks			CDATA		#IMPLIED>
]>
<glyphData format="2">
	<glyph unicode="1E4D0" name="o-example" sortName="ex00" category="Letter" script="example" production="u1E4D0" description="EXAMPLE LETTER O" />
	<glyph unicode="1E4EC" name="toneabove-example" sortName="ex40" category="Mark" subCategory="Nonspacing" script="example" production="u1E4EC" anchors="_top, top" description="EXAMPLE SIGN TONE ABOVE" />
	<glyph name="Asans" unicode="0041" unicode2="1D5A0" script="Sans" category="Letter" case="upper" />
	<glyph name="parenleft.fence" category="Punctuation" subCategory="Fence" script="" decompose="parenleft" />
	<glyph name="nocategory" production="uniE000" />
</glyphData>
"#;

    #[test]
    fn parse_glyphs_app_style_file() {
        let data = from_xml(&[GLYPHS_APP_STYLE_XML]);
        assert_eq!(
            data.query("o-example", None).unwrap(),
            QueryResult {
                category: Category::Letter,
                subcategory: None,
                codepoint: Some(0x1E4D0),
                script: None,
                production_name: Some("u1E4D0".into()),
            }
        );
        assert_eq!(
            categories(&data, "toneabove-example"),
            Some((Category::Mark, Some(Subcategory::Nonspacing)))
        );
        assert_eq!(categories(&data, "Asans"), Some((Category::Letter, None)));
        // 'Fence' isn't a subcategory we know, so it is dropped and we fall back
        // to what the bundled data says about 'parenleft'
        assert_eq!(
            categories(&data, "parenleft.fence"),
            Some((Category::Punctuation, Some(Subcategory::Parenthesis)))
        );
        // no category, but a production name other than its own name
        assert_eq!(
            categories(&data, "nocategory"),
            Some((Category::Other, None))
        );
        assert_eq!(production_name(&data, "nocategory"), Some("uniE000".into()));
    }

    #[test]
    fn parse_entry_without_name_is_an_error() {
        let xml = glyph_data_xml(r#"<glyph category="Letter" />"#);
        assert!(matches!(
            parse_entries(xml.as_bytes()),
            Err(GlyphDataError::MissingRequiredAttribute {
                missing: "name",
                ..
            })
        ));
    }

    #[rstest]
    #[case("<!-- custom marks -->", true)]
    #[case("<?custom marks?>", true)]
    #[case("", false)]
    fn override_xml_preserves_all_entries(#[case] separator: &str, #[case] self_closing: bool) {
        let ending = if self_closing { "/>" } else { "></glyph>" };
        let xml = format!(
            r#"{separator}<glyphData>
            {separator}<glyph name="customMarkOne" category="Mark" subCategory="Nonspacing"{ending}
            {separator}<glyph name="customMarkTwo" category="Mark" subCategory="Spacing"{ending}
            {separator}</glyphData>{separator}"#
        );
        let data = from_xml(&[&xml]);
        assert_eq!(
            categories(&data, "customMarkOne"),
            Some((Category::Mark, Some(Subcategory::Nonspacing)))
        );
        assert_eq!(
            categories(&data, "customMarkTwo"),
            Some((Category::Mark, Some(Subcategory::Spacing)))
        );
    }

    #[rstest]
    #[case(r#"<glyphData><glyph name="foo" category="Mark"/>"#)]
    #[case(r#"<glyphData><glyph name="foo" category="Mark"/></wrong>"#)]
    #[case(r#"<glyphData><glyph name="foo" category="Mark"/></glyphData><glyphData/>"#)]
    #[case(r#"<glyphData><glyph name="foo" category="Mark"/></glyphData><!--"#)]
    fn override_xml_rejects_incomplete_documents(#[case] xml: &str) {
        assert!(Overrides::default().add_xml(xml.as_bytes()).is_err());
    }

    #[test]
    fn partial_override_keeps_bundled_fields() {
        // only the category is set: subcategory and production name come from bundled data
        let data = from_xml(&[&glyph_data_xml(
            r#"<glyph name="gravecomb" category="Mark" />"#,
        )]);
        assert_eq!(
            categories(&data, "gravecomb"),
            Some((Category::Mark, Some(Subcategory::Nonspacing)))
        );
        assert_eq!(
            production_name(&data, "gravecomb"),
            Some("gravecomb".into())
        );

        // only the production name is set: categories come from bundled data
        let data = from_xml(&[&glyph_data_xml(
            r#"<glyph name="Aacute" production="AacuteProd" />"#,
        )]);
        assert_eq!(
            categories(&data, "Aacute"),
            categories(&GlyphData::default(), "Aacute")
        );
        assert_eq!(production_name(&data, "Aacute"), Some("AacuteProd".into()));
    }

    #[test]
    fn override_lookup_order() {
        // name, then alternative name, then production name
        let data = from_xml(&[&glyph_data_xml(
            r#"<glyph name="foo" category="Letter" production="bar" altNames="baz" />
<glyph name="baz" category="Number" production="qux" altNames="bar" />
<glyph name="quux" category="Symbol" production="qux" />"#,
        )]);
        assert_eq!(categories(&data, "foo"), Some((Category::Letter, None)));
        // 'baz' is both a name and an alternative name: the name wins
        assert_eq!(categories(&data, "baz"), Some((Category::Number, None)));
        // 'bar' is both an alternative name and a production name: the alt name wins
        assert_eq!(categories(&data, "bar"), Some((Category::Number, None)));
        // 'qux' is only a production name, and the later entry wins
        assert_eq!(categories(&data, "qux"), Some((Category::Symbol, None)));
    }

    #[test]
    fn later_files_win() {
        let first = glyph_data_xml(
            r#"<glyph name="foo" category="Letter" production="first" />
<glyph name="bar" category="Letter" />"#,
        );
        let second =
            glyph_data_xml(r#"<glyph name="foo" category="Mark" subCategory="Nonspacing" />"#);
        let data = from_xml(&[&first, &second]);
        // the later file replaces the whole entry, not just the fields it sets
        assert_eq!(
            categories(&data, "foo"),
            Some((Category::Mark, Some(Subcategory::Nonspacing)))
        );
        // (glyphsLib's fallback production name is the glyph name itself)
        assert_eq!(production_name(&data, "foo"), Some("foo".into()));
        // entries only in the earlier file are kept
        assert_eq!(categories(&data, "bar"), Some((Category::Letter, None)));

        let data = from_xml(&[&second, &first]);
        assert_eq!(categories(&data, "foo"), Some((Category::Letter, None)));
        assert_eq!(production_name(&data, "foo"), Some("first".into()));
    }

    #[test]
    fn override_by_codepoint_only_for_unknown_names() {
        let data = from_xml(&[&glyph_data_xml(
            r#"<glyph name="custom" unicode="0301" category="Letter" />"#,
        )]);
        let codepoints = [0x0301];
        // a name the bundled data knows is not overridden by codepoint...
        assert_eq!(
            data.query("acutecomb", Some(&codepoints))
                .map(|r| (r.category, r.subcategory)),
            Some((Category::Mark, Some(Subcategory::Nonspacing)))
        );
        // ...but a name nobody knows is
        assert_eq!(
            data.query("myacute", Some(&codepoints))
                .map(|r| (r.category, r.subcategory)),
            Some((Category::Letter, Some(Subcategory::Nonspacing)))
        );
    }

    // Seen in Noto Sans Nag Mundari: custom data that doesn't mention a glyph the
    // bundled data only knows by codepoint doesn't change its production name, as in
    // Glyphs.app and glyphsLib >= 6.16.2 (before that, glyphsLib used the glyph name).
    #[test]
    fn unrelated_overrides_keep_production_name_found_by_codepoint() {
        let codepoints = [0x0965];
        assert_eq!(
            GlyphData::default()
                .query("dandaDouble", Some(&codepoints))
                .unwrap()
                .production_name,
            Some("uni0965".into())
        );
        let data = from_xml(&[&glyph_data_xml(r#"<glyph name="foo" category="Letter" />"#)]);
        let result = data.query("dandaDouble", Some(&codepoints)).unwrap();
        assert_eq!(result.production_name, Some("uni0965".into()));
        // the categories still come from the bundled data
        assert_eq!(
            (result.category, result.subcategory),
            (Category::Punctuation, None)
        );
    }

    #[test]
    fn unknown_glyph_has_no_info_with_overrides() {
        let data = from_xml(&[&glyph_data_xml(r#"<glyph name="foo" category="Letter" />"#)]);
        assert_eq!(GlyphData::default().query("xyzzy", None), None);
        assert_eq!(data.query("xyzzy", None), None);
    }

    #[test]
    fn synthesis_uses_overrides() {
        let data = from_xml(&[&glyph_data_xml(
            r#"<glyph name="foo" category="Mark" subCategory="Nonspacing" production="uniE000" />"#,
        )]);
        assert_eq!(
            categories(&data, "foo.case"),
            Some((Category::Mark, Some(Subcategory::Nonspacing)))
        );
        assert_eq!(
            production_name(&data, "foo.case"),
            Some("uniE000.case".into())
        );
    }

    #[test]
    fn synthesis_with_non_unicode_production_name() {
        let data = from_xml(&[&glyph_data_xml(
            r#"<glyph name="customOne" category="Letter" production="customProd"/>
            <glyph name="customTwo" category="Letter" production="uniE001"/>"#,
        )]);
        let result = data.query("customOne_customTwo", None).unwrap();
        assert_eq!(result.category, Category::Letter);
        assert_eq!(result.subcategory, Some(Subcategory::Ligature));
        // The custom names cannot form a Unicode name; keep the glyph's own name.
        assert_eq!(result.production_name, Some("customOne_customTwo".into()));
    }

    // The `Other` and `Conjunct` subcategories are the values used to tell Glyphs (and
    // fontc) that an underscore-named glyph is not a component-based ligature; if they
    // fail to parse, the explicit source override is silently dropped and the name-based
    // Ligature guess wins. https://github.com/googlefonts/fontc/issues/2055
    #[test]
    fn parse_non_ligature_subcategories() {
        assert_eq!(Subcategory::from_str("Other"), Ok(Subcategory::Other));
        assert_eq!(Subcategory::from_str("Conjunct"), Ok(Subcategory::Conjunct));
    }

    fn get_category(name: &str, codepoints: &[u32]) -> Option<(Category, Option<Subcategory>)> {
        GlyphData::new(None)
            .query(name, Some(codepoints))
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
    fn unknown_name_combined_with_mark() {
        // if first part is unknown, we don't assign a category
        assert_eq!(get_category("Whata-WEIRDNameLOL_brevecomb", &[]), None)
    }

    #[test]
    fn known_name_with_unknown_mark() {
        // if first part is a letter and rest is unknown, we use categories of
        // first part
        assert_eq!(
            get_category("i_acutecombcombcy", &[]),
            get_category("i", &[])
        )
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

    // Entries like allahlong-ar carry their codepoint only as unicodeLegacy, so
    // "uniFDFA" is reachable solely through the production name; glyphsLib finds
    // these via its production name map.
    #[rstest(name, expected,
        case("uniFDFA", Some((Category::Letter, Some(Subcategory::Ligature)))),
        case("uniFDFB", Some((Category::Letter, Some(Subcategory::Ligature)))),
        case("uniFBF9", Some((Category::Letter, Some(Subcategory::Ligature)))),
        // an absent uniXXXX name still synthesizes via AGL + unicode category
        case("uniE000", Some((Category::Letter, Some(Subcategory::Compatibility)))),
    )]
    fn unicode_legacy_only_prod_name(
        name: &str,
        expected: Option<(Category, Option<Subcategory>)>,
    ) {
        assert_eq!(expected, get_category(name, &[]));
    }

    #[test]
    fn unicode_legacy_not_found_by_codepoint() {
        // glyphsLib only indexes the unicode attribute, not unicodeLegacy
        assert!(GlyphData::query_bundled("allahlong", Some(&[0xFDFA])).is_none());
    }

    #[test]
    fn unicode_legacy_only_prod_name_result() {
        let result = GlyphData::query_bundled("uniFDFA", None).unwrap();
        assert_eq!(
            (
                Category::Letter,
                Some(Subcategory::Ligature),
                None,
                Some(Script::Arabic),
                Some("uniFDFA".to_string())
            ),
            (
                result.category,
                result.subcategory,
                result.codepoint,
                result.script,
                result.production_name.map(|p| p.to_string())
            )
        );
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
        let production_name = GlyphData::query_bundled(name, None)
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
        // CJK Hangzhou numerals (U+3021-U+3029): glyphsLib 6.13.1 renamed
        // these from e.g. one-hangzhou to one-han
        case("one-han.half.vert", "uni3021.half.vert"),
        case("one_parenleft-han.vert", "uni30210028.vert"),
        case("one_one-han.paren", "uni30213021.paren"),
        case("one_parenleftsmall-han.vert", "uni3021FE59.vert"),
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

    // https://github.com/googlefonts/fontc/issues/XXXX
    #[test]
    fn prod_name_lookup_before_codepoint_lookup() {
        // Inter-Italic has a glyph named "triagrt" (AGLFN name for U+25BA)
        // with unicode = (0x25BA, 0x25B6). The codepoint fallback must not
        // match U+25B6 → "uni25B6" before the production name lookup can
        // match "triagrt" → U+25BA (which needs no rename).
        let result = GlyphData::new(None)
            .query("triagrt", Some(&[0x25BA, 0x25B6]))
            .unwrap();
        // "triagrt" IS the AGLFN production name for U+25BA, so the result's
        // production name must be "triagrt" (a no-op rename), NOT "uni25B6".
        assert_eq!(
            result.production_name.as_ref().map(|p| p.to_string()),
            Some("triagrt".to_string()),
        );
    }

    #[test]
    fn codepoint_lookup_uses_first_listed_codepoint() {
        let prod_name = |codepoints: &[u32]| {
            GlyphData::new(None)
                .query("Nablasansbold-math", Some(codepoints))
                .and_then(|result| result.production_name)
                .map(|name| name.to_string())
        };
        assert_eq!(prod_name(&[0x1D76F, 0x1D6C1]).as_deref(), Some("u1D76F"));
        assert_eq!(prod_name(&[0x1D6C1, 0x1D76F]).as_deref(), Some("u1D6C1"));
    }
}
