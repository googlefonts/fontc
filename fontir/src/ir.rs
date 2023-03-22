//! Serde types for font IR.

use crate::{
    coords::{CoordConverter, NormalizedLocation, UserCoord},
    error::{PathConversionError, VariationModelError, WorkError},
    serde::{GlyphSerdeRepr, StaticMetadataSerdeRepr},
    variations::VariationModel,
};
use fontdrasil::types::GlyphName;
use indexmap::IndexSet;
use kurbo::{Affine, BezPath, Point};
use log::warn;
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::{Path, PathBuf},
};

/// Global font info that cannot vary.
///
/// For example, upem, axis definitions, etc, as distinct from
/// metadata that varies across design space such as ascender/descender.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(from = "StaticMetadataSerdeRepr", into = "StaticMetadataSerdeRepr")]
pub struct StaticMetadata {
    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/head>.
    pub units_per_em: u16,

    pub ascender: OrderedFloat<f32>,
    pub descender: OrderedFloat<f32>,
    pub cap_height: OrderedFloat<f32>,
    pub x_height: OrderedFloat<f32>,

    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/name>.
    pub names: HashMap<NameKey, String>,

    /// Every axis used by the font being compiled
    pub axes: Vec<Axis>,
    /// The name of every glyph, in the order it will be emitted
    ///
    /// <https://rsheeter.github.io/font101/#glyph-ids-and-the-cmap-table>
    pub glyph_order: IndexSet<GlyphName>,
    /// A model of how the space defined by [Self::axes] is split into regions that have deltas.
    ///
    /// This copy includes all locations used in the entire font. Users, such as glyph BE, may wish
    /// to narrow (submodel in FontTools terms) to the set of locations they actually use. Use of a
    /// location not in the global model is an error.
    pub variation_model: VariationModel,
}

impl StaticMetadata {
    pub fn new(
        units_per_em: u16,
        names: HashMap<NameKey, String>,
        axes: Vec<Axis>,
        glyph_order: IndexSet<GlyphName>,
        glyph_locations: HashSet<NormalizedLocation>,
    ) -> Result<StaticMetadata, VariationModelError> {
        let axis_names = axes.iter().map(|a| a.name.clone()).collect();
        let variation_model = VariationModel::new(glyph_locations, axis_names)?;

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L38-L45
        let ascender = (0.8 * units_per_em as f32).into();
        let descender = (-0.2 * units_per_em as f32).into();

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L48-L55
        let cap_height = (0.7 * units_per_em as f32).into();
        let x_height = (0.5 * units_per_em as f32).into();

        Ok(StaticMetadata {
            units_per_em,
            ascender,
            descender,
            cap_height,
            x_height,
            names,
            axes,
            glyph_order,
            variation_model,
        })
    }

    pub fn glyph_id(&self, name: &GlyphName) -> Option<u32> {
        self.glyph_order.get_index_of(name).map(|i| i as u32)
    }
}

/// Helps accumulate 'name' values.
///
/// See <https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/outlineCompiler.py#L367>.
#[derive(Default)]
pub struct NameBuilder {
    names: HashMap<NameKey, String>,
    /// Helps lookup entries in name when all we have is a NameId
    name_to_key: HashMap<NameId, NameKey>,
    version_major: i32,
    version_minor: u32,
}

impl NameBuilder {
    pub fn add(&mut self, name_id: NameId, value: String) {
        let key = NameKey::new(name_id, &value);
        self.names.insert(key, value);
        self.name_to_key.insert(name_id, key);
    }

    pub fn add_if_present(&mut self, name_id: NameId, value: &Option<String>) {
        let value = if let Some(value) = value {
            Some(value.clone())
        } else {
            name_id.default_value().map(String::from)
        };
        if let Some(value) = value {
            self.add(name_id, value);
        }
    }

    pub fn apply_fallback(&mut self, name_id: NameId, fallbacks: &[NameId]) {
        if let Some(fallback_id) = fallbacks.iter().find(|n| {
            let Some(key) = self.name_to_key.get(*n) else {
                    return false;
                };
            self.names.contains_key(key)
        }) {
            self.add(name_id, self.names[&self.name_to_key[fallback_id]].clone());
        }
    }

    pub fn contains_key(&self, name_id: NameId) -> bool {
        self.name_to_key.contains_key(&name_id)
    }

    pub fn get(&self, name_id: NameId) -> Option<&str> {
        self.name_to_key
            .get(&name_id)
            .and_then(|key| self.names.get(key).map(|s| s.as_str()))
    }

    pub fn into_inner(self) -> HashMap<NameKey, String> {
        self.names
    }

    pub fn apply_default_fallbacks(&mut self) {
        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L188
        self.apply_fallback(NameId::TypographicFamilyName, &[NameId::FamilyName]);

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L195
        self.apply_fallback(NameId::TypographicSubfamilyName, &[NameId::SubfamilyName]);

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L169
        if !self.contains_key(NameId::Version) {
            let major = self.version_major;
            let minor = self.version_minor;
            self.add(NameId::Version, format!("Version {major}.{minor:0>3}"));
        }

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L296
        if !self.contains_key(NameId::FullName) {
            self.add(
                NameId::FullName,
                format!(
                    "{} {}",
                    self.get(NameId::TypographicFamilyName).unwrap_or_default(),
                    self.get(NameId::TypographicSubfamilyName)
                        .unwrap_or_default(),
                ),
            );
        }

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L178-L185
        if !self.contains_key(NameId::PostScriptName) {
            let mut value = format!(
                "{} {}",
                self.get(NameId::TypographicFamilyName).unwrap_or_default(),
                self.get(NameId::TypographicSubfamilyName)
                    .unwrap_or_default(),
            );
            normalize_for_postscript(&mut value, false);
            self.add(NameId::PostScriptName, value);
        }

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L178-L185
        if !self.contains_key(NameId::UniqueIdentifier) {
            let version = self.get(NameId::Version).unwrap().replace("Version ", "");
            // fontmake pulls the openTypeOS2VendorID but we don't have that so just use their default
            let vendor = "NONE";
            let postscript_name = self.get(NameId::PostScriptName).unwrap();
            self.add(
                NameId::UniqueIdentifier,
                format!("{version};{vendor};{postscript_name}"),
            );
        }
    }

    pub fn set_version(&mut self, major: i32, minor: u32) {
        self.version_major = major;
        self.version_minor = minor;
    }
}

/// <https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L263>
fn normalize_for_postscript(value: &mut String, allow_spaces: bool) {
    value.retain(|c| {
        if !allow_spaces && c.is_ascii_whitespace() {
            return false;
        }
        if "[](){}<>/%".contains(c) {
            return false;
        }
        if !(33..127).contains(&(c as u32)) {
            warn!("fontmake performs decomposition, we just ignore the character");
            return false;
        }
        true
    });
}

/// See <https://learn.microsoft.com/en-us/typography/opentype/spec/name#name-ids>
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum NameId {
    Copyright,
    FamilyName,
    SubfamilyName,
    UniqueIdentifier,
    FullName,
    Version,
    PostScriptName,
    Trademark,
    Manufacturer,
    Designer,
    Description,
    ManufacturerUrl,
    DesignerUrl,
    License,
    LicenseUrl,
    Reserved15,
    TypographicFamilyName,
    TypographicSubfamilyName,
    MacCompatibleFullName,
    SampleText,
    PostScriptCidName,
    WwsFamilyName,
    WwsSubfamilyName,
    LightBackgroundPalette,
    DarkBackgroundPalette,
    VariationsPostScriptNamePrefix,
    Other(u16),
}

impl From<u16> for NameId {
    fn from(value: u16) -> Self {
        match value {
            0 => NameId::Copyright,
            1 => NameId::FamilyName,
            2 => NameId::SubfamilyName,
            3 => NameId::UniqueIdentifier,
            4 => NameId::FullName,
            5 => NameId::Version,
            6 => NameId::PostScriptName,
            7 => NameId::Trademark,
            8 => NameId::Manufacturer,
            9 => NameId::Designer,
            10 => NameId::Description,
            11 => NameId::ManufacturerUrl,
            12 => NameId::DesignerUrl,
            13 => NameId::License,
            14 => NameId::LicenseUrl,
            15 => NameId::Reserved15,
            16 => NameId::TypographicFamilyName,
            17 => NameId::TypographicSubfamilyName,
            18 => NameId::MacCompatibleFullName,
            19 => NameId::SampleText,
            20 => NameId::PostScriptCidName,
            21 => NameId::WwsFamilyName,
            22 => NameId::WwsSubfamilyName,
            23 => NameId::LightBackgroundPalette,
            24 => NameId::DarkBackgroundPalette,
            25 => NameId::VariationsPostScriptNamePrefix,
            _ => NameId::Other(value),
        }
    }
}

impl From<NameId> for u16 {
    fn from(value: NameId) -> Self {
        match value {
            NameId::Copyright => 0,
            NameId::FamilyName => 1,
            NameId::SubfamilyName => 2,
            NameId::UniqueIdentifier => 3,
            NameId::FullName => 4,
            NameId::Version => 5,
            NameId::PostScriptName => 6,
            NameId::Trademark => 7,
            NameId::Manufacturer => 8,
            NameId::Designer => 9,
            NameId::Description => 10,
            NameId::ManufacturerUrl => 11,
            NameId::DesignerUrl => 12,
            NameId::License => 13,
            NameId::LicenseUrl => 14,
            NameId::Reserved15 => 15,
            NameId::TypographicFamilyName => 16,
            NameId::TypographicSubfamilyName => 17,
            NameId::MacCompatibleFullName => 18,
            NameId::SampleText => 19,
            NameId::PostScriptCidName => 20,
            NameId::WwsFamilyName => 21,
            NameId::WwsSubfamilyName => 22,
            NameId::LightBackgroundPalette => 23,
            NameId::DarkBackgroundPalette => 24,
            NameId::VariationsPostScriptNamePrefix => 25,
            NameId::Other(value) => value,
        }
    }
}

impl From<font_types::NameId> for NameId {
    fn from(value: font_types::NameId) -> Self {
        NameId::from(value.to_u16())
    }
}

impl NameId {
    /// Match fontmake defaults.
    pub fn default_value(&self) -> Option<&'static str> {
        match self {
            // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L352
            NameId::FamilyName => Some("New Font"),
            // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L77
            NameId::SubfamilyName => Some("Regular"),
            _ => None,
        }
    }
}

/// The encoding for a Windows-platform (which works everywhere) name.
///
/// See <https://learn.microsoft.com/en-us/typography/opentype/spec/name#platform-specific-encoding-and-language-ids-windows-platform-platform-id-3>
fn encoding_for(value: &str) -> u16 {
    if value.chars().all(|c| (c as u32) < 0xFFFF) {
        1 // Unicode BMP
    } else {
        10 // Unicode full repetoire
    }
}

/// See <https://learn.microsoft.com/en-us/typography/opentype/spec/name>
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NameKey {
    pub name_id: NameId,
    pub platform_id: u16,
    pub encoding_id: u16,
    pub lang_id: u16,
}

impl NameKey {
    /// Create's a [NameKey] suitable for use with the provided value.
    ///
    /// The value matters because if it uses values from outside the Unicode BMP
    /// the key changes.
    pub fn new(name_id: NameId, value: &str) -> NameKey {
        // The spec offers a Unicode platform but fontmake uses Windows because that's more widely supported.
        // Match that. <https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/outlineCompiler.py#L430-L432>.
        NameKey {
            platform_id: 3, // Windows
            encoding_id: encoding_for(value),
            // https://learn.microsoft.com/en-us/typography/opentype/spec/name#windows-language-ids
            lang_id: 0x409, // English, United States.
            name_id,
        }
    }

    pub fn new_bmp_only(name_id: NameId) -> NameKey {
        Self::new(name_id, "")
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Axis {
    pub name: String,
    pub tag: String,
    pub min: UserCoord,
    pub default: UserCoord,
    pub max: UserCoord,
    pub hidden: bool,
    pub converter: CoordConverter,
}

/// Features (Adobe fea).
///
/// In time will split gpos/gsub, have different features for different
/// locations, etc.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub enum Features {
    Empty,
    File(PathBuf),
    Memory(String),
}

impl Features {
    pub fn empty() -> Features {
        Features::Empty
    }
    pub fn from_file(file: &Path) -> Features {
        Features::File(file.to_path_buf())
    }
    pub fn from_string(fea_content: String) -> Features {
        Features::Memory(fea_content)
    }
}

/// A variable definition of a single glyph.
///
/// Guarrantees at least one definition. Currently that must be at
/// the default location. In theory that limitation is unnecessary,
/// a variable glyph that covers the default location could compile.
///
/// If defined in many locations, presumed to vary continuously
/// between positions and required to have variation compatible structure.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(from = "GlyphSerdeRepr", into = "GlyphSerdeRepr")]
pub struct Glyph {
    pub name: GlyphName,
    pub codepoints: HashSet<u32>, // single unicodes that each point to this glyph. Typically 0 or 1.
    default_location: NormalizedLocation,
    sources: HashMap<NormalizedLocation, GlyphInstance>,
}

impl Glyph {
    pub fn new(
        name: GlyphName,
        codepoints: HashSet<u32>,
        sources: HashMap<NormalizedLocation, GlyphInstance>,
    ) -> Result<Self, WorkError> {
        if sources.is_empty() {
            return Err(WorkError::InvalidSourceGlyph {
                glyph_name: name,
                message: "No sources".into(),
            });
        }
        let defaults: Vec<_> = sources
            .keys()
            .filter(|loc| !loc.iter().any(|(_, c)| c.into_inner() != 0.0))
            .collect();
        if defaults.len() != 1 {
            return Err(WorkError::InvalidSourceGlyph {
                glyph_name: name,
                message: format!("Must have exactly 1 default, got {defaults:?} from {sources:?}"),
            });
        }
        let default_location = defaults[0].clone();
        Ok(Glyph {
            name,
            codepoints,
            default_location,
            sources,
        })
    }

    pub fn default_instance(&self) -> &GlyphInstance {
        self.sources.get(&self.default_location).unwrap()
    }

    pub fn sources(&self) -> &HashMap<NormalizedLocation, GlyphInstance> {
        &self.sources
    }
}

/// A variable definition of a single glyph.
///
/// If defined in many locations, presumed to vary continuously
/// between positions and required to have variation compatible structure.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GlyphBuilder {
    pub name: GlyphName,
    pub codepoints: HashSet<u32>, // single unicodes that each point to this glyph. Typically 0 or 1.
    pub sources: HashMap<NormalizedLocation, GlyphInstance>,
}

impl GlyphBuilder {
    pub fn new(name: GlyphName) -> Self {
        Self {
            name,
            codepoints: HashSet::new(),
            sources: HashMap::new(),
        }
    }

    pub fn try_add_source(
        &mut self,
        unique_location: &NormalizedLocation,
        source: GlyphInstance,
    ) -> Result<(), WorkError> {
        if self.sources.contains_key(unique_location) {
            return Err(WorkError::DuplicateNormalizedLocation {
                what: format!("glyph '{}' source", self.name.as_str()),
                loc: unique_location.clone(),
            });
        }
        self.sources.insert(unique_location.clone(), source);
        Ok(())
    }
}

impl TryInto<Glyph> for GlyphBuilder {
    type Error = WorkError;

    fn try_into(self) -> Result<Glyph, Self::Error> {
        Glyph::new(self.name, self.codepoints, self.sources)
    }
}

impl From<&Glyph> for GlyphBuilder {
    fn from(value: &Glyph) -> Self {
        Self {
            name: value.name.clone(),
            codepoints: value.codepoints.clone(),
            sources: value.sources.clone(),
        }
    }
}

impl From<Glyph> for GlyphBuilder {
    fn from(value: Glyph) -> Self {
        Self {
            name: value.name,
            codepoints: value.codepoints,
            sources: value.sources,
        }
    }
}

/// A Glyph at a specific position in designspace.
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
pub struct GlyphInstance {
    /// Advance width.
    pub width: f64,
    /// Advance height; if None, assumed to equal font's ascender - descender.
    pub height: Option<f64>,
    /// List of glyph contours.
    pub contours: Vec<BezPath>,
    /// List of glyph components.
    pub components: Vec<Component>,
}

/// A single glyph component, reference to another glyph.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Component {
    /// The name of the referenced glyph.
    pub base: GlyphName,
    /// Affine transformation to apply to the referenced glyph.
    pub transform: Affine,
}

/// Helps convert points-of-type to a bezier path.
///
/// Source formats tend to use streams of point-of-type. Curve manipulation is
/// often easier on bezier path, so provide a mechanism to convert.
#[derive(Debug)]
pub struct GlyphPathBuilder {
    glyph_name: GlyphName,
    offcurve: Vec<Point>,
    path: BezPath,
    last_move_to: Option<Point>,
}

impl GlyphPathBuilder {
    pub fn new(glyph_name: GlyphName) -> GlyphPathBuilder {
        GlyphPathBuilder {
            glyph_name,
            offcurve: Vec::new(),
            path: BezPath::new(),
            last_move_to: None,
        }
    }

    fn check_num_offcurve(
        &self,
        expected: impl Fn(usize) -> bool,
    ) -> Result<(), PathConversionError> {
        if !expected(self.offcurve.len()) {
            return Err(PathConversionError::TooManyOffcurvePoints {
                glyph_name: self.glyph_name.clone(),
                num_offcurve: self.offcurve.len(),
                points: self.offcurve.clone(),
            });
        }
        Ok(())
    }

    pub fn is_empty(&self) -> bool {
        self.offcurve.is_empty() && self.path.elements().is_empty()
    }

    pub fn move_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.check_num_offcurve(|v| v == 0)?;
        let p = p.into();
        self.path.move_to(p);
        self.last_move_to = Some(p);
        Ok(())
    }

    pub fn line_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.check_num_offcurve(|v| v == 0)?;
        if self.is_empty() {
            self.move_to(p)?;
        } else {
            self.path.line_to(p);
        }
        Ok(())
    }

    pub fn qcurve_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        // https://github.com/googlefonts/fontmake-rs/issues/110
        // Guard clauses: degenerate cases
        if self.is_empty() {
            return self.move_to(p);
        }
        if self.offcurve.is_empty() {
            return self.line_to(p);
        }

        // Insert an implied oncurve between every pair of offcurve points
        for window in self.offcurve.windows(2) {
            let curr = window[0];
            let next = window[1];
            // current offcurve to halfway to the next one
            let implied = Point::new((curr.x + next.x) / 2.0, (curr.y + next.y) / 2.0);
            self.path.quad_to(curr, implied);
        }
        // last but not least, the last offcurve to the provided point
        self.path.quad_to(*self.offcurve.last().unwrap(), p.into());
        self.offcurve.clear();

        Ok(())
    }

    /// Type of curve depends on accumulated off-curves
    ///
    /// <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn curve_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        if !self.is_empty() {
            match self.offcurve.len() {
                0 => self.path.line_to(p),
                1 => self.path.quad_to(self.offcurve[0], p.into()),
                2 => self
                    .path
                    .curve_to(self.offcurve[0], self.offcurve[1], p.into()),
                _ => self.check_num_offcurve(|v| v < 3)?,
            }
            self.offcurve.clear();
        } else {
            self.move_to(p)?;
        }
        Ok(())
    }

    pub fn offcurve(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.offcurve.push(p.into());
        Ok(())
    }

    pub fn close_path(&mut self) -> Result<(), PathConversionError> {
        // Take dangling off-curves to imply a curve back to sub-path start
        if let Some(last_move) = self.last_move_to {
            if !self.offcurve.is_empty() {
                self.curve_to(last_move)?;
            }
            self.path.close_path();
        }
        Ok(())
    }

    pub fn build(self) -> BezPath {
        self.path
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        coords::{CoordConverter, UserCoord},
        ir::{Axis, NameId},
    };

    use super::GlyphPathBuilder;

    fn test_axis() -> Axis {
        let min = UserCoord::new(100.0);
        let default = UserCoord::new(400.0);
        let max = UserCoord::new(900.0);
        let converter = CoordConverter::unmapped(min, default, max);
        Axis {
            name: String::from("Weight"),
            tag: String::from("wght"),
            min,
            default,
            max,
            hidden: false,
            converter,
        }
    }

    #[test]
    fn axis_yaml() {
        let test_axis = test_axis();
        let yml = serde_yaml::to_string(&test_axis).unwrap();
        assert_eq!(test_axis, serde_yaml::from_str(&yml).unwrap());
    }

    #[test]
    fn axis_bincode() {
        let test_axis = test_axis();
        let bin = bincode::serialize(&test_axis).unwrap();
        assert_eq!(test_axis, bincode::deserialize(&bin).unwrap());
    }

    #[test]
    fn a_qcurve_with_no_offcurve_is_a_line() {
        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2", builder.build().to_svg());
    }

    #[test]
    fn a_qcurve_with_one_offcurve_is_a_single_quad_to() {
        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2", builder.build().to_svg());
    }

    #[test]
    fn a_qcurve_with_two_offcurve_is_two_quad_to() {
        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((5.0, 4.0)).unwrap();
        builder.qcurve_to((6.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2 Q5,4 6,2", builder.build().to_svg());
    }

    #[test]
    fn name_id_went_on_a_fieldtrip() {
        for raw_id in 0..255u16 {
            let name_id: NameId = raw_id.into();
            let back_to_raw: u16 = name_id.into();
            let back_to_name_id: NameId = back_to_raw.into();
            assert_eq!((raw_id, name_id), (back_to_raw, back_to_name_id));
        }
    }
}
