//! Global font metadata

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Debug,
    io::Read,
};

use chrono::{DateTime, Utc};
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use write_fonts::{
    tables::{gasp::GaspRange, gdef::GlyphClassDef, os2::SelectionFlags},
    types::{NameId, Tag},
};

use fontdrasil::{
    coords::{DesignCoord, NormalizedCoord, NormalizedLocation, UserLocation},
    types::{Axis, GlyphName},
};

use crate::{error::VariationModelError, orchestration::Persistable, variations::VariationModel};

/// Glyph names mapped to postscript names
pub type PostscriptNames = HashMap<GlyphName, GlyphName>;

/// Global font info that cannot vary across the design space.
///
/// For example, upem, axis definitions, etc, as distinct from
/// metadata that varies across design space such as ascender/descender.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct StaticMetadata {
    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/head>.
    pub units_per_em: u16,

    /// Every axis used by the font being compiled, including point axes.
    ///
    /// This is relatively rarely what you want.
    pub all_source_axes: Vec<Axis>,

    /// Every variable (non-point) axis used by the font being compiled.
    ///
    /// If empty this is a static font.
    pub axes: Vec<Axis>,

    /// Named locations in variation space
    pub named_instances: Vec<NamedInstance>,

    /// A model of how variation space is split into regions that have deltas.
    ///
    /// This copy includes all locations used in the entire font. That is, every
    /// location any glyph has an instance. Use of a location not in the global model
    /// is an error. This model enforces the no delta at the default location constraint
    /// used in things like gvar.
    pub variation_model: VariationModel,
    /// Glyphsapp only; named numbers defined per-master
    pub number_values: HashMap<NormalizedLocation, BTreeMap<SmolStr, OrderedFloat<f64>>>,
    default_location: NormalizedLocation,

    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/name>.
    pub names: HashMap<NameKey, String>,

    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/post> and
    /// <https://github.com/adobe-type-tools/agl-specification>
    pub postscript_names: PostscriptNames,

    /// Italic angle in counter-clockwise degrees from the vertical. Zero for
    /// upright fonts, negative for right-leaning fonts.
    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/post>.
    pub italic_angle: OrderedFloat<f64>,

    /// Miscellaneous font-wide data that didn't seem worthy of top billing
    pub misc: MiscMetadata,
    pub gdef_categories: GdefCategories,
    /// Feature variation rules
    pub variations: Option<VariableFeature>,
}

/// IR for a named position in variation space
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NamedInstance {
    pub name: String,
    pub location: UserLocation,
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
            encoding_id: Self::encoding_for(value),
            // https://learn.microsoft.com/en-us/typography/opentype/spec/name#windows-language-ids
            lang_id: 0x409, // English, United States.
            name_id,
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

    pub fn new_bmp_only(name_id: NameId) -> NameKey {
        Self::new(name_id, "")
    }
}

#[derive(Serialize, Deserialize, Default, Debug, Clone, PartialEq, Eq)]
pub struct GdefCategories {
    /// A map of glyphs to categories.
    ///
    /// If this is empty, classes should be inferred.
    pub categories: BTreeMap<GlyphName, GlyphClassDef>,
    /// If set, we should prefer categories defined in FEA source to ones here.
    ///
    /// This is set for UFO/DS sources, but not for glyphs sources.
    pub prefer_gdef_categories_in_fea: bool,
}

/// Metadata primarily feeding the OS/2 table.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/os2>
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MiscMetadata {
    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fstype>
    pub fs_type: Option<u16>,

    /// If set, the value the source file specifically stated. Otherwise compiler can choose.
    ///
    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/post#header>
    pub is_fixed_pitch: Option<bool>,

    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection>
    pub selection_flags: SelectionFlags,

    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#achvendid>
    pub vendor_id: Tag,

    /// UFO appears to allow negative major versions.
    ///
    /// See <https://unifiedfontobject.org/versions/ufo3/fontinfo.plist/#generic-identification-information>
    pub version_major: i32,
    pub version_minor: u32,

    pub head_flags: u16,
    pub lowest_rec_ppm: u16,

    pub created: Option<DateTime<Utc>>,

    // <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#sfamilyclass>
    pub family_class: Option<i16>,

    pub panose: Option<Panose>,

    // Allows source to explicitly control bits. <https://github.com/googlefonts/fontc/issues/1027>
    pub unicode_range_bits: Option<HashSet<u32>>,

    // Allows source to explicitly control bits. <https://github.com/googlefonts/fontc/issues/1027>
    pub codepage_range_bits: Option<HashSet<u32>>,
    pub meta_table: Option<MetaTableValues>,

    /// <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#usweightclass>
    ///
    /// If empty and there is a weight axis OS/2 will use the weight default
    pub us_weight_class: Option<u16>,
    /// <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#uswidthclass>
    ///
    /// If empty and there is a width axis OS/2 will use the width default
    pub us_width_class: Option<u16>,

    // <https://learn.microsoft.com/en-us/typography/opentype/spec/gasp>
    pub gasp: Vec<GaspRange>,
}

/// Records that will go in the '[meta]' table.
///
/// This can be used to specify explicit languages a font is designed for,
/// as well as languages it is capable of supporting.
///
/// See [design and supported languages][dlng slng].
///
/// [meta]: https://learn.microsoft.com/en-us/typography/opentype/spec/meta
/// [dlng slng]: https://learn.microsoft.com/en-us/typography/opentype/spec/meta#dlng-and-slng-design-and-supported-languages
#[derive(Serialize, Deserialize, Default, Debug, Clone, PartialEq, Eq)]
pub struct MetaTableValues {
    /// ScriptLangTags for the design languages
    pub dlng: Vec<SmolStr>,
    /// ScriptLangTags for the supported languages
    pub slng: Vec<SmolStr>,
}

/// PANOSE bytes
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#panose>
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Panose {
    pub family_type: u8,
    pub serif_style: u8,
    pub weight: u8,
    pub proportion: u8,
    pub contrast: u8,
    pub stroke_variation: u8,
    pub arm_style: u8,
    pub letterform: u8,
    pub midline: u8,
    pub x_height: u8,
}

/// A series of substitution rules to be applied to layout features
/// at specific points in design space.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct VariableFeature {
    /// The features that for which these rules should apply, as part of a
    /// [`FeatureVariations`] table.
    ///
    /// [`FeatureVariations`]: write_fonts::tables::layout::FeatureVariations
    pub features: Vec<Tag>,
    pub rules: Vec<Rule>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Rule {
    /// sets of conditions that trigger this rule.
    ///
    /// Only one of these needs to be true for the substitutions to be applied.
    pub conditions: Vec<ConditionSet>,
    /// Substitutions to be applied if a condition matches.
    pub substitutions: Vec<Substitution>,
}

/// A glyph substitution
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Substitution {
    /// The glyph to be substituted
    pub replace: GlyphName,
    /// The substitute glyph
    pub with: GlyphName,
}

/// A series of [`Condition`]s.
///
/// All conditions in the set must be true for it to to be applied.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ConditionSet(pub Vec<Condition>);

/// A range on an axis.
///
/// One of `min` or `max` must be set.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Condition {
    pub axis: Tag,
    /// The minimum position for this condition, in design space coordinates.
    pub min: Option<DesignCoord>,
    /// The maximum position for this condition, in design space coordinates.
    pub max: Option<DesignCoord>,
}

impl FromIterator<Condition> for ConditionSet {
    fn from_iter<T: IntoIterator<Item = Condition>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl StaticMetadata {
    const DEFAULT_VENDOR_ID_TAG: Tag = Tag::new(b"NONE");
    // TODO: we could consider a builder or something for this?
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        units_per_em: u16,
        names: HashMap<NameKey, String>,
        axes: Vec<Axis>,
        named_instances: Vec<NamedInstance>,
        global_locations: HashSet<NormalizedLocation>,
        postscript_names: PostscriptNames,
        italic_angle: f64,
        gdef_categories: GdefCategories,
        glyphsapp_number_values: Option<
            HashMap<NormalizedLocation, BTreeMap<SmolStr, OrderedFloat<f64>>>,
        >,
    ) -> Result<StaticMetadata, VariationModelError> {
        // Point axes are less exciting than ranged ones
        let variable_axes: Vec<_> = axes.iter().filter(|a| !a.is_point()).cloned().collect();

        // Named instances of static fonts are unhelpful <https://github.com/googlefonts/fontc/issues/1008>
        let named_instances = if !variable_axes.is_empty() {
            named_instances
        } else {
            Default::default()
        };

        // Claim names for axes and named instances
        let mut name_id_gen = 255;
        let mut key_to_name = names;
        let mut visited = key_to_name.values().cloned().collect::<HashSet<_>>();

        variable_axes
            .iter()
            .map(|axis| axis.ui_label_name())
            .chain(named_instances.iter().map(|ni| ni.name.as_ref()))
            .for_each(|name| {
                if !visited.insert(name.to_string()) {
                    return;
                }
                name_id_gen += 1;
                key_to_name.insert(NameKey::new(name_id_gen.into(), name), name.to_string());
            });

        let variation_model = VariationModel::new(global_locations, variable_axes.clone())?;

        let default_location = axes
            .iter()
            .map(|a| (a.tag, NormalizedCoord::new(0.0)))
            .collect();

        Ok(StaticMetadata {
            units_per_em,
            names: key_to_name,
            all_source_axes: axes,
            axes: variable_axes,
            named_instances,
            variation_model,
            default_location,
            postscript_names,
            italic_angle: italic_angle.into(),
            gdef_categories,
            number_values: glyphsapp_number_values.unwrap_or_default(),
            misc: MiscMetadata {
                fs_type: None, // default is, sigh, inconsistent across source formats
                is_fixed_pitch: None,
                selection_flags: Default::default(),
                vendor_id: Self::DEFAULT_VENDOR_ID_TAG,
                // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L353-L354
                version_major: 0,
                version_minor: 0,
                // <https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L364>
                lowest_rec_ppm: 6,
                // <https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L365>
                head_flags: 3,
                created: None,
                family_class: None,
                panose: None,
                unicode_range_bits: None,
                codepage_range_bits: None,
                meta_table: None,
                us_weight_class: None,
                us_width_class: None,
                gasp: Vec::new(),
            },
            variations: None,
        })
    }

    /// The default on all variable axes.
    pub fn default_location(&self) -> &NormalizedLocation {
        &self.default_location
    }

    pub fn axis(&self, tag: &Tag) -> Option<&Axis> {
        self.axes.iter().find(|a| &a.tag == tag)
    }

    /// Calculate a deterministic mapping of existing name text to the smallest
    /// name ID that provides it.
    pub fn reverse_names(&self) -> HashMap<&str, NameId> {
        self.names
            .iter()
            .fold(HashMap::new(), |mut accum, (key, name)| {
                accum
                    .entry(name.as_str())
                    .and_modify(|value| *value = key.name_id.min(*value))
                    .or_insert(key.name_id);
                accum
            })
    }
}

impl From<[u8; 10]> for Panose {
    fn from(value: [u8; 10]) -> Self {
        Self {
            family_type: value[0],
            serif_style: value[1],
            weight: value[2],
            proportion: value[3],
            contrast: value[4],
            stroke_variation: value[5],
            arm_style: value[6],
            letterform: value[7],
            midline: value[8],
            x_height: value[9],
        }
    }
}

impl Panose {
    pub fn to_bytes(&self) -> [u8; 10] {
        [
            self.family_type,
            self.serif_style,
            self.weight,
            self.proportion,
            self.contrast,
            self.stroke_variation,
            self.arm_style,
            self.letterform,
            self.midline,
            self.x_height,
        ]
    }
}

impl Persistable for StaticMetadata {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::coords::UserCoord;

    use super::*;

    fn test_static_metadata() -> StaticMetadata {
        let axis = Axis::for_test("wght");
        let mut point_axis = axis.clone();
        point_axis.min = point_axis.default;
        point_axis.max = point_axis.default;

        StaticMetadata {
            units_per_em: 1000,
            all_source_axes: vec![axis.clone(), point_axis],
            axes: vec![axis.clone()],
            named_instances: vec![NamedInstance {
                name: "Nobody".to_string(),
                location: vec![(WGHT, UserCoord::new(100.0))].into(),
            }],
            variation_model: VariationModel::new(
                HashSet::from([
                    vec![(WGHT, NormalizedCoord::new(-1.0))].into(),
                    vec![(WGHT, NormalizedCoord::new(0.0))].into(),
                    vec![(WGHT, NormalizedCoord::new(1.0))].into(),
                ]),
                vec![axis.clone()],
            )
            .unwrap(),
            default_location: vec![(WGHT, NormalizedCoord::new(0.0))].into(),
            names: HashMap::from([
                (
                    NameKey::new_bmp_only(NameId::FAMILY_NAME),
                    "Fam".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::TYPOGRAPHIC_FAMILY_NAME),
                    "Fam".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::new(256)),
                    "Weight".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::new(257)),
                    "Nobody".to_string(),
                ),
            ]),
            postscript_names: HashMap::from([("lhs".into(), "rhs".into())]),
            italic_angle: 0.0.into(),
            gdef_categories: GdefCategories {
                categories: [
                    ("a", GlyphClassDef::Base),
                    ("f_f", GlyphClassDef::Ligature),
                    ("acutecomb", GlyphClassDef::Mark),
                ]
                .into_iter()
                .map(|(name, cls)| (GlyphName::new(name), cls))
                .collect(),
                prefer_gdef_categories_in_fea: false,
            },
            misc: MiscMetadata {
                fs_type: None,
                is_fixed_pitch: None,
                selection_flags: SelectionFlags::default(),
                vendor_id: Tag::from_be_bytes(*b"DUCK"),
                version_major: 42,
                version_minor: 24,
                head_flags: 42,
                lowest_rec_ppm: 42,
                created: None,
                family_class: None,
                panose: None,
                unicode_range_bits: None,
                codepage_range_bits: None,
                meta_table: None,
                us_weight_class: None,
                us_width_class: None,
                gasp: Vec::new(),
            },
            number_values: Default::default(),
            variations: None,
        }
    }

    const WGHT: Tag = Tag::from_be_bytes(*b"wght");

    fn assert_yml_round_trip<T>(thing: T)
    where
        for<'a> T: Serialize + Deserialize<'a> + PartialEq + Debug,
    {
        let yml = serde_yaml::to_string(&thing).unwrap();
        assert_eq!(thing, serde_yaml::from_str(&yml).unwrap());
    }

    fn assert_bincode_round_trip<T>(thing: T)
    where
        for<'a> T: Serialize + Deserialize<'a> + PartialEq + Debug,
    {
        let bin = bincode::serialize(&thing).unwrap();
        assert_eq!(thing, bincode::deserialize(&bin).unwrap());
    }

    #[test]
    fn axis_yaml() {
        assert_yml_round_trip(Axis::for_test("wght"));
    }

    #[test]
    fn axis_bincode() {
        assert_bincode_round_trip(Axis::for_test("wght"));
    }

    #[test]
    fn static_metadata_yaml() {
        assert_yml_round_trip(test_static_metadata());
    }

    #[test]
    fn static_metadata_bincode() {
        assert_bincode_round_trip(test_static_metadata());
    }

    #[test]
    fn static_metadata_smallest_id() {
        let static_metadata = test_static_metadata();
        let reverse_names = static_metadata.reverse_names();
        assert_eq!(reverse_names.get("Fam").unwrap(), &NameId::FAMILY_NAME);
    }
}
