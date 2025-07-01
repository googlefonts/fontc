//! Global font metadata

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
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
    types::{Axes, Axis, GlyphName},
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
    pub all_source_axes: Axes,

    /// Every variable (non-point) axis used by the font being compiled.
    ///
    /// If empty this is a static font.
    pub axes: Axes,

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
    pub postscript_names: Option<PostscriptNames>,

    /// Italic angle in counter-clockwise degrees from the vertical. Zero for
    /// upright fonts, negative for right-leaning fonts.
    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/post>.
    pub italic_angle: OrderedFloat<f64>,

    /// Records whether this font contains sufficient non-default vertical data
    /// to warrant building a vhea and vmtx table. (The criteria for Glyphs and
    /// UFO sources is different.)
    pub build_vertical: bool,

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
    pub postscript_name: Option<String>,
    pub location: UserLocation,
}

/// See <https://learn.microsoft.com/en-us/typography/opentype/spec/name>
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
///
/// This type can be constructed with `collect()` from an iterator of `Condition`.
/// The inner conditions are always sorted.
#[derive(Serialize, Deserialize, Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConditionSet(Vec<Condition>);

/// A range on an axis.
///
/// One of `min` or `max` must be set.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Condition {
    pub axis: Tag,
    /// The minimum position for this condition, in design space coordinates.
    pub min: Option<DesignCoord>,
    /// The maximum position for this condition, in design space coordinates.
    pub max: Option<DesignCoord>,
}

impl Condition {
    pub fn new(axis: Tag, min: Option<DesignCoord>, max: Option<DesignCoord>) -> Self {
        Self { axis, min, max }
    }
}

impl Rule {
    /// `condition_sets` is a slice of slices of (axis, (min, max))
    #[doc(hidden)]
    pub fn for_test(condition_sets: &[&[(&str, (f64, f64))]], subs: &[(&str, &str)]) -> Rule {
        Rule {
            conditions: condition_sets
                .iter()
                .map(|cond_set| {
                    cond_set
                        .iter()
                        .map(|(tag, (min, max))| Condition {
                            axis: std::str::FromStr::from_str(tag).unwrap(),
                            min: Some(DesignCoord::new(*min)),
                            max: Some(DesignCoord::new(*max)),
                        })
                        .collect()
                })
                .collect(),
            substitutions: subs
                .iter()
                .map(|(a, b)| Substitution {
                    replace: GlyphName::new(a),
                    with: GlyphName::new(b),
                })
                .collect(),
        }
    }
}

impl FromIterator<Condition> for ConditionSet {
    fn from_iter<T: IntoIterator<Item = Condition>>(iter: T) -> Self {
        let mut inner: Vec<_> = iter.into_iter().collect();
        inner.sort();
        Self(inner)
    }
}

impl<'a> IntoIterator for &'a ConditionSet {
    type Item = &'a Condition;

    type IntoIter = std::slice::Iter<'a, Condition>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.as_slice().iter()
    }
}

impl std::ops::Deref for ConditionSet {
    type Target = [Condition];
    fn deref(&self) -> &Self::Target {
        &self.0
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
        mut named_instances: Vec<NamedInstance>,
        global_locations: HashSet<NormalizedLocation>,
        postscript_names: Option<PostscriptNames>,
        italic_angle: f64,
        gdef_categories: GdefCategories,
        glyphsapp_number_values: Option<
            HashMap<NormalizedLocation, BTreeMap<SmolStr, OrderedFloat<f64>>>,
        >,
        build_vertical: bool,
    ) -> Result<StaticMetadata, VariationModelError> {
        // Point axes are less exciting than ranged ones
        let variable_axes: Axes = axes.iter().filter(|a| !a.is_point()).cloned().collect();

        // Named instances of static fonts are unhelpful <https://github.com/googlefonts/fontc/issues/1008>
        if !variable_axes.is_empty() {
            for instance in &mut named_instances {
                instance.location = instance.location.subset_axes(&variable_axes);
            }
        } else {
            named_instances.clear();
        };

        // Claim names for axes and named instances
        let mut name_id_gen = 255;
        // Spec-reserved names (<= 255) are not allowed in the set of unique reusable strings,
        // with the exception of the default instance's subfamily name which can reuse the
        // existing nameID 2 or 17:
        // https://github.com/googlefonts/fontc/issues/1502
        let mut reusable_names: HashMap<String, NameKey> = names
            .iter()
            .filter(|&(k, _)| (k.name_id > 255.into()))
            .map(|(k, v)| (v.clone(), *k))
            .collect();

        let default_instance_location: UserLocation =
            variable_axes.iter().map(|a| (a.tag, a.default)).collect();

        let mut register_if_new = |name: &str| {
            reusable_names.entry(name.to_owned()).or_insert_with(|| {
                name_id_gen += 1;
                NameKey::new(name_id_gen.into(), name)
            });
        };

        for axes in variable_axes.iter() {
            register_if_new(axes.ui_label_name());
        }

        for ni in named_instances.iter() {
            let instance_name = ni.name.as_str();
            if ni.location == default_instance_location
                && names
                    .iter()
                    .find_map(|(key, string)| (*string == instance_name).then_some(key.name_id))
                    .is_some_and(|name_id| {
                        name_id == NameId::SUBFAMILY_NAME
                            || name_id == NameId::TYPOGRAPHIC_SUBFAMILY_NAME
                    })
            {
                log::debug!(
                    "Reuse existing subfamily name '{instance_name}' for default instance at {default_instance_location:?}",
                );
            } else {
                register_if_new(instance_name);
            }

            if let Some(ps_name) = ni.postscript_name.as_deref() {
                register_if_new(ps_name);
            }
        }

        let mut names = names;
        names.extend(
            reusable_names
                .into_iter()
                .map(|(string, key)| (key, string)),
        );

        let variation_model = VariationModel::new(global_locations, variable_axes.clone())?;

        let default_location = axes
            .iter()
            .map(|a| (a.tag, NormalizedCoord::new(0.0)))
            .collect();

        Ok(StaticMetadata {
            units_per_em,
            names,
            all_source_axes: Axes::new(axes),
            axes: variable_axes,
            named_instances,
            variation_model,
            default_location,
            postscript_names,
            italic_angle: italic_angle.into(),
            gdef_categories,
            number_values: glyphsapp_number_values.unwrap_or_default(),
            build_vertical,
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

    /// Calculate a mapping of existing name text to the sorted set of name ID(s) that provide it.
    pub fn reverse_names(&self) -> HashMap<&str, BTreeSet<NameId>> {
        // https://github.com/fonttools/fonttools/blob/d5aec1b9/Lib/fontTools/ttLib/tables/_n_a_m_e.py#L326-L329
        self.names
            .iter()
            .fold(HashMap::new(), |mut accum, (key, name)| {
                accum.entry(name).or_default().insert(key.name_id);
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
            all_source_axes: vec![axis.clone(), point_axis].into(),
            axes: Axes::new(vec![axis.clone()]),
            named_instances: vec![NamedInstance {
                name: "Nobody".to_string(),
                postscript_name: None,
                location: vec![(WGHT, UserCoord::new(100.0))].into(),
            }],
            variation_model: VariationModel::new(
                HashSet::from([
                    vec![(WGHT, NormalizedCoord::new(-1.0))].into(),
                    vec![(WGHT, NormalizedCoord::new(0.0))].into(),
                    vec![(WGHT, NormalizedCoord::new(1.0))].into(),
                ]),
                vec![axis.clone()].into(),
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
            postscript_names: Some(HashMap::from([("lhs".into(), "rhs".into())])),
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
            build_vertical: false,
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
        // in a sorted BTreeSet, the first is always the smallest
        assert_eq!(
            reverse_names.get("Fam").unwrap().iter().next().unwrap(),
            &NameId::FAMILY_NAME
        );
    }

    #[test]
    fn condition_set_sorted() {
        let one = Condition::new(Tag::new(b"test"), None, None);
        let two = Condition::new(Tag::new(b"blah"), None, None);
        let tre = Condition::new(Tag::new(b"derp"), None, None);

        assert_eq!(
            [one, two, tre].into_iter().collect::<ConditionSet>(),
            [two, tre, one].into_iter().collect()
        );
    }
}
