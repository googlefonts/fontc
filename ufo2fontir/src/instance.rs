//! Building one designspace instance as a static font, like `fontmake -i`.
//!
//! The instance must sit on a source (or anywhere, when there is only one
//! source: a single master is the same everywhere), so nothing is
//! interpolated. The designspace is then cut down to that source, with every
//! axis a point at it, and the instance's metadata is applied to the source's
//! fontinfo the way fontmake does: ufo2ft's instantiator
//! (`_generate_instance_info`) followed by glyphsLib's
//! `apply_instance_data_to_ufo`, which gftools' `instantiateUfo` runs too.
//! glyphsLib takes part for any designspace, not only one it wrote from a
//! Glyphs file: `fontmake -i` runs `apply_instance_data_to_ufo` on every
//! instance it generates.
//!
//! As in ufo2ft's instantiator, the glyph set, features, lib, groups and the
//! fontinfo that isn't interpolated come from the default source, and the
//! instance's source only gives what is interpolated: outlines, metrics,
//! kerning and some fontinfo (`INTERPOLATED_ATTRIBUTES`). The two are the same
//! source unless the instance sits on another one, which must then have every
//! glyph of the default source.

use std::{path::Path, str::FromStr, sync::Arc};

use fontdrasil::coords::{CoordConverter, DesignCoord, UserCoord};
use fontir::error::{BadSource, Error};
use log::warn;
use norad::designspace::{self, DesignSpaceDocument};
use plist::{Dictionary, Value};
use write_fonts::types::{Fixed, Tag};

/// The instance a static build applies, chosen with `--export`.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SelectedInstance {
    /// Index into the (cut down) designspace's instances
    pub index: usize,
    /// The instance's location in user coordinates, on every axis
    pub user_location: Vec<(Tag, f64)>,
    /// Whether the instance inherits all of the source's fontinfo (one info
    /// master and the instance at the default), or only the interpolating
    /// and generic attributes, as in ufo2ft's instantiator
    pub copy_all_info: bool,
    /// The default source, when the instance sits on another one: like
    /// ufo2ft's instantiator, the glyph set, features, lib, groups and the
    /// fontinfo that isn't interpolated come from it, and the instance's source
    /// only gives what is interpolated (outlines, metrics, kerning, some
    /// fontinfo).
    pub base: Option<designspace::Source>,
}

/// An axis as declared, with its map alone: the map needn't cover the
/// declared range for the source's own location to be convertible.
struct RawAxis {
    tag: Tag,
    name: String,
    converter: Option<CoordConverter>,
    default_design: f64,
}

impl RawAxis {
    fn to_design(&self, user: f64) -> f64 {
        match &self.converter {
            Some(converter) => UserCoord::new(user).to_design(converter).to_f64(),
            None => user,
        }
    }

    fn to_user(&self, design: f64) -> f64 {
        match &self.converter {
            Some(converter) => DesignCoord::new(design).to_user(converter).to_f64(),
            None => design,
        }
    }
}

fn raw_axes(designspace: &DesignSpaceDocument) -> Result<Vec<RawAxis>, Error> {
    designspace
        .axes
        .iter()
        .map(|axis| {
            let tag = Tag::from_str(&axis.tag).map_err(|cause| Error::InvalidTag {
                raw_tag: axis.tag.clone(),
                cause,
            })?;
            let converter = axis
                .map
                .as_ref()
                .filter(|map| !map.is_empty())
                .map(|map| {
                    CoordConverter::new(
                        map.iter()
                            .map(|m| (UserCoord::new(m.input), DesignCoord::new(m.output)))
                            .collect(),
                        0,
                    )
                })
                .transpose()?;
            let mut raw = RawAxis {
                tag,
                name: axis.name.clone(),
                converter,
                default_design: 0.0,
            };
            raw.default_design = raw.to_design(axis.default);
            Ok(raw)
        })
        .collect()
}

/// A location in design coordinates on every axis; a dimension the location
/// leaves out takes the axis default.
fn design_location(axes: &[RawAxis], dims: &[designspace::Dimension]) -> Vec<f64> {
    axes.iter()
        .map(|axis| {
            dims.iter()
                .find(|d| d.name == axis.name)
                .and_then(|d| match (d.xvalue, d.uservalue) {
                    (Some(x), _) => Some(x),
                    (None, Some(u)) => Some(axis.to_design(u)),
                    (None, None) => None,
                })
                .unwrap_or(axis.default_design)
        })
        .collect()
}

/// A location as the font stores it, in 16.16 fixed point: comparing these
/// rather than the f64s tells locations apart as finely as the font can, without
/// the float noise of mapping a user location through an axis map.
fn fixed(location: &[f64]) -> Vec<Fixed> {
    location.iter().copied().map(Fixed::from_f64).collect()
}

/// What an instance is called when selecting one: its name, else
/// `"{family} {style}"`, the two names gftools matches.
fn display_name(instance: &designspace::Instance) -> String {
    instance
        .name
        .clone()
        .unwrap_or_else(|| family_style(instance))
}

fn family_style(instance: &designspace::Instance) -> String {
    format!(
        "{} {}",
        instance.familyname.as_deref().unwrap_or_default(),
        instance.stylename.as_deref().unwrap_or_default()
    )
}

/// Find the instance named `requested` and the source to build it from, and
/// cut the designspace down to the two: every axis becomes a point at the
/// source, so the font is static.
///
/// The name matches an instance's `name` attribute exactly, else its
/// `"{family} {style}"`, as gftools resolves instance names, though it matches
/// the name attribute across all instances first; fontmake's `-i` matches a
/// regex against the same names. The designspace needs a source at the default
/// location, as for fontmake. The instance must sit on a source, or anywhere
/// when the source is the only one; fontc doesn't interpolate. When the
/// instance sits on a source other than the default, the default one gives what
/// the instance doesn't interpolate.
pub(crate) fn select_instance(
    path: &Path,
    designspace: &mut DesignSpaceDocument,
    requested: &str,
) -> Result<Arc<SelectedInstance>, Error> {
    let bad_source = |message: String| Err(BadSource::custom(path, message).into());
    let by_name: Vec<_> = designspace
        .instances
        .iter()
        .enumerate()
        .filter(|(_, instance)| instance.name.as_deref() == Some(requested))
        .map(|(i, _)| i)
        .collect();
    let matches = if by_name.is_empty() {
        designspace
            .instances
            .iter()
            .enumerate()
            .filter(|(_, instance)| family_style(instance) == requested)
            .map(|(i, _)| i)
            .collect()
    } else {
        by_name
    };
    let index = match matches.as_slice() {
        [index] => *index,
        [] => {
            let available = if designspace.instances.is_empty() {
                "there are none".to_string()
            } else {
                format!(
                    "available: {}",
                    designspace
                        .instances
                        .iter()
                        .map(|instance| format!("\"{}\"", display_name(instance)))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            return bad_source(format!("No instance named \"{requested}\"; {available}"));
        }
        _ => {
            return bad_source(format!(
                "{} instances are named \"{requested}\"; rename all but one",
                matches.len()
            ));
        }
    };

    let axes = raw_axes(designspace)?;
    let instance_location = design_location(&axes, &designspace.instances[index].location);
    let default_location: Vec<_> = axes.iter().map(|axis| axis.default_design).collect();
    // Layer sources are masters at other locations, not fonts to build
    let full_sources: Vec<_> = designspace
        .sources
        .iter()
        .enumerate()
        .filter(|(_, source)| source.layer.is_none())
        .collect();
    let source_at = |location: &[f64]| {
        full_sources
            .iter()
            .find(|(_, source)| fixed(&design_location(&axes, &source.location)) == fixed(location))
            .map(|(i, _)| *i)
    };
    let describe = |location: &[f64]| {
        axes.iter()
            .zip(location)
            .map(|(axis, value)| format!("{}={value}", axis.name))
            .collect::<Vec<_>>()
            .join(", ")
    };
    // Like fontmake -i, and fontc without --export, require a source at the
    // default location, even when it's the only one
    let Some(default_source) = source_at(&default_location) else {
        return bad_source(format!(
            "Cannot build instance \"{requested}\": there is no source at the default \
             location ({})",
            describe(&default_location)
        ));
    };
    let source = match (source_at(&instance_location), designspace.sources.len()) {
        // A single master is the same everywhere
        (_, 1) => 0,
        (Some(source), _) => source,
        (None, _) => {
            return bad_source(format!(
                "Cannot build instance \"{requested}\": its location ({}) is on no \
                 source and fontc doesn't interpolate",
                describe(&instance_location)
            ));
        }
    };

    // The instance gets its source's interpolated data on top of the default
    // source's
    let base = (source != default_source).then(|| designspace.sources[default_source].clone());

    // A static font has no feature variations: rules not covering the instance
    // are dropped, as fontmake -i does; the glyph swap a covering rule asks for
    // isn't done yet
    let value_of = |name: &str| {
        axes.iter()
            .position(|axis| axis.name == name)
            .map(|i| instance_location[i])
    };
    let covers = |rule: &designspace::Rule| {
        rule.condition_sets.iter().any(|set| {
            set.conditions.iter().all(|condition| {
                value_of(&condition.name).is_some_and(|value| {
                    condition.minimum.is_none_or(|min| min <= value)
                        && condition.maximum.is_none_or(|max| value <= max)
                })
            })
        })
    };
    if let Some(rule) = designspace.rules.rules.iter().find(|rule| covers(rule)) {
        return bad_source(format!(
            "Cannot build instance \"{requested}\": rule \"{}\" swaps glyphs at its \
             location ({}), and fontc doesn't apply rules to a static font yet",
            rule.name.as_deref().unwrap_or_default(),
            describe(&instance_location)
        ));
    }
    designspace.rules.rules.clear();

    let user_location = axes
        .iter()
        .zip(&instance_location)
        .map(|(axis, design)| (axis.tag, axis.to_user(*design)))
        .collect();
    let copy_all_info =
        full_sources.len() == 1 && fixed(&instance_location) == fixed(&default_location);

    // Keep the source and the instance, and make every axis a point at the source
    let source_location = design_location(&axes, &designspace.sources[source].location);
    for (axis, design) in designspace.axes.iter_mut().zip(&source_location) {
        let user = axes
            .iter()
            .find(|a| a.name == axis.name)
            .map(|a| a.to_user(*design))
            .unwrap_or(*design);
        log::debug!("Static font: axis {} is a point at user {user}", axis.name);
        axis.minimum = Some(user);
        axis.default = user;
        axis.maximum = Some(user);
        axis.values = None;
        if let Some(map) = axis.map.as_mut() {
            *map = vec![designspace::AxisMapping {
                input: user,
                output: *design,
            }];
        }
    }
    designspace.sources = vec![designspace.sources[source].clone()];
    // Named as selected, so messages don't call a nameless instance by the
    // placeholder name the source gives it later
    let mut instance = designspace.instances[index].clone();
    instance.name = Some(display_name(&instance));
    designspace.instances = vec![instance];

    Ok(Arc::new(SelectedInstance {
        index: 0,
        user_location,
        copy_all_info,
        base,
    }))
}

/// fontinfo attributes an instance doesn't inherit unless it is the one
/// master at the default: they are instance-specific, so ufo2ft's instantiator
/// leaves them out of the interpolated info, and they aren't in its
/// `UFO_INFO_ATTRIBUTES_TO_COPY_TO_INSTANCES` either.
const INSTANCE_SPECIFIC_ATTRIBUTES: &[&str] = &[
    "guidelines",
    "macintoshFONDFamilyID",
    "macintoshFONDName",
    "openTypeNameCompatibleFullName",
    "openTypeNamePreferredFamilyName",
    "openTypeNamePreferredSubfamilyName",
    "openTypeNameUniqueID",
    "openTypeNameWWSFamilyName",
    "openTypeNameWWSSubfamilyName",
    "postscriptFontName",
    "postscriptFullName",
    "postscriptUniqueID",
    "postscriptWeightName",
    "styleMapFamilyName",
    "styleMapStyleName",
    "styleName",
    "woffMetadataUniqueID",
    "year",
];

/// fontinfo attributes ufo2ft's instantiator interpolates (fontMath's
/// `MathInfo`): an instance sitting on a source other than the default takes
/// them from that source, and the rest of the info from the default source.
const INTERPOLATED_ATTRIBUTES: &[&str] = &[
    "unitsPerEm",
    "descender",
    "xHeight",
    "capHeight",
    "ascender",
    "italicAngle",
    "openTypeHeadLowestRecPPEM",
    "openTypeHheaAscender",
    "openTypeHheaDescender",
    "openTypeHheaLineGap",
    "openTypeHheaCaretSlopeRise",
    "openTypeHheaCaretSlopeRun",
    "openTypeHheaCaretOffset",
    "openTypeOS2WidthClass",
    "openTypeOS2WeightClass",
    "openTypeOS2TypoAscender",
    "openTypeOS2TypoDescender",
    "openTypeOS2TypoLineGap",
    "openTypeOS2WinAscent",
    "openTypeOS2WinDescent",
    "openTypeOS2SubscriptXSize",
    "openTypeOS2SubscriptYSize",
    "openTypeOS2SubscriptXOffset",
    "openTypeOS2SubscriptYOffset",
    "openTypeOS2SuperscriptXSize",
    "openTypeOS2SuperscriptYSize",
    "openTypeOS2SuperscriptXOffset",
    "openTypeOS2SuperscriptYOffset",
    "openTypeOS2StrikeoutSize",
    "openTypeOS2StrikeoutPosition",
    "openTypeVheaVertTypoAscender",
    "openTypeVheaVertTypoDescender",
    "openTypeVheaVertTypoLineGap",
    "openTypeVheaCaretSlopeRise",
    "openTypeVheaCaretSlopeRun",
    "openTypeVheaCaretOffset",
    "postscriptSlantAngle",
    "postscriptUnderlineThickness",
    "postscriptUnderlinePosition",
    "postscriptBlueValues",
    "postscriptOtherBlues",
    "postscriptFamilyBlues",
    "postscriptFamilyOtherBlues",
    "postscriptStemSnapH",
    "postscriptStemSnapV",
    "postscriptBlueFuzz",
    "postscriptBlueShift",
    "postscriptBlueScale",
    "postscriptDefaultWidthX",
    "postscriptNominalWidthX",
];

/// The fontinfo of an instance on a source other than the default: the default
/// source's, with the attributes ufo2ft interpolates taken from the instance's
/// source, which the instance sits exactly on. The panose is the source's too
/// when it has one, as it describes that source (its weight, say) better than
/// the default's does.
pub(crate) fn layer_font_info(
    path: &Path,
    base: &norad::FontInfo,
    source: &norad::FontInfo,
) -> Result<norad::FontInfo, Error> {
    let bad_value =
        |e: plist::Error| Error::BadSource(BadSource::custom(path, format!("fontinfo: {e}")));
    let to_dict = |info: &norad::FontInfo| -> Result<Dictionary, Error> {
        Ok(plist::to_value(info)
            .map_err(bad_value)?
            .into_dictionary()
            .unwrap_or_default())
    };
    let mut info = to_dict(base)?;
    let source = to_dict(source)?;
    for key in INTERPOLATED_ATTRIBUTES {
        match source.get(key) {
            Some(value) => {
                info.insert((*key).into(), value.clone());
            }
            None => {
                info.remove(key);
            }
        }
    }
    if let Some(panose) = source.get("openTypeOS2Panose") {
        info.insert("openTypeOS2Panose".into(), panose.clone());
    }
    plist::from_value(&Value::Dictionary(info)).map_err(bad_value)
}

/// ufo2ft's `WDTH_VALUE_TO_OS2_WIDTH_CLASS`, also glyphsLib's `WIDTH_CLASS_TO_VALUE`
const WIDTH_CLASS_VALUES: [(u8, f64); 9] = [
    (1, 50.0),
    (2, 62.5),
    (3, 75.0),
    (4, 87.5),
    (5, 100.0),
    (6, 112.5),
    (7, 125.0),
    (8, 150.0),
    (9, 200.0),
];

/// Windows language IDs by lowercase BCP 47 tag, fontTools'
/// `_WINDOWS_LANGUAGE_CODES` (fontTools 4.67.0), which ufo2ft looks the
/// instance's localized names up in
const WINDOWS_LANGUAGES: &[(&str, u16)] = &[
    ("ar-sa", 0x0401),
    ("bg", 0x0402),
    ("ca", 0x0403),
    ("zh-tw", 0x0404),
    ("cs", 0x0405),
    ("da", 0x0406),
    ("de", 0x0407),
    ("el", 0x0408),
    ("en", 0x0409),
    ("es", 0x040A),
    ("fi", 0x040B),
    ("fr", 0x040C),
    ("he", 0x040D),
    ("hu", 0x040E),
    ("is", 0x040F),
    ("it", 0x0410),
    ("ja", 0x0411),
    ("ko", 0x0412),
    ("nl", 0x0413),
    ("nb", 0x0414),
    ("pl", 0x0415),
    ("pt", 0x0416),
    ("rm", 0x0417),
    ("ro", 0x0418),
    ("ru", 0x0419),
    ("hr", 0x041A),
    ("sk", 0x041B),
    ("sq", 0x041C),
    ("sv", 0x041D),
    ("th", 0x041E),
    ("tr", 0x041F),
    ("ur", 0x0420),
    ("id", 0x0421),
    ("uk", 0x0422),
    ("be", 0x0423),
    ("sl", 0x0424),
    ("et", 0x0425),
    ("lv", 0x0426),
    ("lt", 0x0427),
    ("tg", 0x0428),
    ("vi", 0x042A),
    ("hy", 0x042B),
    ("az", 0x042C),
    ("eu", 0x042D),
    ("hsb", 0x042E),
    ("mk", 0x042F),
    ("tn", 0x0432),
    ("xh", 0x0434),
    ("zu", 0x0435),
    ("af", 0x0436),
    ("ka", 0x0437),
    ("fo", 0x0438),
    ("hi", 0x0439),
    ("mt", 0x043A),
    ("se", 0x043B),
    ("ms", 0x043E),
    ("kk", 0x043F),
    ("ky", 0x0440),
    ("sw", 0x0441),
    ("tk", 0x0442),
    ("uz", 0x0443),
    ("tt", 0x0444),
    ("bn-in", 0x0445),
    ("pa", 0x0446),
    ("gu", 0x0447),
    ("or", 0x0448),
    ("ta", 0x0449),
    ("te", 0x044A),
    ("kn", 0x044B),
    ("ml", 0x044C),
    ("as", 0x044D),
    ("mr", 0x044E),
    ("sa", 0x044F),
    ("mn", 0x0450),
    ("bo", 0x0451),
    ("cy", 0x0452),
    ("km", 0x0453),
    ("lo", 0x0454),
    ("gl", 0x0456),
    ("kok", 0x0457),
    ("syr", 0x045A),
    ("si", 0x045B),
    ("iu", 0x045D),
    ("am", 0x045E),
    ("ne", 0x0461),
    ("fy", 0x0462),
    ("ps", 0x0463),
    ("fil", 0x0464),
    ("dv", 0x0465),
    ("ha", 0x0468),
    ("yo", 0x046A),
    ("qu-bo", 0x046B),
    ("nso", 0x046C),
    ("ba", 0x046D),
    ("lb", 0x046E),
    ("kl", 0x046F),
    ("ig", 0x0470),
    ("ii", 0x0478),
    ("arn", 0x047A),
    ("moh", 0x047C),
    ("br", 0x047E),
    ("ug", 0x0480),
    ("mi", 0x0481),
    ("oc", 0x0482),
    ("co", 0x0483),
    ("gsw", 0x0484),
    ("sah", 0x0485),
    ("quc", 0x0486),
    ("rw", 0x0487),
    ("wo", 0x0488),
    ("prs", 0x048C),
    ("ar-iq", 0x0801),
    ("zh", 0x0804),
    ("de-ch", 0x0807),
    ("en-gb", 0x0809),
    ("es-mx", 0x080A),
    ("fr-be", 0x080C),
    ("it-ch", 0x0810),
    ("nl-be", 0x0813),
    ("nn", 0x0814),
    ("pt-pt", 0x0816),
    ("sr-latn", 0x081A),
    ("sv-fi", 0x081D),
    ("az-cyrl", 0x082C),
    ("dsb", 0x082E),
    ("se-se", 0x083B),
    ("ga", 0x083C),
    ("ms-bn", 0x083E),
    ("uz-cyrl", 0x0843),
    ("bn", 0x0845),
    ("mn-cn", 0x0850),
    ("iu-latn", 0x085D),
    ("tzm", 0x085F),
    ("qu-ec", 0x086B),
    ("ar", 0x0C01),
    ("zh-hk", 0x0C04),
    ("de-at", 0x0C07),
    ("en-au", 0x0C09),
    ("fr-ca", 0x0C0C),
    ("sr", 0x0C1A),
    ("se-fi", 0x0C3B),
    ("qu", 0x0C6B),
    ("ar-ly", 0x1001),
    ("zh-sg", 0x1004),
    ("de-lu", 0x1007),
    ("en-ca", 0x1009),
    ("es-gt", 0x100A),
    ("fr-ch", 0x100C),
    ("hr-ba", 0x101A),
    ("smj-no", 0x103B),
    ("ar-dz", 0x1401),
    ("zh-mo", 0x1404),
    ("de-li", 0x1407),
    ("en-nz", 0x1409),
    ("es-cr", 0x140A),
    ("fr-lu", 0x140C),
    ("bs", 0x141A),
    ("smj", 0x143B),
    ("ary", 0x1801),
    ("en-ie", 0x1809),
    ("es-pa", 0x180A),
    ("fr-mc", 0x180C),
    ("sr-latn-ba", 0x181A),
    ("sma-no", 0x183B),
    ("aeb", 0x1C01),
    ("en-za", 0x1C09),
    ("es-do", 0x1C0A),
    ("sr-cyrl-ba", 0x1C1A),
    ("sma", 0x1C3B),
    ("ar-om", 0x2001),
    ("en-jm", 0x2009),
    ("es-ve", 0x200A),
    ("bs-cyrl", 0x201A),
    ("sms", 0x203B),
    ("ar-ye", 0x2401),
    ("en-029", 0x2409),
    ("es-co", 0x240A),
    ("smn", 0x243B),
    ("ar-sy", 0x2801),
    ("en-bz", 0x2809),
    ("es-pe", 0x280A),
    ("ar-jo", 0x2C01),
    ("en-tt", 0x2C09),
    ("es-ar", 0x2C0A),
    ("ar-lb", 0x3001),
    ("en-zw", 0x3009),
    ("es-ec", 0x300A),
    ("ar-kw", 0x3401),
    ("en-ph", 0x3409),
    ("es-cl", 0x340A),
    ("ar-ae", 0x3801),
    ("es-uy", 0x380A),
    ("ar-bh", 0x3C01),
    ("es-py", 0x3C0A),
    ("ar-qa", 0x4001),
    ("en-in", 0x4009),
    ("es-bo", 0x400A),
    ("en-my", 0x4409),
    ("es-sv", 0x440A),
    ("en-sg", 0x4809),
    ("es-hn", 0x480A),
    ("es-ni", 0x4C0A),
    ("es-pr", 0x500A),
    ("es-us", 0x540A),
];

fn windows_language_id(tag: &str) -> Option<u16> {
    let tag = tag.to_lowercase();
    WINDOWS_LANGUAGES
        .iter()
        .find(|(t, _)| *t == tag)
        .map(|(_, id)| *id)
}

/// Overlay a `public.fontInfo` dictionary, like ufo2ft's `merge_public_font_info`:
/// name records are matched by (platform, encoding, language, name ID) and
/// otherwise appended, every other key replaces the attribute.
fn merge_font_info(info: &mut Dictionary, overrides: &Dictionary) {
    for (key, value) in overrides {
        if key == "openTypeNameRecords" {
            if let Some(records) = value.as_array() {
                add_name_records(info, records.iter().cloned());
            }
        } else {
            info.insert(key.clone(), value.clone());
        }
    }
}

fn add_name_records(info: &mut Dictionary, records: impl IntoIterator<Item = Value>) {
    if !info.contains_key("openTypeNameRecords") {
        info.insert("openTypeNameRecords".into(), Value::Array(Vec::new()));
    }
    let Some(existing) = info
        .get_mut("openTypeNameRecords")
        .and_then(Value::as_array_mut)
    else {
        return;
    };
    let key = |record: &Dictionary| {
        ["platformID", "encodingID", "languageID", "nameID"]
            .map(|k| record.get(k).and_then(Value::as_signed_integer))
    };
    for record in records {
        let Some(new) = record.as_dictionary() else {
            continue;
        };
        let mut new = new.clone();
        if let Some(Value::String(s)) = new.get("string") {
            new.insert("string".to_string(), Value::String(s.trim().to_string()));
        }
        match existing
            .iter_mut()
            .find(|r| r.as_dictionary().is_some_and(|r| key(r) == key(&new)))
        {
            Some(old) => *old = Value::Dictionary(new),
            None => existing.push(Value::Dictionary(new)),
        }
    }
}

fn name_record(name_id: u16, language_id: u16, value: &str) -> Value {
    let mut record = Dictionary::new();
    record.insert("nameID".into(), (name_id as i64).into());
    record.insert("platformID".into(), 3i64.into());
    record.insert("encodingID".into(), 1i64.into());
    record.insert("languageID".into(), (language_id as i64).into());
    record.insert("string".into(), value.trim().to_string().into());
    Value::Dictionary(record)
}

fn localized(names: &[designspace::LocalizedString], lang: &str) -> Option<String> {
    names
        .iter()
        .find(|name| name.language == lang)
        .map(|name| name.string.clone())
}

fn has(info: &Dictionary, key: &str) -> bool {
    info.get(key)
        .is_some_and(|v| !matches!(v, Value::String(s) if s.is_empty()))
}

/// Apply the instance's metadata to its source's fontinfo, as `fontmake -i`
/// does: ufo2ft's `_generate_instance_info` (the source's info, the root lib
/// `public.fontInfo`, the instance's attributes and localized names, then the
/// instance lib `public.fontInfo`), with the OS/2 classes from the location, as
/// ufo2ft sets them since <https://github.com/googlefonts/ufo2ft/pull/1014>
/// (and glyphsLib's `apply_instance_data_to_ufo` before it). Glyphs.app's
/// defaults for fsType and the underline, which glyphsLib also fills in, are
/// left out: they belong to Glyphs sources, which the Glyphs frontend builds,
/// as do the `com.schriftgestaltung.*` lib keys glyphsLib reads.
pub(crate) fn apply_instance_info(
    path: &Path,
    font_info: &mut norad::FontInfo,
    designspace: &DesignSpaceDocument,
    selected: &SelectedInstance,
) -> Result<(), Error> {
    let instance = &designspace.instances[selected.index];
    let bad_value = |what: &str, e: plist::Error| {
        Error::BadSource(BadSource::custom(path, format!("{what}: {e}")))
    };
    let mut info = plist::to_value(&*font_info)
        .map_err(|e| bad_value("fontinfo", e))?
        .into_dictionary()
        .unwrap_or_default();

    if !selected.copy_all_info {
        for key in INSTANCE_SPECIFIC_ATTRIBUTES {
            info.remove(key);
        }
    }

    if let Some(Value::Dictionary(root)) = designspace.lib.get("public.fontInfo") {
        merge_font_info(&mut info, root);
    }

    if let Some(family) = &instance.familyname {
        info.insert("familyName".into(), family.clone().into());
    }
    match &instance.stylename {
        Some(style) => {
            info.insert("styleName".into(), style.clone().into());
        }
        None if !has(&info, "styleName") => {
            // ufo2ft falls back to the source's style name, with a warning
            if let Some(style) = &font_info.style_name {
                warn!(
                    "Instance \"{}\" has no stylename; using the source's, \"{style}\"",
                    display_name(instance)
                );
                info.insert("styleName".into(), style.clone().into());
            }
        }
        None => {}
    }
    if let Some(name) = &instance.postscriptfontname {
        info.insert("postscriptFontName".into(), name.clone().into());
    }
    if let Some(name) = &instance.stylemapfamilyname {
        info.insert("styleMapFamilyName".into(), name.clone().into());
    }
    if let Some(style) = &instance.stylemapstylename {
        let style = style.to_lowercase();
        if ["regular", "italic", "bold", "bold italic"].contains(&style.as_str()) {
            info.insert("styleMapStyleName".into(), style.into());
        } else {
            warn!(
                "Instance \"{}\": styleMapStyleName \"{style}\" is not one of regular, \
                 italic, bold or bold italic; ignored",
                display_name(instance)
            );
        }
    }

    // Localized names, as Windows records: family and style names are the
    // typographic names (16, 17), with ID 1 from the style map family, else
    // the family plus the localized style, or the default style when it
    // isn't a standard one
    let mut languages: Vec<&str> = [
        &instance.localised_family_names,
        &instance.localised_style_names,
        &instance.localised_style_map_family_names,
        &instance.localised_style_map_style_names,
    ]
    .into_iter()
    .flatten()
    .map(|name| name.language.as_str())
    .filter(|lang| *lang != "en")
    .collect();
    languages.sort();
    languages.dedup();
    let mut records = Vec::new();
    for lang in languages {
        let Some(lang_id) = windows_language_id(lang) else {
            warn!(
                "Instance \"{}\": no Windows language for \"{lang}\"; its names are ignored",
                display_name(instance)
            );
            continue;
        };
        let family = localized(&instance.localised_family_names, lang);
        let style = localized(&instance.localised_style_names, lang);
        if let Some(family) = &family {
            records.push(name_record(16, lang_id, family));
        }
        if let Some(style) = &style {
            records.push(name_record(17, lang_id, style));
        }
        if let Some(name) = localized(&instance.localised_style_map_family_names, lang) {
            records.push(name_record(1, lang_id, &name));
        } else if let Some(family) = &family {
            let default_style = info
                .get("styleName")
                .and_then(Value::as_string)
                .unwrap_or_default();
            let name = match &style {
                Some(style) => format!("{family} {style}"),
                None if !["regular", "italic", "bold", "bold italic"]
                    .contains(&default_style.to_lowercase().as_str()) =>
                {
                    format!("{family} {default_style}")
                }
                None => family.clone(),
            };
            records.push(name_record(1, lang_id, &name));
        }
        if let Some(name) = localized(&instance.localised_style_map_style_names, lang) {
            records.push(name_record(2, lang_id, &name));
        }
    }
    if !records.is_empty() {
        add_name_records(&mut info, records);
    }

    // The italic angle from the location, when the masters set none
    let user = |tag: &[u8; 4]| {
        selected
            .user_location
            .iter()
            .find(|(t, _)| *t == Tag::new(tag))
            .map(|(_, v)| *v)
    };
    let has_axis = |tag: &[u8; 4]| user(tag).is_some();
    if !has(&info, "italicAngle")
        && let Some(slnt) = user(b"slnt")
    {
        info.insert("italicAngle".into(), slnt.clamp(-90.0, 90.0).into());
    }

    let instance_font_info = match instance.lib.get("public.fontInfo") {
        Some(Value::Dictionary(lib)) => Some(lib),
        _ => None,
    };
    if let Some(lib) = instance_font_info {
        merge_font_info(&mut info, lib);
    }

    // The classes whenever the axis exists, overriding the source's, as ufo2ft
    // does since googlefonts/ufo2ft#1014: the instance lib's class, else the
    // user location, rounded and clamped to 1-1000 for weight and the nearest
    // class for width
    let lib_class = |key: &str| {
        instance_font_info
            .and_then(|lib| lib.get(key))
            .and_then(Value::as_signed_integer)
    };
    if has_axis(b"wght") {
        let class = lib_class("openTypeOS2WeightClass")
            .unwrap_or_else(|| user(b"wght").unwrap().clamp(1.0, 1000.0).round() as i64);
        info.insert("openTypeOS2WeightClass".into(), class.into());
    }
    if has_axis(b"wdth") {
        let class = lib_class("openTypeOS2WidthClass")
            .unwrap_or_else(|| width_class_nearest(user(b"wdth").unwrap()) as i64);
        info.insert("openTypeOS2WidthClass".into(), class.into());
    }

    *font_info = plist::from_value(&Value::Dictionary(info))
        .map_err(|e| bad_value("instance fontinfo", e))?;
    Ok(())
}

/// nearest, the lower one on a tie
fn width_class_nearest(wdth: f64) -> u8 {
    WIDTH_CLASS_VALUES
        .iter()
        .min_by(|(_, a), (_, b)| {
            (a - wdth)
                .abs()
                .partial_cmp(&(b - wdth).abs())
                .unwrap_or(std::cmp::Ordering::Equal)
        })
        .map(|(class, _)| *class)
        .unwrap_or(5)
}
