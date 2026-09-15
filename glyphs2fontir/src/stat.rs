//! Reproduce Glyphs' automatic STAT-label derivation.
//!
//! The behaviour intentionally follows glyphsLib's `builder/stat.py`, which
//! reproduces what Glyphs.app does on export:
//!
//! - STAT is derived only when the source has an active Variable Font
//!   Setting, and not when every active setting has `Export STAT Table` off.
//!   Labels come from the active, non-variable instances.
//! - Every source axis becomes a STAT axis, point axes included, in source
//!   order. Each distinct instance coordinate on an axis becomes one axis
//!   value, named with the whole style name of an instance at that
//!   coordinate, preferring one that sits at the default on every other axis:
//!   with Regular, Bold, Condensed and Bold Condensed instances, wght gets
//!   Regular and Bold, wdth gets Normal and Condensed.
//! - The default coordinate of each axis gets an elidable "Regular"
//!   ("Normal" for wdth), except on the first axis the instances vary on,
//!   which gets the default instance's own style name; a name that equals
//!   the default one is still elidable. `Elidable STAT Axis Value Name` on an
//!   instance marks its label elidable on the listed axes.
//! - An elidable default weight is style-linked to the weight of the instance
//!   flagged `isBold`. A real `ital` axis links its default to its highest
//!   value. When the source has no `ital` axis a STAT-only one is added:
//!   "Italic" at 1 if the default instance is italic (flag or name), else an
//!   elidable "Roman" at 0 linked to 1. For an italic family whose default
//!   instance is named just "Italic", "Italic" is stripped from every label.
//! - `Style Name as STAT entry` on any instance switches the whole font to
//!   manual mode: only the instances listing an axis produce labels on it,
//!   named as-is, and axes nobody lists get no values at all.
//!
//! Two deviations from glyphsLib. An instance's value on an axis is its
//! Glyphs axis coordinate converted to user space through the font's axis
//! mapping, the same conversion fvar and avar use, so a label always sits
//! where its instance does. glyphsLib 6.14 takes the instance's weightClass
//! and widthClass as the user value instead, which puts labels at coordinates
//! no instance occupies when the mapping and the classes disagree
//! (googlefonts/glyphsLib#1171). And inactive Variable Font Settings are
//! ignored, as Glyphs.app does not export them; glyphsLib 6.14 counts them
//! (fixed by googlefonts/glyphsLib#1172). Labels outside the variable font's user
//! region are dropped afterwards, as fontTools' `getStatAxes` does.

use std::{collections::BTreeMap, sync::LazyLock};

use fontdrasil::{
    coords::DesignCoord,
    types::{Axes, Axis},
};
use fontir::ir::{AxisValueLabel, StatAxis};
use glyphs_reader::{Font, InstanceType};
use ordered_float::OrderedFloat;
use regex::Regex;
use smol_str::SmolStr;
use write_fonts::types::Tag;

const ITAL: Tag = Tag::new(b"ital");
const WDTH: Tag = Tag::new(b"wdth");
const WGHT: Tag = Tag::new(b"wght");

#[derive(Clone, Debug)]
struct StatInstance {
    name: String,
    active: bool,
    type_: InstanceType,
    user_locations: Vec<OrderedFloat<f64>>,
    is_bold: bool,
    is_italic: bool,
    export_stat_table: Option<bool>,
    elidable: Vec<SmolStr>,
    manual: Vec<SmolStr>,
}

/// The STAT design axes Glyphs derives from a source: the source axes, point
/// axes included, plus the synthetic ital axis. `None` means no STAT.
pub(crate) fn to_stat_axes(font: &Font, axes: &Axes) -> Option<Vec<StatAxis>> {
    // Copy the small subset needed by the pure derivation core.
    let instances: Vec<_> = font
        .instances
        .iter()
        .map(|instance| StatInstance {
            name: instance.name.clone(),
            active: instance.active,
            type_: instance.type_.clone(),
            user_locations: axes
                .iter()
                .zip(instance.axes_values.iter())
                .map(|(axis, value)| {
                    DesignCoord::new(*value)
                        .to_user(&axis.converter)
                        .into_inner()
                })
                .collect(),
            is_bold: instance.is_bold,
            is_italic: instance.is_italic,
            export_stat_table: instance.custom_parameters.export_stat_table,
            elidable: instance
                .custom_parameters
                .elidable_stat_axis_value_names
                .clone(),
            manual: instance
                .custom_parameters
                .style_names_as_stat_entries
                .clone(),
        })
        .collect();
    let mut stat_axes = derive_stat_axes(&instances, axes)?;
    // fontTools only keeps the labels within the variable font's user region:
    // https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Lib/fontTools/varLib/stat.py#L70-L74
    for (stat_axis, axis) in stat_axes.iter_mut().zip(axes.iter()) {
        let range = axis.min.into_inner()..=axis.max.into_inner();
        stat_axis
            .labels
            .retain(|label| range.contains(&label.value));
    }
    Some(stat_axes)
}

fn derive_stat_axes(instances: &[StatInstance], axes: &Axes) -> Option<Vec<StatAxis>> {
    // Glyphs only builds a STAT for a variable font, which needs an active
    // Variable Font Setting, and "Export STAT Table" turns it off per variable
    // font. fontc builds one font per source, so the STAT only goes when every
    // active setting turns it off.
    let variable_instances: Vec<_> = instances
        .iter()
        .filter(|instance| instance.type_ == InstanceType::Variable && instance.active)
        .collect();
    if variable_instances.is_empty()
        || variable_instances
            .iter()
            .all(|instance| instance.export_stat_table == Some(false))
    {
        return None;
    }

    let instances: Vec<_> = instances
        .iter()
        .filter(|instance| instance.type_ == InstanceType::Single && instance.active)
        .collect();

    let default_instance = instances
        .iter()
        .copied()
        .find(|instance| at_default(instance, axes, None));
    let italic = default_instance.is_some_and(is_italic);
    let plain_italic = italic
        && default_instance.is_some_and(|instance| instance.name.trim().to_lowercase() == "italic");
    let manual = instances.iter().any(|instance| !instance.manual.is_empty());

    let mut stat_axes = if manual {
        manual_labels(axes, &instances)
    } else {
        automatic_labels(axes, &instances, default_instance, plain_italic)
    };

    if !axes.contains(&ITAL) {
        stat_axes.push(synthetic_italic_axis(italic));
    }
    Some(stat_axes)
}

fn user_loc(instance: &StatInstance, axis_idx: usize) -> Option<OrderedFloat<f64>> {
    instance.user_locations.get(axis_idx).copied()
}

fn at_default(instance: &StatInstance, axes: &Axes, skip: Option<usize>) -> bool {
    axes.iter().enumerate().all(|(axis_idx, axis)| {
        skip == Some(axis_idx)
            || user_loc(instance, axis_idx).is_none_or(|loc| loc == axis.default.into_inner())
    })
}

fn is_italic(instance: &StatInstance) -> bool {
    instance.is_italic || instance.name.to_lowercase().contains("italic")
}

fn is_elidable(instance: &StatInstance, axis: &Axis) -> bool {
    contains_tag(&instance.elidable, axis.tag)
}

fn contains_tag(values: &[SmolStr], tag: Tag) -> bool {
    values
        .iter()
        .any(|value| value.as_bytes() == tag.to_be_bytes())
}

fn default_name(tag: Tag) -> &'static str {
    if tag == WDTH { "Normal" } else { "Regular" }
}

fn make_label(name: impl Into<String>, value: OrderedFloat<f64>, elidable: bool) -> AxisValueLabel {
    AxisValueLabel {
        name: name.into(),
        value,
        min_value: None,
        max_value: None,
        linked_value: None,
        elidable,
        older_sibling: false,
    }
}

fn stat_axis(axis: &Axis, labels: Vec<AxisValueLabel>) -> StatAxis {
    StatAxis {
        labels,
        ..StatAxis::from_axis(axis)
    }
}

fn manual_labels(axes: &Axes, instances: &[&StatInstance]) -> Vec<StatAxis> {
    axes.iter()
        .enumerate()
        .map(|(axis_idx, axis)| {
            let mut labels = BTreeMap::new();
            for instance in instances {
                if !contains_tag(&instance.manual, axis.tag) {
                    continue;
                }
                let Some(loc) = user_loc(instance, axis_idx) else {
                    continue;
                };
                labels.entry(loc).or_insert_with(|| {
                    make_label(instance.name.as_str(), loc, is_elidable(instance, axis))
                });
            }
            stat_axis(axis, labels.into_values().collect())
        })
        .collect()
}

fn automatic_labels(
    axes: &Axes,
    instances: &[&StatInstance],
    default_instance: Option<&StatInstance>,
    plain_italic: bool,
) -> Vec<StatAxis> {
    // The first axis on which instances actually vary receives the default
    // instance's full style name at its default coordinate.
    let first_varying = (0..axes.len()).find(|&axis_idx| {
        let mut locs = instances
            .iter()
            .filter_map(|instance| user_loc(instance, axis_idx));
        locs.next()
            .is_some_and(|first| locs.any(|loc| loc != first))
    });

    let mut stat_axes: Vec<_> = axes
        .iter()
        .enumerate()
        .map(|(axis_idx, axis)| {
            let default = default_name(axis.tag);
            let mut by_value: BTreeMap<OrderedFloat<f64>, Vec<&StatInstance>> = BTreeMap::new();
            for instance in instances {
                if let Some(loc) = user_loc(instance, axis_idx) {
                    by_value.entry(loc).or_default().push(*instance);
                }
            }

            let labels = by_value
                .into_iter()
                .map(|(loc, instances_at_loc)| {
                    let representative = if loc == axis.default.into_inner() {
                        // The default instance's own name goes on the first
                        // axis the instances vary on and nowhere else, so it
                        // cannot repeat when names are composed across axes;
                        // every other axis elides its default to the default
                        // name
                        match default_instance {
                            Some(instance) if first_varying == Some(axis_idx) => instance,
                            _ => return make_label(default, loc, true),
                        }
                    } else {
                        // Prefer the instance at the default on every other
                        // axis (Bold over Bold Condensed), else the first one
                        instances_at_loc
                            .iter()
                            .copied()
                            .find(|instance| at_default(instance, axes, Some(axis_idx)))
                            .unwrap_or(instances_at_loc[0])
                    };
                    let name = label_name(&representative.name, default, plain_italic);
                    // The default name is elidable by definition, any other
                    // name only when the instance's parameter says so
                    let elidable = name == default || is_elidable(representative, axis);
                    make_label(name, loc, elidable)
                })
                .collect();
            stat_axis(axis, labels)
        })
        .collect();

    for (axis_idx, axis) in axes.iter().enumerate() {
        let default = axis.default.into_inner();
        let stat_axis = &mut stat_axes[axis_idx];
        let linked = match axis.tag {
            // The elidable default weight links to the bold instance's weight,
            // wherever that instance sits on the other axes. A default named
            // e.g. Book only links when its parameter makes it elidable.
            WGHT if default_label(stat_axis, default).is_some_and(|label| label.elidable) => {
                instances
                    .iter()
                    .find(|instance| instance.is_bold)
                    .and_then(|instance| user_loc(instance, axis_idx))
            }
            // A real ital axis links upright to the highest generated italic
            // value. A slnt axis is deliberately not linked.
            ITAL => stat_axis.labels.iter().map(|label| label.value).max(),
            _ => None,
        };
        // No self-links: a bold instance at the default weight, or an ital
        // axis whose only value is the upright one, link nowhere
        if let Some(linked) = linked.filter(|linked| *linked != default)
            && let Some(label) = default_label(stat_axis, default)
        {
            label.linked_value = Some(linked);
        }
    }

    stat_axes
}

fn default_label(axis: &mut StatAxis, default: OrderedFloat<f64>) -> Option<&mut AxisValueLabel> {
    axis.labels.iter_mut().find(|label| label.value == default)
}

fn synthetic_italic_axis(italic: bool) -> StatAxis {
    let mut label = if italic {
        make_label("Italic", 1.0.into(), false)
    } else {
        make_label("Roman", 0.0.into(), true)
    };
    label.linked_value = (!italic).then_some(1.0.into());
    StatAxis {
        tag: ITAL,
        name: "Italic".to_string(),
        labels: vec![label],
    }
}

fn label_name(name: &str, default: &str, plain_italic: bool) -> String {
    let name = if plain_italic {
        strip_italic(name)
    } else {
        name.to_string()
    };
    if name.is_empty() {
        default.to_string()
    } else {
        name
    }
}

/// Strip "Italic" from a style name the way glyphsLib does,
/// `re.sub(r"\s*italic\s*", " ", name, flags=re.IGNORECASE).strip()`.
fn strip_italic(value: &str) -> String {
    static ITALIC: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?i-u)\s*italic\s*").unwrap());
    ITALIC.replace_all(value, " ").trim().to_string()
}

/// Assert the (name, value, elidable, linked value) labels on the axis `tag`.
#[cfg(test)]
pub(crate) fn assert_labels(
    stat: &[StatAxis],
    tag: &str,
    expected: &[(&str, f64, bool, Option<f64>)],
) {
    let tag = Tag::new(tag.as_bytes().try_into().expect("four-letter tag"));
    let axis = stat
        .iter()
        .find(|axis| axis.tag == tag)
        .unwrap_or_else(|| panic!("no STAT axis {tag}"));
    let actual: Vec<_> = axis
        .labels
        .iter()
        .map(|label| {
            (
                label.name.as_str(),
                label.value.0,
                label.elidable,
                label.linked_value.map(|value| value.0),
            )
        })
        .collect();
    assert_eq!(expected, actual, "labels on {tag}");
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use fontdrasil::{
        coords::{CoordConverter, UserCoord},
        types::{Axes, Axis},
    };
    use write_fonts::types::Tag;

    use fontir::ir::StatAxis;
    use glyphs_reader::InstanceType;

    use super::{StatInstance, assert_labels, derive_stat_axes, strip_italic};

    impl StatInstance {
        fn single(name: &str, coords: &[f64]) -> Self {
            Self {
                name: name.to_string(),
                active: true,
                type_: InstanceType::Single,
                user_locations: coords.iter().copied().map(Into::into).collect(),
                is_bold: false,
                is_italic: false,
                export_stat_table: None,
                elidable: Vec::new(),
                manual: Vec::new(),
            }
        }

        fn variable(export_stat_table: Option<bool>) -> Self {
            Self {
                name: "VF".to_string(),
                active: true,
                type_: InstanceType::Variable,
                user_locations: Vec::new(),
                is_bold: false,
                is_italic: false,
                export_stat_table,
                elidable: Vec::new(),
                manual: Vec::new(),
            }
        }
    }

    fn axis(tag: &str, name: &str, min: f64, default: f64, max: f64) -> Axis {
        let min = UserCoord::new(min);
        let default = UserCoord::new(default);
        let max = UserCoord::new(max);
        Axis {
            name: name.to_string(),
            tag: Tag::from_str(tag).unwrap(),
            min,
            default,
            max,
            hidden: false,
            converter: CoordConverter::unmapped(min, default, max),
            localized_names: Default::default(),
        }
    }

    fn stat(axes: Vec<Axis>, mut instances: Vec<StatInstance>) -> Vec<StatAxis> {
        instances.push(StatInstance::variable(None));
        derive_stat_axes(&instances, &Axes::new(axes)).unwrap()
    }

    #[test]
    fn style_linked_bold_links_the_default_weight() {
        for (is_bold, linked) in [(true, Some(700.0)), (false, None)] {
            let regular = StatInstance::single("Regular", &[400.0]);
            let mut bold = StatInstance::single("Bold", &[700.0]);
            bold.is_bold = is_bold;
            let stat = stat(
                vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
                vec![regular, bold],
            );
            assert_labels(
                &stat,
                "wght",
                &[
                    ("Regular", 400.0, true, linked),
                    ("Bold", 700.0, false, None),
                ],
            );
        }
    }

    #[test]
    fn weight_width_labels_and_stat_only_italic() {
        let stat = stat(
            vec![
                axis("wght", "Weight", 400.0, 400.0, 700.0),
                axis("wdth", "Width", 75.0, 100.0, 100.0),
            ],
            vec![
                StatInstance::single("Regular", &[400.0, 100.0]),
                StatInstance::single("Bold", &[700.0, 100.0]),
                StatInstance::single("Condensed", &[400.0, 75.0]),
            ],
        );
        assert_labels(
            &stat,
            "wght",
            &[("Regular", 400.0, true, None), ("Bold", 700.0, false, None)],
        );
        assert_labels(
            &stat,
            "wdth",
            &[
                ("Condensed", 75.0, false, None),
                ("Normal", 100.0, true, None),
            ],
        );
        assert_labels(&stat, "ital", &[("Roman", 0.0, true, Some(1.0))]);
    }

    // Deliberately incomplete source: "Compact High" is the only instance off
    // the default, with no single-axis "Compact" or "High" sibling, so its
    // whole name lands on both axes (and would compose to "Compact High
    // Compact High"). Glyphs does this; a complete family never hits it
    #[test]
    fn style_name_is_used_for_every_differing_axis() {
        let stat = stat(
            vec![
                axis("SPAC", "Spacing", -100.0, 0.0, 0.0),
                axis("MSHQ", "Mashq", 10.0, 10.0, 20.0),
            ],
            vec![
                StatInstance::single("Regular", &[0.0, 10.0]),
                StatInstance::single("Compact High", &[-100.0, 20.0]),
            ],
        );
        assert_labels(
            &stat,
            "SPAC",
            &[
                ("Compact High", -100.0, false, None),
                ("Regular", 0.0, true, None),
            ],
        );
        assert_labels(
            &stat,
            "MSHQ",
            &[
                ("Regular", 10.0, true, None),
                ("Compact High", 20.0, false, None),
            ],
        );
    }

    // Values come from the instances, not the axis extremes (Medium at 500),
    // and a value shared by several instances is named after the one at the
    // default on every other axis: Bold, not Bold Compact
    #[test]
    fn instance_driven_values_and_representative_instance() {
        let stat = stat(
            vec![
                axis("wght", "Weight", 400.0, 400.0, 700.0),
                axis("SPAC", "Spacing", -100.0, 0.0, 0.0),
            ],
            vec![
                StatInstance::single("Regular", &[400.0, 0.0]),
                StatInstance::single("Medium", &[500.0, 0.0]),
                StatInstance::single("Bold", &[700.0, 0.0]),
                StatInstance::single("Compact", &[400.0, -100.0]),
                StatInstance::single("Bold Compact", &[700.0, -100.0]),
            ],
        );
        assert_labels(
            &stat,
            "wght",
            &[
                ("Regular", 400.0, true, None),
                ("Medium", 500.0, false, None),
                ("Bold", 700.0, false, None),
            ],
        );
        assert_labels(
            &stat,
            "SPAC",
            &[
                ("Compact", -100.0, false, None),
                ("Regular", 0.0, true, None),
            ],
        );
    }

    // The "Book" default instance names the default of the first axis the
    // instances vary on (SPAC) and nowhere else: the pinned MSHQ axis gets an
    // elidable Regular, or names would compose to "Book Book"
    #[test]
    fn non_regular_default_labels_only_the_first_varying_axis() {
        let stat = stat(
            vec![
                axis("SPAC", "Spacing", -100.0, 0.0, 0.0),
                axis("MSHQ", "Mashq", 10.0, 10.0, 10.0),
            ],
            vec![
                StatInstance::single("Book", &[0.0, 10.0]),
                StatInstance::single("Compact", &[-100.0, 10.0]),
            ],
        );
        assert_labels(
            &stat,
            "SPAC",
            &[("Compact", -100.0, false, None), ("Book", 0.0, false, None)],
        );
        assert_labels(&stat, "MSHQ", &[("Regular", 10.0, true, None)]);
    }

    // "First varying" is about the instances, not the axis order: no instance
    // leaves wght 400, so Book goes on wdth and wght gets an elidable Regular
    #[test]
    fn default_instance_labels_first_axis_the_instances_vary_on() {
        let stat = stat(
            vec![
                axis("wght", "Weight", 400.0, 400.0, 700.0),
                axis("wdth", "Width", 75.0, 100.0, 100.0),
            ],
            vec![
                StatInstance::single("Book", &[400.0, 100.0]),
                StatInstance::single("Condensed", &[400.0, 75.0]),
            ],
        );
        assert_labels(&stat, "wght", &[("Regular", 400.0, true, None)]);
        assert_labels(
            &stat,
            "wdth",
            &[
                ("Condensed", 75.0, false, None),
                ("Book", 100.0, false, None),
            ],
        );
    }

    #[test]
    fn no_variable_font_setting_means_no_stat() {
        let instances = vec![
            StatInstance::single("Regular", &[400.0]),
            StatInstance::single("Bold", &[700.0]),
        ];
        assert!(
            derive_stat_axes(
                &instances,
                &Axes::new(vec![axis("wght", "Weight", 400.0, 400.0, 700.0)])
            )
            .is_none()
        );
    }

    #[test]
    fn all_variable_settings_opting_out_means_no_stat() {
        let instances = vec![
            StatInstance::single("Regular", &[400.0]),
            StatInstance::variable(Some(false)),
            StatInstance::variable(Some(false)),
        ];
        assert!(
            derive_stat_axes(
                &instances,
                &Axes::new(vec![axis("wght", "Weight", 400.0, 400.0, 700.0)])
            )
            .is_none()
        );
    }

    // Glyphs.app exports no variable font for an inactive setting, so it
    // neither triggers a STAT nor counts in the opt-out gate (glyphsLib
    // counts it)
    #[test]
    fn inactive_variable_settings_do_not_count() {
        let mut inactive = StatInstance::variable(None);
        inactive.active = false;
        let axes = Axes::new(vec![axis("wght", "Weight", 400.0, 400.0, 700.0)]);
        let regular = StatInstance::single("Regular", &[400.0]);
        assert!(derive_stat_axes(&[regular.clone(), inactive.clone()], &axes).is_none());
        assert!(
            derive_stat_axes(
                &[regular, inactive, StatInstance::variable(Some(false))],
                &axes
            )
            .is_none()
        );
    }

    // The opt-out is per variable font in Glyphs; fontc builds one, so STAT
    // stays as long as one active setting does not opt out
    #[test]
    fn one_variable_setting_not_opting_out_keeps_stat() {
        let instances = vec![
            StatInstance::single("Regular", &[400.0]),
            StatInstance::single("Bold", &[700.0]),
            StatInstance::variable(Some(false)),
            StatInstance::variable(None),
        ];
        let stat = derive_stat_axes(
            &instances,
            &Axes::new(vec![axis("wght", "Weight", 400.0, 400.0, 700.0)]),
        )
        .unwrap();
        assert_labels(
            &stat,
            "wght",
            &[("Regular", 400.0, true, None), ("Bold", 700.0, false, None)],
        );
    }

    #[test]
    fn inactive_static_instances_are_not_labels() {
        let regular = StatInstance::single("Regular", &[400.0]);
        let mut ignored = StatInstance::single("Ignored", &[600.0]);
        ignored.active = false;
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![regular, ignored, StatInstance::single("Bold", &[700.0])],
        );
        assert_labels(
            &stat,
            "wght",
            &[("Regular", 400.0, true, None), ("Bold", 700.0, false, None)],
        );
    }

    #[test]
    fn elidable_stat_axis_value_name_parameter() {
        let regular = StatInstance::single("Regular", &[400.0]);
        let mut bold = StatInstance::single("Bold", &[700.0]);
        bold.elidable.push("wght".into());
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![regular, bold],
        );
        assert_labels(
            &stat,
            "wght",
            &[("Regular", 400.0, true, None), ("Bold", 700.0, true, None)],
        );
    }

    // The bold link hangs off the default label being elidable, not off its
    // name: Book made elidable by the parameter still links
    #[test]
    fn elidable_named_default_still_links_to_bold() {
        let mut book = StatInstance::single("Book", &[400.0]);
        book.elidable.push("wght".into());
        let mut bold = StatInstance::single("Bold", &[700.0]);
        bold.is_bold = true;
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![book, bold],
        );
        assert_labels(
            &stat,
            "wght",
            &[
                ("Book", 400.0, true, Some(700.0)),
                ("Bold", 700.0, false, None),
            ],
        );
    }

    // One instance with the parameter turns off automatic labels on every
    // axis: wdth gets no values although Condensed sits at 75, and the wght
    // default is unlabelled. Glyphs.app and glyphsLib do the same
    #[test]
    fn style_name_as_stat_entry_switches_the_whole_font_to_manual_mode() {
        let regular = StatInstance::single("Regular", &[400.0, 100.0]);
        let mut bold = StatInstance::single("Bold", &[700.0, 100.0]);
        bold.manual.push("wght".into());
        let condensed = StatInstance::single("Condensed", &[400.0, 75.0]);
        let stat = stat(
            vec![
                axis("wght", "Weight", 400.0, 400.0, 700.0),
                axis("wdth", "Width", 75.0, 100.0, 100.0),
            ],
            vec![regular, bold, condensed],
        );
        assert_labels(&stat, "wght", &[("Bold", 700.0, false, None)]);
        assert_labels(&stat, "wdth", &[]);
        assert_labels(&stat, "ital", &[("Roman", 0.0, true, Some(1.0))]);
    }

    // Two manual instances at one coordinate is a source error. Glyphs.app
    // writes both values, which the STAT spec forbids ("no two tables should
    // provide information for the same combination of axis values"); we keep
    // the first like glyphsLib, which is what a consumer would pick anyway
    #[test]
    fn manual_mode_keeps_first_instance_at_a_duplicate_value() {
        let mut first = StatInstance::single("First", &[700.0]);
        first.manual.push("wght".into());
        let mut second = StatInstance::single("Second", &[700.0]);
        second.manual.push("wght".into());
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![first, second],
        );
        assert_labels(&stat, "wght", &[("First", 700.0, false, None)]);
    }

    // A source ital axis is used as is: no second synthetic one, and the
    // upright default links to the italic value like wght links to bold
    #[test]
    fn real_italic_axis_suppresses_synthetic_axis_and_links_upright() {
        let stat = stat(
            vec![
                axis("wght", "Weight", 400.0, 400.0, 700.0),
                axis("ital", "Italic", 0.0, 0.0, 1.0),
            ],
            vec![
                StatInstance::single("Regular", &[400.0, 0.0]),
                StatInstance::single("Bold", &[700.0, 0.0]),
                StatInstance::single("Italic", &[400.0, 1.0]),
                StatInstance::single("Bold Italic", &[700.0, 1.0]),
            ],
        );
        assert_eq!(
            stat.iter()
                .filter(|axis| axis.tag == Tag::new(b"ital"))
                .count(),
            1
        );
        assert_labels(
            &stat,
            "ital",
            &[
                ("Regular", 0.0, true, Some(1.0)),
                ("Italic", 1.0, false, None),
            ],
        );
    }

    #[test]
    fn slant_axis_is_not_linked_and_does_not_suppress_synthetic_italic() {
        let stat = stat(
            vec![axis("slnt", "Slant", -10.0, 0.0, 0.0)],
            vec![
                StatInstance::single("Regular", &[0.0]),
                StatInstance::single("Slanted", &[-10.0]),
            ],
        );
        assert_labels(
            &stat,
            "slnt",
            &[
                ("Slanted", -10.0, false, None),
                ("Regular", 0.0, true, None),
            ],
        );
        assert_labels(&stat, "ital", &[("Roman", 0.0, true, Some(1.0))]);
    }

    #[test]
    fn italic_family_is_detected_from_default_style_name() {
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![
                StatInstance::single("Italic", &[400.0]),
                StatInstance::single("Bold Italic", &[700.0]),
            ],
        );
        assert_labels(&stat, "ital", &[("Italic", 1.0, false, None)]);
    }

    // A default instance named just "Italic" makes the family italic: the
    // synthetic axis says Italic at 1 and every label loses the word, so the
    // default elides to Regular and links to Bold
    #[test]
    fn plain_italic_family_drops_italic_from_axis_value_names() {
        let mut italic = StatInstance::single("Italic", &[400.0]);
        italic.is_italic = true;
        let mut bold = StatInstance::single("Bold Italic", &[700.0]);
        bold.is_italic = true;
        bold.is_bold = true;
        let mut black = StatInstance::single("Black Italic", &[900.0]);
        black.is_italic = true;
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 900.0)],
            vec![italic, bold, black],
        );
        assert_labels(
            &stat,
            "wght",
            &[
                ("Regular", 400.0, true, Some(700.0)),
                ("Bold", 700.0, false, None),
                ("Black", 900.0, false, None),
            ],
        );
        assert_labels(&stat, "ital", &[("Italic", 1.0, false, None)]);
    }

    // Only a default named exactly "Italic" strips the word: "Book Italic"
    // keeps every name whole, none of them elidable
    #[test]
    fn named_italic_default_keeps_italic_in_all_names() {
        let mut book = StatInstance::single("Book Italic", &[400.0]);
        book.is_italic = true;
        let mut bold = StatInstance::single("Bold Italic", &[700.0]);
        bold.is_italic = true;
        bold.is_bold = true;
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![book, bold],
        );
        assert_labels(
            &stat,
            "wght",
            &[
                ("Book Italic", 400.0, false, None),
                ("Bold Italic", 700.0, false, None),
            ],
        );
    }

    // The link takes the flagged instance's weight wherever it sits on the
    // other axes. Deliberately incomplete source: with no plain Bold or
    // Condensed, the whole "Bold Condensed" name lands on both axes
    #[test]
    fn style_linked_bold_can_link_from_another_axis_position() {
        let regular = StatInstance::single("Regular", &[400.0, 100.0]);
        let mut bold = StatInstance::single("Bold Condensed", &[700.0, 75.0]);
        bold.is_bold = true;
        let stat = stat(
            vec![
                axis("wght", "Weight", 400.0, 400.0, 700.0),
                axis("wdth", "Width", 75.0, 100.0, 100.0),
            ],
            vec![regular, bold],
        );
        assert_labels(
            &stat,
            "wght",
            &[
                ("Regular", 400.0, true, Some(700.0)),
                ("Bold Condensed", 700.0, false, None),
            ],
        );
        assert_labels(
            &stat,
            "wdth",
            &[
                ("Bold Condensed", 75.0, false, None),
                ("Normal", 100.0, true, None),
            ],
        );
    }

    #[test]
    fn italic_word_removal_matches_glyphslib() {
        assert_eq!(strip_italic("  Bold   ITALIC  "), "Bold");
        // glyphsLib's regex is not word-bounded; Glyphs.app keeps e.g.
        // "Semitalic" intact, but is erratic on names starting with "Italic"
        assert_eq!(strip_italic("fooitalicbar"), "foo bar");
        assert_eq!(strip_italic("Italic Italic"), "");
    }
}
