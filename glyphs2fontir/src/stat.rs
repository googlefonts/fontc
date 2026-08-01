//! Reproduce Glyphs' automatic STAT-label derivation.
//!
//! Glyphs derives labels from active, non-variable instances, but only when the
//! source contains at least one Variable Font Setting instance. The behavior
//! here intentionally follows glyphsLib's `builder/stat.py`, including its
//! whole-style names and synthetic `ital` axis.

use std::collections::BTreeMap;

use fontdrasil::{
    coords::UserCoord,
    types::{Axes, Axis},
};
use fontir::ir::{AxisLabel, StatAxis, StatLabels};
use glyphs_reader::{Font, InstanceType};
use smol_str::SmolStr;
use write_fonts::types::Tag;

const ITAL: Tag = Tag::new(b"ital");
const SLNT: Tag = Tag::new(b"slnt");
const WDTH: Tag = Tag::new(b"wdth");
const WGHT: Tag = Tag::new(b"wght");

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Kind {
    Single,
    Variable,
}

#[derive(Clone, Debug)]
struct StatInstance {
    name: String,
    active: bool,
    kind: Kind,
    user_locations: Vec<ordered_float::OrderedFloat<f64>>,
    is_bold: bool,
    is_italic: bool,
    export_stat_table: Option<bool>,
    elidable: Vec<SmolStr>,
    manual: Vec<SmolStr>,
}

pub(crate) fn to_stat_labels(font: &Font, axes: &Axes) -> Option<StatLabels> {
    // Copy the small subset needed by the pure derivation core.
    let instances: Vec<_> = font
        .instances
        .iter()
        .map(|instance| StatInstance {
            name: instance.name.clone(),
            active: instance.active,
            kind: match instance.type_ {
                InstanceType::Single => Kind::Single,
                InstanceType::Variable => Kind::Variable,
            },
            user_locations: instance.user_locations.clone(),
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
    derive_stat_labels(&instances, axes)
}

fn derive_stat_labels(instances: &[StatInstance], axes: &Axes) -> Option<StatLabels> {
    // Unlike static instances, Variable Font Settings participate in this gate
    // even when `active`/`exports` is false.
    let variable_instances: Vec<_> = instances
        .iter()
        .filter(|instance| instance.kind == Kind::Variable)
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
        .filter(|instance| instance.kind == Kind::Single && instance.active)
        .collect();

    let default_instance = instances
        .iter()
        .copied()
        .find(|instance| at_default(instance, axes, None));
    let italic = default_instance.is_some_and(is_italic);
    let plain_italic = italic
        && default_instance
            .is_some_and(|instance| python_trim(&instance.name).to_lowercase() == "italic");
    let manual = instances.iter().any(|instance| !instance.manual.is_empty());

    let mut stat_axes = if manual {
        manual_labels(axes, &instances)
    } else {
        automatic_labels(axes, &instances, default_instance, plain_italic)
    };

    if !axes.contains(&ITAL) {
        stat_axes.push(synthetic_italic_axis(italic));
    }

    Some(StatLabels {
        elided_fallback_name: "Regular".to_string(),
        axes: stat_axes,
    })
}

fn user_loc(instance: &StatInstance, axis_idx: usize) -> Option<UserCoord> {
    instance
        .user_locations
        .get(axis_idx)
        .map(|coord| UserCoord::new(*coord))
}

fn at_default(instance: &StatInstance, axes: &Axes, skip: Option<usize>) -> bool {
    axes.iter().enumerate().all(|(axis_idx, axis)| {
        skip == Some(axis_idx) || user_loc(instance, axis_idx).is_none_or(|loc| loc == axis.default)
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

fn make_label(name: impl Into<String>, user_value: UserCoord, elidable: bool) -> AxisLabel {
    AxisLabel {
        name: name.into(),
        user_value,
        elidable,
        older_sibling: false,
        linked_user_value: None,
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
            StatAxis {
                tag: axis.tag,
                name: axis.ui_label_name().to_string(),
                labels: labels.into_values().collect(),
            }
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
    let first_varying = axes.iter().enumerate().find_map(|(axis_idx, _)| {
        let values: std::collections::BTreeSet<_> = instances
            .iter()
            .filter_map(|instance| user_loc(instance, axis_idx))
            .collect();
        (values.len() > 1).then_some(axis_idx)
    });

    let slope_tag = if axes.contains(&ITAL) {
        Some(ITAL)
    } else if axes.contains(&SLNT) {
        Some(SLNT)
    } else {
        None
    };

    let mut stat_axes: Vec<_> = axes
        .iter()
        .enumerate()
        .map(|(axis_idx, axis)| {
            let default = default_name(axis.tag);
            let mut by_value: BTreeMap<UserCoord, Vec<&StatInstance>> = BTreeMap::new();
            for instance in instances {
                if let Some(loc) = user_loc(instance, axis_idx) {
                    by_value.entry(loc).or_default().push(*instance);
                }
            }

            let labels = by_value
                .into_iter()
                .map(|(loc, instances_at_loc)| {
                    if loc == axis.default
                        && (first_varying != Some(axis_idx) || default_instance.is_none())
                    {
                        return make_label(default, loc, true);
                    }

                    let representative = if loc == axis.default {
                        default_instance.unwrap()
                    } else {
                        instances_at_loc
                            .iter()
                            .copied()
                            .find(|instance| at_default(instance, axes, Some(axis_idx)))
                            .unwrap_or(instances_at_loc[0])
                    };
                    let name = label_name(&representative.name, default, plain_italic);
                    let elidable = name == default || is_elidable(representative, axis);
                    make_label(name, loc, elidable)
                })
                .collect();

            StatAxis {
                tag: axis.tag,
                name: axis.ui_label_name().to_string(),
                labels,
            }
        })
        .collect();

    if let Some(wght) = stat_axes.iter_mut().find(|axis| axis.tag == WGHT) {
        let default = axes.get(&WGHT).unwrap().default;
        if default_value_elides(wght, default) {
            let linked = instances
                .iter()
                .copied()
                .find(|instance| instance.is_bold)
                .and_then(|instance| {
                    let axis_idx = axes.iter().position(|axis| axis.tag == WGHT).unwrap();
                    user_loc(instance, axis_idx)
                });
            set_linked_value(wght, default, linked);
        }
    }

    // A real ital axis links upright to the highest generated italic value.
    // A slnt axis is recognized as the slope axis but is deliberately not linked.
    if slope_tag == Some(ITAL) {
        let ital = stat_axes.iter_mut().find(|axis| axis.tag == ITAL).unwrap();
        let default = axes.get(&ITAL).unwrap().default;
        let linked = ital
            .labels
            .iter()
            .map(|label| label.user_value)
            .max()
            .unwrap_or(default);
        set_linked_value(ital, default, Some(linked));
    }

    stat_axes
}

fn default_value_elides(axis: &StatAxis, default: UserCoord) -> bool {
    axis.labels
        .iter()
        .find(|label| label.user_value == default)
        .is_some_and(|label| label.elidable)
}

fn set_linked_value(axis: &mut StatAxis, default: UserCoord, value: Option<UserCoord>) {
    let Some(value) = value.filter(|value| *value != default) else {
        return;
    };
    if let Some(label) = axis
        .labels
        .iter_mut()
        .find(|label| label.user_value == default)
    {
        label.linked_user_value = Some(value);
    }
}

fn synthetic_italic_axis(italic: bool) -> StatAxis {
    let (name, value, elidable, linked) = if italic {
        ("Italic", UserCoord::new(1.0), false, None)
    } else {
        (
            "Roman",
            UserCoord::new(0.0),
            true,
            Some(UserCoord::new(1.0)),
        )
    };
    let mut label = make_label(name, value, elidable);
    label.linked_user_value = linked;
    StatAxis {
        tag: ITAL,
        name: "Italic".to_string(),
        labels: vec![label],
    }
}

fn label_name(name: &str, default: &str, plain_italic: bool) -> String {
    let name = if plain_italic {
        strip_italic_regex(name)
    } else {
        name.to_string()
    };
    if name.is_empty() {
        default.to_string()
    } else {
        name
    }
}

// Equivalent to Python's `re.sub(r"\s*italic\s*", " ", name,
// flags=re.IGNORECASE).strip()`. Python's case-insensitive literal `i` also
// matches the dotted and dotless Turkish I characters.
fn strip_italic_regex(value: &str) -> String {
    let mut result = String::with_capacity(value.len());
    let mut cursor = 0;
    while let Some((italic_start, italic_end)) = find_italic(value, cursor) {
        let mut match_start = italic_start;
        while match_start > cursor {
            let (previous, ch) = value[..match_start].char_indices().next_back().unwrap();
            if !python_whitespace(ch) {
                break;
            }
            match_start = previous;
        }

        result.push_str(&value[cursor..match_start]);
        result.push(' ');
        cursor = italic_end;
        while cursor < value.len() {
            let ch = value[cursor..].chars().next().unwrap();
            if !python_whitespace(ch) {
                break;
            }
            cursor += ch.len_utf8();
        }
    }
    result.push_str(&value[cursor..]);
    python_trim(&result).to_string()
}

fn find_italic(value: &str, from: usize) -> Option<(usize, usize)> {
    const EXPECTED: [char; 6] = ['i', 't', 'a', 'l', 'i', 'c'];
    for (start_offset, _) in value[from..].char_indices() {
        let start = from + start_offset;
        let mut chars = value[start..].char_indices();
        let mut end = start;
        let mut matched = true;
        for expected in EXPECTED {
            let Some((offset, actual)) = chars.next() else {
                matched = false;
                break;
            };
            let char_matches = if expected == 'i' {
                matches!(actual, 'i' | 'I' | '\u{0130}' | '\u{0131}')
            } else {
                actual.eq_ignore_ascii_case(&expected)
            };
            if !char_matches {
                matched = false;
                break;
            }
            end = start + offset + actual.len_utf8();
        }
        if matched {
            return Some((start, end));
        }
    }
    None
}

fn python_whitespace(ch: char) -> bool {
    ch.is_whitespace() || ('\u{001c}'..='\u{001f}').contains(&ch)
}

fn python_trim(value: &str) -> &str {
    value.trim_matches(python_whitespace)
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use fontdrasil::{
        coords::{CoordConverter, DesignCoord, UserCoord},
        types::{Axes, Axis},
    };
    use write_fonts::types::Tag;

    use super::{Kind, StatInstance, StatLabels, derive_stat_labels, strip_italic_regex};

    type TestInstance = StatInstance;

    impl StatInstance {
        fn single(name: &str, coords: &[f64]) -> Self {
            Self {
                name: name.to_string(),
                active: true,
                kind: Kind::Single,
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
                kind: Kind::Variable,
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

    fn mapped_axis(tag: &str, name: &str, mappings: &[(f64, f64)], default_idx: usize) -> Axis {
        let mappings: Vec<_> = mappings
            .iter()
            .map(|(user, design)| (UserCoord::new(*user), DesignCoord::new(*design)))
            .collect();
        Axis {
            name: name.to_string(),
            tag: Tag::from_str(tag).unwrap(),
            min: mappings.first().unwrap().0,
            default: mappings[default_idx].0,
            max: mappings.last().unwrap().0,
            hidden: false,
            converter: CoordConverter::new(mappings, default_idx).unwrap(),
            localized_names: Default::default(),
        }
    }

    fn stat(axes: Vec<Axis>, mut instances: Vec<TestInstance>) -> StatLabels {
        instances.push(TestInstance::variable(None));
        derive_stat_labels(&instances, &Axes::new(axes)).unwrap()
    }

    fn assert_labels(stat: &StatLabels, tag: &str, expected: &[(&str, f64, bool, Option<f64>)]) {
        let tag = Tag::from_str(tag).unwrap();
        let axis = stat.axes.iter().find(|axis| axis.tag == tag).unwrap();
        let actual: Vec<_> = axis
            .labels
            .iter()
            .map(|label| {
                (
                    label.name.as_str(),
                    label.user_value.to_f64(),
                    label.elidable,
                    label.linked_user_value.map(|value| value.to_f64()),
                )
            })
            .collect();
        assert_eq!(expected, actual);
    }

    #[test]
    fn style_linked_bold_links_the_default_weight() {
        for (is_bold, linked) in [(true, Some(700.0)), (false, None)] {
            let regular = TestInstance::single("Regular", &[400.0]);
            let mut bold = TestInstance::single("Bold", &[700.0]);
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
                TestInstance::single("Regular", &[400.0, 100.0]),
                TestInstance::single("Bold", &[700.0, 100.0]),
                TestInstance::single("Condensed", &[400.0, 75.0]),
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

    #[test]
    fn style_name_is_used_for_every_differing_axis() {
        let stat = stat(
            vec![
                axis("SPAC", "Spacing", -100.0, 0.0, 0.0),
                axis("MSHQ", "Mashq", 10.0, 10.0, 20.0),
            ],
            vec![
                TestInstance::single("Regular", &[0.0, 10.0]),
                TestInstance::single("Compact High", &[-100.0, 20.0]),
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

    #[test]
    fn multi_word_style_name_is_not_split_across_axes() {
        let stat = stat(
            vec![
                axis("wght", "Weight", 400.0, 400.0, 700.0),
                axis("wdth", "Width", 75.0, 100.0, 100.0),
            ],
            vec![
                TestInstance::single("Regular", &[400.0, 100.0]),
                TestInstance::single("Bold Condensed", &[700.0, 75.0]),
            ],
        );
        assert_labels(
            &stat,
            "wght",
            &[
                ("Regular", 400.0, true, None),
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
    fn instance_driven_values_and_representative_instance() {
        let stat = stat(
            vec![
                axis("wght", "Weight", 400.0, 400.0, 700.0),
                axis("SPAC", "Spacing", -100.0, 0.0, 0.0),
            ],
            vec![
                TestInstance::single("Regular", &[400.0, 0.0]),
                TestInstance::single("Medium", &[500.0, 0.0]),
                TestInstance::single("Bold", &[700.0, 0.0]),
                TestInstance::single("Compact", &[400.0, -100.0]),
                TestInstance::single("Bold Compact", &[700.0, -100.0]),
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

    #[test]
    fn non_regular_default_labels_only_the_first_varying_axis() {
        let stat = stat(
            vec![
                axis("SPAC", "Spacing", -100.0, 0.0, 0.0),
                axis("MSHQ", "Mashq", 10.0, 10.0, 10.0),
            ],
            vec![
                TestInstance::single("Book", &[0.0, 10.0]),
                TestInstance::single("Compact", &[-100.0, 10.0]),
            ],
        );
        assert_labels(
            &stat,
            "SPAC",
            &[("Compact", -100.0, false, None), ("Book", 0.0, false, None)],
        );
        assert_labels(&stat, "MSHQ", &[("Regular", 10.0, true, None)]);
        assert_eq!(stat.elided_fallback_name, "Regular");
    }

    #[test]
    fn default_instance_labels_first_axis_the_instances_vary_on() {
        let stat = stat(
            vec![
                axis("wght", "Weight", 400.0, 400.0, 700.0),
                axis("wdth", "Width", 75.0, 100.0, 100.0),
            ],
            vec![
                TestInstance::single("Book", &[400.0, 100.0]),
                TestInstance::single("Condensed", &[400.0, 75.0]),
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
    fn elided_fallback_is_regular_even_for_a_bold_default() {
        let mut bold = TestInstance::single("Bold", &[400.0]);
        bold.is_bold = true;
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 900.0)],
            vec![bold, TestInstance::single("Black", &[900.0])],
        );
        assert_eq!(stat.elided_fallback_name, "Regular");
    }

    #[test]
    fn style_name_is_used_as_is() {
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![
                TestInstance::single("Regular", &[400.0]),
                TestInstance::single("Zzz", &[600.0]),
                TestInstance::single("Bold", &[700.0]),
            ],
        );
        assert_labels(
            &stat,
            "wght",
            &[
                ("Regular", 400.0, true, None),
                ("Zzz", 600.0, false, None),
                ("Bold", 700.0, false, None),
            ],
        );
    }

    #[test]
    fn no_variable_font_setting_means_no_stat() {
        let instances = vec![
            TestInstance::single("Regular", &[400.0]),
            TestInstance::single("Bold", &[700.0]),
        ];
        assert!(
            derive_stat_labels(
                &instances,
                &Axes::new(vec![axis("wght", "Weight", 400.0, 400.0, 700.0)])
            )
            .is_none()
        );
    }

    #[test]
    fn all_variable_settings_opting_out_means_no_stat_even_when_inactive() {
        let mut inactive = TestInstance::variable(Some(false));
        inactive.active = false;
        let instances = vec![
            TestInstance::single("Regular", &[400.0]),
            inactive,
            TestInstance::variable(Some(false)),
        ];
        assert!(
            derive_stat_labels(
                &instances,
                &Axes::new(vec![axis("wght", "Weight", 400.0, 400.0, 700.0)])
            )
            .is_none()
        );
    }

    #[test]
    fn one_variable_setting_not_opting_out_keeps_stat() {
        let instances = vec![
            TestInstance::single("Regular", &[400.0]),
            TestInstance::single("Bold", &[700.0]),
            TestInstance::variable(Some(false)),
            TestInstance::variable(None),
        ];
        let stat = derive_stat_labels(
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
        let regular = TestInstance::single("Regular", &[400.0]);
        let mut ignored = TestInstance::single("Ignored", &[600.0]);
        ignored.active = false;
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![regular, ignored, TestInstance::single("Bold", &[700.0])],
        );
        assert_labels(
            &stat,
            "wght",
            &[("Regular", 400.0, true, None), ("Bold", 700.0, false, None)],
        );
    }

    #[test]
    fn elidable_stat_axis_value_name_parameter() {
        let regular = TestInstance::single("Regular", &[400.0]);
        let mut bold = TestInstance::single("Bold", &[700.0]);
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

    #[test]
    fn elidable_named_default_still_links_to_bold() {
        let mut book = TestInstance::single("Book", &[400.0]);
        book.elidable.push("wght".into());
        let mut bold = TestInstance::single("Bold", &[700.0]);
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

    #[test]
    fn style_name_as_stat_entry_switches_the_whole_font_to_manual_mode() {
        let regular = TestInstance::single("Regular", &[400.0, 100.0]);
        let mut bold = TestInstance::single("Bold", &[700.0, 100.0]);
        bold.manual.push("wght".into());
        let condensed = TestInstance::single("Condensed", &[400.0, 75.0]);
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

    #[test]
    fn manual_mode_keeps_first_instance_at_a_duplicate_value() {
        let mut first = TestInstance::single("First", &[700.0]);
        first.manual.push("wght".into());
        let mut second = TestInstance::single("Second", &[700.0]);
        second.manual.push("wght".into());
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![first, second],
        );
        assert_labels(&stat, "wght", &[("First", 700.0, false, None)]);
    }

    #[test]
    fn real_italic_axis_suppresses_synthetic_axis_and_links_upright() {
        let stat = stat(
            vec![
                axis("wght", "Weight", 400.0, 400.0, 700.0),
                axis("ital", "Italic", 0.0, 0.0, 1.0),
            ],
            vec![
                TestInstance::single("Regular", &[400.0, 0.0]),
                TestInstance::single("Bold", &[700.0, 0.0]),
                TestInstance::single("Italic", &[400.0, 1.0]),
                TestInstance::single("Bold Italic", &[700.0, 1.0]),
            ],
        );
        assert_eq!(
            stat.axes
                .iter()
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
                TestInstance::single("Regular", &[0.0]),
                TestInstance::single("Slanted", &[-10.0]),
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
    fn italic_family_gets_italic_synthetic_value() {
        let mut italic = TestInstance::single("Italic", &[400.0]);
        italic.is_italic = true;
        let mut bold_italic = TestInstance::single("Bold Italic", &[700.0]);
        bold_italic.is_italic = true;
        bold_italic.is_bold = true;
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![italic, bold_italic],
        );
        assert_labels(&stat, "ital", &[("Italic", 1.0, false, None)]);
    }

    #[test]
    fn italic_family_is_detected_from_default_style_name() {
        let stat = stat(
            vec![axis("wght", "Weight", 400.0, 400.0, 700.0)],
            vec![
                TestInstance::single("Italic", &[400.0]),
                TestInstance::single("Bold Italic", &[700.0]),
            ],
        );
        assert_labels(&stat, "ital", &[("Italic", 1.0, false, None)]);
    }

    #[test]
    fn plain_italic_family_drops_italic_from_axis_value_names() {
        let mut italic = TestInstance::single("Italic", &[400.0]);
        italic.is_italic = true;
        let mut bold = TestInstance::single("Bold Italic", &[700.0]);
        bold.is_italic = true;
        bold.is_bold = true;
        let mut black = TestInstance::single("Black Italic", &[900.0]);
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
    }

    #[test]
    fn named_italic_default_keeps_italic_in_all_names() {
        let mut book = TestInstance::single("Book Italic", &[400.0]);
        book.is_italic = true;
        let mut bold = TestInstance::single("Bold Italic", &[700.0]);
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

    #[test]
    fn style_linked_bold_can_link_from_another_axis_position() {
        let regular = TestInstance::single("Regular", &[400.0, 100.0]);
        let mut bold = TestInstance::single("Bold Condensed", &[700.0, 75.0]);
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
    }

    #[test]
    fn instance_user_locations_are_not_remapped_through_the_font_axis_converter() {
        let stat = stat(
            vec![mapped_axis(
                "wght",
                "Weight",
                &[(400.0, 0.0), (700.0, 100.0)],
                0,
            )],
            vec![
                TestInstance::single("Regular", &[400.0]),
                TestInstance::single("Semibold", &[600.0]),
                TestInstance::single("Bold", &[700.0]),
            ],
        );
        assert_labels(
            &stat,
            "wght",
            &[
                ("Regular", 400.0, true, None),
                ("Semibold", 600.0, false, None),
                ("Bold", 700.0, false, None),
            ],
        );
    }

    #[test]
    fn point_axes_use_raw_instance_design_coordinates() {
        let stat = stat(
            vec![axis("opsz", "Optical Size", 12.0, 12.0, 12.0)],
            vec![
                TestInstance::single("Regular", &[12.0]),
                TestInstance::single("Display", &[14.0]),
            ],
        );
        assert_labels(
            &stat,
            "opsz",
            &[
                ("Regular", 12.0, true, None),
                ("Display", 14.0, false, None),
            ],
        );
    }

    #[test]
    fn italic_word_removal_has_python_regex_semantics() {
        assert_eq!(strip_italic_regex("  Bold   ITALIC  "), "Bold");
        assert_eq!(strip_italic_regex("fooitalicbar"), "foo bar");
        assert_eq!(strip_italic_regex("Italic Italic"), "");
        assert_eq!(strip_italic_regex("\u{0130}talic"), "");
        assert_eq!(strip_italic_regex("\u{0131}talic"), "");
        assert_eq!(strip_italic_regex("I\u{0307}talic"), "I\u{0307}talic");
        assert_eq!(strip_italic_regex("\u{001c}Italic\u{001f}"), "");
    }
}
