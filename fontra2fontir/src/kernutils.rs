//! Port of Fontra's kernutils:
//! <https://github.com/fontra/fontra/blob/2a19b8bd1/src/fontra/core/kernutils.py>

use std::{
    collections::{BTreeSet, HashMap, HashSet},
    path::{Path, PathBuf},
    sync::Arc,
};

use fea_rs::{
    GlyphMap as FeaGlyphMap, Opts,
    compile::{NopFeatureProvider, VariationInfo},
    parse::{SourceLoadError, SourceResolver},
};
use fontdrasil::{
    coords::{CoordConverter, NormalizedLocation, UserCoord},
    types::{Axis, GlyphName},
};
use icu_properties::{CodePointMapData, props::BidiClass};
use log::error;
use smol_str::SmolStr;
use write_fonts::{
    dump_table,
    read::{FontData, FontRead, collections::IntSet, tables::gsub::Gsub},
    tables::variations::VariationRegion,
    types::{GlyphId16, Tag},
};

use crate::fontra::{FontAxis, GlyphMap, Kerning, KerningValues};

type NestedKerningValues = KerningValues;
type FlatKerningValues = HashMap<(SmolStr, SmolStr), Vec<Option<f64>>>;
type KerningGroups = HashMap<SmolStr, Vec<SmolStr>>;

/// The Python dict union, the right side wins.
fn union<K: std::hash::Hash + Eq, V>(mut a: HashMap<K, V>, b: HashMap<K, V>) -> HashMap<K, V> {
    a.extend(b);
    a
}

pub(crate) fn split_kerning_by_direction(
    kerning: &Kerning,
    ltr_glyphs: &HashSet<GlyphName>,
    rtl_glyphs: &HashSet<GlyphName>,
) -> (Kerning, Kerning) {
    let (ltr_groups_side1, neutral_groups_side1, rtl_groups_side1) =
        classify_groups_by_direction(&kerning.groups_side1, ltr_glyphs, rtl_glyphs);
    let (ltr_groups_side2, neutral_groups_side2, rtl_groups_side2) =
        classify_groups_by_direction(&kerning.groups_side2, ltr_glyphs, rtl_glyphs);

    let unnested_values = unnest_kerning_values(&kerning.values);
    let mut ltr_values = FlatKerningValues::new();
    let mut rtl_values = FlatKerningValues::new();

    for ((left, right), values) in unnested_values {
        let left_group = left.strip_prefix('@');
        let right_group = right.strip_prefix('@');

        let left_is_rtl = match left_group {
            Some(left_group) => rtl_groups_side1.contains_key(left_group),
            None => rtl_glyphs.contains(&GlyphName::new(&left)),
        };

        let right_is_rtl = match right_group {
            Some(right_group) => rtl_groups_side2.contains_key(right_group),
            None => rtl_glyphs.contains(&GlyphName::new(&right)),
        };

        if left_is_rtl || right_is_rtl {
            rtl_values.insert((left, right), values);
        } else {
            ltr_values.insert((left, right), values);
        }
    }

    let (ltr_neutral_groups_side1, ltr_neutral_groups_side2) =
        filter_groups_by_value_usage(&neutral_groups_side1, &neutral_groups_side2, &ltr_values);

    let ltr_kerning = Kerning {
        groups_side1: union(ltr_groups_side1, ltr_neutral_groups_side1),
        groups_side2: union(ltr_groups_side2, ltr_neutral_groups_side2),
        source_identifiers: kerning.source_identifiers.clone(),
        values: nest_kerning_values(ltr_values),
    };

    let (rtl_neutral_groups_side1, rtl_neutral_groups_side2) =
        filter_groups_by_value_usage(&neutral_groups_side1, &neutral_groups_side2, &rtl_values);

    let rtl_kerning = Kerning {
        groups_side1: union(rtl_groups_side1, rtl_neutral_groups_side1),
        groups_side2: union(rtl_groups_side2, rtl_neutral_groups_side2),
        source_identifiers: kerning.source_identifiers.clone(),
        values: nest_kerning_values(rtl_values),
    };

    (ltr_kerning, rtl_kerning)
}

pub(crate) fn flip_kerning_direction(kerning: &Kerning) -> Kerning {
    let unnested_values = unnest_kerning_values(&kerning.values);
    let flipped_values = unnested_values
        .into_iter()
        .map(|((left, right), values)| ((right, left), values))
        .collect();

    Kerning {
        groups_side1: kerning.groups_side2.clone(),
        groups_side2: kerning.groups_side1.clone(),
        source_identifiers: kerning.source_identifiers.clone(),
        values: nest_kerning_values(flipped_values),
    }
}

pub(crate) fn merge_kerning(kerning_a: &Kerning, kerning_b: &Kerning) -> Kerning {
    assert_eq!(kerning_a.source_identifiers, kerning_b.source_identifiers);
    let kerning_b = disambiguate_kerning_group_names(kerning_b, kerning_a, true);
    Kerning {
        groups_side1: union(kerning_a.groups_side1.clone(), kerning_b.groups_side1),
        groups_side2: union(kerning_a.groups_side2.clone(), kerning_b.groups_side2),
        source_identifiers: kerning_a.source_identifiers.clone(),
        values: nest_kerning_values(union(
            unnest_kerning_values(&kerning_a.values),
            unnest_kerning_values(&kerning_b.values),
        )),
    }
}

/// The left-to-right glyphs and the right-to-left glyphs.
///
/// Includes in `feature_text` resolve against `include_dir`.
pub(crate) fn classify_glyphs_by_direction(
    glyph_map: &GlyphMap,
    feature_text: &str,
    fontra_axes: &[FontAxis],
    include_dir: &Path,
) -> (HashSet<GlyphName>, HashSet<GlyphName>) {
    let glyph_order: Vec<GlyphName> = glyph_map
        .keys()
        .cloned()
        .chain([GlyphName::new(".notdef")])
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    let glyph_ids = FeaGlyphMap::new(glyph_order.iter().cloned()).expect("the names are unique");
    let gid = |name: &GlyphName| {
        glyph_ids
            .get(name)
            .expect("every name is in the glyph order")
    };
    let cmap: HashMap<u32, GlyphId16> = glyph_map
        .iter()
        .flat_map(|(glyph_name, code_points)| {
            code_points
                .iter()
                .map(move |code_point| (*code_point, gid(glyph_name)))
        })
        .collect();

    let glyph_names: HashSet<&str> = glyph_map.keys().map(GlyphName::as_str).collect();
    let name_based_substitutions: HashMap<GlyphId16, IntSet<GlyphId16>> =
        make_name_based_substitutions(&glyph_names)
            .into_iter()
            .map(|(name, substitutes)| {
                (
                    gid(&GlyphName::new(name)),
                    substitutes
                        .into_iter()
                        .map(|substitute| gid(&GlyphName::new(substitute)))
                        .collect(),
                )
            })
            .collect();

    let classify = |gsub: Option<&Gsub>| {
        fontir::classify::classify(
            &cmap,
            |code_point, buf| buf.extend(unicode_bidi_type(code_point)),
            gsub,
            Some(&name_based_substitutions),
        )
    };
    let mut classifications = classify(None).expect("no GSUB to read");
    if classifications
        .get(&BidiClass::RightToLeft)
        .is_some_and(|glyphs| !glyphs.is_empty())
        && let Some(bytes) = compile_gsub(feature_text, &glyph_ids, fontra_axes, include_dir)
    {
        match Gsub::read(FontData::new(&bytes)).and_then(|gsub| classify(Some(&gsub))) {
            Ok(with_gsub) => classifications = with_gsub,
            Err(e) => error!("Can't close the glyphs over GSUB: {e}"),
        }
    }

    let names = |bidi_class: BidiClass| -> HashSet<GlyphName> {
        classifications
            .get(&bidi_class)
            .into_iter()
            .flat_map(|glyphs| glyphs.iter())
            .map(|gid| glyph_order[gid.to_u32() as usize].clone())
            .collect()
    };
    (names(BidiClass::LeftToRight), names(BidiClass::RightToLeft))
}

/// <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b340c/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L49>
fn unicode_bidi_type(c: u32) -> Option<BidiClass> {
    match CodePointMapData::<BidiClass>::new().get32(c) {
        BidiClass::RightToLeft | BidiClass::ArabicLetter => Some(BidiClass::RightToLeft),
        BidiClass::LeftToRight | BidiClass::ArabicNumber | BidiClass::EuropeanNumber => {
            Some(BidiClass::LeftToRight)
        }
        _ => None,
    }
}

fn classify_groups_by_direction(
    groups: &KerningGroups,
    ltr_glyphs: &HashSet<GlyphName>,
    rtl_glyphs: &HashSet<GlyphName>,
) -> (KerningGroups, KerningGroups, KerningGroups) {
    let mut ltr_groups = KerningGroups::new();
    let mut neutral_groups = KerningGroups::new();
    let mut rtl_groups = KerningGroups::new();

    for (group_name, glyph_names) in groups {
        let is_ltr = glyph_names
            .iter()
            .any(|glyph_name| ltr_glyphs.contains(&GlyphName::new(glyph_name)));
        let is_rtl = glyph_names
            .iter()
            .any(|glyph_name| rtl_glyphs.contains(&GlyphName::new(glyph_name)));
        if is_ltr && !is_rtl {
            ltr_groups.insert(group_name.clone(), glyph_names.clone());
        } else if is_rtl && !is_ltr {
            rtl_groups.insert(group_name.clone(), glyph_names.clone());
        } else {
            neutral_groups.insert(group_name.clone(), glyph_names.clone());
        }
    }

    (ltr_groups, neutral_groups, rtl_groups)
}

fn disambiguate_kerning_group_names(
    kern_table_a: &Kerning,
    kern_table_b: &Kerning,
    merge_same_content: bool,
) -> Kerning {
    let (group_side1_name_map, pair_side1_name_map) = get_conflict_resolution_mappings(
        &kern_table_a.groups_side1,
        &kern_table_b.groups_side1,
        merge_same_content,
    );

    let (group_side2_name_map, pair_side2_name_map) = get_conflict_resolution_mappings(
        &kern_table_a.groups_side2,
        &kern_table_b.groups_side2,
        merge_same_content,
    );

    if group_side1_name_map.is_empty() && group_side2_name_map.is_empty() {
        return kern_table_a.clone();
    }

    let groups_side1 = rename_groups(&kern_table_a.groups_side1, &group_side1_name_map);
    let groups_side2 = rename_groups(&kern_table_a.groups_side2, &group_side2_name_map);

    let rename = |name_map: &HashMap<SmolStr, SmolStr>, name: &SmolStr| -> SmolStr {
        name_map.get(name).cloned().unwrap_or_else(|| name.clone())
    };
    let values = kern_table_a
        .values
        .iter()
        .map(|(left, right_dict)| {
            (
                rename(&pair_side1_name_map, left),
                right_dict
                    .iter()
                    .map(|(right, values)| (rename(&pair_side2_name_map, right), values.clone()))
                    .collect(),
            )
        })
        .collect();

    Kerning {
        groups_side1,
        groups_side2,
        source_identifiers: kern_table_a.source_identifiers.clone(),
        values,
    }
}

/// The glyph names that each glyph name substitutes by its name.
pub(crate) fn make_name_based_substitutions<'a>(
    glyph_names: &HashSet<&'a str>,
) -> HashMap<&'a str, HashSet<&'a str>> {
    let mut substitutions: HashMap<&str, HashSet<&str>> = HashMap::new();

    for glyph_name in glyph_names.iter().copied() {
        let mut base_glyph_name = glyph_name
            .split_once('.')
            .map(|(base, _)| base)
            .unwrap_or(glyph_name);
        let mut lang_ext = String::new();
        if let Some((base, ext)) = base_glyph_name.rsplit_once('-') {
            base_glyph_name = base;
            lang_ext = format!("-{ext}");
        }
        for part_glyph_name in base_glyph_name.split('_') {
            let part_glyph_name = format!("{part_glyph_name}{lang_ext}");
            if part_glyph_name == glyph_name {
                continue;
            }
            if let Some(part_glyph_name) = glyph_names.get(part_glyph_name.as_str()) {
                substitutions
                    .entry(part_glyph_name)
                    .or_default()
                    .insert(glyph_name);
            }
        }
    }

    substitutions
}

fn get_conflict_resolution_mappings(
    groups_a: &KerningGroups,
    groups_b: &KerningGroups,
    merge_same_content: bool,
) -> (HashMap<SmolStr, SmolStr>, HashMap<SmolStr, SmolStr>) {
    let groups_names_a: HashSet<&SmolStr> = groups_a.keys().collect();
    let groups_names_b: HashSet<&SmolStr> = groups_b.keys().collect();

    let groups_b_by_content: HashMap<&Vec<SmolStr>, &SmolStr> = if merge_same_content {
        let mut sorted_b: Vec<_> = groups_b.iter().collect();
        sorted_b.sort();
        sorted_b.into_iter().map(|(k, v)| (v, k)).collect()
    } else {
        HashMap::new()
    };

    if groups_names_a.is_disjoint(&groups_names_b) {
        return (HashMap::new(), HashMap::new());
    }

    let mut used_names: HashSet<SmolStr> = groups_names_a
        .union(&groups_names_b)
        .map(|name| (*name).clone())
        .collect();

    let mut group_name_map = HashMap::new();
    let mut sorted_a: Vec<_> = groups_a.iter().collect();
    sorted_a.sort();
    for (name, glyph_names) in sorted_a {
        if merge_same_content && let Some(name_b) = groups_b_by_content.get(glyph_names) {
            if name != *name_b {
                group_name_map.insert(name.clone(), (*name_b).clone());
            }
            continue;
        }

        if !groups_names_b.contains(name) {
            continue;
        }

        let mut count = 1;
        let new_name = loop {
            let new_name = SmolStr::from(format!("{name}.{count}"));
            if !used_names.contains(&new_name) {
                break new_name;
            }
            count += 1;
        };
        used_names.insert(new_name.clone());
        group_name_map.insert(name.clone(), new_name);
    }

    let pair_name_map = group_name_map
        .iter()
        .map(|(k, v)| {
            (
                SmolStr::from(format!("@{k}")),
                SmolStr::from(format!("@{v}")),
            )
        })
        .collect();

    (group_name_map, pair_name_map)
}

fn rename_groups(groups: &KerningGroups, rename_map: &HashMap<SmolStr, SmolStr>) -> KerningGroups {
    groups
        .iter()
        .map(|(name, group)| {
            (
                rename_map
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| name.clone()),
                group.clone(),
            )
        })
        .collect()
}

/// The axes of the GSUB compile, like the fvar of Fontra's `compileGSUB`.
struct GsubVariationInfo {
    axes: Vec<Axis>,
}

impl VariationInfo for GsubVariationInfo {
    type Error = std::convert::Infallible;

    fn axis_count(&self) -> u16 {
        self.axes.len() as u16
    }

    fn axis(&self, axis_tag: Tag) -> Option<(usize, &Axis)> {
        self.axes
            .iter()
            .enumerate()
            .find(|(_, axis)| axis.tag == axis_tag)
    }

    // GPOS is not compiled.
    fn resolve_variable_metric(
        &self,
        _: &HashMap<NormalizedLocation, i16>,
    ) -> Result<(i16, Vec<(VariationRegion, i16)>), Self::Error> {
        Ok((0, Vec::new()))
    }

    fn resolve_glyphs_number_value(
        &self,
        _: &str,
    ) -> Result<HashMap<NormalizedLocation, f64>, Self::Error> {
        Ok(HashMap::new())
    }
}

/// The feature text as the root source, with the includes under `include_dir`.
struct FeatureTextResolver {
    text: Arc<str>,
    include_dir: PathBuf,
}

impl SourceResolver for FeatureTextResolver {
    fn get_contents(&self, rel_path: &Path) -> Result<Arc<str>, SourceLoadError> {
        if rel_path == Path::new("") {
            return Ok(self.text.clone());
        }
        std::fs::read_to_string(self.include_dir.join(rel_path))
            .map(Arc::from)
            .map_err(|e| SourceLoadError::new(rel_path.to_path_buf(), e))
    }
}

/// The compiled GSUB table, `None` when the features do not compile.
fn compile_gsub(
    feature_text: &str,
    glyph_order: &FeaGlyphMap,
    fontra_axes: &[FontAxis],
    include_dir: &Path,
) -> Option<Vec<u8>> {
    let axes = fontra_axes
        .iter()
        .map(|axis| {
            let min = UserCoord::new(axis.min_value);
            let default = UserCoord::new(axis.default_value);
            let max = UserCoord::new(axis.max_value);
            Axis {
                name: axis.name.clone(),
                tag: axis.tag,
                min,
                default,
                max,
                hidden: false,
                converter: CoordConverter::unmapped(min, default, max),
                localized_names: Default::default(),
            }
        })
        .collect();
    let var_info = GsubVariationInfo { axes };

    let resolver = FeatureTextResolver {
        text: feature_text.into(),
        include_dir: include_dir.to_path_buf(),
    };
    let (tree, diagnostics) =
        match fea_rs::parse::parse_root(PathBuf::new(), Some(glyph_order), Box::new(resolver)) {
            Ok(parsed) => parsed,
            Err(e) => {
                error!("Can't parse features: {e}");
                return None;
            }
        };
    if diagnostics.has_errors() {
        error!("Can't parse features: {}", diagnostics.display());
        return None;
    }
    let compilation = match fea_rs::compile::compile::<_, NopFeatureProvider>(
        &tree,
        glyph_order,
        Some(&var_info),
        None,
        Opts::new().compile_gpos(false),
    ) {
        Ok((compilation, _)) => compilation,
        Err(errors) => {
            error!("Can't parse features: {}", errors.display());
            return None;
        }
    };
    compilation
        .gsub
        .as_ref()
        .and_then(|gsub| dump_table(gsub).ok())
}

fn unnest_kerning_values(values: &NestedKerningValues) -> FlatKerningValues {
    values
        .iter()
        .flat_map(|(left, right_dict)| {
            right_dict
                .iter()
                .map(|(right, values)| ((left.clone(), right.clone()), values.clone()))
        })
        .collect()
}

fn nest_kerning_values(unnested_values: FlatKerningValues) -> NestedKerningValues {
    let mut nested_values = NestedKerningValues::new();

    for ((left, right), values) in unnested_values {
        nested_values.entry(left).or_default().insert(right, values);
    }

    nested_values
}

fn filter_groups_by_value_usage(
    groups_side1: &KerningGroups,
    groups_side2: &KerningGroups,
    unnested_values: &FlatKerningValues,
) -> (KerningGroups, KerningGroups) {
    let mut left_used_group_names = HashSet::new();
    let mut right_used_group_names = HashSet::new();

    for (left, right) in unnested_values.keys() {
        if let Some(group) = left.strip_prefix('@') {
            left_used_group_names.insert(group);
        }
        if let Some(group) = right.strip_prefix('@') {
            right_used_group_names.insert(group);
        }
    }

    let filtered_groups_side1 = groups_side1
        .iter()
        .filter(|(k, _)| left_used_group_names.contains(k.as_str()))
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    let filtered_groups_side2 = groups_side2
        .iter()
        .filter(|(k, _)| right_used_group_names.contains(k.as_str()))
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    (filtered_groups_side1, filtered_groups_side2)
}

/// Port of Fontra's kernutils tests:
/// <https://github.com/fontra/fontra/blob/2a19b8bd1/test-py/test_kernutils.py>
#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};
    use std::path::Path;

    use fontdrasil::types::GlyphName;
    use smol_str::SmolStr;
    use write_fonts::types::Tag;

    use crate::fontra::{FontAxis, GlyphMap, Kerning};

    use super::{
        classify_glyphs_by_direction, flip_kerning_direction, make_name_based_substitutions,
        merge_kerning, split_kerning_by_direction,
    };

    fn glyph_map() -> GlyphMap {
        [
            ("A", vec!['A' as u32]),
            ("A.alt", vec![]),
            ("C", vec!['C' as u32]),
            ("D", vec!['D' as u32]),
            ("F", vec!['F' as u32]),
            ("O", vec!['O' as u32]),
            ("O.alt", vec![]),
            ("V", vec!['V' as u32]),
            ("alef-ar", vec![0x0627]),
            ("alef-ar.init", vec![]),
            ("beh-ar", vec![0x0628]),
            ("beh-ar.init", vec![]),
            ("zero", vec!['0' as u32]),
            ("period", vec!['.' as u32]),
            ("comma", vec![',' as u32]),
        ]
        .into_iter()
        .map(|(name, code_points)| (GlyphName::new(name), code_points))
        .collect()
    }

    const FEATURE_TEXT: &str = "
feature salt {
  sub A by A.alt;
  sub O by O.alt;
} salt;

feature init {
  sub alef-ar by alef-ar.init;
  sub beh-ar by beh-ar.init;
} init;
";

    fn glyph_names(names: &[&str]) -> HashSet<GlyphName> {
        names.iter().map(GlyphName::new).collect()
    }

    fn expected_ltr_glyphs() -> HashSet<GlyphName> {
        glyph_names(&["A", "A.alt", "C", "D", "F", "O", "O.alt", "V", "zero"])
    }

    fn expected_rtl_glyphs() -> HashSet<GlyphName> {
        glyph_names(&["alef-ar", "alef-ar.init", "beh-ar", "beh-ar.init"])
    }

    #[test]
    fn classify_glyphs_by_direction_test() {
        let (ltr_glyphs, rtl_glyphs) = classify_glyphs_by_direction(
            &glyph_map(),
            FEATURE_TEXT,
            &[FontAxis {
                name: "Weight".to_string(),
                tag: Tag::new(b"wght"),
                min_value: 100.0,
                default_value: 400.0,
                max_value: 900.0,
                label: "Weight".to_string(),
                mapping: Vec::new(),
                value_labels: Vec::new(),
                hidden: false,
            }],
            Path::new(""),
        );
        assert_eq!(ltr_glyphs, expected_ltr_glyphs());
        assert_eq!(rtl_glyphs, expected_rtl_glyphs());
        let neutral_glyphs: HashSet<GlyphName> = glyph_map()
            .keys()
            .filter(|name| !ltr_glyphs.contains(*name) && !rtl_glyphs.contains(*name))
            .cloned()
            .collect();
        assert_eq!(neutral_glyphs, glyph_names(&["comma", "period"]));
        assert!(ltr_glyphs.is_disjoint(&rtl_glyphs));
    }

    fn groups(groups: &[(&str, &[&str])]) -> HashMap<SmolStr, Vec<SmolStr>> {
        groups
            .iter()
            .map(|(name, members)| {
                (
                    SmolStr::from(*name),
                    members
                        .iter()
                        .map(|member| SmolStr::from(*member))
                        .collect(),
                )
            })
            .collect()
    }

    fn values(
        values: &[(&str, &[(&str, f64)])],
    ) -> HashMap<SmolStr, HashMap<SmolStr, Vec<Option<f64>>>> {
        values
            .iter()
            .map(|(left, rights)| {
                (
                    SmolStr::from(*left),
                    rights
                        .iter()
                        .map(|(right, value)| (SmolStr::from(*right), vec![Some(*value)]))
                        .collect(),
                )
            })
            .collect()
    }

    fn kerning_data() -> Kerning {
        Kerning {
            groups_side1: groups(&[
                ("A", &["A", "A.alt"]),
                ("O", &["O", "O.alt", "D"]),
                ("alef", &["alef-ar", "alef-ar.init"]),
            ]),
            groups_side2: groups(&[
                ("A", &["A", "A.alt"]),
                ("O", &["O", "O.alt", "C"]),
                ("beh", &["beh-ar", "beh-ar.init"]),
            ]),
            source_identifiers: vec!["-".to_string()],
            values: values(&[
                ("@A", &[("@O", -15.0), ("V", -20.0)]),
                ("F", &[("@O", -10.0), ("period", -13.0)]),
                ("@alef", &[("@beh", -11.0), ("period", -9.0)]),
            ]),
        }
    }

    fn expected_flipped_kerning_data() -> Kerning {
        Kerning {
            groups_side1: groups(&[
                ("A", &["A", "A.alt"]),
                ("O", &["O", "O.alt", "C"]),
                ("beh", &["beh-ar", "beh-ar.init"]),
            ]),
            groups_side2: groups(&[
                ("A", &["A", "A.alt"]),
                ("O", &["O", "O.alt", "D"]),
                ("alef", &["alef-ar", "alef-ar.init"]),
            ]),
            source_identifiers: vec!["-".to_string()],
            values: values(&[
                ("@O", &[("@A", -15.0), ("F", -10.0)]),
                ("V", &[("@A", -20.0)]),
                ("period", &[("F", -13.0), ("@alef", -9.0)]),
                ("@beh", &[("@alef", -11.0)]),
            ]),
        }
    }

    #[test]
    fn flip_kerning_direction_test() {
        let flipped_kerning = flip_kerning_direction(&kerning_data());
        assert_eq!(flipped_kerning, expected_flipped_kerning_data());
    }

    fn expected_ltr_kerning() -> Kerning {
        Kerning {
            groups_side1: groups(&[("A", &["A", "A.alt"]), ("O", &["O", "O.alt", "D"])]),
            groups_side2: groups(&[("A", &["A", "A.alt"]), ("O", &["O", "O.alt", "C"])]),
            source_identifiers: vec!["-".to_string()],
            values: values(&[
                ("@A", &[("@O", -15.0), ("V", -20.0)]),
                ("F", &[("@O", -10.0), ("period", -13.0)]),
            ]),
        }
    }

    fn expected_rtl_kerning() -> Kerning {
        Kerning {
            groups_side1: groups(&[("alef", &["alef-ar", "alef-ar.init"])]),
            groups_side2: groups(&[("beh", &["beh-ar", "beh-ar.init"])]),
            source_identifiers: vec!["-".to_string()],
            values: values(&[("@alef", &[("@beh", -11.0), ("period", -9.0)])]),
        }
    }

    #[test]
    fn split_kerning_by_direction_test() {
        let (ltr_glyphs, rtl_glyphs) =
            classify_glyphs_by_direction(&glyph_map(), FEATURE_TEXT, &[], Path::new(""));
        let (ltr_kerning, rtl_kerning) =
            split_kerning_by_direction(&kerning_data(), &ltr_glyphs, &rtl_glyphs);

        assert_eq!(ltr_kerning, expected_ltr_kerning());
        assert_eq!(rtl_kerning, expected_rtl_kerning());
    }

    #[test]
    fn merge_kerning_test() {
        let merged_kerning = merge_kerning(&expected_ltr_kerning(), &expected_rtl_kerning());
        assert_eq!(merged_kerning, kerning_data());
    }

    fn kerning_data_a() -> Kerning {
        Kerning {
            groups_side1: groups(&[("A", &["A", "A.alt"]), ("O", &["O", "O.alt", "D"])]),
            groups_side2: groups(&[("A", &["A", "A.alt"]), ("O", &["O", "O.alt", "C"])]),
            source_identifiers: vec!["-".to_string()],
            values: values(&[
                ("@A", &[("@O", -15.0), ("V", -20.0)]),
                ("F", &[("@O", -10.0), ("period", -13.0)]),
            ]),
        }
    }

    fn kerning_data_b() -> Kerning {
        Kerning {
            groups_side1: groups(&[("A", &["A", "A.alt"]), ("O", &["O", "O.alt", "D"])]),
            groups_side2: groups(&[("A", &["A", "A.alt"]), ("O", &["O", "O.alt", "C", "G"])]),
            source_identifiers: vec!["-".to_string()],
            values: values(&[
                ("@A", &[("@O", -15.0), ("V", -20.0)]),
                ("F", &[("@O", -10.0), ("period", -13.0)]),
            ]),
        }
    }

    fn expected_merged_kerning_data() -> Kerning {
        Kerning {
            groups_side1: groups(&[("A", &["A", "A.alt"]), ("O", &["O", "O.alt", "D"])]),
            groups_side2: groups(&[
                ("A", &["A", "A.alt"]),
                ("O", &["O", "O.alt", "C"]),
                ("O.1", &["O", "O.alt", "C", "G"]),
            ]),
            source_identifiers: vec!["-".to_string()],
            values: values(&[
                ("@A", &[("@O", -15.0), ("V", -20.0), ("@O.1", -15.0)]),
                ("F", &[("@O", -10.0), ("period", -13.0), ("@O.1", -10.0)]),
            ]),
        }
    }

    #[test]
    fn merge_kerning_with_group_conflict_test() {
        let merged_kerning = merge_kerning(&kerning_data_a(), &kerning_data_b());
        assert_eq!(merged_kerning, expected_merged_kerning_data());
    }

    #[test]
    fn make_name_based_substitutions_test() {
        let glyph_names: HashSet<&str> = [
            "A",
            "A.alt",
            "A.alt2",
            "B",
            "B.alt",
            "A_B",
            "A_B.alt",
            "C.alt",
            "alef-ar",
            "beh-ar",
            "alef_beh-ar",
            "alef_beh-ar.alt",
        ]
        .into_iter()
        .collect();

        let expected_substitutions: HashMap<&str, HashSet<&str>> = [
            ("A", ["A.alt", "A_B", "A_B.alt", "A.alt2"].as_slice()),
            ("B", ["B.alt", "A_B", "A_B.alt"].as_slice()),
            ("alef-ar", ["alef_beh-ar", "alef_beh-ar.alt"].as_slice()),
            ("beh-ar", ["alef_beh-ar", "alef_beh-ar.alt"].as_slice()),
        ]
        .into_iter()
        .map(|(base, substitutes)| (base, substitutes.iter().copied().collect()))
        .collect();

        let substitutions = make_name_based_substitutions(&glyph_names);
        assert_eq!(substitutions, expected_substitutions);
    }
}
