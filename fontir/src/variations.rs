//! Helps manipulate variation data.
use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap, HashSet},
    fmt::{Debug, Display},
    ops::{Add, Mul, Sub},
};

use fontdrasil::{
    coords::{NormalizedCoord, NormalizedLocation},
    types::{Axes, Axis},
};
use log::{log_enabled, trace};
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use write_fonts::{
    tables::{gvar, variations::RegionAxisCoordinates},
    types::{F2Dot14, Tag},
};

use crate::error::VariationModelError;

/// Trait for rounding half-way values to the nearest even number.
///
/// For example, 2.5 rounds to 2.0, 3.5 rounds to 4.0, and -2.5 rounds to -2.0.
/// This is the same rounding mode as [`f64::round_ties_even`], which was
/// stabilized in Rust 1.77.0. The trait provides a common way to apply this
/// to different types besides `f64` e.g. `kurbo::Vec2`.
/// We use it below in [`VariationModel`] for rounding variation deltas.
///
/// It also matches the Python's buit-in `round` function which is used by FontTools'
/// VariationModel for rounding deltas.
///
/// In general, this is the preferred approach when doing a lot of additions or
/// subtractions of rounded numbers, for it avoids bias both toward positive/negative
/// values and toward/away from zero.
///
/// For more info, see:
/// - discussion in a related FontTools PR <https://github.com/fonttools/fonttools/pull/2214>
/// - 'Rounding half to even' section in <https://en.wikipedia.org/wiki/Rounding>.
pub trait RoundTiesEven {
    fn round_ties_even(self) -> Self;
}

impl RoundTiesEven for f64 {
    #[inline]
    fn round_ties_even(self) -> f64 {
        self.round_ties_even()
    }
}

impl RoundTiesEven for kurbo::Vec2 {
    #[inline]
    fn round_ties_even(self) -> kurbo::Vec2 {
        kurbo::Vec2::new(self.x.round_ties_even(), self.y.round_ties_even())
    }
}

const ZERO: OrderedFloat<f64> = OrderedFloat(0.0);
const ONE: OrderedFloat<f64> = OrderedFloat(1.0);

/// A model of how variation space is subdivided into regions to create deltas.
///
/// Given a set of master locations, figures out a set of regions and the weights each
/// region assigns to each master. This enables us to compute deltas for variation stores.
///
/// See `class VariationModel` in <https://github.com/fonttools/fonttools/blob/main/Lib/fontTools/varLib/models.py>
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct VariationModel {
    pub default: NormalizedLocation,

    /// Non-point axes
    axes: Axes,

    // TODO: why isn't this a Map<Loc, Region>
    // All Vec's have same length and items at the same index refer to the same master
    // Which sounds a *lot* like we should have a Vec or Map of Some Struct.
    locations: Vec<NormalizedLocation>,
    influence: Vec<VariationRegion>,

    // [n] gives a vec of (master index, scale for deltas from that master)
    delta_weights: Vec<Vec<(usize, OrderedFloat<f64>)>>,
}

impl VariationModel {
    /// Create a model of variation space subdivision suitable for delta construction.
    ///
    /// Locations should be points in variation space where we wish to define something, such as a
    /// glyph instance.
    ///
    /// Axis order should reflect the importance of the axis.
    pub fn new(
        locations: HashSet<NormalizedLocation>,
        axes: Axes,
    ) -> Result<Self, VariationModelError> {
        for axis in axes.iter() {
            if axis.is_point() {
                return Err(VariationModelError::PointAxis(axis.tag));
            }
        }

        let default = axes
            .iter()
            .map(|axis| (axis.tag, NormalizedCoord::new(ZERO)))
            .collect();

        let mut expanded_locations = HashSet::new();
        for mut location in locations.into_iter() {
            // Make sure locations are defined on all axes we know of, and only axes we know of
            location.retain(|tag, _| axes.contains(tag));

            // Fill in missing axis positions with 0
            for axis in axes.iter() {
                if !location.contains(axis.tag) {
                    location.insert(axis.tag, NormalizedCoord::new(0.0));
                }
            }

            expanded_locations.insert(location);
        }

        // sort locations such that [i..N] cannot influence [0..i-1]
        let axis_order = axes.iter().map(|a| a.tag).collect();
        let mut locations: Vec<_> = expanded_locations.into_iter().collect();
        let sorting_hat = LocationSortingHat::new(&locations, &axis_order);
        locations.sort_by_cached_key(|loc| sorting_hat.key_for(loc));

        let regions = regions_for(&axis_order, &locations);
        let influence = master_influence(&axis_order, &regions);
        let delta_weights = delta_weights(&locations, &influence);

        if log::log_enabled!(log::Level::Trace) {
            trace!("Model");
            for (loc, region) in locations.iter().zip(regions.iter().as_ref()) {
                trace!("  {loc:?} {region:?}");
            }
        }

        Ok(VariationModel {
            default,
            axes,
            locations,
            influence,
            delta_weights,
        })
    }

    pub fn empty() -> Self {
        VariationModel {
            default: NormalizedLocation::new(),
            axes: Default::default(),
            locations: Vec::new(),
            influence: Vec::new(),
            delta_weights: Vec::new(),
        }
    }

    pub fn num_locations(&self) -> usize {
        self.locations.len()
    }

    /// Unique master locations for model
    pub fn locations(&self) -> impl Iterator<Item = &NormalizedLocation> {
        self.locations.iter()
    }

    /// Iterate over all axes in the model
    pub fn axes(&self) -> impl Iterator<Item = &Axis> {
        self.axes.iter()
    }

    pub fn supports(&self, location: &NormalizedLocation) -> bool {
        // current assumption is the #locations is relatively small
        self.locations.contains(location)
    }

    /// Convert absolute positions at master locations to offsets.
    ///
    /// <ul>
    ///     <li>All keys in points must be known to the variation model.</li>
    ///     <li>All point vectors must have the same length.</li>
    ///     <li>A point vector for the default location is required</li>
    /// </ul>
    ///
    /// Note that it is NOT required to provide a point sequence for every location known
    /// to the variation model.
    ///
    /// P is the point type, meant to be absolute position in 1 or 2 dimensional space.
    /// V is the vector type.
    ///
    /// For 2d [kurbo::Point] and [kurbo::Vec2] would be typical choices. For 1d a floating
    /// point primitive should suffice.
    ///
    /// Returns a delta, as the vector type, for every input point. Intended use is to support
    /// construction of a variation store.
    ///
    /// Rust version of <https://github.com/fonttools/fonttools/blob/3b9a73ff8379ab49d3ce35aaaaf04b3a7d9d1655/Lib/fontTools/varLib/models.py#L449-L461>
    pub fn deltas<P, V>(
        &self,
        point_seqs: &HashMap<NormalizedLocation, Vec<P>>,
    ) -> Result<Vec<(VariationRegion, Vec<V>)>, DeltaError>
    where
        P: Copy + Default + Sub<P, Output = V>,
        V: Copy + Mul<f64, Output = V> + Sub<V, Output = V> + RoundTiesEven,
    {
        if point_seqs.is_empty() {
            return Ok(Vec::new());
        }

        let point_seqs: HashMap<_, _> = point_seqs
            .iter()
            .map(|(loc, seq)| {
                let mut loc = loc.clone();
                loc.retain(|tag, _| self.axes.contains(tag));
                (loc, seq)
            })
            .collect();

        for loc in point_seqs.keys() {
            if !self.locations.contains(loc) {
                return Err(DeltaError::UnknownLocation(loc.clone()));
            }
        }

        // we know point_seqs is non-empty
        let point_seq_len = point_seqs.values().next().unwrap().len();
        if point_seqs.values().any(|pts| pts.len() != point_seq_len) {
            return Err(DeltaError::InconsistentNumbersOfPoints);
        }

        let mut result: Vec<(VariationRegion, Vec<V>)> = Vec::new();
        let mut model_idx_to_result_idx = HashMap::new();

        // The fields of self are sorted such that[i] is only influenced by[i+1..N]
        // so we know subsequent spins won't invalidate our delta if we go in the same order
        for (model_idx, region, points) in self
            .influence
            .iter()
            .zip(self.locations.iter())
            .enumerate()
            .filter_map(|(idx, (region, loc))| {
                point_seqs.get(loc).map(|points| (idx, region, points))
            })
        {
            let master_influences = &self.delta_weights[model_idx];
            let mut deltas = Vec::with_capacity(points.len());

            for (idx, point) in points.iter().enumerate() {
                let initial_vector: V = *point - Default::default();
                deltas.push(
                    // Find other masters that are active (have influence)
                    // Any master with influence on us was processed already so we can get that masters
                    // deltas from the results so far. If we subtract away all such influences what's
                    // left is the delta to take us to point.
                    master_influences
                        .iter()
                        .filter_map(|(master_idx, master_weight)| {
                            let result_idx = model_idx_to_result_idx.get(master_idx)?;
                            let master_deltas: &Vec<V> = result
                                .get(*result_idx)
                                .map(|(_, master_deltas)| master_deltas)?;
                            let delta = master_deltas.get(idx)?;
                            Some((delta, master_weight.into_inner()))
                        })
                        .fold(initial_vector, |acc, (other, other_weight)| {
                            acc - *other * other_weight
                        })
                        // deltas will be stored as integers in the VarStore hence must be rounded at
                        // some point; this is the correct place to round them, instead of at the end,
                        // otherwise rounding errors can compound especially where master influences
                        // overlap. This also matches FontTools behavior, see:
                        // https://github.com/fonttools/fonttools/issues/2213
                        // https://github.com/fonttools/fonttools/pull/2214
                        .round_ties_even(),
                );
            }
            model_idx_to_result_idx.insert(model_idx, result.len());
            result.push((region.clone(), deltas));
        }

        Ok(result)
    }

    /// Convert relative deltas to absolute values at the given location.
    ///
    /// Rust version of <https://github.com/fonttools/fonttools/blob/4ad6b0db/Lib/fontTools/varLib/models.py#L514-L545>
    ///
    /// Not `pub` yet as it's currently only used for self-tests.
    ///
    /// TODO: document invariants and what we are returning. Perhaps allow a different
    /// type parameter for the return value so that e.g. absolute Points are returned
    /// when the deltas are Vec2?
    #[allow(dead_code)]
    fn interpolate_from_deltas<V>(
        location: &NormalizedLocation,
        deltasets: &[(VariationRegion, Vec<V>)],
    ) -> Vec<V>
    where
        V: Copy + Mul<f64, Output = V> + Add<V, Output = V>,
    {
        deltasets
            .iter()
            .map(|(region, deltas)| (region.scalar_at(location).0, deltas))
            .filter(|(scalar, _deltas)| *scalar != 0.0)
            .fold(None, |result, (scalar, deltas)| {
                let contribution = deltas.iter().map(|d| *d * scalar);
                match result {
                    None => Some(contribution.collect::<Vec<_>>()),
                    Some(mut existing) => {
                        existing
                            .iter_mut()
                            .zip(contribution)
                            .for_each(|(acc, val)| *acc = *acc + val);
                        Some(existing)
                    }
                }
            })
            .unwrap_or_default()
    }
}

#[derive(Error, Debug)]
pub enum DeltaError {
    #[error("The default must have a point sequence")]
    DefaultUndefined,
    #[error("Every point sequence must have the same length")]
    InconsistentNumbersOfPoints,
    #[error("{0:?} is not present in the variation model")]
    UnknownLocation(NormalizedLocation),
}

/// Gryffindor!
///
/// Sorts locations, and thus the resulting regions, based on the intuition that from most
/// to least influential are the default master, then corner masters, and finally
/// any other masters (sometimes referred to as knockout or fixup masters). This is computed
/// based on the master location being offset from default on fewer axes rather than more
///
/// * This is [LocationSortKey.rank] and [LocationSortKey.on_axis_points]
/// * The default master will thus come first, followed by any masters directly on axes
/// * For example, if default is weight 400, width 100 then the master at weight 700, width 100
///   will sort before the master at weight 700, weight 75
///
/// Additional sorting is applied after this to ensure a deteriministic order in case of ties.
///
/// This algorithm started in MutatorMath and was subsequently modified in FontTools.
///
/// This is a Rust version of FontTools getMasterLocationsSortKeyFunc.
/// <https://github.com/fonttools/fonttools/blob/2f1f5e5e7be331d960a0e30d537c2b4c70d89285/Lib/fontTools/varLib/models.py#L295>
struct LocationSortingHat<'a> {
    axis_order: &'a Vec<Tag>,
    on_axis_points: HashMap<Tag, HashSet<NormalizedCoord>>,
}

impl<'a> LocationSortingHat<'a> {
    fn new(locations: &[NormalizedLocation], axis_order: &'a Vec<Tag>) -> LocationSortingHat<'a> {
        // Location is on-axis if it has exactly 1 non-zero coordinate
        let mut on_axis_points: HashMap<Tag, HashSet<NormalizedCoord>> = HashMap::new();
        'location: for location in locations {
            let mut on_axis: Option<(Tag, NormalizedCoord)> = None;
            for (tag, pos) in location.iter() {
                if pos.into_inner() == ZERO {
                    continue;
                }
                if on_axis.is_some() {
                    continue 'location; // multiple non-zero coords, bail out
                }
                on_axis = Some((*tag, *pos));
            }
            if let Some((tag, pos)) = on_axis {
                on_axis_points.entry(tag).or_default().insert(pos);
            }
        }
        LocationSortingHat {
            axis_order,
            on_axis_points,
        }
    }

    fn key_for(&self, location: &NormalizedLocation) -> LocationSortKey {
        let mut rank = 0;
        let mut on_axis_points: i16 = 0;
        let mut ordered_axes = Vec::new();
        let mut non_zero_axes = Vec::new();

        // Score the index of the axis for any axis with non-zero value in the user specified axis order
        let mut known_axes = Vec::new();
        for (idx, tag) in self.axis_order.iter().enumerate() {
            if location.has_non_zero(*tag) {
                known_axes.push(idx);
                ordered_axes.push(*tag);
            }
        }

        for (tag, pos) in location.iter() {
            if pos.into_inner() != ZERO {
                // Rank is the # of axes with non-zero values
                rank += 1;

                // Score more than the max allowable axes for any axis with non-zero value that has no user-specified order
                // This shouldn't hapen because it's an AxesWithoutAssignedOrder error in ctor, retained
                // because I want key_for to be infallible.
                if !self.axis_order.contains(tag) {
                    known_axes.push(0x10000);
                }

                non_zero_axes.push(*tag);
            }
            // score -1 for every axis position that matches a location that was directly on an axis
            on_axis_points += self
                .on_axis_points
                .get(tag)
                .map(|on_axis| on_axis.get(pos).map_or(0, |_| -1))
                .unwrap_or_default();
        }

        let mut unordered_axes: Vec<Tag> = non_zero_axes
            .into_iter()
            .filter(|tag| !ordered_axes.contains(tag))
            .collect();
        unordered_axes.sort();
        ordered_axes.extend(unordered_axes);

        let axis_value_signs = ordered_axes
            .iter()
            .map(|tag| {
                location
                    .get(*tag)
                    .map(|coord| match coord.into_inner().cmp(&ZERO) {
                        Ordering::Greater => 1_i8,
                        Ordering::Less => -1_i8,
                        Ordering::Equal => 0_i8,
                    })
                    .unwrap_or_default()
            })
            .collect();

        let axis_value_abs = ordered_axes
            .iter()
            .map(|tag| {
                location
                    .get(*tag)
                    .map(|coord| OrderedFloat(coord.into_inner().abs()))
                    .unwrap_or_default()
            })
            .collect();

        let result = LocationSortKey {
            rank,
            on_axis_points,
            known_axes,
            ordered_axes,
            axis_value_signs,
            axis_value_abs,
        };
        trace!("key for {location:?} is {result:?}");

        result
    }
}

/// Sort key for a location.
///
/// Only axes with a non-zero normalized value are of interest.
///
/// <https://github.com/fonttools/fonttools/blob/2f1f5e5e7be331d960a0e30d537c2b4c70d89285/Lib/fontTools/varLib/models.py#L326>
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct LocationSortKey {
    // 1 for every non-zero entry in the map
    rank: usize,
    // -1 for every axis:value entry in location where a location exists whose only non-zero
    // position is on axis at value. That is, a location exists that is directly on the axis there.
    on_axis_points: i16,
    // index in user-specified axis order, otherwise score 0x10000 (65536)
    known_axes: Vec<usize>,
    // ordered list of axis in location
    ordered_axes: Vec<Tag>,
    // signs of positions on axes in ordered_axes order
    axis_value_signs: Vec<i8>,
    // abs values of positions on axes in ordered_axes order
    axis_value_abs: Vec<OrderedFloat<f64>>,
}

/// A chunk of variation space characterized by a set of per-axis tents.
#[derive(Serialize, Deserialize, Default, Debug, Clone, PartialEq, Eq)]
pub struct VariationRegion {
    axis_tents: BTreeMap<Tag, Tent>,
    active_axes: HashSet<Tag>,
}

impl VariationRegion {
    fn new() -> Self {
        Self::default()
    }

    /// The scalar multiplier for the provided location for this region
    ///
    /// In Python, supportScalar. We only implement the ot=True, extrapolate=False paths.
    /// <https://github.com/fonttools/fonttools/blob/2f1f5e5e7be331d960a0e30d537c2b4c70d89285/Lib/fontTools/varLib/models.py#L123>.
    pub fn scalar_at(&self, location: &NormalizedLocation) -> OrderedFloat<f64> {
        let scalar = self.axis_tents.iter().filter(|(_, ar)| ar.validate()).fold(
            ONE,
            |scalar, (tag, tent)| {
                let v = location
                    .get(*tag)
                    .map(|v| v.into_inner())
                    .unwrap_or_default();
                let min = tent.min.into_inner();
                let peak = tent.peak.into_inner();
                let max = tent.max.into_inner();

                // If we're at the peak by definition we have full influence
                if v == peak {
                    return scalar; // *= 1
                }

                // If the Tent is 0,0,0 it's always in full effect
                if (min, peak, max) == (ZERO, ZERO, ZERO) {
                    return scalar; // *= 1
                }

                if v <= min || max <= v {
                    trace!("  {self:?} => 0 due to {tag} {tent:?} at {location:?}",);
                    return ZERO;
                }

                let subtract_me = if v < peak {
                    tent.min.into_inner()
                } else {
                    tent.max.into_inner()
                };
                scalar * (v - subtract_me) / (peak - subtract_me)
            },
        );
        scalar
    }

    pub fn insert(&mut self, tag: Tag, tent: Tent) {
        if tent.has_non_zero() {
            self.active_axes.insert(tag);
        }
        self.axis_tents.insert(tag, tent);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Tag, &Tent)> {
        self.axis_tents.iter()
    }

    pub fn is_default(&self) -> bool {
        self.active_axes.is_empty()
    }

    pub fn get(&self, tag: &Tag) -> Option<&Tent> {
        self.axis_tents.get(tag)
    }

    /// Convert to a write-fonts VariationRegion in the order of the given fvar axes.
    ///
    /// If an axis is not present in the region, it will be assumed as not participating
    /// (peak at 0).
    pub fn to_write_fonts_variation_region(
        &self,
        axes: &Axes,
    ) -> write_fonts::tables::variations::VariationRegion {
        // https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#variation-regions
        // Array of region axis coordinates records, in the order of axes given in the 'fvar' table.
        let mut region_axes = Vec::with_capacity(axes.len());
        let zeroes = Tent::zeroes();
        for axis in axes.iter() {
            let tent = self.get(&axis.tag).unwrap_or(&zeroes);
            region_axes.push(tent.to_region_axis_coords());
        }
        write_fonts::tables::variations::VariationRegion { region_axes }
    }
}

/// The min/peak/max of a masters influence.
///
/// Visualize as a tent of influence, starting at min, peaking at peak,
/// and dropping off to zero at max.
#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq)]
pub struct Tent {
    pub min: NormalizedCoord,
    pub peak: NormalizedCoord,
    pub max: NormalizedCoord,
}

impl Tent {
    pub fn new(mut min: NormalizedCoord, peak: NormalizedCoord, mut max: NormalizedCoord) -> Self {
        let zero = NormalizedCoord::new(0.0);
        if peak > zero {
            min = zero;
        } else {
            max = zero;
        }
        Tent { min, peak, max }
    }

    pub fn zeroes() -> Tent {
        let zero = NormalizedCoord::new(0.0);
        Tent::new(zero, zero, zero)
    }

    /// OT-specific validation of whether we could have any influence
    ///
    /// (0,0,0) IS valid, meaning apply my deltas at full scale always
    ///
    /// <https://github.com/fonttools/fonttools/blob/2f1f5e5e7be331d960a0e30d537c2b4c70d89285/Lib/fontTools/varLib/models.py#L162>
    fn validate(&self) -> bool {
        let min = self.min.into_inner();
        let peak = self.peak.into_inner();
        let max = self.max.into_inner();

        if min > peak || peak > max {
            return false;
        }
        if min < ZERO && max > ZERO {
            return false;
        }
        true
    }

    pub fn has_non_zero(&self) -> bool {
        let zero = NormalizedCoord::new(0.0);
        (zero, zero, zero) != (self.min, self.peak, self.max)
    }

    /// Create an equivalent [RegionAxisCoordinates] instance.
    pub fn to_region_axis_coords(&self) -> RegionAxisCoordinates {
        RegionAxisCoordinates {
            start_coord: F2Dot14::from_f32(self.min.to_f64() as _),
            peak_coord: F2Dot14::from_f32(self.peak.to_f64() as _),
            end_coord: F2Dot14::from_f32(self.max.to_f64() as _),
        }
    }
}

impl Debug for Tent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self}"))
    }
}

impl Display for Tent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let comment = if self.validate() { "" } else { " (invalid)" };
        f.write_fmt(format_args!(
            "Tent {{{}, {}, {}{}}}",
            self.min.into_inner(),
            self.peak.into_inner(),
            self.max.into_inner(),
            comment
        ))?;
        Ok(())
    }
}

impl From<(f64, f64, f64)> for Tent {
    fn from(value: (f64, f64, f64)) -> Self {
        Tent::new(
            NormalizedCoord::new(value.0),
            NormalizedCoord::new(value.1),
            NormalizedCoord::new(value.2),
        )
    }
}

impl From<Tent> for gvar::Tent {
    fn from(val: Tent) -> Self {
        let Tent { peak, min, max } = val;
        gvar::Tent::new(peak.into(), Some((min.into(), max.into())))
    }
}

/// Split space into regions.
///
/// VariationModel::_locationsToRegions in Python.
/// <https://github.com/fonttools/fonttools/blob/2f1f5e5e7be331d960a0e30d537c2b4c70d89285/Lib/fontTools/varLib/models.py#L416>
fn regions_for(axis_order: &[Tag], locations: &[NormalizedLocation]) -> Vec<VariationRegion> {
    let mut minmax = HashMap::<Tag, (NormalizedCoord, NormalizedCoord)>::new();
    for location in locations.iter() {
        for (tag, value) in location.iter() {
            let (min, max) = minmax.entry(*tag).or_default();
            if value < min {
                *min = *value;
            }
            if value > max {
                *max = *value;
            }
        }
    }

    locations
        .iter()
        .map(|location| {
            let mut region = VariationRegion::new();
            for tag in axis_order {
                // We expand locations to cover all axes so this is safe
                let value = location.get(*tag).unwrap();

                // Python just scrubs 0's out of the location's. We elect to store representative tents.
                let (min, max) = if value.into_inner() == ZERO {
                    (NormalizedCoord::new(ZERO), NormalizedCoord::new(ZERO))
                } else {
                    *minmax.get(tag).unwrap()
                };
                region.insert(*tag, Tent::new(min, value, max));
            }
            region
        })
        .collect()
}

/// Compute the influence of each master, if any, on each region.
///
/// The regions must have been sorted by the [LocationSortingHat] as this ensures
/// that for each region we only need to look at preceeding regions for overlaps.
///
/// VariationModel::_computeMasterSupports in Python.
/// <https://github.com/fonttools/fonttools/blob/2f1f5e5e7be331d960a0e30d537c2b4c70d89285/Lib/fontTools/varLib/models.py#L360>
fn master_influence(axis_order: &[Tag], regions: &[VariationRegion]) -> Vec<VariationRegion> {
    let mut influence: Vec<VariationRegion> = Vec::new();
    for region in regions.iter() {
        let mut region = region.clone();
        // Walk over the previous masters accummulated so far.
        // The Python version here iterates over the previous regions[:i], and these might have
        // been modified in a previous iteration (the latter is important for the algorithm to work);
        // in Rust, we can't at the same time borrow regions.iter_mut() mutably in the outer loop and
        // then again borrow regions[..i].iter() immutably in the inner one. Instead, in the inner
        // loop we iterate over the master influences computed so far, which is a distinct vector.
        for prev_region in influence.iter() {
            if region.active_axes != prev_region.active_axes {
                continue;
            }
            // If prev doesn't overlap current we aren't interested
            let overlap = region.iter().all(|(axis_name, tent)| {
                let prev_peak = prev_region.axis_tents[axis_name].peak;
                prev_peak == tent.peak || (tent.min < prev_peak && prev_peak < tent.max)
            });
            if !overlap {
                continue;
            }

            // As in Python version, split the box for the new master in the direction with the
            // largest range ratio. Cut across multiple axes if they have the largest equal ratio.
            // https://github.com/fonttools/fonttools/commit/7ee81c8821671157968b097f3e55309a1faa511e#commitcomment-31054804
            let mut axis_regions: HashMap<Tag, Tent> = HashMap::new();
            let mut best_ratio = OrderedFloat(-1.0);
            for tag in axis_order.iter() {
                if !region.active_axes.contains(tag) {
                    continue;
                }
                let prev_peak = prev_region.axis_tents[tag].peak;
                let mut axis_region = region.axis_tents[tag];
                let ratio;
                match prev_peak.cmp(&axis_region.peak) {
                    Ordering::Less => {
                        ratio = (prev_peak - axis_region.peak).into_inner()
                            / (axis_region.min - axis_region.peak).into_inner();
                        axis_region.min = prev_peak;
                    }
                    Ordering::Greater => {
                        ratio = (prev_peak - axis_region.peak).into_inner()
                            / (axis_region.max - axis_region.peak).into_inner();
                        axis_region.max = prev_peak;
                    }
                    Ordering::Equal => continue, // can't split in this direction
                }
                if ratio > best_ratio {
                    axis_regions.clear();
                    best_ratio = ratio;
                }
                if ratio == best_ratio {
                    axis_regions.insert(*tag, axis_region);
                }
            }
            for (tag, tent) in axis_regions {
                region.insert(tag, tent);
            }
        }
        influence.push(region);
    }
    influence
}

/// Figure out the multipliers to use when applying for deltas from masters.
///
/// VariationModel::_computeDeltaWeights in Python.
/// <https://github.com/fonttools/fonttools/blob/2f1f5e5e7be331d960a0e30d537c2b4c70d89285/Lib/fontTools/varLib/models.py#L438>
fn delta_weights(
    locations: &[NormalizedLocation],
    influencers: &[VariationRegion],
) -> Vec<Vec<(usize, OrderedFloat<f64>)>> {
    if log_enabled!(log::Level::Trace) {
        for (l, i) in locations.iter().zip(influencers) {
            trace!("{l:?}");
            for (axis_name, tent) in i.iter() {
                trace!("  {axis_name} {tent}");
            }
        }
    }
    trace!("Delta Weights");
    let mut weights = Vec::new();
    for (loc_idx, location) in locations.iter().enumerate() {
        weights.push(
            influencers[..loc_idx]
                .iter()
                .enumerate()
                .filter_map(|(inf_idx, influence)| {
                    let scalar = influence.scalar_at(location);
                    if scalar == ZERO {
                        trace!("  no influence: {inf_idx} {influence:?} at {location:?}",);
                        return None;
                    }
                    Some((inf_idx, scalar))
                })
                .collect(),
        );
        trace!("  {} {:?}", loc_idx, weights.last().unwrap());
    }
    weights
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        str::FromStr,
    };

    use fontdrasil::{
        coords::{CoordConverter, DesignCoord, NormalizedLocation, UserCoord},
        types::Axis,
    };
    use kurbo::{Point, Vec2};
    use ordered_float::OrderedFloat;

    use pretty_assertions::assert_eq;

    use crate::variations::ONE;

    use super::*;

    fn default_master_weight() -> Vec<(usize, OrderedFloat<f64>)> {
        // no locations are contributing deltas
        Vec::new()
    }

    /// Python
    ///
    /// ```text
    /// >>> supportScalar({}, {})
    /// 1.0
    /// ```
    #[test]
    fn scalar_at_default_for_default() {
        let loc = NormalizedLocation::new();
        assert_eq!(ONE, VariationRegion::new().scalar_at(&loc));
    }

    /// Python
    ///
    /// ```text
    /// >>> supportScalar({'wght':.2}, {})
    /// 1.0
    /// ```
    #[test]
    fn scalar_at_off_default_for_default() {
        let loc = NormalizedLocation::for_pos(&[("wght", 0.2)]);
        assert_eq!(ONE, VariationRegion::new().scalar_at(&loc));
    }

    /// Python
    ///
    /// ```text
    /// >>> supportScalar({'wght':.2}, {'wght':(0,2,3)})
    /// 0.1
    /// ```
    #[test]
    fn scalar_at_off_default_for_simple_weight() {
        let loc = NormalizedLocation::for_pos(&[("wght", 0.2)]);
        let mut region = VariationRegion::new();
        region.insert(Tag::from_str("wght").unwrap(), (0.0, 2.0, 3.0).into());
        assert_eq!(OrderedFloat(0.1), region.scalar_at(&loc));
    }

    /// Python
    ///
    /// ```text
    /// >>> supportScalar({'wght':2.5}, {'wght':(0,2,4)})
    /// 0.75
    /// ```
    #[test]
    fn scalar_at_for_weight() {
        let loc = NormalizedLocation::for_pos(&[("wght", 2.5)]);
        let mut region = VariationRegion::new();
        region.insert(Tag::from_str("wght").unwrap(), (0.0, 2.0, 4.0).into());
        assert_eq!(OrderedFloat(0.75), region.scalar_at(&loc));
    }

    /// Python
    ///
    /// ```text
    /// >>> supportScalar({'wght':2.5, 'wdth':0}, {'wght':(0,2,4), 'wdth':(-1,0,+1)})
    /// 0.75
    /// ```
    /// Note that under font rules a peak of 0 means no influence
    #[test]
    fn scalar_at_for_weight_width_fixup() {
        let loc = NormalizedLocation::for_pos(&[("wght", 2.5), ("wdth", 0.0)]);
        let mut region = VariationRegion::new();
        region.insert(Tag::from_str("wght").unwrap(), (0.0, 2.0, 4.0).into());
        region.insert(Tag::from_str("wdth").unwrap(), (-1.0, 0.0, 1.0).into());
        assert_eq!(OrderedFloat(0.75), region.scalar_at(&loc));
    }

    /// Python
    ///
    /// ```text
    /// >>> supportScalar({'wght':1, 'wdth':1}, {'wght':(0, 1, 1)})
    /// 1.0
    /// ```
    #[test]
    fn scalar_at_for_weight_width_corner() {
        let loc = NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", 1.0)]);
        let mut region = VariationRegion::new();
        region.insert(Tag::from_str("wght").unwrap(), (0.0, 1.0, 1.0).into());
        assert_eq!(OrderedFloat(1.0), region.scalar_at(&loc));
    }

    /// ```text
    /// `>>> models.VariationModel([{'wght':0}]).locations`
    /// `[{}]`
    /// `>>> pprint(models.VariationModel([{'wght':0}]).deltaWeights)`
    /// `[{}]`
    /// ```
    #[test]
    fn delta_weights_for_static_family_one_axis() {
        let loc = NormalizedLocation::new();
        let locations = HashSet::from([loc]);
        let axes = Axes::for_test(&["wght"]);
        let model = VariationModel::new(locations, axes).unwrap();

        assert_eq!(
            vec![NormalizedLocation::for_pos(&[("wght", 0.0)])],
            model.locations
        );
        assert_eq!(vec![default_master_weight()], model.delta_weights);
    }

    /// ```text
    /// `>>> models.VariationModel([{'wght':0, 'ital': 0, 'wdth': 0}]).locations`
    /// `[{}]`
    /// `>>> pprint(models.VariationModel([{'wght':0, 'ital': 0, 'wdth': 0}]).deltaWeights)`
    /// `[{}]`
    /// ```
    #[test]
    fn delta_weights_for_static_family_many_axes() {
        let loc = NormalizedLocation::for_pos(&[("wght", 0.0), ("ital", 0.0), ("wdth", 0.0)]);
        let locations = HashSet::from([loc.clone()]);
        let axes = Axes::for_test(&["wdth", "wght", "ital"]);
        let model = VariationModel::new(locations, axes).unwrap();

        assert_eq!(vec![loc], model.locations);
        assert_eq!(vec![default_master_weight()], model.delta_weights);
    }

    /// # two-master weight family
    ///
    /// ```text
    /// `>>> models.VariationModel([{'wght':0}, {'wght': 1}]).locations`
    /// `[{}, {'wght': 1}]`
    /// `>>> pprint(models.VariationModel([{'wght':0}, {'wght': 1}]).deltaWeights)`
    /// `[{}, {0: 1.0}]`
    /// ```
    #[test]
    fn delta_weights_for_2_master_weight_variable_family() {
        let weight_0 = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let weight_1 = NormalizedLocation::for_pos(&[("wght", 1.0)]);
        let locations = HashSet::from([weight_1.clone(), weight_0.clone()]);
        let axes = Axes::for_test(&["wght"]);
        let model = VariationModel::new(locations, axes).unwrap();

        assert_eq!(vec![weight_0, weight_1], model.locations);
        assert_eq!(
            vec![default_master_weight(), vec![(0_usize, OrderedFloat(1.0))]],
            model.delta_weights
        );
    }

    /// # three-master weight family
    ///
    /// ```text
    /// `>>> models.VariationModel([{'wght':-1}, {'wght': 0}, {'wght': 1}]).locations`
    /// `[{}, {'wght': -1}, {'wght': 1}]`
    /// `>>> models.VariationModel([{'wght':-1}, {'wght': 0}, {'wght': 1}]).deltaWeights`
    /// `[{}, {0: 1.0}, {0: 1.0}]`
    /// ```
    #[test]
    fn delta_weights_for_3_master_weight_variable_family() {
        let weight_minus_1 = NormalizedLocation::for_pos(&[("wght", -1.0)]);
        let weight_0 = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let weight_1 = NormalizedLocation::for_pos(&[("wght", 1.0)]);
        let locations = HashSet::from([weight_1.clone(), weight_0.clone(), weight_minus_1.clone()]);
        let axes = Axes::for_test(&["wght"]);
        let model = VariationModel::new(locations, axes).unwrap();

        assert_eq!(vec![weight_0, weight_minus_1, weight_1], model.locations);
        assert_eq!(
            vec![
                default_master_weight(),
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))]
            ],
            model.delta_weights
        );
    }

    /// # corner-master weight + width
    ///
    /// ```text
    /// `>>> models.VariationModel([{'wght':0, 'wdth': 0}, {'wght':1, 'wdth': 0}, {'wght':0, 'wdth': 1}, {'wght':1, 'wdth': 1}]).locations`
    /// `[{}, {'wdth': 1}, {'wght': 1}, {'wght': 1, 'wdth': 1}]`
    /// `>>> models.VariationModel([{'wght':0, 'wdth': 0}, {'wght':1, 'wdth': 0}, {'wght':0, 'wdth': 1}, {'wght':1, 'wdth': 1}]).deltaWeights`
    /// `[{}, {0: 1.0}, {0: 1.0}, {0: 1.0, 1: 1.0, 2: 1.0}]`
    /// ```
    #[test]
    fn delta_weights_for_corner_master_weight_width_family() {
        let wght0_wdth0 = NormalizedLocation::for_pos(&[("wght", 0.0), ("wdth", 0.0)]);
        let wght0_wdth1 = NormalizedLocation::for_pos(&[("wght", 0.0), ("wdth", 1.0)]);
        let wght1_wdth0 = NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", 0.0)]);
        let wght1_wdth1 = NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", 1.0)]);
        let locations = HashSet::from([
            wght0_wdth0.clone(),
            wght0_wdth1.clone(),
            wght1_wdth0.clone(),
            wght1_wdth1.clone(),
        ]);
        let axes = Axes::for_test(&["wght", "wdth"]);
        let model = VariationModel::new(locations, axes).unwrap();

        assert_eq!(
            vec![wght0_wdth0, wght1_wdth0, wght0_wdth1, wght1_wdth1],
            model.locations
        );
        assert_eq!(
            vec![
                default_master_weight(),
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![
                    (0_usize, OrderedFloat(1.0)),
                    (1_usize, OrderedFloat(1.0)),
                    (2_usize, OrderedFloat(1.0))
                ],
            ],
            model.delta_weights
        );
    }

    /// # corner-master weight + width rig with a default master and a fixup
    /// ```text
    /// `>>> models.VariationModel([{'wght':0, 'wdth': 0}, {'wght':-1, 'wdth': -1}, {'wght':-1, 'wdth': 1}, {'wght':1, 'wdth': -1}, {'wght':1, 'wdth': 1}, {'wght':0.5, 'wdth': 0.5}], axisOrder=['wght', 'wdth']).locations`
    /// `[{}, {'wght': -1, 'wdth': -1}, {'wght': -1, 'wdth': 1}, {'wght': 1, 'wdth': -1}, {'wght': 0.5, 'wdth': 0.5}, {'wght': 1, 'wdth': 1}]`
    /// `>>> models.VariationModel([{'wght':0, 'wdth': 0}, {'wght':-1, 'wdth': -1}, {'wght':-1, 'wdth': 1}, {'wght':1, 'wdth': -1}, {'wght':1, 'wdth': 1}, {'wght':0.5, 'wdth': 0.5}], axisOrder=['wght', 'wdth']).deltaWeights`
    /// `[{}, {0: 1.0}, {0: 1.0}, {0: 1.0}, {0: 1.0}, {0: 1.0}]`
    /// ```
    #[test]
    fn delta_weights_for_corner_default_and_fixup_master_weight_width_family() {
        let default_master = NormalizedLocation::for_pos(&[("wght", 0.0), ("wdth", 0.0)]);
        let min_wght_min_wdth = NormalizedLocation::for_pos(&[("wght", -1.0), ("wdth", -1.0)]);
        let min_wght_max_wdth = NormalizedLocation::for_pos(&[("wght", -1.0), ("wdth", 1.0)]);
        let max_wght_min_wdth = NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", -1.0)]);
        let max_wght_max_wdth = NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", 1.0)]);
        let fixup = NormalizedLocation::for_pos(&[("wght", 0.5), ("wdth", 0.5)]);
        let locations = HashSet::from([
            // Default master
            default_master.clone(),
            // Corners: the far ends of each axis
            max_wght_max_wdth.clone(),
            max_wght_min_wdth.clone(),
            min_wght_max_wdth.clone(),
            min_wght_min_wdth.clone(),
            // A fixup or knockout
            fixup.clone(),
        ]);
        let axes = vec![Axis::for_test("wght"), Axis::for_test("wdth")];
        let model = VariationModel::new(locations, axes.into()).unwrap();

        assert_eq!(
            vec![
                default_master,
                min_wght_min_wdth,
                min_wght_max_wdth,
                max_wght_min_wdth,
                fixup,
                max_wght_max_wdth
            ],
            model.locations
        );
        assert_eq!(
            vec![
                default_master_weight(),
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
            ],
            model.delta_weights
        );
    }

    /// A scenario from models_test.py::test_init
    ///
    /// <https://github.com/fonttools/fonttools/blob/bf265ce49e0cae6f032420a4c80c31d8e16285b8/Tests/varLib/models_test.py#L199>
    #[test]
    fn delta_weights_for_many_master_weight_width_family() {
        let locations = HashSet::from([
            NormalizedLocation::for_pos(&[("wght", 0.55), ("wdth", 0.0)]),
            NormalizedLocation::for_pos(&[("wght", -0.55), ("wdth", 0.0)]),
            NormalizedLocation::for_pos(&[("wght", -1.0), ("wdth", 0.0)]),
            NormalizedLocation::for_pos(&[("wght", 0.0), ("wdth", 1.0)]),
            NormalizedLocation::for_pos(&[("wght", 0.66), ("wdth", 1.0)]),
            NormalizedLocation::for_pos(&[("wght", 0.66), ("wdth", 0.66)]),
            NormalizedLocation::for_pos(&[("wght", 0.0), ("wdth", 0.0)]),
            NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", 1.0)]),
            NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", 0.0)]),
        ]);
        let axes = Axes::for_test(&["wght", "wdth"]);
        let model = VariationModel::new(locations, axes).unwrap();

        assert_eq!(
            vec![
                NormalizedLocation::for_pos(&[("wght", 0.0), ("wdth", 0.0)]),
                NormalizedLocation::for_pos(&[("wght", -0.55), ("wdth", 0.0)]),
                NormalizedLocation::for_pos(&[("wght", -1.0), ("wdth", 0.0)]),
                NormalizedLocation::for_pos(&[("wght", 0.55), ("wdth", 0.0)]),
                NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", 0.0)]),
                NormalizedLocation::for_pos(&[("wght", 0.0), ("wdth", 1.0)]),
                NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", 1.0)]),
                NormalizedLocation::for_pos(&[("wght", 0.66), ("wdth", 1.0)]),
                NormalizedLocation::for_pos(&[("wght", 0.66), ("wdth", 0.66)]),
            ],
            model.locations
        );

        assert_eq!(
            vec![
                default_master_weight(),
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![
                    (0_usize, OrderedFloat(1.0)),
                    (4_usize, OrderedFloat(1.0)),
                    (5_usize, OrderedFloat(1.0))
                ],
                vec![
                    (0_usize, OrderedFloat(1.0)),
                    (3_usize, OrderedFloat(0.7555555555555555)),
                    (4_usize, OrderedFloat(0.24444444444444444)),
                    (5_usize, OrderedFloat(1.0)),
                    (6_usize, OrderedFloat(0.66))
                ],
                vec![
                    (0_usize, OrderedFloat(1.0)),
                    (3_usize, OrderedFloat(0.7555555555555555)),
                    (4_usize, OrderedFloat(0.24444444444444444)),
                    (5_usize, OrderedFloat(0.66)),
                    (6_usize, OrderedFloat(0.43560000000000004)),
                    (7_usize, OrderedFloat(0.66))
                ],
            ],
            model.delta_weights
        );
    }

    /// A scenario from models_test.py::test_init
    ///
    /// <https://github.com/fonttools/fonttools/blob/bf265ce49e0cae6f032420a4c80c31d8e16285b8/Tests/varLib/models_test.py#L259>
    #[test]
    fn delta_weights_for_foo_bar_family_case_1() {
        let locations = HashSet::from([
            NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 0.0)]),
            NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 0.5)]),
            NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 1.0)]),
            NormalizedLocation::for_pos(&[("foo", 1.0), ("bar", 0.0)]),
            NormalizedLocation::for_pos(&[("foo", 1.0), ("bar", 0.5)]),
            NormalizedLocation::for_pos(&[("foo", 1.0), ("bar", 1.0)]),
        ]);
        let axes = Axes::for_test(&["bar", "foo"]);
        let model = VariationModel::new(locations, axes).unwrap();

        assert_eq!(
            vec![
                NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 0.0)]),
                NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 0.5)]),
                NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 1.0)]),
                NormalizedLocation::for_pos(&[("foo", 1.0), ("bar", 0.0)]),
                NormalizedLocation::for_pos(&[("foo", 1.0), ("bar", 0.5)]),
                NormalizedLocation::for_pos(&[("foo", 1.0), ("bar", 1.0)]),
            ],
            model.locations
        );
        assert_eq!(
            vec![
                default_master_weight(),
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![
                    (0_usize, OrderedFloat(1.0)),
                    (1_usize, OrderedFloat(1.0)),
                    (3_usize, OrderedFloat(1.0))
                ],
                vec![
                    (0_usize, OrderedFloat(1.0)),
                    (2_usize, OrderedFloat(1.0)),
                    (3_usize, OrderedFloat(1.0))
                ],
            ],
            model.delta_weights
        );
    }

    /// A scenario from models_test.py::test_init
    ///
    /// <https://github.com/fonttools/fonttools/blob/bf265ce49e0cae6f032420a4c80c31d8e16285b8/Tests/varLib/models_test.py#L294>
    #[test]
    fn delta_weights_for_foo_bar_family_case_2() {
        let locations = HashSet::from([
            NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 0.0)]),
            NormalizedLocation::for_pos(&[("foo", 0.25), ("bar", 0.0)]),
            NormalizedLocation::for_pos(&[("foo", 0.5), ("bar", 0.0)]),
            NormalizedLocation::for_pos(&[("foo", 0.75), ("bar", 0.0)]),
            NormalizedLocation::for_pos(&[("foo", 1.0), ("bar", 0.0)]),
            NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 0.25)]),
            NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 0.75)]),
            NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 1.0)]),
        ]);
        let axes = Axes::for_test(&["bar", "foo"]);
        let model = VariationModel::new(locations, axes).unwrap();

        assert_eq!(
            vec![
                NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 0.0)]),
                NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 0.25)]),
                NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 0.75)]),
                NormalizedLocation::for_pos(&[("foo", 0.0), ("bar", 1.0)]),
                NormalizedLocation::for_pos(&[("foo", 0.25), ("bar", 0.0)]),
                NormalizedLocation::for_pos(&[("foo", 0.5), ("bar", 0.0)]),
                NormalizedLocation::for_pos(&[("foo", 0.75), ("bar", 0.0)]),
                NormalizedLocation::for_pos(&[("foo", 1.0), ("bar", 0.0)]),
            ],
            model.locations
        );
        assert_eq!(
            vec![
                default_master_weight(),
                vec![(0_usize, OrderedFloat(1.0))],
                vec![
                    (0_usize, OrderedFloat(1.0)),
                    (1_usize, OrderedFloat(1.0 / 3.0))
                ],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![(0_usize, OrderedFloat(1.0))],
                vec![
                    (0_usize, OrderedFloat(1.0)),
                    (4_usize, OrderedFloat(2.0 / 3.0))
                ],
                vec![
                    (0_usize, OrderedFloat(1.0)),
                    (4_usize, OrderedFloat(1.0 / 3.0)),
                    (5_usize, OrderedFloat(0.5))
                ],
                vec![(0_usize, OrderedFloat(1.0))],
            ],
            model.delta_weights
        );
    }

    fn region(spec: &[(&str, f64, f64, f64)]) -> VariationRegion {
        let mut region = VariationRegion::new();
        for (tag, min, peak, max) in spec {
            region.insert(Tag::from_str(tag).unwrap(), (*min, *peak, *max).into());
        }
        region
    }

    #[test]
    fn compute_simple_delta_corner_masters() {
        let origin = NormalizedLocation::for_pos(&[("wght", 0.0), ("wdth", 0.0)]);
        let max_wght = NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", 0.0)]);
        let max_wdth = NormalizedLocation::for_pos(&[("wght", 0.0), ("wdth", 1.0)]);
        let max_wght_wdth = NormalizedLocation::for_pos(&[("wght", 1.0), ("wdth", 1.0)]);
        let locations = HashSet::from([
            origin.clone(),
            max_wght.clone(),
            max_wdth.clone(),
            max_wght_wdth.clone(),
        ]);
        let axes = Axes::for_test(&["wght", "wdth"]);
        let model = VariationModel::new(locations, axes).unwrap();

        let point_seqs = HashMap::from([
            (origin, vec![Point::new(10.0, 10.0)]),
            (max_wght, vec![Point::new(12.0, 11.0)]),
            (max_wdth, vec![Point::new(11.0, 12.0)]),
            (max_wght_wdth, vec![Point::new(14.0, 11.0)]),
        ]);

        assert_eq!(
            vec![
                // default
                (
                    region(&[("wght", 0.0, 0.0, 0.0), ("wdth", 0.0, 0.0, 0.0)]),
                    vec![Vec2::new(10.0, 10.0)]
                ),
                // at max weight/width no other master has influence so it's just delta from default
                (
                    region(&[("wght", 0.0, 1.0, 1.0), ("wdth", 0.0, 0.0, 0.0)]),
                    vec![Vec2::new(2.0, 1.0)]
                ),
                (
                    region(&[("wght", 0.0, 0.0, 0.0), ("wdth", 0.0, 1.0, 1.0)]),
                    vec![Vec2::new(1.0, 2.0)]
                ),
                // at max wdth and wght the deltas for max_wght and max_wdth are in full effect
                // so we get (10+2+1, 10+1+2) "for free" and our delta is from there to (14, 11)
                (
                    region(&[("wght", 0.0, 1.0, 1.0), ("wdth", 0.0, 1.0, 1.0)]),
                    vec![Vec2::new(1.0, -2.0)]
                ),
            ],
            model.deltas(&point_seqs).unwrap()
        );
    }

    #[test]
    fn compute_1d_deltas() {
        let origin = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let max_wght = NormalizedLocation::for_pos(&[("wght", 1.0)]);
        let min_wght = NormalizedLocation::for_pos(&[("wght", -1.0)]);
        let locations = HashSet::from([origin.clone(), max_wght.clone(), min_wght.clone()]);
        let axes = Axes::for_test(&["wght"]);
        let model = VariationModel::new(locations, axes).unwrap();

        let point_seqs = HashMap::from([
            (origin, vec![10.0]),
            (max_wght, vec![12.0]),
            (min_wght, vec![5.0]),
        ]);

        let deltas = model.deltas(&point_seqs).unwrap();

        assert_eq!(
            vec![
                (region(&[("wght", 0.0, 0.0, 0.0)]), vec![10.0]),
                (region(&[("wght", -1.0, -1.0, 0.0)]), vec![-5.0]),
                (region(&[("wght", 0.0, 1.0, 1.0)]), vec![2.0]),
            ],
            deltas
        );

        let loc = NormalizedLocation::for_pos(&[("wght", -0.5)]);
        let expected = vec![7.5];
        assert_eq!(
            expected,
            VariationModel::interpolate_from_deltas(&loc, &deltas)
        );
    }

    #[derive(Debug, Default, Copy, Clone, PartialEq)]
    struct NoRoundF64(f64);

    impl RoundTiesEven for NoRoundF64 {
        #[inline]
        fn round_ties_even(self) -> NoRoundF64 {
            self
        }
    }

    impl std::ops::Sub for NoRoundF64 {
        type Output = Self;

        fn sub(self, rhs: Self) -> Self::Output {
            NoRoundF64(self.0 - rhs.0)
        }
    }

    impl std::ops::Mul<f64> for NoRoundF64 {
        type Output = Self;

        fn mul(self, rhs: f64) -> Self::Output {
            NoRoundF64(self.0 * rhs)
        }
    }

    #[test]
    fn modeling_error_within_tolerance() {
        // Compare interpolating from un-rounded float deltas vs rounded deltas,
        // and check that rounding errors don't accummulate but stay within <= 0.5.
        // This test was ported from:
        // https://github.com/fonttools/fonttools/blob/3b9a73ff/Tests/varLib/models_test.py#L167
        let num_locations = 31;
        let num_samples = 251;
        let locations: Vec<_> = (0..num_locations + 1)
            .map(|i| NormalizedLocation::for_pos(&[("axis", i as f64 / num_locations as f64)]))
            .collect();
        let master_values: HashMap<_, _> = locations
            .iter()
            .zip((0..num_locations + 1).map(|i| if i == 0 { 0.0 } else { 100.0 }))
            .map(|(loc, value)| (loc.clone(), vec![value]))
            .collect();
        // Same as master_values, but using special f64s that won't get rounded.
        let master_values_noround: HashMap<_, _> = master_values
            .iter()
            .map(|(loc, values)| (loc.clone(), values.iter().map(|v| NoRoundF64(*v)).collect()))
            .collect();

        let model = VariationModel::new(locations.into_iter().collect(), Axes::for_test(&["axis"]))
            .unwrap();

        let mut num_bad_errors = 0;
        for i in 0..num_samples {
            let loc = NormalizedLocation::for_pos(&[("axis", i as f64 / num_samples as f64)]);

            // unrounded float deltas
            let deltas_float: Vec<_> = model
                .deltas(&master_values_noround)
                .unwrap()
                .into_iter()
                .map(|(region, deltas)| (region, deltas.iter().map(|d| d.0).collect::<Vec<_>>()))
                .collect();
            // deltas rounded within the delta computation loop
            let deltas_round = model.deltas(&master_values).unwrap();
            // float deltas only rounded at the very end. This is how NOT to round deltas.
            let deltas_late_round: Vec<_> = deltas_float
                .iter()
                .map(|(region, deltas)| {
                    (
                        region.clone(),
                        deltas
                            .iter()
                            .map(|d| d.round_ties_even())
                            .collect::<Vec<_>>(),
                    )
                })
                .collect();
            // Sanity checks
            assert_ne!(deltas_float, deltas_round);
            assert_ne!(deltas_float, deltas_late_round);
            assert_ne!(deltas_round, deltas_late_round);
            assert!(deltas_round
                .iter()
                .all(|(_, deltas)| { deltas.iter().all(|d| d.fract() == 0.0) }));

            let expected: Vec<f64> = VariationModel::interpolate_from_deltas(&loc, &deltas_float);
            let actual: Vec<f64> = VariationModel::interpolate_from_deltas(&loc, &deltas_round);

            let err = (actual[0] - expected[0]).abs();
            assert!(err <= 0.5, "i: {i}, err: {err}");

            // when the influence of many masters overlap a particular location
            // interpolating from late-rounded deltas may lead to an accummulation
            // of rounding errors that exceed the max tolerance
            let bad = VariationModel::interpolate_from_deltas(&loc, &deltas_late_round);
            let err_bad = (bad[0] - expected[0]).abs();
            if err_bad > 0.5 {
                num_bad_errors += 1;
            }
        }
        assert!(num_bad_errors > 0);
    }

    #[test]
    fn rounding_of_deltas_matches_fonttools() {
        // This reproduces an off-by-one diff in MVAR table's deltas for Gelasio.glyphspackage
        // between fontc and fontmake (rather fonttools) which was caused by the former's incorrect
        // (late) rounding of deltas:
        // https://github.com/googlefonts/fontc/issues/1043
        // https://github.com/googlefonts/fontc/issues/235
        let min = UserCoord::new(400.0);
        let default = UserCoord::new(400.0);
        let max = UserCoord::new(700.0);
        let model = VariationModel::new(
            HashSet::from([
                NormalizedLocation::for_pos(&[("wght", 0.0)]),
                NormalizedLocation::for_pos(&[("wght", 1.0)]),
            ]),
            vec![Axis {
                name: "Weight".to_string(),
                tag: Tag::new(b"wght"),
                min,
                default,
                max,
                hidden: false,
                converter: CoordConverter::new(
                    vec![
                        (default, DesignCoord::new(default.into_inner())),
                        (max, DesignCoord::new(max.into_inner())),
                    ],
                    0,
                ),
                localized_names: Default::default(),
            }]
            .into(),
        )
        .unwrap();

        let master_values = HashMap::from([
            (NormalizedLocation::for_pos(&[("wght", 0.0)]), vec![591.6]),
            (NormalizedLocation::for_pos(&[("wght", 1.0)]), vec![596.4]),
        ]);

        // when working with unrounded values, the delta at wght=1.0 would be 4.8. By the
        // time this gets ot-rounded to i16, it becomes 5, i.e. an extra +1 off the
        // expected correct value (4).
        assert_eq!(
            vec![vec![592.0], vec![4.0]], // not vec![5.0]
            model
                .deltas(&master_values.clone())
                .unwrap()
                .into_iter()
                .map(|(_, ds)| ds)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn variations_for_tag_interpolation_of_weight_loudness() {
        // Hypothetical weight axis [200, 800], default 400
        let wght_min = UserCoord::new(200.0);
        let wght_default = UserCoord::new(400.0);
        let wght_max = UserCoord::new(800.0);
        let weight_axis = Axis {
            name: "Weight".to_string(),
            tag: Tag::new(b"wght"),
            min: wght_min,
            default: wght_default,
            max: wght_max,
            hidden: false,
            converter: CoordConverter::new(
                vec![
                    (wght_default, DesignCoord::new(wght_default.into_inner())),
                    (wght_max, DesignCoord::new(wght_max.into_inner())),
                ],
                0,
            ),
            localized_names: Default::default(),
        };

        // (wght, tag value) tuples basically
        // Tag values are [0, 100] default 0
        // wght is expressed in normalized values
        let master_values = HashMap::from([
            (NormalizedLocation::for_pos(&[("wght", -1.0)]), vec![0.0]),
            (NormalizedLocation::for_pos(&[("wght", 0.0)]), vec![0.0]),
            (NormalizedLocation::for_pos(&[("wght", 1.0)]), vec![90.0]),
        ]);

        // Make a variation model covering the locations for which we have data
        let model = VariationModel::new(
            master_values.keys().cloned().collect(),
            vec![weight_axis].into(),
        )
        .unwrap();

        let deltas = model.deltas(&master_values.clone()).unwrap();

        // Now we can compute the value at in-betweens!
        let expected_values = vec![
            // halfway between min and default
            (NormalizedLocation::for_pos(&[("wght", -0.5)]), 0.0),
            // halfway between default and max
            (NormalizedLocation::for_pos(&[("wght", 0.5)]), 45.0),
            // the max
            (NormalizedLocation::for_pos(&[("wght", 1.0)]), 90.0),
        ];
        assert_eq!(
            expected_values,
            expected_values
                .iter()
                .map(|(loc, _)| (
                    loc.clone(),
                    VariationModel::interpolate_from_deltas(loc, &deltas)
                        .iter()
                        .sum()
                ))
                .collect::<Vec<_>>()
        );
    }
}
