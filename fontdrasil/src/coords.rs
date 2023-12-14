//! Using confusable types for coords is an endless source of confusion; don't.
//!
//! See <https://github.com/googlefonts/fontmake-rs/blob/main/resources/text/units.md>

use std::{
    collections::{BTreeMap, HashMap},
    fmt::{Debug, Write},
    marker::PhantomData,
    ops::Sub,
};

use ordered_float::OrderedFloat;
use serde::{ser::SerializeSeq, Deserialize, Deserializer, Serialize};
use write_fonts::types::{F2Dot14, Fixed, Tag};

use crate::{piecewise_linear_map::PiecewiseLinearMap, types::Axis};

/// A trait for converting coordinates between coordinate spaces.
///
/// This trait is intended to be implemented on types that represent the
/// coordinate spaces themselves.
///
/// You don't ever need to use this directly; it is used to implement the
/// [`Coord::convert`] and [`Location::convert`] methods.
pub trait ConvertSpace<ToSpace>: Sized {
    /// Convert a coord from our space to the target space.
    fn convert_coord(coord: Coord<Self>, converter: &CoordConverter) -> Coord<ToSpace>;
}

/// The coordinate space used by the type designer/editing software.
///
/// This space has arbitrary bounds defined on a per-project basis. For instance,
/// a font might internally represent the 'weight' axis as a value from 0-200.
///
/// In [.designspace file][dspace], this is an 'xvalue'.
///
/// [dspace]: https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#dimension-element
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DesignSpace;

/// A coordinate space that may be visible to the end user.
///
/// For instance a weight value in CSS is expressed in user coordinates.
///
/// In a [.designspace file][dspace], this is a 'uservalue'.
///
/// [dspace]: https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#dimension-element
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UserSpace;

/// A space containing only values in the range `-1..=1`.
///
/// This is used internally in the font, and is never visible to the user.
/// The default value is always at `0`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NormalizedSpace;

/// A coordinate in some coordinate space.
#[derive(Serialize, Deserialize, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Coord<Space> {
    coord: OrderedFloat<f32>,
    // we want to be covariant but also Send + Sync. See
    // <https://doc.rust-lang.org/1.74.0/nomicon/phantom-data.html#table-of-phantomdata-patterns>
    space: PhantomData<fn() -> Space>,
}

/// A coordinate in design space.
pub type DesignCoord = Coord<DesignSpace>;
/// A coordinate in user space
pub type UserCoord = Coord<UserSpace>;
/// A coordinate in normalized space
pub type NormalizedCoord = Coord<NormalizedSpace>;

impl<Space> Coord<Space> {
    /// Create a new coordinate.
    ///
    /// Note that we do *not* impl From because we want conversion to be explicit.
    pub fn new(value: impl Into<OrderedFloat<f32>>) -> Self {
        Coord {
            coord: value.into(),
            space: PhantomData,
        }
    }

    pub fn into_inner(self) -> OrderedFloat<f32> {
        self.coord
    }

    pub fn to_f32(&self) -> f32 {
        self.coord.into_inner()
    }

    /// Convert this coordinate into the target space.
    pub fn convert<ToSpace>(&self, converter: &CoordConverter) -> Coord<ToSpace>
    where
        Space: ConvertSpace<ToSpace>,
    {
        Space::convert_coord(*self, converter)
    }
}

/// A set of per-axis coordinates that define a specific location in a coordinate system.
///
/// E.g. a user location is a `Location<UserSpace>`. Hashable so it can do things like be
/// the key for a map of sources by location.
#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location<Space>(BTreeMap<Tag, Coord<Space>>);

/// A location in [`DesignSpace`].
pub type DesignLocation = Location<DesignSpace>;
/// A location in [`UserSpace`].
pub type UserLocation = Location<UserSpace>;
/// A location in [`NormalizedSpace`].
pub type NormalizedLocation = Location<NormalizedSpace>;

// a little helper to generate methods on coord/location for specific conversions
macro_rules! convert_convenience_methods {
    ($space:ident, $fn_name:ident) => {
        impl<Space> Coord<Space>
        where
            Space: ConvertSpace<$space>,
        {
            pub fn $fn_name(&self, converter: &CoordConverter) -> Coord<$space> {
                self.convert(converter)
            }
        }

        impl<Space> Location<Space>
        where
            Space: ConvertSpace<$space>,
        {
            pub fn $fn_name(&self, axes: &HashMap<Tag, &Axis>) -> Location<$space> {
                self.convert(axes)
            }
        }
    };
}
convert_convenience_methods!(NormalizedSpace, to_normalized);
convert_convenience_methods!(UserSpace, to_user);
convert_convenience_methods!(DesignSpace, to_design);

/// Converts between Design, User, and Normalized coordinates.
// Stores `PiecewiseLinearMap`'s in several directions. Sources
// suggest <= 10 mappings is typical, we can afford the bytes.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct CoordConverter {
    pub(crate) default_idx: usize,
    pub(crate) user_to_design: PiecewiseLinearMap,
    design_to_user: PiecewiseLinearMap,
    design_to_normalized: PiecewiseLinearMap,
    normalized_to_design: PiecewiseLinearMap,
}

impl CoordConverter {
    /// Initialize a converter from the User:Design examples source files typically provide.
    pub fn new(mut mappings: Vec<(UserCoord, DesignCoord)>, default_idx: usize) -> CoordConverter {
        if mappings.is_empty() {
            mappings.push((UserCoord::new(0.0), DesignCoord::new(0.0)));
        }
        let user_to_design = PiecewiseLinearMap::new(
            mappings
                .iter()
                .map(|(u, d)| (u.into_inner(), d.into_inner()))
                .collect(),
        );

        let design_coords: Vec<_> = mappings.iter().map(|(_, d)| d).collect();
        let design_min = design_coords.iter().min().unwrap();
        let design_max = design_coords.iter().max().unwrap();
        let design_default = design_coords[default_idx];

        let mut examples = Vec::new();
        if *design_min < design_default {
            examples.push((design_min.into_inner(), (-1.0).into())); // leftmost of default *must* be -1
        }
        examples.push((design_default.into_inner(), 0.0.into())); // default *must* land at 0
        if *design_max > design_default {
            examples.push((design_max.into_inner(), 1.0.into())); // right of default *must* be +1
        }
        let design_to_normalized = PiecewiseLinearMap::new(examples);

        let design_to_user = user_to_design.reverse();
        let normalized_to_design = design_to_normalized.reverse();

        CoordConverter {
            default_idx,
            user_to_design,
            design_to_user,
            design_to_normalized,
            normalized_to_design,
        }
    }

    /// Initialize a converter from just min/default/max user coords, e.g. a source with no mapping
    pub fn unmapped(min: UserCoord, default: UserCoord, max: UserCoord) -> CoordConverter {
        let mut mappings = vec![
            (min, DesignCoord::new(min.into_inner())),
            (default, DesignCoord::new(default.into_inner())),
            (max, DesignCoord::new(max.into_inner())),
        ];
        mappings.dedup();
        let default_idx = mappings.iter().position(|(u, _)| *u == default).unwrap();
        CoordConverter::new(mappings, default_idx)
    }

    /// Walk the vertices of the mappings, viewing the user/design/normalized value at each stop.
    pub fn iter(&self) -> impl Iterator<Item = (UserCoord, DesignCoord, NormalizedCoord)> + '_ {
        self.user_to_design.iter().map(|(user, design)| {
            let user = UserCoord::new(user);
            let design = DesignCoord::new(design);
            let normalized = user.to_normalized(self);
            (user, design, normalized)
        })
    }

    /// How many mapping points exist
    pub fn len(&self) -> usize {
        self.user_to_design.len()
    }

    pub fn is_empty(&self) -> bool {
        self.user_to_design.len() == 0
    }
}

impl From<UserCoord> for Fixed {
    fn from(value: UserCoord) -> Self {
        Fixed::from_f64(value.to_f32() as f64)
    }
}

impl From<NormalizedCoord> for F2Dot14 {
    fn from(value: NormalizedCoord) -> Self {
        F2Dot14::from_f32(value.to_f32())
    }
}

impl<Space> Sub<Coord<Space>> for Coord<Space> {
    type Output = Coord<Space>;

    fn sub(self, rhs: Coord<Space>) -> Self::Output {
        Coord::new(self.to_f32() - rhs.to_f32())
    }
}

impl<Space> FromIterator<(Tag, Coord<Space>)> for Location<Space> {
    fn from_iter<I: IntoIterator<Item = (Tag, Coord<Space>)>>(iter: I) -> Self {
        Location(iter.into_iter().collect())
    }
}

impl<Space> From<Vec<(Tag, Coord<Space>)>> for Location<Space> {
    fn from(value: Vec<(Tag, Coord<Space>)>) -> Self {
        value.into_iter().collect()
    }
}

impl<Space> Location<Space> {
    pub fn new() -> Location<Space> {
        Location(Default::default())
    }

    /// For testing only, make a location from raw tags + values
    #[doc(hidden)]
    pub fn for_pos(positions: &[(&str, f32)]) -> Self {
        positions
            .iter()
            .map(|(tag, value)| {
                let tag = tag.parse::<Tag>().unwrap();
                (tag, Coord::new(*value))
            })
            .collect()
    }

    pub fn insert(&mut self, tag: Tag, pos: Coord<Space>) -> &mut Location<Space> {
        self.0.insert(tag, pos);
        self
    }

    pub fn remove(&mut self, tag: Tag) {
        self.0.remove(&tag);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Tag, &Coord<Space>)> {
        self.0.iter()
    }

    pub fn axis_tags(&self) -> impl Iterator<Item = &Tag> {
        self.0.keys()
    }

    pub fn contains(&self, tag: Tag) -> bool {
        self.0.contains_key(&tag)
    }

    pub fn get(&self, tag: Tag) -> Option<Coord<Space>> {
        self.0.get(&tag).copied()
    }

    pub fn retain(&mut self, pred: impl Fn(&Tag, &mut Coord<Space>) -> bool) {
        self.0.retain(pred);
    }

    pub fn convert<ToSpace>(&self, axes: &HashMap<Tag, &Axis>) -> Location<ToSpace>
    where
        Space: ConvertSpace<ToSpace>,
    {
        self.0
            .iter()
            .map(|(tag, coord)| (*tag, coord.convert(&axes.get(tag).unwrap().converter)))
            .collect()
    }
}

// methods we only want available on NormalizedSpace
impl Location<NormalizedSpace> {
    pub fn has_non_zero(&self, tag: Tag) -> bool {
        self.get(tag).unwrap_or_default().to_f32() != 0.0
    }

    pub fn has_any_non_zero(&self) -> bool {
        self.0.values().any(|v| v.to_f32() != 0.0)
    }

    /// Returns true if all normalized coordinates are zero
    pub fn is_default(&self) -> bool {
        !self.has_any_non_zero()
    }
}

impl ConvertSpace<UserSpace> for DesignSpace {
    fn convert_coord(coord: Coord<Self>, converter: &CoordConverter) -> Coord<UserSpace> {
        Coord::new(converter.design_to_user.map(coord.coord))
    }
}

impl ConvertSpace<NormalizedSpace> for DesignSpace {
    fn convert_coord(coord: Coord<Self>, converter: &CoordConverter) -> Coord<NormalizedSpace> {
        Coord::new(converter.design_to_normalized.map(coord.coord))
    }
}

impl ConvertSpace<DesignSpace> for UserSpace {
    fn convert_coord(coord: Coord<Self>, converter: &CoordConverter) -> Coord<DesignSpace> {
        Coord::new(converter.user_to_design.map(coord.coord))
    }
}

impl ConvertSpace<NormalizedSpace> for UserSpace {
    fn convert_coord(coord: Coord<Self>, converter: &CoordConverter) -> Coord<NormalizedSpace> {
        let dspace: DesignCoord = UserSpace::convert_coord(coord, converter);
        DesignSpace::convert_coord(dspace, converter)
    }
}

impl ConvertSpace<DesignSpace> for NormalizedSpace {
    fn convert_coord(coord: Coord<Self>, converter: &CoordConverter) -> Coord<DesignSpace> {
        Coord::new(converter.normalized_to_design.map(coord.coord))
    }
}

impl ConvertSpace<UserSpace> for NormalizedSpace {
    fn convert_coord(coord: Coord<Self>, converter: &CoordConverter) -> Coord<UserSpace> {
        let dspace: DesignCoord = NormalizedSpace::convert_coord(coord, converter);
        DesignSpace::convert_coord(dspace, converter)
    }
}

// we need to manually implement this bc of phantomdata:
// <https://stackoverflow.com/questions/31371027/copy-trait-and-phantomdata-should-this-really-move>
impl<T> Clone for Coord<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Coord<T> {}

impl<Space> PartialEq<f32> for Coord<Space> {
    fn eq(&self, other: &f32) -> bool {
        self.coord.as_ref() == other
    }
}

impl<Space> PartialEq<Coord<Space>> for f32 {
    fn eq(&self, other: &Coord<Space>) -> bool {
        other == self
    }
}

impl<Space> Serialize for Location<Space> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
        for (key, value) in &self.0 {
            seq.serialize_element(&(key, value.to_f32()))?;
        }
        seq.end()
    }
}

impl<'de, Space> Deserialize<'de> for Location<Space> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Vec::<(Tag, OrderedFloat<f32>)>::deserialize(deserializer).map(|vals| {
            Location(
                vals.into_iter()
                    .map(|(tag, val)| (tag, Coord::new(val)))
                    .collect(),
            )
        })
    }
}

fn format_location<'a, 'b>(
    name: &str,
    f: &mut std::fmt::Formatter<'a>,
    items: impl Iterator<Item = (&'b Tag, OrderedFloat<f32>)>,
) -> std::fmt::Result {
    f.write_str(name)?;
    f.write_str(" {")?;
    for (i, (tag, value)) in items.enumerate() {
        if i > 0 {
            f.write_str(", ")?;
        }
        f.write_fmt(format_args!("{tag}: {value:.02}"))?;
    }
    f.write_char('}')
}

impl Debug for DesignLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let it = self.0.iter().map(|(n, v)| (n, v.into_inner()));
        format_location("Design", f, it)
    }
}

impl Debug for UserLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let it = self.0.iter().map(|(n, v)| (n, v.into_inner()));
        format_location("User", f, it)
    }
}

impl Debug for NormalizedLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let it = self.0.iter().map(|(n, v)| (n, v.into_inner()));
        format_location("Normalized", f, it)
    }
}

#[cfg(test)]
mod tests {

    use super::{CoordConverter, DesignCoord, UserCoord};

    // From <https://github.com/googlefonts/fontmake-rs/blob/main/resources/text/units.md>
    fn lexend_weight_mapping() -> (Vec<(UserCoord, DesignCoord)>, usize) {
        (
            vec![
                (UserCoord::new(100.0), DesignCoord::new(26.0)),
                (UserCoord::new(200.0), DesignCoord::new(39.0)),
                (UserCoord::new(300.0), DesignCoord::new(58.0)),
                (UserCoord::new(400.0), DesignCoord::new(90.0)), // [3]; default
                (UserCoord::new(500.0), DesignCoord::new(108.0)),
                (UserCoord::new(600.0), DesignCoord::new(128.0)),
                (UserCoord::new(700.0), DesignCoord::new(151.0)),
                (UserCoord::new(800.0), DesignCoord::new(169.0)),
                (UserCoord::new(900.0), DesignCoord::new(190.0)),
            ],
            3,
        )
    }

    // 200 and 500 (user) are pushed way toward the left/right respectively
    fn bendy_mapping() -> (Vec<(UserCoord, DesignCoord)>, usize) {
        (
            vec![
                (UserCoord::new(100.0), DesignCoord::new(0.0)),
                (UserCoord::new(200.0), DesignCoord::new(1.0)),
                (UserCoord::new(400.0), DesignCoord::new(10.0)), // [2]; default
                (UserCoord::new(500.0), DesignCoord::new(19.0)),
                (UserCoord::new(900.0), DesignCoord::new(20.0)),
            ],
            2,
        )
    }

    #[test]
    pub fn lexend_weight_internal_basics() {
        let (examples, default_idx) = lexend_weight_mapping();
        let converter = CoordConverter::new(examples, default_idx);
        assert_eq!(-1.0, DesignCoord::new(26.0).to_normalized(&converter));
        assert_eq!(0.0, DesignCoord::new(90.0).to_normalized(&converter));
        assert_eq!(1.0, DesignCoord::new(190.0).to_normalized(&converter));
    }

    #[test]
    pub fn design_to_normalized_does_not_bend() {
        let (examples, default_idx) = bendy_mapping();
        let converter = CoordConverter::new(examples, default_idx);

        // 200 and 500 (user) are pushed way toward the left/right respectively
        // But design:normalized doesn't care, it's linear from default=>max and default=>min
        assert_eq!(-1.0, DesignCoord::new(0.0).to_normalized(&converter));
        assert_eq!(-0.5, DesignCoord::new(5.0).to_normalized(&converter));
        assert_eq!(0.0, DesignCoord::new(10.0).to_normalized(&converter));
        assert_eq!(0.5, DesignCoord::new(15.0).to_normalized(&converter));
        assert_eq!(1.0, DesignCoord::new(20.0).to_normalized(&converter));
    }

    #[test]
    pub fn user_to_design_or_normalized_does_bend() {
        let (examples, default_idx) = bendy_mapping();
        let converter = CoordConverter::new(examples, default_idx);

        // 200 and 500 (user) are pushed way toward the left/right respectively

        // User : Design warps; 100..200 are squeezed into a small leftward slice
        // 150 is halfway between 100 and 200
        assert_eq!(0.0, UserCoord::new(100.0).to_design(&converter));
        assert_eq!(0.5, UserCoord::new(150.0).to_design(&converter));
        assert_eq!(1.0, UserCoord::new(200.0).to_design(&converter));

        assert_eq!(-1.0, UserCoord::new(100.0).to_normalized(&converter));
        assert_eq!(-0.95, UserCoord::new(150.0).to_normalized(&converter));
        assert_eq!(-0.9, UserCoord::new(200.0).to_normalized(&converter));

        // 200..400 covers a massive slice!
        // 300 is halway to 400 (breaking news!)
        assert_eq!(5.5, UserCoord::new(300.0).to_design(&converter));
        assert_eq!(10.0, UserCoord::new(400.0).to_design(&converter));

        assert_eq!(-0.45, UserCoord::new(300.0).to_normalized(&converter));
        assert_eq!(0.0, UserCoord::new(400.0).to_normalized(&converter));
    }

    #[test]
    fn unmapped_coords_get_deduped() {
        // min==default==max
        assert_eq!(
            CoordConverter::unmapped(
                UserCoord::new(100.0),
                UserCoord::new(100.0),
                UserCoord::new(100.0),
            )
            .default_idx,
            0
        );
        // min==default<max
        assert_eq!(
            CoordConverter::unmapped(
                UserCoord::new(0.0),
                UserCoord::new(0.0),
                UserCoord::new(100.0),
            )
            .default_idx,
            0
        );
        // min<default==max
        assert_eq!(
            CoordConverter::unmapped(
                UserCoord::new(0.0),
                UserCoord::new(100.0),
                UserCoord::new(100.0),
            )
            .default_idx,
            1
        );
        // min<default<max
        assert_eq!(
            CoordConverter::unmapped(
                UserCoord::new(0.0),
                UserCoord::new(50.0),
                UserCoord::new(100.0),
            )
            .default_idx,
            1
        );
    }
}
