//! Using confusable types for coords is an endless source of confusion; don't.
//!
//! See <https://github.com/googlefonts/fontmake-rs/blob/main/resources/text/units.md>

use std::collections::BTreeMap;

use log::warn;
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};

use crate::piecewise_linear_map::PiecewiseLinearMap;

/// A coordinate in some arbitrary space the designer dreamed up.
///
/// In .designspace, an xvalue. <https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#dimension-element>.
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DesignCoord(OrderedFloat<f32>);

/// A coordinate the end user sees, e.g. what 'fvar' uses, Weight 400.
///
/// In .designspace, a uservalue. <https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#dimension-element>.
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UserCoord(OrderedFloat<f32>);

/// A coordinate used within the font, not seen by any user.
///
/// Always in [-1, 1].
///
/// In .designspace, a uservalue. <https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#dimension-element>.
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternalCoord(OrderedFloat<f32>);

// Using BTreeMap instead of HashMap and OrderedFloat instead of f32 so that
// the location is hashable and can be used as a key in Glyph::sources HashMap
pub type DesignLocation = BTreeMap<String, DesignCoord>;
pub type UserLocation = BTreeMap<String, UserCoord>;
pub type InternalLocation = BTreeMap<String, UserCoord>;

trait Converter<From, To> {
    fn convert(&self, from: From) -> To;
}

/// Converts between Design, User, and Internal coordinates.
///
/// Stores [PiecewiseLinearMap]'s in several directions. Sources
/// suggest <= 10 mappings is typical, we can afford the bytes.
#[derive(Debug, Clone)]
pub struct CoordConverter {
    user_to_design: PiecewiseLinearMap,
    design_to_user: PiecewiseLinearMap,
    design_to_internal: PiecewiseLinearMap,
    internal_to_design: PiecewiseLinearMap,
}

impl CoordConverter {
    pub fn from_user_to_design_examples(
        mut examples: Vec<(UserCoord, DesignCoord)>,
        default_idx: usize,
    ) -> CoordConverter {
        if examples.is_empty() {
            examples.push((UserCoord(0.0.into()), DesignCoord(0.0.into())));
        }
        let user_to_design = PiecewiseLinearMap::new(
            examples
                .iter()
                .map(|(u, d)| (u.into_inner(), d.into_inner()))
                .collect(),
        );

        let design_coords: Vec<_> = examples.iter().map(|(_, d)| d).collect();
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
        let design_to_internal = PiecewiseLinearMap::new(examples);

        let design_to_user = user_to_design.reverse();
        let internal_to_design = design_to_internal.reverse();

        CoordConverter {
            user_to_design,
            design_to_user,
            design_to_internal,
            internal_to_design,
        }
    }
}

// Will be removed once remapping is properly implemented, for now marks where we need to update
// Tentatively expected to put maps onto StaticMetadata so anyone who wants design:user gets from there
pub fn temporary_design_to_user_conversion(coord: DesignCoord) -> UserCoord {
    warn!("Illegal and invalid temporary conversion");
    UserCoord(coord.into_inner())
}

impl DesignCoord {
    /// We do *not* provide From because we want conversion to be explicit
    pub fn new(value: OrderedFloat<f32>) -> DesignCoord {
        DesignCoord(value)
    }

    pub fn to_user(&self, converter: &CoordConverter) -> UserCoord {
        UserCoord::new(converter.design_to_user.map(self.0))
    }

    pub fn to_internal(&self, converter: &CoordConverter) -> InternalCoord {
        InternalCoord::new(converter.design_to_internal.map(self.0))
    }
}

impl UserCoord {
    /// We do *not* provide From because we want conversion to be explicit
    pub fn new(value: OrderedFloat<f32>) -> UserCoord {
        UserCoord(value)
    }

    pub fn to_design(&self, converter: &CoordConverter) -> DesignCoord {
        DesignCoord::new(converter.user_to_design.map(self.0))
    }

    pub fn to_internal(&self, converter: &CoordConverter) -> InternalCoord {
        self.to_design(converter).to_internal(converter)
    }
}

impl InternalCoord {
    /// We do *not* provide From because we want conversion to be explicit
    pub fn new(value: OrderedFloat<f32>) -> InternalCoord {
        InternalCoord(value)
    }

    pub fn to_design(&self, converter: &CoordConverter) -> DesignCoord {
        DesignCoord::new(converter.internal_to_design.map(self.0))
    }
}

impl DesignCoord {
    pub fn into_inner(self) -> OrderedFloat<f32> {
        self.0
    }
}

impl UserCoord {
    pub fn into_inner(self) -> OrderedFloat<f32> {
        self.0
    }
}

impl InternalCoord {
    pub fn into_inner(self) -> OrderedFloat<f32> {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use ordered_float::OrderedFloat;

    use super::{CoordConverter, DesignCoord, UserCoord};

    // From <https://github.com/googlefonts/fontmake-rs/blob/main/resources/text/units.md>
    fn lexend_weight_mapping() -> (Vec<(UserCoord, DesignCoord)>, usize) {
        (
            vec![
                (UserCoord(100.0.into()), DesignCoord(26.0.into())),
                (UserCoord(200.0.into()), DesignCoord(39.0.into())),
                (UserCoord(300.0.into()), DesignCoord(58.0.into())),
                (UserCoord(400.0.into()), DesignCoord(90.0.into())), // [3]; default
                (UserCoord(500.0.into()), DesignCoord(108.0.into())),
                (UserCoord(600.0.into()), DesignCoord(128.0.into())),
                (UserCoord(700.0.into()), DesignCoord(151.0.into())),
                (UserCoord(800.0.into()), DesignCoord(169.0.into())),
                (UserCoord(900.0.into()), DesignCoord(190.0.into())),
            ],
            3,
        )
    }

    // 200 and 500 (user) are pushed way toward the left/right respectively
    fn bendy_mapping() -> (Vec<(UserCoord, DesignCoord)>, usize) {
        (
            vec![
                (UserCoord(100.0.into()), DesignCoord(0.0.into())),
                (UserCoord(200.0.into()), DesignCoord(1.0.into())),
                (UserCoord(400.0.into()), DesignCoord(10.0.into())), // [2]; default
                (UserCoord(500.0.into()), DesignCoord(19.0.into())),
                (UserCoord(900.0.into()), DesignCoord(20.0.into())),
            ],
            2,
        )
    }

    #[test]
    pub fn lexend_weight_internal_basics() {
        let (examples, default_idx) = lexend_weight_mapping();
        let converter = CoordConverter::from_user_to_design_examples(examples, default_idx);
        assert_eq!(
            OrderedFloat(-1.0),
            DesignCoord(26.0.into())
                .to_internal(&converter)
                .into_inner()
        );
        assert_eq!(
            OrderedFloat(0.0),
            DesignCoord(90.0.into())
                .to_internal(&converter)
                .into_inner()
        );
        assert_eq!(
            OrderedFloat(1.0),
            DesignCoord(190.0.into())
                .to_internal(&converter)
                .into_inner()
        );
    }

    #[test]
    pub fn design_to_internal_does_not_bend() {
        let (examples, default_idx) = bendy_mapping();
        let converter = CoordConverter::from_user_to_design_examples(examples, default_idx);

        // 200 and 500 (user) are pushed way toward the left/right respectively
        // But design:internal doesn't care, it's linear from default=>max and default=>min
        assert_eq!(
            OrderedFloat(-1.0),
            DesignCoord(0.0.into()).to_internal(&converter).into_inner()
        );
        assert_eq!(
            OrderedFloat(-0.5),
            DesignCoord(5.0.into()).to_internal(&converter).into_inner()
        );
        assert_eq!(
            OrderedFloat(0.0),
            DesignCoord(10.0.into())
                .to_internal(&converter)
                .into_inner()
        );
        assert_eq!(
            OrderedFloat(0.5),
            DesignCoord(15.0.into())
                .to_internal(&converter)
                .into_inner()
        );
        assert_eq!(
            OrderedFloat(1.0),
            DesignCoord(20.0.into())
                .to_internal(&converter)
                .into_inner()
        );
    }

    #[test]
    pub fn user_to_design_or_internal_does_bend() {
        let (examples, default_idx) = bendy_mapping();
        let converter = CoordConverter::from_user_to_design_examples(examples, default_idx);

        // 200 and 500 (user) are pushed way toward the left/right respectively

        // User : Design warps; 100..200 are squeezed into a small leftward slice
        // 150 is halfway between 100 and 200
        assert_eq!(
            OrderedFloat(0.0),
            UserCoord(100.0.into()).to_design(&converter).into_inner()
        );
        assert_eq!(
            OrderedFloat(0.5),
            UserCoord(150.0.into()).to_design(&converter).into_inner()
        );
        assert_eq!(
            OrderedFloat(1.0),
            UserCoord(200.0.into()).to_design(&converter).into_inner()
        );

        assert_eq!(
            OrderedFloat(-1.0),
            UserCoord(100.0.into()).to_internal(&converter).into_inner()
        );
        assert_eq!(
            OrderedFloat(-0.95),
            UserCoord(150.0.into()).to_internal(&converter).into_inner()
        );
        assert_eq!(
            OrderedFloat(-0.9),
            UserCoord(200.0.into()).to_internal(&converter).into_inner()
        );

        // 200..400 covers a massive slice!
        // 300 is halway to 400 (breaking news!)
        assert_eq!(
            OrderedFloat(5.5),
            UserCoord(300.0.into()).to_design(&converter).into_inner()
        );
        assert_eq!(
            OrderedFloat(10.0),
            UserCoord(400.0.into()).to_design(&converter).into_inner()
        );

        assert_eq!(
            OrderedFloat(-0.45),
            UserCoord(300.0.into()).to_internal(&converter).into_inner()
        );
        assert_eq!(
            OrderedFloat(0.0),
            UserCoord(400.0.into()).to_internal(&converter).into_inner()
        );
    }
}
