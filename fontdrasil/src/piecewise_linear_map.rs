//! Remaps values using a series of linear mappings.
//!
//! Useful for things like designspace : userspace mapping.
//! For example, from a <https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#location>
//! xvalue to a userspace (fvar) value.

use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PiecewiseLinearMap {
    // these two mappings have identical lengths, by construction
    from: Vec<OrderedFloat<f32>>, // sorted, ||'s to
    to: Vec<OrderedFloat<f32>>,   // sorted, ||'s from
}

impl PiecewiseLinearMap {
    /// Create a new map from a series of (from, to) values.
    pub fn new(mut mappings: Vec<(OrderedFloat<f32>, OrderedFloat<f32>)>) -> PiecewiseLinearMap {
        mappings.sort();
        let (from, to): (Vec<_>, Vec<_>) = mappings.into_iter().unzip();
        PiecewiseLinearMap { from, to }
    }

    pub(crate) fn len(&self) -> usize {
        self.from.len()
    }

    pub fn reverse(&self) -> PiecewiseLinearMap {
        let mappings = self
            .to
            .iter()
            .copied()
            .zip(self.from.iter().copied())
            .collect();
        PiecewiseLinearMap::new(mappings)
    }

    /// An iterator over (from, to) values.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (f32, f32)> + '_ {
        self.from
            .iter()
            .zip(self.to.iter())
            .map(|(from, to)| (from.0, to.0))
    }

    /// Based on <https://github.com/fonttools/fonttools/blob/5a0dc4bc8dfaa0c7da146cf902395f748b3cebe5/Lib/fontTools/varLib/models.py#L502>
    pub fn map(&self, value: OrderedFloat<f32>) -> OrderedFloat<f32> {
        match self.from.binary_search(&value) {
            Ok(idx) => self.to[idx], // This value is just right
            Err(idx) => {
                // idx is where we would insert from.
                // Interpolate between the values left/right of it, if any.
                let value = value.into_inner();

                // This value is too small
                if idx == 0 {
                    // FontTools: take min k and compute v + mapping[k] - k
                    return (value + self.to[0].into_inner() - self.from[0].into_inner()).into();
                }
                // This value is too big
                if idx == self.from.len() {
                    // // FontTools: take max k and compute v + mapping[k] - k
                    return (value + self.to[idx - 1].into_inner()
                        - self.from[idx - 1].into_inner())
                    .into();
                }

                // This value is between two known values and we must lerp
                let from_lhs = self.from[idx - 1].into_inner();
                let from_rhs = self.from[idx].into_inner();
                lerp(
                    self.to[idx - 1].into_inner(),
                    self.to[idx].into_inner(),
                    (value - from_lhs) / (from_rhs - from_lhs),
                )
                .into()
            }
        }
    }
}

fn lerp(a: f32, b: f32, t: f32) -> f32 {
    assert!((0_f32..=1_f32).contains(&t));
    a + t * (b - a)
}

#[cfg(test)]
mod tests {
    use ordered_float::OrderedFloat;

    use super::PiecewiseLinearMap;

    #[test]
    fn single_segment_map() {
        // User likes to use 0..10 in their sources to mean wght 0..1000 in userspace
        let from_to = vec![
            (OrderedFloat(0_f32), OrderedFloat(0_f32)),
            (OrderedFloat(10_f32), OrderedFloat(1000_f32)),
        ];
        let plm = PiecewiseLinearMap::new(from_to);

        assert_eq!(plm.map(OrderedFloat(0_f32)), OrderedFloat(0_f32));
        assert_eq!(plm.map(OrderedFloat(5_f32)), OrderedFloat(500_f32));
        assert_eq!(plm.map(OrderedFloat(10_f32)), OrderedFloat(1000_f32));

        // walk off the end, why not
        assert_eq!(plm.map(OrderedFloat(-1_f32)), OrderedFloat(-1_f32));
        assert_eq!(plm.map(OrderedFloat(20_f32)), OrderedFloat(1010_f32));
    }

    #[test]
    fn multi_segment_map() {
        // We obviously want to map -1..0 to 100.400 and 0..10 to 400..700.
        let from_to = vec![
            (OrderedFloat(-1_f32), OrderedFloat(100_f32)),
            (OrderedFloat(0_f32), OrderedFloat(400_f32)),
            (OrderedFloat(10_f32), OrderedFloat(700_f32)),
        ];
        let plm = PiecewiseLinearMap::new(from_to);

        assert_eq!(plm.map(OrderedFloat(-1_f32)), OrderedFloat(100_f32));
        assert_eq!(plm.map(OrderedFloat(0_f32)), OrderedFloat(400_f32));
        assert_eq!(plm.map(OrderedFloat(5_f32)), OrderedFloat(550_f32));
        assert_eq!(plm.map(OrderedFloat(10_f32)), OrderedFloat(700_f32));
    }
}
