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
    from: Vec<OrderedFloat<f64>>, // sorted, ||'s to
    to: Vec<OrderedFloat<f64>>,   // sorted, ||'s from
}

#[inline]
fn as_of64(value: OrderedFloat<f32>) -> OrderedFloat<f64> {
    (value.into_inner() as f64).into()
}

#[inline]
fn as_of32(value: OrderedFloat<f64>) -> OrderedFloat<f32> {
    (value.into_inner() as f32).into()
}

impl PiecewiseLinearMap {
    /// Create a new map from a series of (from, to) values.
    pub fn new(mut mappings: Vec<(OrderedFloat<f32>, OrderedFloat<f32>)>) -> PiecewiseLinearMap {
        mappings.sort();
        let (from, to): (Vec<_>, Vec<_>) = mappings
            .into_iter()
            .map(|(k, v)| (as_of64(k), as_of64(v)))
            .unzip();
        PiecewiseLinearMap { from, to }
    }

    pub(crate) fn len(&self) -> usize {
        self.from.len()
    }

    pub fn reverse(&self) -> PiecewiseLinearMap {
        let mappings = self
            .to
            .iter()
            .zip(self.from.iter())
            .map(|(k, v)| (as_of32(*k), as_of32(*v)))
            .collect();
        PiecewiseLinearMap::new(mappings)
    }

    /// An iterator over (from, to) values.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (f32, f32)> + '_ {
        self.from
            .iter()
            .zip(self.to.iter())
            .map(|(from, to)| (from.into_inner() as f32, to.into_inner() as f32))
    }

    /// Based on <https://github.com/fonttools/fonttools/blob/5a0dc4bc8dfaa0c7da146cf902395f748b3cebe5/Lib/fontTools/varLib/models.py#L502>
    pub fn map(&self, value: OrderedFloat<f32>) -> OrderedFloat<f32> {
        // perform internal computations in f64 to increase precision but keep
        // the current interface in f32; avoids issues like this:
        // https://github.com/googlefonts/fontc/issues/1117
        as_of32(self.map_impl(as_of64(value)))
    }

    fn map_impl(&self, value: OrderedFloat<f64>) -> OrderedFloat<f64> {
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

fn lerp(a: f64, b: f64, t: f64) -> f64 {
    assert!((0_f64..=1_f64).contains(&t));
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

    #[test]
    fn float_precision() {
        // https://github.com/googlefonts/fontc/issues/1117
        let from_to = vec![
            (OrderedFloat(100_f32), OrderedFloat(-1_f32)),
            (OrderedFloat(400_f32), OrderedFloat(0_f32)),
            (OrderedFloat(900_f32), OrderedFloat(1_f32)),
        ];
        let plm = PiecewiseLinearMap::new(from_to);

        assert_eq!(plm.map(OrderedFloat(200_f32)), OrderedFloat(-0.6666667_f32));
        assert_eq!(
            plm.reverse().map(OrderedFloat(-0.6666667_f32)),
            OrderedFloat(200_f32)
        );
    }
}
