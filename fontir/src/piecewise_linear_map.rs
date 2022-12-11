//! Remaps values using a series of linear mappings.
//! 
//! Useful for things like designspace : userspace mapping.

use ordered_float::OrderedFloat;

pub struct PiecewiseLinearMap {
    from: Vec<OrderedFloat<f64>>, // sorted, ||'s to
    to: Vec<OrderedFloat<f64>>,   // sorted, ||'s from
}

impl PiecewiseLinearMap {
    /// Create a new map from a series of (from, to) values.
    pub fn new(mut mappings: Vec<(OrderedFloat<f64>, OrderedFloat<f64>)>) -> PiecewiseLinearMap {
        mappings.sort();
        let (from, to): (Vec<_>, Vec<_>) = mappings.into_iter().unzip();
        PiecewiseLinearMap { from, to }
    }
    

    pub fn reverse(&self) -> PiecewiseLinearMap {
        let mappings = self.to.iter().copied().zip(self.from.iter().copied())
            .collect();
        PiecewiseLinearMap::new(mappings)
    }
}

fn lerp(a: f64, b: f64, t: f64) -> f64 {
    assert!(0_f64 <= t && 1_f64 >= t);
    a + t * (b - a)
}

impl PiecewiseLinearMap {
    /// Based on https://github.com/fonttools/fonttools/blob/5a0dc4bc8dfaa0c7da146cf902395f748b3cebe5/Lib/fontTools/varLib/models.py#L502
    pub fn map(&self, value: OrderedFloat<f64>) -> OrderedFloat<f64> {
        match self.from.binary_search(&value) {
            Ok(idx) => self.to[idx],  // This value is just right
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
                    return (value + self.to[idx - 1].into_inner() - self.from[idx - 1].into_inner()).into();
                }

                // This value is between two known values and we must lerp
                let from_lhs = self.from[idx - 1].into_inner();
                let from_rhs = self.from[idx].into_inner();
                lerp(
                    self.to[idx - 1].into_inner(), 
                    self.to[idx].into_inner(), 
                    (value - from_lhs) / (from_rhs - from_lhs)).into()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use ordered_float::OrderedFloat;

    use super::PiecewiseLinearMap;


    #[test]
    fn single_segment_map() {
        // User likes to map wght 0..1000 onto 0..10
        let mut from_to: Vec<(OrderedFloat<f64>, OrderedFloat<f64>)> = Vec::new();
        from_to.push((OrderedFloat(0_f64), OrderedFloat(0_f64)));
        from_to.push((OrderedFloat(10_f64), OrderedFloat(1000_f64)));
        let plm = PiecewiseLinearMap::new(from_to);

        assert_eq!(plm.map(OrderedFloat(0_f64)), OrderedFloat(0_f64));
        assert_eq!(plm.map(OrderedFloat(5_f64)), OrderedFloat(500_f64));
        assert_eq!(plm.map(OrderedFloat(10_f64)), OrderedFloat(1000_f64));

        // walk off the end, why not
        assert_eq!(plm.map(OrderedFloat(-1_f64)), OrderedFloat(-1_f64));
        assert_eq!(plm.map(OrderedFloat(20_f64)), OrderedFloat(1010_f64));
    }

    #[test]
    fn multi_segment_map() {
        // We obviously want to map -1..0 to 100.400 and 0..10 to 400..700.
        let mut from_to: Vec<(OrderedFloat<f64>, OrderedFloat<f64>)> = Vec::new();
        from_to.push((OrderedFloat(-1_f64), OrderedFloat(100_f64)));
        from_to.push((OrderedFloat(0_f64), OrderedFloat(400_f64)));
        from_to.push((OrderedFloat(10_f64), OrderedFloat(700_f64)));
        let plm = PiecewiseLinearMap::new(from_to);

        assert_eq!(plm.map(OrderedFloat(-1_f64)), OrderedFloat(100_f64));
        assert_eq!(plm.map(OrderedFloat(0_f64)), OrderedFloat(400_f64));
        assert_eq!(plm.map(OrderedFloat(5_f64)), OrderedFloat(550_f64));
        assert_eq!(plm.map(OrderedFloat(10_f64)), OrderedFloat(700_f64));
    }
}