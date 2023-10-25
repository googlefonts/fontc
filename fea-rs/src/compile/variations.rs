//! compiling variable fonts

use std::collections::{BTreeMap, HashMap};

use ordered_float::OrderedFloat;
use write_fonts::{
    tables::variations::VariationRegion,
    types::{F2Dot14, Fixed, Tag},
};

/// A trait for providing variable font information to the compiler.
///
/// In order to compile a variable font, we need to know what axes
/// exist, what ranges are valid, how to map from user to normalized coordinates,
/// and potentially other things that are not part of the FEA file.
///
/// This trait abstracts over that info.
pub trait VariationInfo {
    /// If the tag is an axis in this font, return the min/max values from fvar
    fn axis_info(&self, axis_tag: Tag) -> Option<AxisInfo>;

    /// Return the normalized value for a user coordinate for the given axis.
    ///
    /// NOTE: This is only used for computing the normalized values for ConditionSets.
    ///
    /// NOTE: if unit suffixes become a thing, and ConditionSets remain a thing, then
    /// this should use the same AxisLocation enum as below.
    fn normalize_coordinate(&self, axis_tag: Tag, value: Fixed) -> F2Dot14;

    /// Compute default & deltas for a set of locations and values in variation space.
    ///
    /// On success, returns the default value for this set of locations, as well
    /// as a set of deltas suitable for inclusing in an `ItemVariationStore`.
    fn resolve_variable_metric(
        &self,
        locations: &HashMap<Location, i16>,
    ) -> Result<(i16, Vec<(VariationRegion, i16)>), AnyError>;
}

type AnyError = Box<dyn std::error::Error>;

// btreemap only because hashmap is not hashable
type Location = BTreeMap<Tag, AxisLocation>;

/// A location on an axis, in one of three coordinate spaces
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AxisLocation {
    /// A position in the user coordinate space
    User(OrderedFloat<f32>),
    /// A position in the design coordinate space
    Design(OrderedFloat<f32>),
    /// A normalized position
    Normalized(OrderedFloat<f32>),
}

/// Information about a paritcular axis in a variable font.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AxisInfo {
    /// The index in the fvar table of this axis
    pub index: u16,
    /// The minimum value for this axis, in user coordinates
    pub min_value: Fixed,
    /// The default value for this axis, in user coordinates
    pub default_value: Fixed,
    /// The maximum value for this axis, in user coordinates
    pub max_value: Fixed,
}

/// A type that implements [`VariationInfo`], for testing and debugging.
#[derive(Clone, Debug, Default)]
pub struct MockVariationInfo {
    // Note: This is not considered public API for the purposes of semvar
    #[doc(hidden)]
    pub axes: Vec<(Tag, AxisInfo)>,
}

impl MockVariationInfo {
    /// input is a tuple of (tag, min, default, max)
    #[cfg(any(test, feature = "test"))]
    pub(crate) fn new(raw: &[(&str, i16, i16, i16)]) -> Self {
        Self {
            axes: raw
                .iter()
                .enumerate()
                .map(|(i, (tag, min, default, max))| {
                    (
                        Tag::new_checked(tag.as_bytes()).unwrap(),
                        AxisInfo {
                            index: i as u16,
                            min_value: Fixed::from_i32(*min as _),
                            default_value: Fixed::from_i32(*default as _),
                            max_value: Fixed::from_i32(*max as _),
                        },
                    )
                })
                .collect(),
        }
    }

    /// parse the custom text format that we accept on the CLI.
    ///
    /// If there's an error, returns the line number and description of the
    /// problem (this is only used in the compile binary, which defines the)
    /// actual error type, so we don't have access to that from here)
    ///
    /// The input format here is plaintext, where each line contains
    /// info for one axis, in the format `$TAG $MIN_VALUE $DEFAULT_VALUE $MAX_VALUE`.
    /// The axes are in order. All values are in user coordinates.
    #[cfg(any(test, feature = "cli"))]
    pub fn from_cli_input(input_file: &str) -> Result<Self, (usize, String)> {
        // parse a number that might be a float or an int
        fn parse_fixed(s: &str, line: usize) -> Result<Fixed, (usize, String)> {
            if let Ok(val) = s.parse::<f64>() {
                return Ok(Fixed::from_f64(val));
            }
            s.parse::<i32>()
                .map(Fixed::from_i32)
                .map_err(|_| (line, format!("failed to parse number '{s}'")))
        }

        let mut axes = Vec::new();
        for (i, line) in input_file.lines().enumerate() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            let mut items = line.split(' ');
            match (items.next(), items.next(), items.next(), items.next()) {
                (Some(tag), Some(min), Some(default), Some(max)) => {
                    let tag = tag
                        .parse::<Tag>()
                        .map_err(|e| (i, format!("failed to parse tag: '{e}'")))?;
                    let axis_info = AxisInfo {
                        index: axes.len() as u16,
                        min_value: parse_fixed(min, i)?,
                        default_value: parse_fixed(default, i)?,
                        max_value: parse_fixed(max, i)?,
                    };
                    axes.push((tag, axis_info))
                }
                _ => Err((i, ("expected four space separated words".to_string())))?,
            };
            if let Some(huh) = items.next() {
                return Err((i, format!("unexpected text '{huh}'")));
            }
        }

        Ok(MockVariationInfo { axes })
    }
}

impl VariationInfo for MockVariationInfo {
    fn axis_info(&self, axis_tag: Tag) -> Option<AxisInfo> {
        self.axes.iter().find_map(
            |(tag, axis)| {
                if *tag == axis_tag {
                    Some(*axis)
                } else {
                    None
                }
            },
        )
    }

    fn normalize_coordinate(&self, axis_tag: Tag, value: Fixed) -> F2Dot14 {
        let Some(AxisInfo {
            min_value,
            default_value,
            max_value,
            ..
        }) = self.axis_info(axis_tag)
        else {
            return F2Dot14::ZERO;
        };

        use core::cmp::Ordering::*;
        // Make sure max is >= min to avoid potential panic in clamp.
        let max_value = max_value.max(min_value);
        let value = value.clamp(min_value, max_value);
        let value = match value.cmp(&default_value) {
            Less => -((default_value - value) / (default_value - min_value)),
            Greater => (value - default_value) / (max_value - default_value),
            Equal => Fixed::ZERO,
        };
        value.clamp(-Fixed::ONE, Fixed::ONE).to_f2dot14()
    }

    fn resolve_variable_metric(
        &self,
        _locations: &HashMap<Location, i16>,
    ) -> Result<(i16, Vec<(VariationRegion, i16)>), Box<(dyn std::error::Error + 'static)>> {
        Ok(Default::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn axis_input_format() {
        let s = "wght 100 200 400";
        let parsed = MockVariationInfo::from_cli_input(s).unwrap();
        assert_eq!(parsed.axes[0].0, Tag::new(b"wght"));

        let twotimes = "wght 100 200 400\nwdth 50 55.5 12111";
        let parsed = MockVariationInfo::from_cli_input(twotimes).unwrap();
        assert_eq!(parsed.axes[1].1.max_value.to_i32(), 12111);
    }

    #[test]
    fn bad_input() {
        for (input, err_string_match) in [
            ("wght 100 200", "expected four space separated words"),
            ("wght 100 200 huh", "failed to parse number 'huh'"),
            ("cooltag 100 200 300", "failed to parse tag"),
            ("wdth 100 200 300 400", "unexpected text '400'"),
        ] {
            match MockVariationInfo::from_cli_input(input) {
                Ok(_) => panic!("unexpectedly parsed {input}"),
                Err((_, msg)) => assert!(
                    msg.contains(err_string_match),
                    "'{msg}' does not contain '{err_string_match}'"
                ),
            }
        }
    }
}
