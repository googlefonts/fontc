//! compiling variable fonts

use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use fontdrasil::{coords::NormalizedLocation, types::Axis};
use ordered_float::OrderedFloat;
use write_fonts::{tables::variations::VariationRegion, types::Tag};

type DefaultAndVariations = (i16, Vec<(VariationRegion, i16)>);

/// A trait for providing variable font information to the compiler.
///
/// In order to compile a variable font, we need to know what axes
/// exist, what ranges are valid, how to map from user to normalized coordinates,
/// and potentially other things that are not part of the FEA file.
///
/// This trait abstracts over that info.
pub trait VariationInfo {
    /// The error type
    type Error: std::error::Error;

    /// The number of axes in the fvar table
    fn axis_count(&self) -> u16;

    /// If the tag is an axis in this font, it's fvar index and it's [`Axis`] data.
    fn axis(&self, axis_tag: Tag) -> Option<(usize, &Axis)>;

    /// Compute default & deltas for a set of locations and values in variation space.
    ///
    /// On success, returns the default value for this set of locations, as well
    /// as a set of deltas suitable for inclusing in an `ItemVariationStore`.
    fn resolve_variable_metric(
        &self,
        locations: &HashMap<NormalizedLocation, i16>,
    ) -> Result<DefaultAndVariations, Self::Error>;
}

#[derive(Debug)]
pub struct NopError;

impl std::error::Error for NopError {}

impl Display for NopError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("nop")
    }
}

/// Inert variation info
pub struct NopVariationInfo;

impl VariationInfo for NopVariationInfo {
    type Error = NopError;

    fn axis_count(&self) -> u16 {
        0
    }

    fn axis(&self, _: Tag) -> Option<(usize, &Axis)> {
        None
    }

    fn resolve_variable_metric(
        &self,
        _: &HashMap<NormalizedLocation, i16>,
    ) -> Result<DefaultAndVariations, Self::Error> {
        Ok((0, Default::default()))
    }
}

/// A type that implements [`VariationInfo`], for testing and debugging.
#[derive(Clone, Debug, Default)]
pub struct MockVariationInfo {
    // Note: This is not considered public API for the purposes of semvar
    #[doc(hidden)]
    pub axes: Vec<Axis>,
}

/// A location on an axis, in one of three coordinate spaces, as specified in a fea file.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AxisLocation {
    /// A position in the user coordinate space
    User(OrderedFloat<f32>),
    /// A position in the design coordinate space
    Design(OrderedFloat<f32>),
    /// A normalized position
    Normalized(OrderedFloat<f32>),
}

/// Create an axis where user coords == design coords
#[cfg(any(test, feature = "test", feature = "cli"))]
fn simple_axis(tag: Tag, min: i16, default: i16, max: i16) -> Axis {
    use fontdrasil::coords::{CoordConverter, DesignCoord, UserCoord};

    let min = UserCoord::new(min);
    let default = UserCoord::new(default);
    let max = UserCoord::new(max);
    Axis {
        name: tag.to_string(),
        tag,
        min,
        default,
        max,
        hidden: false,
        converter: CoordConverter::new(
            vec![
                (min, DesignCoord::new(min.into_inner())),
                (default, DesignCoord::new(default.into_inner())),
                (max, DesignCoord::new(max.into_inner())),
            ],
            1,
        ),
    }
}

impl MockVariationInfo {
    /// input is a tuple of (tag, min, default, max)
    #[cfg(any(test, feature = "test"))]
    pub(crate) fn new(raw: &[(&str, i16, i16, i16)]) -> Self {
        Self {
            axes: raw
                .iter()
                .map(|(tag, min, default, max)| {
                    simple_axis(
                        Tag::new_checked(tag.as_bytes()).unwrap(),
                        *min,
                        *default,
                        *max,
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
        use write_fonts::types::Fixed;

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
                    axes.push(simple_axis(
                        tag,
                        parse_fixed(min, i)?.to_i32() as i16,
                        parse_fixed(default, i)?.to_i32() as i16,
                        parse_fixed(max, i)?.to_i32() as i16,
                    ));
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
    type Error = NopError;

    fn axis(&self, axis_tag: Tag) -> Option<(usize, &Axis)> {
        self.axes.iter().enumerate().find_map(|(i, axis)| {
            if axis_tag == axis.tag {
                Some((i, axis))
            } else {
                None
            }
        })
    }

    fn resolve_variable_metric(
        &self,
        _locations: &HashMap<NormalizedLocation, i16>,
    ) -> Result<(i16, Vec<(VariationRegion, i16)>), Self::Error> {
        Ok(Default::default())
    }

    fn axis_count(&self) -> u16 {
        self.axes.len().try_into().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn axis_input_format() {
        let s = "wght 100 200 400";
        let parsed = MockVariationInfo::from_cli_input(s).unwrap();
        assert_eq!(parsed.axes[0].tag, Tag::new(b"wght"));

        let twotimes = "wght 100 200 400\nwdth 50 55.5 12111";
        let parsed = MockVariationInfo::from_cli_input(twotimes).unwrap();
        assert_eq!(parsed.axes[1].max.into_inner(), 12111.0);
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
