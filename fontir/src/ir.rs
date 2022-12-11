//! Serde types for font IR.

use crate::{
    coords::{UserSpaceCoord, UserSpaceLocation},
    error::Error,
    serde::StaticMetadataSerdeRepr,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Global font info that cannot vary.
///
/// For example, upem, axis definitions, etc, as distinct from
/// metadata that varies across design space such as ascender/descender.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(from = "StaticMetadataSerdeRepr", into = "StaticMetadataSerdeRepr")]
pub struct StaticMetadata {
    pub axes: Vec<Axis>,
    pub glyph_order: Vec<String>,
    pub gid_by_name: HashMap<String, u32>,
}

impl StaticMetadata {
    pub fn new(axes: Vec<Axis>, glyph_order: Vec<String>) -> StaticMetadata {
        let gid_by_name = glyph_order
            .iter()
            .enumerate()
            .map(|(idx, name)| (name.clone(), idx as u32))
            .collect();
        StaticMetadata {
            axes,
            glyph_order,
            gid_by_name,
        }
    }
}

impl StaticMetadata {
    pub fn glyph_id(&self, name: &String) -> Option<u32> {
        self.gid_by_name.get(name).copied()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Axis {
    pub name: String,
    pub tag: String,
    pub min: UserSpaceCoord,
    pub default: UserSpaceCoord,
    pub max: UserSpaceCoord,
    pub hidden: bool,
}

/// A variable definition of a single glyph.
///
/// Defined in at least once position in designspace. If defined in
/// many, presumed to vary continuously between positions and required
/// to have variation compatible structure.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Glyph {
    pub name: String,
    pub sources: HashMap<UserSpaceLocation, GlyphInstance>,
}

impl Glyph {
    pub fn new(name: String) -> Self {
        Self {
            name,
            sources: HashMap::new(),
        }
    }

    pub fn try_add_source(
        &mut self,
        unique_location: &UserSpaceLocation,
        source: GlyphInstance,
    ) -> Result<(), Error> {
        if self.sources.contains_key(unique_location) {
            return Err(Error::DuplicateUserSpaceLocation {
                what: format!("glyph '{}' source", self.name),
                loc: unique_location.clone(),
            });
        }
        self.sources.insert(unique_location.clone(), source);
        Ok(())
    }
}

/// A Glyph at a specific position in designspace.
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
pub struct GlyphInstance {
    /// Advance width.
    pub width: f64,
    /// Advance height; if None, assumed to equal font's ascender - descende.
    pub height: Option<f64>,
    /// List of glyph contours.
    pub contours: Vec<Contour>,
    /// List of glyph components.
    pub components: Vec<Component>,
}

/// A single glyph contour consisting of a list of points.
pub type Contour = Vec<ContourPoint>;

/// A single point in a glyph contour.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct ContourPoint {
    pub x: f64,
    pub y: f64,
    pub typ: PointType,
}

/// Possible types of a point in a glyph contour, following UFO GLIF semantics.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum PointType {
    Move,
    Line,
    OffCurve,
    Curve,
    QCurve,
}

/// A single glyph component, reference to another glyph.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Component {
    /// The name of the referenced glyph.
    pub base: String,
    /// Affine transformation to apply to the referenced glyph.
    pub transform: Affine2x3,
}

/// A 2×3 affine transformation matrix.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Affine2x3 {
    /// x-component of transformed x-basis vector.
    pub xx: f64,
    /// y-component of transformed x-basis vector.
    pub yx: f64,
    /// x-component of transformed y-basis vector.
    pub xy: f64,
    /// y-component of transformed y-basis vector.
    pub yy: f64,
    /// x-component of translation vector.
    pub dx: f64,
    /// y-component of translation vector.
    pub dy: f64,
}

#[cfg(test)]
mod tests {
    use ordered_float::OrderedFloat;

    use crate::{coords::UserSpaceCoord, ir::Axis};

    fn user_coord(v: f32) -> UserSpaceCoord {
        UserSpaceCoord::new(OrderedFloat(v))
    }

    fn test_axis() -> Axis {
        Axis {
            name: String::from("Weight"),
            tag: String::from("wght"),
            min: user_coord(100_f32),
            default: user_coord(400_f32),
            max: user_coord(900_f32),
            hidden: false,
        }
    }

    #[test]
    fn axis_yaml() {
        let test_axis = test_axis();
        let yml = serde_yaml::to_string(&test_axis).unwrap();
        assert_eq!(test_axis, serde_yaml::from_str(&yml).unwrap());
    }

    #[test]
    fn axis_bincode() {
        let test_axis = test_axis();
        let bin = bincode::serialize(&test_axis).unwrap();
        assert_eq!(test_axis, bincode::deserialize(&bin).unwrap());
    }
}
