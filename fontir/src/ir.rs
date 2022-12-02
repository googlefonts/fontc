//! Serde types for font IR. See TODO:PublicLink.

use std::collections::{BTreeMap, HashMap};

use serde::{Deserialize, Serialize};

///Global font info that cannot vary.
///
/// For example, upem, axis definitions, etc, as distinct from
/// metadata that varies across design space such as ascender/descender.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct StaticMetadata {}

use ordered_float::OrderedFloat;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Axis {
    pub name: String,
    pub tag: String,
    pub min: f32,
    pub default: f32,
    pub max: f32,
    pub hidden: bool,
}

// Using BTreeMap instead of HashMap and OrderedFloat instead of f32 so that
// the location is hashable and can be used as a key in Glyph::sources HashMap
pub type DesignSpaceLocation = BTreeMap<String, OrderedFloat<f32>>;

/// A variable definition of a single glyph.
///
/// Defined in at least once position in designspace. If defined in
/// many, presumed to vary continuously between positions and required
/// to have variation compatible structure.
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct Glyph {
    pub name: String,
    pub sources: HashMap<DesignSpaceLocation, GlyphInstance>,
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
    use crate::ir::Axis;

    fn test_axis() -> Axis {
        Axis {
            name: String::from("Weight"),
            tag: String::from("wght"),
            min: 100.,
            default: 400.,
            max: 900.,
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
