//! Serde types for font IR.

use crate::{
    coords::{CoordConverter, NormalizedLocation, UserCoord},
    error::Error,
    serde::{GlyphSerdeRepr, StaticMetadataSerdeRepr},
};
use fontdrasil::types::GlyphName;
use indexmap::IndexSet;
use kurbo::Affine;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

/// Global font info that cannot vary.
///
/// For example, upem, axis definitions, etc, as distinct from
/// metadata that varies across design space such as ascender/descender.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(from = "StaticMetadataSerdeRepr", into = "StaticMetadataSerdeRepr")]
pub struct StaticMetadata {
    pub axes: Vec<Axis>,
    pub glyph_order: IndexSet<GlyphName>,
}

impl StaticMetadata {
    pub fn new(axes: Vec<Axis>, glyph_order: IndexSet<GlyphName>) -> StaticMetadata {
        StaticMetadata { axes, glyph_order }
    }
}

impl StaticMetadata {
    pub fn glyph_id(&self, name: &GlyphName) -> Option<u32> {
        self.glyph_order.get_index_of(name).map(|i| i as u32)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Axis {
    pub name: String,
    pub tag: String,
    pub min: UserCoord,
    pub default: UserCoord,
    pub max: UserCoord,
    pub hidden: bool,
    pub converter: CoordConverter,
}

/// Features (Adobe fea).
///
/// In time will split gpos/gsub, have different features for different
/// locations, etc.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub enum Features {
    Empty,
    File(PathBuf),
    Memory(String),
}

impl Features {
    pub fn empty() -> Features {
        Features::Empty
    }
    pub fn from_file(file: &Path) -> Features {
        Features::File(file.to_path_buf())
    }
    pub fn from_string(fea_content: String) -> Features {
        Features::Memory(fea_content)
    }
}

/// A variable definition of a single glyph.
///
/// Defined in at least one position. If defined in
/// many, presumed to vary continuously between positions and required
/// to have variation compatible structure.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(from = "GlyphSerdeRepr", into = "GlyphSerdeRepr")]
pub struct Glyph {
    pub name: GlyphName,
    pub sources: HashMap<NormalizedLocation, GlyphInstance>,
}

impl Glyph {
    pub fn new(name: GlyphName) -> Self {
        Self {
            name,
            sources: HashMap::new(),
        }
    }

    pub fn try_add_source(
        &mut self,
        unique_location: &NormalizedLocation,
        source: GlyphInstance,
    ) -> Result<(), Error> {
        if self.sources.contains_key(unique_location) {
            return Err(Error::DuplicateNormalizedLocation {
                what: format!("glyph '{}' source", self.name.as_str()),
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
    pub transform: Affine,
}

#[cfg(test)]
mod tests {

    use crate::{
        coords::{CoordConverter, UserCoord},
        ir::Axis,
    };

    fn test_axis() -> Axis {
        let min = UserCoord::new(100.0);
        let default = UserCoord::new(400.0);
        let max = UserCoord::new(900.0);
        let converter = CoordConverter::unmapped(min, default, max);
        Axis {
            name: String::from("Weight"),
            tag: String::from("wght"),
            min,
            default,
            max,
            hidden: false,
            converter,
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
