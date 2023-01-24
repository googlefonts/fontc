//! Serde types for font IR.

use crate::{
    coords::{CoordConverter, NormalizedLocation, UserCoord},
    error::{PathConversionError, WorkError},
    serde::{GlyphSerdeRepr, StaticMetadataSerdeRepr},
};
use fontdrasil::types::GlyphName;
use indexmap::IndexSet;
use kurbo::{Affine, BezPath, Point};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fmt::Debug,
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
    ) -> Result<(), WorkError> {
        if self.sources.contains_key(unique_location) {
            return Err(WorkError::DuplicateNormalizedLocation {
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
    pub contours: Vec<BezPath>,
    /// List of glyph components.
    pub components: Vec<Component>,
}

/// A single glyph component, reference to another glyph.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Component {
    /// The name of the referenced glyph.
    pub base: GlyphName,
    /// Affine transformation to apply to the referenced glyph.
    pub transform: Affine,
}

/// Helps convert points-of-type to a bezier path.
///
/// Source formats tend to use streams of point-of-type. Curve manipulation is
/// often easier on bezier path, so provide a mechanism to convert.
#[derive(Debug)]
pub struct GlyphPathBuilder {
    glyph_name: GlyphName,
    offcurve: Vec<Point>,
    path: BezPath,
    last_move_to: Option<Point>,
}

impl GlyphPathBuilder {
    pub fn new(glyph_name: GlyphName) -> GlyphPathBuilder {
        GlyphPathBuilder {
            glyph_name,
            offcurve: Vec::new(),
            path: BezPath::new(),
            last_move_to: None,
        }
    }

    fn check_num_offcurve(
        &self,
        expected: impl Fn(usize) -> bool,
    ) -> Result<(), PathConversionError> {
        if !expected(self.offcurve.len()) {
            return Err(PathConversionError::TooManyOffcurvePoints {
                glyph_name: self.glyph_name.clone(),
                num_offcurve: self.offcurve.len(),
                points: self.offcurve.clone(),
            });
        }
        Ok(())
    }

    pub fn is_empty(&self) -> bool {
        self.offcurve.is_empty() && self.path.elements().is_empty()
    }

    pub fn move_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.check_num_offcurve(|v| v == 0)?;
        let p = p.into();
        self.path.move_to(p);
        self.last_move_to = Some(p);
        Ok(())
    }

    pub fn line_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.check_num_offcurve(|v| v == 0)?;
        if self.is_empty() {
            self.move_to(p)?;
        } else {
            self.path.line_to(p);
        }
        Ok(())
    }

    pub fn qcurve_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        todo!("Support qcurve to {}", p.into());
    }

    /// Type of curve depends on accumulated off-curves
    ///
    /// <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn curve_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        if !self.is_empty() {
            match self.offcurve.len() {
                0 => self.path.line_to(p),
                1 => self.path.quad_to(self.offcurve[0], p.into()),
                2 => self
                    .path
                    .curve_to(self.offcurve[0], self.offcurve[1], p.into()),
                _ => self.check_num_offcurve(|v| v < 3)?,
            }
            self.offcurve.clear();
        } else {
            self.move_to(p)?;
        }
        Ok(())
    }

    pub fn offcurve(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.offcurve.push(p.into());
        Ok(())
    }

    pub fn close_path(&mut self) -> Result<(), PathConversionError> {
        // Take dangling off-curves to imply a curve back to sub-path start
        if let Some(last_move) = self.last_move_to {
            if !self.offcurve.is_empty() {
                self.curve_to(last_move)?;
            }
            self.path.close_path();
        }
        Ok(())
    }

    pub fn build(self) -> BezPath {
        self.path
    }
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
