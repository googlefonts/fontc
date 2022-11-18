use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use fontir::ir;
use norad::{
    designspace::{self, DesignSpaceDocument, Dimension},
    fontinfo::NonNegativeIntegerOrFloat,
};

use crate::error::UfoToIrError;

struct Font {
    path: PathBuf,
    location: ir::DesignSpaceLocation,
    font_info: norad::FontInfo,
}

// TODO we will need the ability to map coordinates and a test font that does. Then no unwrap.
fn to_ir_location(loc: &[Dimension]) -> ir::DesignSpaceLocation {
    loc.iter()
        .map(|d| (d.name.clone(), d.xvalue.unwrap()))
        .collect()
}

fn fonts(designspace: &DesignSpaceDocument, dir: &Path) -> Result<Vec<Font>, UfoToIrError> {
    let datareq = norad::DataRequest::none();
    designspace
        .sources
        .iter()
        .map(|s| (dir.join(&s.filename), to_ir_location(&s.location)))
        .map(|(p, l)| {
            norad::Font::load_requested_data(&p, &datareq).map(|f| Font {
                path: p,
                location: l,
                font_info: f.font_info,
            })
        })
        .collect::<Result<Vec<Font>, _>>()
        .map_err(UfoToIrError::UfoLoadError)
}

fn extract_upem(val: NonNegativeIntegerOrFloat) -> Result<u16, UfoToIrError> {
    let val = val.as_f64();
    if val > u16::MAX.into() {
        return Err(UfoToIrError::AmbiguousUpemError);
    }
    if val - val.trunc() > f64::EPSILON {
        return Err(UfoToIrError::AmbiguousUpemError);
    }
    Ok(val as u16)
}

fn upem(fonts: &[Font]) -> Result<u16, UfoToIrError> {
    // Optional NonNegativeIntegerOrFloat for a u16 field is super awesome
    let upem: HashSet<u16> = fonts
        .iter()
        .filter_map(|f| f.font_info.units_per_em)
        .map(extract_upem)
        .collect::<Result<HashSet<u16>, _>>()?;
    if upem.len() != 1 {
        return Err(UfoToIrError::AmbiguousUpemError);
    }
    Ok(*upem.iter().next().unwrap())
}

pub fn designspace_to_ir(
    path: impl AsRef<Path>,
) -> Result<(ir::Font, Vec<ir::Glyph>), UfoToIrError> {
    let designspace =
        DesignSpaceDocument::load(&path).map_err(UfoToIrError::DesignSpaceLoadError)?;
    let dir = path.as_ref().parent().unwrap(); // designspace *must* exist in a directory

    let axes: Vec<ir::Axis> = designspace.axes.iter().map(to_ir_axis).collect();

    // TODO: to support file-change checks we may want to do our own granular loading
    let fonts = fonts(&designspace, dir)?;
    let upem = upem(&fonts)?;

    let mut glyphs = HashMap::<String, ir::Glyph>::new();
    for font in fonts.iter() {
        let glyph_dir = font.path.join("glyphs");
        for glif_file in fs::read_dir(glyph_dir).map_err(UfoToIrError::IoError)? {
            let glif_file = glif_file.map_err(UfoToIrError::IoError)?.path();
            let norad_glyph =
                norad::Glyph::load(&glif_file).map_err(UfoToIrError::GlifLoadError)?;
            eprintln!("{:#?} {:#?}", glif_file, norad_glyph.name());
            let name = norad_glyph.name().as_str().to_owned();
            if !glyphs.contains_key(&name) {
                glyphs.insert(
                    name.clone(),
                    ir::Glyph {
                        name: name.clone(),
                        instances: Vec::new(),
                    },
                );
            }

            let glyph = glyphs.get_mut(&name).unwrap();

            let glyph_instance = ir::GlyphInstance {
                location: font.location.clone(),
                width: None,
                height: None,
            };

            glyph.instances.push(glyph_instance);
        }
    }

    Ok((ir::Font { upem, axes }, glyphs.into_values().collect()))
}

fn to_ir_axis(axis: &designspace::Axis) -> ir::Axis {
    ir::Axis {
        tag: axis.tag.clone(),
        min: axis.minimum.expect("Discrete axes not supported yet"),
        default: axis.default,
        max: axis.maximum.expect("Discrete axes not supported yet"),
        hidden: axis.hidden,
    }
}

#[cfg(test)]
mod tests {
    use crate::toir::designspace_to_ir;
    use fontir::ir;

    #[test]
    fn simple_wght_variable() {
        let (font, glyphs) = designspace_to_ir("testdata/wght_var.designspace").unwrap();
        assert_eq!(1000, font.upem);
        assert_eq!(
            vec![ir::Axis {
                tag: "wght".to_string(),
                min: 400.,
                default: 400.,
                max: 700.,
                hidden: false
            }],
            font.axes
        );
        assert_eq!(1, glyphs.len());
    }
}
