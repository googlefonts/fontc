use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

use fontir::ir;
use norad::{
    designspace::{self, DesignSpaceDocument, Dimension},
    fontinfo::NonNegativeIntegerOrFloat,
};
use ordered_float::OrderedFloat;

use crate::error::UfoToIrError;

// TODO we will need the ability to map coordinates and a test font that does. Then no unwrap.
fn to_ir_location(loc: &[Dimension]) -> ir::DesignSpaceLocation {
    loc.iter()
        .map(|d| (d.name.clone(), OrderedFloat(d.xvalue.unwrap())))
        .collect()
}

fn load_fonts(
    designspace: &DesignSpaceDocument,
    dir: &Path,
) -> Result<HashMap<String, norad::Font>, UfoToIrError> {
    let mut font_load_params: HashMap<String, norad::DataRequest> = HashMap::new();
    for source in &designspace.sources {
        let mut datareq = font_load_params
            .remove(&source.filename)
            .unwrap_or_else(norad::DataRequest::none);
        if let Some(layer) = &source.layer {
            datareq = datareq.filter_layers(|name, _path| name == layer.clone());
        } else {
            datareq = datareq.default_layer(true);
        }
        font_load_params.insert(source.filename.clone(), datareq);
    }

    let mut fonts: HashMap<String, norad::Font> = HashMap::new();
    for (filename, datareq) in font_load_params {
        let path = dir.join(&filename);
        fonts.insert(
            filename,
            norad::Font::load_requested_data(path, datareq).map_err(UfoToIrError::UfoLoadError)?,
        );
    }

    Ok(fonts)
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

fn upem<'a, I>(fonts: I) -> Result<u16, UfoToIrError>
where
    I: Iterator<Item = &'a norad::Font>,
{
    // Optional NonNegativeIntegerOrFloat for a u16 field is super awesome
    let upem: HashSet<u16> = fonts
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
) -> Result<(ir::FontInfo, Vec<ir::Glyph>), UfoToIrError> {
    let designspace =
        DesignSpaceDocument::load(&path).map_err(UfoToIrError::DesignSpaceLoadError)?;
    let dir = path.as_ref().parent().unwrap(); // designspace *must* exist in a directory

    let axes: Vec<ir::Axis> = designspace.axes.iter().map(to_ir_axis).collect();

    // TODO: to support file-change checks we may want to do our own granular loading
    let fonts = load_fonts(&designspace, dir)?;
    let upem = upem(fonts.values())?;

    let mut glyphs = HashMap::<String, ir::Glyph>::new();
    for source in designspace.sources.iter() {
        let font = fonts.get(&source.filename).unwrap();
        let layer = match &source.layer {
            Some(l) => font
                .layers
                .get(l)
                .ok_or_else(|| UfoToIrError::LayerNotFoundError(l.clone()))?,
            None => font.default_layer(),
        };
        for norad_glyph in layer.iter() {
            let name = norad_glyph.name().to_string();
            if !glyphs.contains_key(&name) {
                glyphs.insert(
                    name.clone(),
                    ir::Glyph {
                        name: name.clone(),
                        sources: HashMap::new(),
                    },
                );
            }

            let glyph = glyphs.get_mut(&name).unwrap();

            let glyph_instance = ir::GlyphInstance {
                width: None,
                height: None,
            };

            let location = to_ir_location(&source.location);
            if glyph.sources.contains_key(&location) {
                return Err(UfoToIrError::DuplicateLocationError);
            }

            glyph.sources.insert(location.clone(), glyph_instance);
        }
    }

    Ok((ir::FontInfo { upem, axes }, glyphs.into_values().collect()))
}

fn to_ir_axis(axis: &designspace::Axis) -> ir::Axis {
    ir::Axis {
        name: axis.name.clone(),
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
    use std::path::PathBuf;

    fn testdata_dir() -> PathBuf {
        let path = PathBuf::from("../resources/testdata")
            .canonicalize()
            .unwrap();
        assert!(path.is_dir(), "{:#?} isn't a dir", path);
        path
    }

    #[test]
    fn simple_wght_variable() {
        let designspace_path = testdata_dir().join("wght_var.designspace");
        let (font, glyphs) = designspace_to_ir(designspace_path).unwrap();
        assert_eq!(1000, font.upem);
        assert_eq!(
            vec![ir::Axis {
                name: "Weight".to_string(),
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
