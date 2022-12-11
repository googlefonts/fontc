use fontir::error::{Error, WorkError};
use fontir::ir::{Axis, StaticMetadata};
use fontir::orchestration::Context;
use fontir::source::{Input, Paths, Source, Work};
use fontir::stateset::StateSet;
use glyphs_reader::{Font, Plist};
use log::{debug, warn};
use ordered_float::OrderedFloat;
use std::collections::HashSet;
use std::sync::Arc;
use std::{collections::HashMap, fs, path::PathBuf};

pub struct GlyphsIrSource {
    glyphs_file: PathBuf,
    ir_paths: Paths,
    cache: Option<Cache>,
}

impl GlyphsIrSource {
    pub fn new(glyphs_file: PathBuf, ir_paths: Paths) -> GlyphsIrSource {
        GlyphsIrSource {
            glyphs_file,
            ir_paths,
            cache: None,
        }
    }
}

struct Cache {
    global_metadata: StateSet,
    font: Arc<Font>,
}

impl Cache {
    fn is_valid_for(&self, global_metadata: &StateSet) -> bool {
        self.global_metadata == *global_metadata
    }
}

fn glyph_identifier(glyph_name: &str) -> String {
    format!("/glyph/{glyph_name}")
}

fn glyph_states(font: &Font) -> Result<HashMap<String, StateSet>, Error> {
    let mut glyph_states = HashMap::new();

    for glyph in font.glyphs.iter() {
        let mut state = StateSet::new();
        state.track_memory(glyph_identifier(&glyph.glyphname), &glyph)?;
        glyph_states.insert(glyph.glyphname.clone(), state);
    }

    Ok(glyph_states)
}

impl GlyphsIrSource {
    // When things like upem may have changed forget incremental and rebuild the whole thing
    fn global_rebuild_triggers(&self, font: &Font) -> Result<StateSet, Error> {
        // Naive mk1: if anything other than glyphs and date changes do a global rebuild
        // TODO experiment with actual glyphs saves to see what makes sense
        let mut state = StateSet::new();
        state.track_memory("/font_master".to_string(), &font.font_master)?;
        for (key, plist) in font.other_stuff.iter() {
            if key == "date" {
                continue;
            }
            state.track_memory(format!("/{}", key), &plist)?;
        }
        Ok(state)
    }
}

impl Source for GlyphsIrSource {
    fn inputs(&mut self) -> Result<Input, Error> {
        // We have to read the glyphs file then shred it to figure out if anything changed
        let font = Font::load(&self.glyphs_file).map_err(|e| {
            Error::ParseError(
                self.glyphs_file.clone(),
                format!("Unable to read glyphs file: {}", e),
            )
        })?;
        let glyphs = glyph_states(&font)?;
        let global_metadata = self.global_rebuild_triggers(&font)?;

        self.cache = Some(Cache {
            global_metadata: global_metadata.clone(),
            font: Arc::from(font),
        });

        Ok(Input {
            global_metadata,
            glyphs,
        })
    }

    fn create_static_metadata_work(&self, context: &Context) -> Result<Box<dyn Work>, Error> {
        todo!()
    }

    fn create_glyph_ir_work(
        &self,
        glyph_names: &HashSet<&str>,
        input: &Input,
    ) -> Result<Vec<Box<dyn Work>>, fontir::error::Error> {
        let mut work: Vec<Box<dyn Work>> = Vec::new();

        // Do we have a plist cache?
        // TODO: consider just recomputing here instead of failing
        if !self
            .cache
            .as_ref()
            .map(|pc| pc.is_valid_for(&input.global_metadata))
            .unwrap_or(false)
        {
            return Err(Error::UnableToCreateGlyphIrWork);
        }

        for glyph_name in glyph_names {
            work.push(Box::from(
                self.create_work_for_one_glyph(glyph_name, input)?,
            ));
        }

        Ok(work)
    }
}

impl GlyphsIrSource {
    fn create_work_for_one_glyph(
        &self,
        glyph_name: &str,
        input: &Input,
    ) -> Result<GlyphIrWork, Error> {
        let glyph_name = glyph_name.to_string();
        let _stateset = input
            .glyphs
            .get(&glyph_name)
            .ok_or_else(|| Error::NoStateForGlyph(glyph_name.clone()))?;

        Ok(GlyphIrWork {
            glyph_name: glyph_name.clone(),
            ir_file: self.ir_paths.glyph_ir_file(&glyph_name),
        })
    }
}

struct StaticMetadataWork {
    font: Arc<Font>,
}

impl Work for StaticMetadataWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        let font = self.font.as_ref();
        debug!("Static metadata for {}", font.family_name);

        let mut axis_values = Vec::new();
        for master in font.font_master.iter() {
            master
                .axes_values
                .as_ref()
                .ok_or_else(|| WorkError::InconsistentAxisDefinitions)?
                .iter()
                .enumerate()
                .for_each(|(idx, value)| {
                    while axis_values.len() < idx {
                        axis_values.push(Vec::new());
                    }
                    axis_values[idx].push(value);
                });
        }

        let axes = font.axes.as_ref().ok_or(WorkError::NoAxisDefinitions)?;
        if axes.is_empty() {
            return Err(WorkError::NoAxisDefinitions);
        }
        if axis_values.iter().any(|av| axes.len() != av.len()) {
            return Err(WorkError::InconsistentAxisDefinitions);
        }

        let default_master_idx = font.default_master_idx();

        let axes = axes
            .iter()
            .enumerate()
            .map(|(idx, a)| {
                let min = axis_values[idx]
                    .iter()
                    .map(|v| OrderedFloat::<f32>(v.into_inner() as f32))
                    .min()
                    .unwrap();
                let max = axis_values[idx]
                    .iter()
                    .map(|v| OrderedFloat::<f32>(v.into_inner() as f32))
                    .max()
                    .unwrap();
                let default =
                    OrderedFloat::<f32>(axis_values[default_master_idx][idx].into_inner() as f32);

                Axis {
                    name: a.name.clone(),
                    tag: a.tag.clone(),
                    hidden: a.hidden.unwrap_or(false),
                    min,
                    default,
                    max,
                }
            })
            .collect();
        context.set_static_metadata(StaticMetadata {
            axes,
            glyph_order: Vec::new(), // TODO Glyph Order
        });
        Ok(())
    }
}

struct GlyphIrWork {
    glyph_name: String,
    ir_file: PathBuf,
}

impl Work for GlyphIrWork {
    fn exec(&self, _: &Context) -> Result<(), WorkError> {
        debug!("Generate {:#?} for {}", self.ir_file, self.glyph_name);
        fs::write(&self.ir_file, &self.glyph_name).map_err(WorkError::IoError)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        path::{Path, PathBuf},
    };

    use fontir::stateset::StateSet;
    use glyphs_reader::Font;

    use super::glyph_states;

    use pretty_assertions::assert_eq;

    fn testdata_dir() -> PathBuf {
        let dir = Path::new("../resources/testdata");
        assert!(dir.is_dir());
        dir.to_path_buf()
    }

    fn glyphs3_dir() -> PathBuf {
        testdata_dir().join("glyphs3")
    }

    fn glyph_state_for_file(dir: &Path, filename: &str) -> HashMap<String, StateSet> {
        let glyphs_file = dir.join(filename);
        let font = Font::load(&glyphs_file).unwrap();
        glyph_states(&font).unwrap()
    }

    #[test]
    fn find_glyphs() {
        let expected_keys = HashSet::from(["space", "hyphen", "exclam"]);
        assert_eq!(
            expected_keys,
            glyph_state_for_file(&glyphs3_dir(), "WghtVar.glyphs")
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<&str>>()
        );
        assert_eq!(
            expected_keys,
            glyph_state_for_file(&glyphs3_dir(), "WghtVar_HeavyHyphen.glyphs")
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<&str>>()
        );
    }

    #[test]
    fn detect_changed_glyphs() {
        let keys = HashSet::from(["space", "hyphen", "exclam"]);

        let g1 = glyph_state_for_file(&glyphs3_dir(), "WghtVar.glyphs");
        let g2 = glyph_state_for_file(&glyphs3_dir(), "WghtVar_HeavyHyphen.glyphs");

        let changed = keys
            .iter()
            .filter_map(|key| {
                let key = key.to_string();
                if g1.get(&key).unwrap() == g2.get(&key).unwrap() {
                    return None;
                }
                Some(key)
            })
            .collect::<HashSet<String>>();
        assert_eq!(HashSet::from(["hyphen".to_string()]), changed);
    }
}
