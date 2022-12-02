use std::collections::HashSet;
use std::path::Path;
use std::{collections::HashMap, fs, path::PathBuf};

use fontir::error::Error;
use fontir::source::{Input, Paths, Source, Work};
use fontir::stateset::StateSet;

use crate::openstep_plist::Plist;

pub struct GlyphsIrSource {
    glyphs_file: PathBuf,
    ir_paths: Paths,
    plist_cache: Option<PlistCache>,
}

impl GlyphsIrSource {
    pub fn new(glyphs_file: PathBuf, ir_paths: Paths) -> GlyphsIrSource {
        GlyphsIrSource {
            glyphs_file,
            ir_paths,
            plist_cache: None,
        }
    }
}

struct PlistCache {
    _global_metadata: StateSet,
    _root_dict: HashMap<String, Plist>,
}

fn glyph_identifier(glyph_name: &str) -> String {
    format!("/glyph/{glyph_name}")
}

fn read_glyphs(glyphs_file: &Path) -> Result<(HashMap<String, Plist>, String), Error> {
    let raw_plist = fs::read_to_string(glyphs_file).map_err(Error::IoError)?;
    let plist = Plist::parse(&raw_plist).unwrap();
    let Plist::Dictionary(root_dict, _) = plist else {
        return Err(Error::ParseError(glyphs_file.to_path_buf(), "Root is not a dict".to_string()));
    };
    Ok((root_dict, raw_plist))
}

fn glyphs(
    glyphs_file: &Path,
    root_dict: &HashMap<String, Plist>,
    raw_plist: &str,
) -> Result<HashMap<String, StateSet>, Error> {
    let Some(Plist::Array(raw_glyphs, _)) = root_dict.get("glyphs") else {
        return Err(Error::ParseError(glyphs_file.to_path_buf(), "No glyphs array".to_string()));
    };

    let mut glyphs = HashMap::new();
    for glyph in raw_glyphs {
        let Plist::Dictionary(glyph, range) = glyph else {
            return Err(Error::ParseError(glyphs_file.to_path_buf(), "Glyphs must be dicts".to_string()));
        };
        let Some(Plist::String(glyph_name, _)) = glyph.get("glyphname") else {
            return Err(Error::ParseError(glyphs_file.to_path_buf(), "No glyph name".to_string()));
        };
        let mut change_tracker = StateSet::new();
        change_tracker.track_slice(
            glyph_identifier(glyph_name),
            &raw_plist[range.start..range.end],
        )?;
        glyphs.insert(glyph_name.clone(), change_tracker);
    }

    Ok(glyphs)
}

impl GlyphsIrSource {
    // When things like upem may have changed forget incremental and rebuild the whole thing
    fn global_rebuild_triggers(
        &self,
        _root_dict: &HashMap<String, Plist>,
    ) -> Result<StateSet, Error> {
        todo!()
    }
}

impl Source for GlyphsIrSource {
    fn inputs(&mut self) -> Result<Input, Error> {
        // We have to read the glyphs file then shred it to figure out if anything changed
        let (root_dict, raw_plist) = read_glyphs(&self.glyphs_file)?;

        let glyphs = glyphs(&self.glyphs_file, &root_dict, &raw_plist)?;
        let global_metadata = self.global_rebuild_triggers(&root_dict)?;
        self.plist_cache = Some(PlistCache {
            _global_metadata: global_metadata.clone(),
            _root_dict: root_dict,
        });

        Ok(Input {
            global_metadata,
            glyphs,
        })
    }

    fn create_glyph_ir_work(
        &self,
        _glyph_names: &HashSet<&str>,
        _input: &Input,
    ) -> Result<Vec<Box<dyn Work<()>>>, fontir::error::Error> {
        todo!("TODO write glyph IR to {:#?}", self.ir_paths.glyph_ir_dir());
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        path::{Path, PathBuf},
    };

    use fontir::stateset::StateSet;

    use super::{glyphs, read_glyphs};

    fn testdata_dir() -> PathBuf {
        let dir = Path::new("../resources/testdata");
        assert!(dir.is_dir());
        dir.to_path_buf()
    }

    fn glyphs_in_file(filename: &str) -> HashMap<String, StateSet> {
        let glyphs_file = testdata_dir().join(filename);
        let (root_dict, raw_plist) = read_glyphs(&glyphs_file).unwrap();
        glyphs(&glyphs_file, &root_dict, &raw_plist).unwrap()
    }

    #[test]
    fn find_glyphs() {
        let expected_keys = HashSet::from(["space", "hyphen", "exclam"]);
        assert_eq!(
            expected_keys,
            glyphs_in_file("WghtVar.glyphs")
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<&str>>()
        );
        assert_eq!(
            expected_keys,
            glyphs_in_file("WghtVar_HeavyHyphen.glyphs")
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<&str>>()
        );
    }

    #[test]
    fn detect_changed_glyphs() {
        let keys = HashSet::from(["space", "hyphen", "exclam"]);

        let g1 = glyphs_in_file("WghtVar.glyphs");
        let g2 = glyphs_in_file("WghtVar_HeavyHyphen.glyphs");

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
