//! Generic model of font sources.

use fontdrasil::coords::NormalizedLocation;

use crate::{
    error::Error,
    orchestration::{Flags, IrWork},
};

/// A source of data from which one could compile a font.
///
/// Expected to be implemented once per font format, e.g. one for .glyphs, one for ufo+ds, etc.
pub trait Source {
    /// Create a function that could be called to generate [crate::ir::StaticMetadata].
    ///
    /// When run work should update [crate::orchestration::Context] with new [crate::ir::StaticMetadata].
    fn create_static_metadata_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate [crate::ir::StaticMetadata].
    ///
    /// When run work should update[crate::orchestration::Context] with new [crate::ir::GlobalMetrics].
    fn create_global_metric_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate IR for glyphs.
    ///
    /// Batched because some formats require IO to figure out the work.
    /// Expected to return a Vec aligned with the glyph_names input. That is,
    /// result vec nth entry is the work for the nth glyph name.
    ///
    /// When run work should update [crate::orchestration::Context] with [crate::ir::Glyph] and [crate::ir::Anchor]
    /// for the glyph name.
    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, Error>;

    /// Create a function that could be called to generate or identify fea file(s).
    ///
    /// When run work should update [crate::orchestration::Context] with [crate::ir::FeatureSources].
    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to produce the kerning locations.
    ///
    /// When run work should update [crate::orchestration::Context] with [crate::ir::KerningLocations].
    fn create_kerning_locations_ir_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate or identify kerning for a location.
    ///
    /// When run work should update [crate::orchestration::Context] with [crate::ir::KerningInstance].
    fn create_kerning_instance_ir_work(&self, at: NormalizedLocation)
    -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate [crate::ir::ColorPalettes].
    ///
    /// When run work should update [crate::orchestration::Context] with new [crate::ir::ColorPalettes].
    fn create_color_palette_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate [crate::ir::ColorGlyphs].
    ///
    /// When run work should update [crate::orchestration::Context] with new [crate::ir::ColorGlyphs].
    fn create_color_glyphs_work(&self) -> Result<Box<IrWork>, Error>;

    /// Returns compilation flags derived from source file settings.
    ///
    /// CLI flags will be combined with these using bitwise OR.
    /// See <https://github.com/googlefonts/fontc/issues/1701>
    fn compilation_flags(&self) -> Flags {
        Flags::empty() // default: no flags from source
    }

    /// The names of the static instances, in source order.
    ///
    /// These are the names one can request, e.g. with fontc's `--instance`.
    /// Building one can still fail: fontc doesn't interpolate, so an instance
    /// between masters can't be built.
    fn instances(&self) -> Vec<String> {
        Vec::new()
    }
}

/// Find the instance named `requested` among `instances`.
///
/// Returns its index in `instances`, as [`Source::instances`] lists them; the
/// name must match exactly. The error says why there is no such instance: none
/// has that name, listing the ones there are, or several do.
pub fn find_instance(requested: &str, instances: &[String]) -> Result<usize, Error> {
    let invalid = |reason: String| {
        Err(Error::InvalidInstance {
            name: requested.to_string(),
            reason,
        })
    };
    let matches: Vec<_> = instances
        .iter()
        .enumerate()
        .filter(|(_, name)| *name == requested)
        .map(|(i, _)| i)
        .collect();
    match matches.as_slice() {
        [index] => Ok(*index),
        [] if instances.is_empty() => invalid("the source has no static instances".to_string()),
        [] => invalid(format!(
            "no instance has that name; available: {}",
            instances
                .iter()
                .map(|name| format!("\"{name}\""))
                .collect::<Vec<_>>()
                .join(", ")
        )),
        _ => invalid(format!(
            "{} instances have that name; rename or disable all but one",
            matches.len()
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn instances(names: &[&str]) -> Vec<String> {
        names.iter().map(|name| name.to_string()).collect()
    }

    #[test]
    fn finds_instance_by_exact_name() {
        let instances = instances(&["Example Regular", "Example Bold"]);
        assert_eq!(find_instance("Example Bold", &instances).unwrap(), 1);
        for (requested, reason) in [
            (
                "example bold",
                "no instance has that name; available: \"Example Regular\", \"Example Bold\"",
            ),
            (
                "Example",
                "no instance has that name; available: \"Example Regular\", \"Example Bold\"",
            ),
        ] {
            let error = find_instance(requested, &instances)
                .unwrap_err()
                .to_string();
            assert!(error.ends_with(reason), "{error}");
        }
    }

    #[test]
    fn no_or_several_instances_by_that_name() {
        let error = find_instance("A", &[]).unwrap_err().to_string();
        assert!(
            error.ends_with("the source has no static instances"),
            "{error}"
        );
        let error = find_instance("A", &instances(&["A", "B", "A"]))
            .unwrap_err()
            .to_string();
        assert!(
            error.ends_with("2 instances have that name; rename or disable all but one"),
            "{error}"
        );
    }
}
