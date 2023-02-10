//! 'glyf' Glyph binary compilation

use std::collections::{BTreeSet, HashMap, HashSet};

use fontdrasil::{orchestration::Work, types::GlyphName};
use fontir::{coords::NormalizedLocation, ir};
use kurbo::{Affine, BezPath, PathEl};
use log::{error, info, trace, warn};

use write_fonts::tables::glyf::SimpleGlyph;

use crate::{
    error::{Error, GlyphProblem},
    orchestration::{BeWork, Context},
};

struct GlyphWork {
    glyph_name: GlyphName,
}

pub fn create_glyph_work(glyph_name: GlyphName) -> Box<BeWork> {
    Box::new(GlyphWork { glyph_name })
}

impl Work<Context, Error> for GlyphWork {
    fn exec(&self, context: &Context) -> Result<(), Error> {
        trace!("BE glyph work for {}", self.glyph_name);

        let static_metadata = context.ir.get_static_metadata();
        let var_model = &static_metadata.variation_model;
        let default_location = var_model.default_location();
        let ir_glyph = &*context.ir.get_glyph_ir(&self.glyph_name);
        let glyph: CheckedGlyph = ir_glyph.try_into()?;

        // TODO refine (submodel) var model if glyph locations is a subset of var model locations

        // TODO do we want to write to ensure we don't lose interpolability?
        match glyph {
            CheckedGlyph::Composite {
                components,
                transforms,
            } => {
                warn!(
                    "'{}': composite glyphs not implemented yet; uses {:?} {:?}",
                    ir_glyph.name, components, transforms
                );
            }
            CheckedGlyph::Contour { contours } => {
                // Draw the default outline of our simple glyph
                let Some(path) = contours.get(default_location) else {
                    return Err(Error::GlyphError(ir_glyph.name.clone(), GlyphProblem::MissingDefault));
                };
                let base_glyph = SimpleGlyph::from_kurbo(path)
                    .map_err(|e| Error::KurboError(self.glyph_name.clone(), e))?;
                info!("'{}' base is '{}'", ir_glyph.name, path.to_svg());
                context.set_glyph(ir_glyph.name.clone(), base_glyph);
            }
        }

        Ok(())
    }
}

/// An [ir::Glyph] that has been confirmed to maintain invariants:
///
/// <ul>
///     <li>Components are consistent across the design space</li>
///     <li>Paths are interpolation compatible</li>
/// </ul>
enum CheckedGlyph {
    Composite {
        components: Vec<GlyphName>,
        transforms: HashMap<(GlyphName, NormalizedLocation), Affine>,
    },
    Contour {
        contours: HashMap<NormalizedLocation, BezPath>,
    },
}

impl TryFrom<&ir::Glyph> for CheckedGlyph {
    type Error = Error;

    fn try_from(glyph: &ir::Glyph) -> Result<Self, Self::Error> {
        // every instance must have consistent component glyphs
        let components: HashSet<BTreeSet<GlyphName>> = glyph
            .sources
            .values()
            .map(|s| s.components.iter().map(|c| c.base.clone()).collect())
            .collect();
        if components.len() > 1 {
            warn!("{} has inconsistent component glyph sequences; fontir is supposed to fix that for us", glyph.name);
            return Err(Error::GlyphError(
                glyph.name.clone(),
                GlyphProblem::InconsistentComponents,
            ));
        }

        // every instance must have consistent path element types
        let path_els: HashSet<String> = glyph
            .sources
            .values()
            .map(|s| {
                s.contours
                    .iter()
                    .map(|c| c.elements().iter().map(path_el_type).collect::<String>())
                    .collect()
            })
            .collect();
        if path_els.len() > 1 {
            return Err(Error::GlyphError(
                glyph.name.clone(),
                GlyphProblem::InconsistentPathElements,
            ));
        }
        let components = components.into_iter().next().unwrap_or_default();
        let path_els = path_els.into_iter().next().unwrap_or_default();
        trace!(
            "'{}' consistent: components '{:?}', paths '{}'",
            glyph.name,
            components,
            path_els
        );

        if !components.is_empty() && !path_els.is_empty() {
            warn!("{} has component *and* paths; fontir is supposed to fix that for us", glyph.name);
            return Err(Error::GlyphError(
                glyph.name.clone(),
                GlyphProblem::HasComponentsAndPath,
            ));
        }

        // TEMPORARY HACKERY; real fix is to cu2qu in an interpolation friendly manner
        if path_els.contains('C') {
            error!("'{}' outline discarded due to use of cubics", glyph.name);
            let contours = glyph
                .sources
                .keys()
                .map(|location| (location.clone(), BezPath::new()))
                .collect();
            return Ok(CheckedGlyph::Contour { contours });
        }

        // All is well, build the result
        Ok(if components.is_empty() {
            let contours = glyph
                .sources
                .iter()
                .map(|(location, instance)| {
                    if instance.contours.len() > 1 {
                        trace!(
                            "Merging {} contours to form '{}' at {:?}",
                            instance.contours.len(),
                            glyph.name,
                            location
                        );
                    }
                    let mut path = instance.contours.first().cloned().unwrap_or_default();
                    for contour in instance.contours.iter().skip(1) {
                        for el in contour.elements() {
                            path.push(*el);
                        }
                    }
                    (location.clone(), path)
                })
                .collect();
            CheckedGlyph::Contour { contours }
        } else {
            // Stable ordering is nice
            let mut components: Vec<_> = components.iter().cloned().collect();
            components.sort();

            let transforms = glyph
                .sources
                .iter()
                .flat_map(|(location, instance)| {
                    instance
                        .components
                        .iter()
                        .map(|c| ((c.base.clone(), location.clone()), c.transform))
                })
                .collect();
            CheckedGlyph::Composite {
                components,
                transforms,
            }
        })
    }
}

fn path_el_type(el: &PathEl) -> &'static str {
    match el {
        PathEl::MoveTo(..) => "M",
        PathEl::LineTo(..) => "L",
        PathEl::QuadTo(..) => "Q",
        PathEl::CurveTo(..) => "C",
        PathEl::ClosePath => "Z",
    }
}

struct GlyphMergeWork {}

pub fn create_glyph_merge_work() -> Box<BeWork> {
    Box::new(GlyphMergeWork {})
}

impl Work<Context, Error> for GlyphMergeWork {
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.get_static_metadata();

        error!(
            "TODO merge {} glyphs in glyph order => final result",
            static_metadata.glyph_order.len()
        );

        Ok(())
    }
}
